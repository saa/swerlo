-module(swerlo).

-export([auth/3]).
-export([head_storage/1]).
-export([get_containers/1]).
-export([get_containers/2]).
-export([put_container/2]).
-export([put_container/3]).
-export([head_container/2]).
-export([delete_container/2]).
-export([get_objects/2]).
-export([get_objects/3]).
-export([put_object/4]).
-export([put_object/5]).

-record(state, {storage_url       :: binary(),
                auth_token        :: binary(),
                expire_auth_token :: integer(),
                auth_user         :: binary(),
                auth_key          :: binary(),
                auth_url          :: binary(),
                auth_header       :: list()
               }).

-type format() :: json | xml.

-define(DEFAULT_FORMAT, json).

%% ===================================================================
%% API functions
%% ===================================================================

auth(URL, User, Key) ->
    Headers = [{<<"x-auth-user">>, User}, {<<"x-auth-key">>, Key}],
    case hackney:get(URL, Headers) of
        {ok, 204, RespHeaders, _ClientRef} ->
            AuthToken = proplists:get_value(<<"X-Auth-Token">>, RespHeaders),
            State = #state{storage_url = proplists:get_value(<<"X-Storage-Url">>, RespHeaders),
                           auth_token = AuthToken,
                           expire_auth_token = binary_to_integer(proplists:get_value(<<"X-Expire-Auth-Token">>, RespHeaders)),
                           auth_user = User,
                           auth_key = Key,
                           auth_url = URL,
                           auth_header = [{<<"X-Auth-Token">>, AuthToken}]
                          },
            {ok, State};
        {ok, Code, RespHeaders, _ClientRef} ->
            {error, Code, RespHeaders}
    end.

head_storage(State) ->
    case hackney:head(State#state.storage_url, State#state.auth_header) of
        {ok, 204, RespHeaders, _ClientRef} ->
            {ok, RespHeaders, State};
        {ok, Code, RespHeaders, _ClientRef} ->
            {error, Code, RespHeaders}
    end.

get_containers(State) ->
    get_containers(State, [{format, ?DEFAULT_FORMAT}]).

get_containers(State, Opts) ->
    ReqUrl = iolist_to_binary([State#state.storage_url, constract_query(Opts)]),
    case hackney:get(ReqUrl, State#state.auth_header) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, decode_resp_body(proplists:get_value(format, Opts), RespBody), State};
        {ok, Code, RespHeaders, _ClientRef} ->
            {error, Code, RespHeaders}
    end.

put_container(Container, State) when is_binary(Container) ->
    put_container(Container, [], State).

put_container(Container, Headers, State) when is_binary(Container) ->
    Headers2 = lists:merge(Headers, State#state.auth_header),
    ReqUrl = iolist_to_binary([State#state.storage_url, "/", Container]),
    case hackney:put(ReqUrl, Headers2) of
        {ok, Code, _RespHeaders, _ClientRef} when Code == 201; Code == 202 ->
            {ok, State};
        {ok, Code, RespHeaders, _ClientRef} ->
            {error, Code, RespHeaders}
    end.

head_container(Container, State) when is_binary(Container) ->
    ReqUrl = iolist_to_binary([State#state.storage_url, "/", Container]),
    case hackney:head(ReqUrl, State#state.auth_header) of
        {ok, 204, RespHeaders} ->
            {ok, RespHeaders, State};
        {ok, Code, RespHeaders, _ClientRef} ->
            {error, Code, RespHeaders}
    end.

delete_container(Container, State) when is_binary(Container) ->
    ReqUrl = iolist_to_binary([State#state.storage_url, "/", Container]),
    case hackney:delete(ReqUrl, State#state.auth_header) of
        {ok, 204, _RespHeaders, _ClientRef} ->
            {ok, State};
        {ok, 404, _RespHeaders, _ClientRef} ->
            {error, container_not_found};
        {ok, 409, _RespHeaders, _ClientRef} ->
            {error, container_no_empty}
    end.

get_objects(Container, State) when is_binary(Container) ->
    get_objects(Container, State, [{format, ?DEFAULT_FORMAT}]).

get_objects(Container, State, Opts) ->
    ReqUrl = iolist_to_binary([State#state.storage_url, "/", Container, constract_query(Opts)]),
    case hackney:get(ReqUrl, State#state.auth_header) of
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, RespBody} = hackney:body(ClientRef),
            {ok, decode_resp_body(proplists:get_value(format, Opts), RespBody), State};
        {ok, Code, RespHeaders, _ClientRef} ->
            {error, Code, RespHeaders}
    end.

put_object(Name, Data, Container, State) when is_binary(Data), is_binary(Container) ->
    put_object(Name, Data, Container, [], State).

put_object(Name, Data, Container, Headers, State) when is_binary(Data), is_binary(Container) ->
    Headers2 = lists:merge(Headers, State#state.auth_header),
    Headers3 = [{<<"ETag">>, etag(Data)}, {<<"Content-Length">>, size(Data)} | Headers2],
    ReqUrl = iolist_to_binary([State#state.storage_url, "/", Container, "/", Name]),
    case hackney:put(ReqUrl, Headers3, Data) of
        {ok, 201, _RespHeaders, _ClientRef} ->
            {ok, State};
        {ok, 422, _RespHeaders, _ClientRef} ->
            {error, bad_md5};
        {ok, 404, _RespHeaders, _ClientRef} ->
            {error, container_not_found}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec etag(Data) -> ETag when
        Data :: binary(),
        ETag :: binary().

etag(Data) when is_binary(Data) ->
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= crypto:hash(md5, Data)])).

-spec constract_query(Opts) -> Query when
        Opts  :: list(),
        Query :: binary().

constract_query(Opts) ->
    L = [iolist_to_binary([to_binary(Key), "=", to_binary(Value)]) || {Key, Value} <- Opts],
    constract_query(L, <<"">>).

-spec constract_query(Opts, Acc) -> Acc2 when
        Opts :: list(),
        Acc  :: binary(),
        Acc2 :: binary().

constract_query([], Acc) ->
    Acc;
constract_query([Head | []], Acc) ->
    <<"?", Acc/binary, Head/binary>>;
constract_query([Head | Tail], Acc) ->
    constract_query(Tail, <<Head/binary, "&", Acc/binary>>).

-spec to_binary(Value) -> Binary when
        Value  :: integer() | list() | atom() | binary(),
        Binary :: binary().

to_binary(Value) when is_integer(Value) ->
    integer_to_binary(Value);
to_binary(Value) when is_list(Value) ->
    list_to_binary(Value);
to_binary(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
to_binary(Value) when is_binary(Value) ->
    Value.

-spec decode_resp_body(Format, RespBody) -> Result when
        Format   :: format(),
        RespBody :: binary(),
        Result   :: list() | binary().

decode_resp_body(json, RespBody) ->
    case jsx:decode(RespBody) of
        [] -> [];
        [Body] -> Body
    end;
decode_resp_body(xml, RespBody) ->
    RespBody.
