REBAR=./rebar

all: deps compile

deps: get-deps update-deps

compile:
	$(REBAR) compile

get-deps:
	$(REBAR) get-deps

update-deps:
	$(REBAR) update-deps

console:
	erl -pa deps/*/ebin -pa deps/*/include -pa ebin

eunit:
	$(REBAR) eunit skip_deps=true

ct:
	$(REBAR) ct skip_deps=true
