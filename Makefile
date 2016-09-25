.PHONY:	all compile get-deps eunit clean

all: get-deps compile check

compile:
	rebar compile

get-deps:
	rebar get-deps

check:
	dialyzer --src src

eunit:
	rebar eunit skip_deps=true

clean:
	rebar clean
	$(RM) ebin/*.d
