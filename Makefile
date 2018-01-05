.PHONY:	all compile get-deps eunit clean

all: get-deps compile check

compile:
	rebar3 compile

get-deps:
	rebar3 get-deps

check:
	dialyzer --src src

eunit:
	rebar3 eunit skip_deps=true

clean:
	rebar3 clean
	$(RM) ebin/*.d
