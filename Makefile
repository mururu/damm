.PHONY: all compile clean xref eunit dialyzer

all: compile xref eunit dialyzer

compile:
	@./rebar3 compile

xref:
	@./rebar3 xref

clean:
	@./rebar3 clean

eunit:
	@./rebar3 eunit

dialyzer:
	@./rebar3 dialyzer
