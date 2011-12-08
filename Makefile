
REBAR = ./rebar

.PHONY: all clean compile test

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

test: compile
	@$(REBAR) eunit
