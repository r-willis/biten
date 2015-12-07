REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)

REBAR_FLAGS ?=

all: compile

compile:
	$(REBAR) compile $(REBAR_FLAGS)

check: test

test: compile
	$(REBAR) eunit $(REBAR_FLAGS)

clean:
	$(REBAR) clean $(REBAR_FLAGS)

dialyzer:
	$(REBAR) dialyze $(REBAR_FLAGS)
