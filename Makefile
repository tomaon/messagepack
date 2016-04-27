#
 ELIXIR_HOME ?= /opt/elixir/release/latest
 ERLANG_HOME ?= /opt/erlang/release/latest

 IEX ?= iex
 MIX ?= mix
 REBAR ?= ../../../bin/rebar3

 ENV  =
#ENV += MIX_ENV=prod
 ENV += PATH=$(ELIXIR_HOME)/bin:$(ERLANG_HOME)/bin:$(PATH)

 WORK = .mix .rebar3

#
default: compile

#
.PHONY: test

$(VERBOSE).SILENT:

all: clean compile

clean compile test: rm
	$(ENV) $(MIX) $@

cover: rm
	$(ENV) $(MIX) test --$@

distclean: rm-lock
	rm -rf $(WORK)

#
iex:
	$(ENV) $(IEX) -S mix

#
dialyzer:
	$(ENV) $(REBAR) $@

#
rm: rm-autosave

rm-autosave:
	find . -name "*~" | xargs rm -f
rm-lock:
	rm -f *.lock
