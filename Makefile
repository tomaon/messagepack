#
 ELIXIR_HOME ?= /opt/elixir/release/latest
 ERLANG_HOME ?= /opt/erlang/release/latest

 IEX ?= iex
 MIX ?= mix
 REBAR ?= rebar3

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

clean compile deps.get docs test: rm
	$(ENV) $(MIX) $@

cover: rm
	$(ENV) $(MIX) test --$@

distclean: rm-deps rm-doc rm-lock
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
rm-deps:
	rm -rf deps
rm-doc:
	rm -rf doc
rm-lock:
	rm -f *.lock
