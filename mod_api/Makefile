# A simple Makefile
# ERLC_FLAGS=-DNOTEST
ERLC_FLAGS=
SOURCES=$(wildcard src/*.erl)
OBJECTS=$(SOURCES:src/%.erl=ebin/%.beam)

all: $(OBJECTS) test

ebin/%.beam: src/%.erl
	erlc $(ERLC_FLAGS) -smp -v -o ebin/ -I include $<
clean:
	-rm -f ebin/*.beam

test:
	-../apisrv.sh start
	erl $(ERLC_FLAGS) -noshell -pa ebin \
	-eval 'eunit:test("ebin",[verbose])' \
	-s init stop

release: clean
	$(MAKE) ERLC_FLAGS="$(ERLC_FLAGS) -DNOTEST"
