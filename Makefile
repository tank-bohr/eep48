.PHONY: all clean console compile docs

LIBS=_build/default/lib
INDEX=doc/index.html

all: clean compile docs

clean:
	rm -rf doc priv/ebin/*.beam

console:
	iex -S mix

docs: $(INDEX)
	open $(INDEX)

compile:
	rebar3 compile
	erl -noshell -env ERL_LIBS $(LIBS) -s eep48 edocc -s init stop

$(INDEX):
	mix deps.get
	mix docs
