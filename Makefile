.PHONY: all clean console docs
INDEX=doc/index.html

all: clean docs

clean:
	rm -rf doc

console:
	iex -S mix

docs: $(INDEX)
	open $(INDEX)

$(INDEX):
	mix docs
