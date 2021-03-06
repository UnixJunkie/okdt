.PHONY: test install doc uninstall clean

all:
	jbuilder build @install

test:
	jbuilder build test.exe
	./_build/default/test.exe

install: all
	jbuilder install

doc:
	mkdir -p doc
	ocamldoc -html -d doc kd_tree.mli

uninstall:
	jbuilder uninstall

clean:
	rm -rf _build
