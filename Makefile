all: convert-coordinates convert-coordinates-cli

convert-coordinates: convert-coordinates.asd convert-coordinates.lisp
	sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-asd (truename "convert-coordinates.asd"))' --eval '(asdf:make "convert-coordinates")'

convert-coordinates-cli: convert-coordinates-cli.asd convert-coordinates-cli.lisp
	sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-asd (truename "convert-coordinates-cli.asd"))' --eval '(asdf:make "convert-coordinates-cli")'

clean:
	rm -f convert-coordinates convert-coordinates-cli
