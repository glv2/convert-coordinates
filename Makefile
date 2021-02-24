all: convert-coordinates.asd convert-coordinates.lisp
	sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:make "convert-coordinates")'
