all: convert-coordinates

convert-coordinates: convert-coordinates.asd convert-coordinates.lisp
	sbcl --non-interactive --eval '(require :asdf)' --eval '(asdf:load-asd (truename "convert-coordinates.asd"))' --eval '(asdf:make "convert-coordinates")'

clean:
	rm -f convert-coordinates
