update:
	-eask install
	-eask upgrade
	-rm lisp/*.elc
	-rm inits/*.elc
	-eask compile

compile:
	-rm lisp/*.elc
	-rm inits/*.elc
	-eask compile
