include Makefile.base

.PHONY: exe
exe: build
	stack exec -- prop-unit-exe
