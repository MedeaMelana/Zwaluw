default:
	# Use either "make th" or "make regular"

th:
	ghci -XOverloadedStrings ExampleTH

regular:
	ghci -XOverloadedStrings ExampleRegular

clean:
	cabal clean

configure:
	cabal configure

docs: configure
	cabal haddock

install:
	cabal install

opendocs: docs
	open dist/doc/html/Zwaluw/index.html
