default:
	# Use either "make th" or "make regular"

th:
	ghci -XOverloadedStrings -Wall ExampleTH

regular:
	ghci -XOverloadedStrings -Wall ExampleRegular

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
