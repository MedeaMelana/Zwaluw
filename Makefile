default:
	ghci -XOverloadedStrings -ddump-splices Example

clean:
	cabal clean

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Zwaluw/index.html
