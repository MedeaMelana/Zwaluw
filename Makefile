default:
	ghci -XOverloadedStrings Example

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
