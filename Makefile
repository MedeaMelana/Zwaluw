default:
	ghci Zwaluw

clean:
	rm -rf dist

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Zwaluw/index.html
