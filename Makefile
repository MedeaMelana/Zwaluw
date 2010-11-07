default:
	ghci Zwaluw

configure:
	cabal configure

docs: configure
	cabal haddock

opendocs: docs
	open dist/doc/html/Zwaluw/index.html
