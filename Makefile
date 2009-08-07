all:
	runhaskell Setup.lhs build

clean:
	runhaskell Setup.lhs clean

configure:
	runhaskell Setup.lhs configure

install:
	sudo runhaskell Setup.lhs install
