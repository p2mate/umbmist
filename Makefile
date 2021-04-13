all: umbmist.sys

clean:
	-rm *.o *.sys

umbmist.sys: umbmist.asm
	wasm umbmist.asm
	wlink NAME umbmist.sys FILE umbmist.o FORMAT DOS COM
