CA65="../../cc65/bin/ca65"
LD65="../../cc65/bin/ld65"

CAOPTS=-g
LDOPTS=

.PHONY: clean all

all: hello.nes

hello.nes: hello.o ppu.o joy.o ../inc/nrom.cfg 
	${LD65} ${LDOPTS} -o hello.nes -C ../inc/nrom.cfg -m hello.map.txt \
		-Ln hello.labels.txt --dbgfile hello.nes.dbg \
		hello.o ppu.o joy.o

hello.o: hello.s westminster-ascii.chr ppu.inc joy.inc ines.inc
	${CA65} ${CAOPTS} -o hello.o hello.s

ppu.o: ppu.s
	${CA65} ${CAOPTS} -o ppu.o ppu.s

joy.o: joy.s
	${CA65} ${CAOPTS} -o joy.o joy.s

clean:
	rm -f hello.nes hello.o ppu.o joy.o *.map.txt *.labels.txt *.nes.dbg
