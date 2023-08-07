all:  	assist.com


assist.com: assist.asm
	lwasm --6809 assist.asm -s --list=assist.lst --map=assist.map --output=assist.s --format=srec
	srec_cat  assist.s -exclude 0x0000 0xF000 -offset -0x8000 -o  assist.hex  -intel -address-length=2
	srec_cat  assist.s -offset -0xEFE0 -o assist.com -binary
	@rm assist.s
	truncate assist.com -s 4130
	@cp assist.com ../bin/nhyodyne

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/assist.asm assist.asm

clean:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.co6
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s19
	@rm -f *.s

cleandust:
	@rm -f *.o
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst