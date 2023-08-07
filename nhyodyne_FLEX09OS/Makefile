ASM=lwasm

ALL:    flex09.com        \
        flex.sys        \
        loader.s19

flex9.s19:    flex9.asm spooler.asm init.asm flx29ccp.asm flx29fms.asm drivers.asm ../software/monitor.asm flexidedrv.asm
	$(ASM)  --6809 --6800compat flex9.asm -s --list=flex9.lst --map=flex9.map --output=flex9.s --format=srec
	srec_cat  flex9.s -o flex9.s19 -Motorola -Data_Only

loader.s19:    loader.asm
	$(ASM)  --6809 --6800compat loader.asm -s --list=loader.lst --map=loader.map --output=loader.s --format=srec
	srec_cat  loader.s -o loader.s19 -Motorola -Data_Only

flex.sys:   flex9.s19
	srec_cat  flex9.s19 -offset -0xC400 -o flex.sys -binary
	truncate flex.sys -s 15360
	@cp flex.sys ../bin/nhyodyne

flex09.com:   loader.s19 flex9.s19
	cat loader.s19 flex9.s19 >flex.s
	srec_cat  flex.s -offset -0xC3E0 -o flex09.com -binary
	truncate flex09.com -s 15392
	@cp flex09.com ../bin/nhyodyne

pretty:
	cp *.asm ./backup
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/fdskdrvr.asm fdskdrvr.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/flexflpy.asm flexflpy.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/flx29ccp.asm flx29ccp.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/flxdsky.asm flxdsky.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/loader.asm loader.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/spooler.asm spooler.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/drivers.asm drivers.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/flex9.asm flex9.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/flexidedrv.asm flexidedrv.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/flx29fms.asm flx29fms.asm
	pretty6502 -p1 -a0 -du -mu -c50 -l ./backup/init.asm init.asm

clean:
	@rm -f *.bin
	@rm -f *.o
	@rm -f *.out
	@rm -f *.com
	@rm -f *.co6
	@rm -f *.map
	@rm -f *.lst
	@rm -f *.hex
	@rm -f *.s
	@rm -f *.sys
	@rm -f *.s19

cleandust:
	@rm -f *.o
	@rm -f *.s
	@rm -f *.out
	@rm -f *.map
	@rm -f *.lst