*
* assemble with as09, build binary
* as09 -inxsl FLEX9.asm
* srec_cat FLEX9.s19 -offset - -minimum-addr FLEX9.s19 -o FLEX.SYS -binary
*

		include "SPOOLER.asm"
		include "INIT.asm"
		include "FLX29CCP.asm"
		include "FLX29FMS.asm"
		include "DRIVERS.asm"
		include "prop_io.asm"
		include "rtc_driver.asm"
