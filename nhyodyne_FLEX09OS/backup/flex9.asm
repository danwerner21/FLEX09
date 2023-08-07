        NAM     flex9.asm
        OPT     pag
        PAG
        PRAGMA  CD


        INCLUDE "init.asm"
        INCLUDE "spooler.asm"
        INCLUDE "flx29ccp.asm"
        INCLUDE "flx29fms.asm"
        INCLUDE "drivers.asm"
        INCLUDE "../software/monitor.asm"

        ORG     $FFF2                             ; SET RESET VECTOR TO MAIN PROGRAM
        FDB     SWIVEC
SW2VECP
        FDB     COLDS
FRQVECP
        FDB     COLDS
        FDB     IRQVEC
SW1VECP
        FDB     COLDS
NMIVECP
        FDB     COLDS
RESETV
        FDB     COLDS
