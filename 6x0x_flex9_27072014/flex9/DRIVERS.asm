;                NAM     DRIVERS 
                page

* ---------------------------------------------------------------------------
* File Name   : DRIVERS.ASM
* Format      : AS09
*
* Processor:       6809 []
* Target assebler: Public domain 6809 assembler v2.01 (OS9 support)
*
*

SCRATCH3        equ     $CC30
CPUFLAG         equ     $CC33
SPLFLG          equ     $CC34           ;spooling active flag
;WARMS           equ     $CD03

MONTH           equ     $CC0E
DAY             equ     $CC0F
YEAR            equ     $CC10

HOUR            equ     $D370               
MINUTE          equ     $D371
SECOND          equ     $D372
TICK            equ     $D373

SWIVEC          equ     $DFC2
IRQVEC          equ     $DFC8

;ACIADD          equ     $DFE0

;ACIAS           equ     $E004
;MPT             equ     $E042
;MPLPIA          equ     $E070

;OUTCHAR           equ    $F9c9 ;$FAEB
;INCHEK          equ     $F808
;INCHE           equ     $F806
;INCHAR            equ    $FACF
UART1			equ 	$ef04
PTM				equ     $e808
VIA0			equ		$ec00
*
* CONSOLE I/O DRIVER VECTOR TABLE
*
                org     $D3E1       ;* TABLE STARTS AT $D3E1
                                    ;
ADDDEVP         fdb     ADDDEV      ;* add an IRQ handler to table
                fdb     DELDEV      ;* delete an IRQ handler from table
                                    ;
INCHNEP         fdb     INCHNE_IMPL      ;* ($E86F) INPUT CHARACTER W/O ECHO
IHNDLRP         fdb     IHNDLR      ;* ($E873) IRQ INTERRUPT HANDLER
SWIVECP         fdb     SWIVEC      ;* ($DFC2) SWI3 VECTOR LOCATION
IRQVECP         fdb     IRQVEC      ;* ($DFC8) IRQ VECTOR LOCATION
TMOFFP          fdb     TMOFF       ;* ($E8D6) TIMER OFF ROUTINE
TMONP           fdb     TMON        ;* ($E8D0) TIMER ON ROUTINE
TMINTP          fdb     TMINT       ;* ($E8B4) TIMER INITIALIZATION
MONITR          fdb     MONITOR     ;* ($E844) MONITOR ENTRY ADDRESS
TINITP          fdb     TINIT       ;* ($E850) TERMINAL INITIALIZATION
TRMCHK          fdb     CONSTAT_IMPL ;STAT        ;* ($E867) CHECK TERMINAL STATUS
TIMOUT          fdb     CONOUT_IMPL ;VOUTCH      ;* ($E863) OUTPUT CHARACTER
TIMINE          fdb     CONIN_IMPL ;VINCH       ;* ($E85A) INPUT CHARACTER W/ ECHO

*
* DISK DRIVER ROUTINE JUMP TABLE
*
                org     $DE00

DREAD           jmp     >READ       ;* DE00 7E E955   READ      Read a single sector
DWRITE          jmp     >WRITE      ;* DE03 7E E9B9   WRITE     Write a single sector
DVERFY          jmp     >VERIFY     ;* DE06 7E EA21   VERIFY    Verify last sector written
DRESTOR         jmp     >RESTORE    ;* DE09 7E EAE8   RESTORE   Restore head to track #0
DDRIVE          jmp     >DRIVE      ;* DE0C 7E EB00   DRIVE     Select the specified drive
DCHECK          jmp     >CHKRDY     ;* DE0F 7E EB42   CHKRDY    Check for drive ready
DQUICK          jmp     >QUICK      ;* DE12 7E EB82   QUICK     Quick check for drive ready
DINIT           jmp     >INITIAL       ;* DE15 7E EAD2   INIT      Driver initialize (cold start)
DWARM           jmp     >WARM       ;* DE18 7E EADD   WARM      Driver initialize (warm start)
DSEEK           jmp     >SEEKIT     ;* DE1B 7E EA36   SEEK      Seek to specified track

DRVTBL          fcb     $00
                fcb     $00
                fcb     $00
                fcb     $00

                fcb     $00,$00,$00,$00
                fcb     $00,$00,$00,$00

* ---------------------------------------------------------------------------

PPORT           fdb     VIA0                ;* address of device
                fdb     0                   ;* IRQ handler address
IRQBM           fcb     0                   ;* bit mask for IRQ flag

* --------------------------------------------------------------------

IRQLL           fdb     0                   ;pointer to first entry in the table
                fcb     1                   

TAPPTR          fdb     0                   ;no terminal input redirection
* --------------------------------------------------------------------

TIMER           fdb     PTM+1               ;* where we can find device status byte
                fdb     IHND                ;* pointer to Interrupt handler
                fdb     0                   ;* pointer to next device in chain
                fcb     $80                 ;* mask for testing Interrupt occured
* --------------------------------------------------------------------
* disk driver temp. storage

CURTRK:		db $00
CURSEC:		db $00
CURDRV:		db $00
CURSSE:		db $00
CURSECO:	db $00
TEMPA:		db $00
DMAAD:      dw $0000

*---------------------------------------------------------------------
* date time functions temp storage
* 
RTCMONTH          fcb    $1F
RTCDATE           fcb    $3F
RTCYEAR           fcb    $7F
RTCHOURS          fcb    7
RTCMINUTES        fcb    3
RTCSECONDS        fcb    1
RTCDAY            fcb    $F

LASTCHAR       db 0
* --------------------------------------------------------------------
* Beginning of rom, drivers are in rom
                org $f000
BOOTFLEX        jmp FLEXBOOT
* ---------------------------------------------------------------------------
*       Printer port initialization

OPNPRT          rts						; no printer on N8VEM

* ---------------------------------------------------------------------------
*       Close the printer device (issue a <CR>)

CLSPRT          lda     #$D

* ---------------------------------------------------------------------------
*       Output a character to the printer

OUTPRT          rts						; no printer on N8VEM

* ---------------------------------------------------------------------------
*       check for printer ready

CHKPRT          rts						; no printer on N8VEM



* ---------------------------------------------------------------------------
*       Monitor entry routine

MONITOR         lda     #$A
                bsr     VOUTCH              ;* output character
                lda     #$D                 ;
                bsr     VOUTCH              ;* output character
                swi                         ;* enter monitor
                fcb		8					     ; assist09
                jmp     WARMS               ;* re-enter FLEX gracefully

* ---------------------------------------------------------------------------
*       Terminal init routine

TINIT           ;pshs    a
                ;lda     #$11
                ;sta     [ACIADD]
                ;puls    pc,a
                jsr     prop_init
                rts

* ---------------------------------------------------------------------------
*       Terminal input routine

VINCH           tst     >TAPPTR             ;* allow redirection of input from terminal
                beq     TREDIR              ;* redirection not required
                jsr     [TAPPTR]            ;* redirect to alternate routine

* ---------------------------------------------------------------------------
*       Terminal output routine
* --------------------------------------------------------
VOUTCH    jmp      OUTCHAR                 
* --------------------------------------------------------
*       Terminal status check routine
                 
STAT_      jmp      TRSCHK_
                 
* --------------------------------------------------------
*       Monitor terminal input character with echo
                 
TREDIR    jmp      INCHE_
                 



* ---------------------------------------------------------------------------
*       Terminal status check routine
TRSCHK_     pshs	a
				lda     UART1+1                 ; GET STATUS 
				bita    #%00001000              ; IS RX READY
				bne		TRSCHK1
				puls	a
				orcc	#$04					; set zero flag
				rts
TRSCHK1			puls	a
				andcc	#$fb					; clear zero
				rts
				

* ---------------------------------------------------------------------------
*       Monitor terminal input character with echo
INCHE_		jsr      INCHNE_
				jmp	   OUTCHAR

* ---------------------------------------------------------------------------
*       Monitor terminal input character without echo
INCHNE_     jsr      INCHAR
				bcc		INCHNE_
				rts
				
INCHAR		lda UART1+1		;,u ;LOAD STATUS REGISTER
				lsra
				lsra
				lsra
				lsra ;TEST RECIEVER REGISTER FLAG
				bcc CIRTN ;RETURN IF NOTHING
        
				lda UART1   ;0,u ;LOAD DATA BYTE
CIRTN 			rts ;RETURN TO CALLER
* ---------------------------------------------------------------------------
*       Monitor terminal output character 
				
OUTCHAR pshs b       ;SAVE REGISTERS,WORK BYTE
CODTAO  ldb UART1+1             ;LOAD ACIA CONTROL REGISTER
        bitb #%00010000      ;           ? TX REGISTER CLEAR >LSAB FIXME
        beq CODTAO           ;     RELEASE CONTROL IF NOT
        
        sta UART1              ;     STORE INTO DATA REGISTER
                             ;     RETURN TO CALLER
CODTRT  puls b      ;RESTORE REGISTERS AND RETURN
		rts

				
* ---------------------------------------------------------------------------
* Interupt handler

IHNDLR          ldu     IRQLL-4             ; leau    <IRQLL-4,pc         ;get address of IRQ handler linked list
CHKNXT          ldu     4,u                 ;get a link
                beq     IHDONE              ;end of list
                                            ;
                lda     [,u]                ;get status from device
                bita    6,u                 ;did it interrupt?
                beq     CHKNXT              ;no - check next
                                            ;
                ldy     ,u                  ;yes - get address of device in Y
                jmp     [2,u]               ;go to device ISR
                                            ;
IHDONE          rti                         ;return from interrupt

* ---------------------------------------------------------------------------
*       add a device to IRQ handler table

ADDDEV          pshs    x,y,u               
                bsr     SRCHLST             ;find link in chain
                beq     DEVARE              ;already exists - exit
                stu     4,x                 ;set link pointer in new ctl blk
                stx     4,y                 ;link this one into prev blk
DEVARE          puls    pc,u,y,x            

* ---------------------------------------------------------------------------
*       delete a device from IRQ handler table

DELDEV          pshs    x,y,u
                bsr     SRCHLST             ;find link in chain
                bne     NODEV               ;not found - exit
                ldx     4,x                 ;get link from ctl blk to delete
                stx     4,u                 ;set in prev block
NODEV           puls    pc,u,y,x            

* ---------------------------------------------------------------------------
*       search a linked list
*
*               entry: X = address of ISR control block
*               exit:  Y = address of link pointer
*                     CC = Z bit set if already linked
*
*               format of ISR control block:
*
*                       offset  description
*                         0     address of I/O device
*                         2     address of ISR
*                         4     link to next ISR control block
*                         6     bit mask for determining IRQ

SRCHLST         ldu      IRQLL-4            ;  leau    <IRQLL-4,pc
SRCHLP          leay    ,u
                cmpx    4,u                 ;X = link?
                beq     FNDENT              ;yes - return pointer in U
                                            ;
                ldu     4,u                 ;no - get link in U
                bne     SRCHLP              ;not end of chain - loop
                andcc   #$FB                ;clear Z flag in CC
                                            ;
FNDENT          rts                         ;return - not found


* --------------------------------------------------------------------
*       Timer routines for MPT
*
*       timer initialize - install the handler into the chain

TMINT           ;ldx     #MPT                ;point to hardware
                ;lda     #$FF                ;set up the port direction
                ;sta     ,x
                ;lda     #$3C
                ;sta     1,x
                ;lda     #$8F                ;this turns it off so it does
                ;sta     ,x                  ;not fire before the handler
                ;lda     ,x                  ;get's installed
                ;lda     #$3D
                ;sta     1,x

*               point X at address of timer handler control block

                ldx     #TIMER
                jmp     [ADDDEVP]             ;Add it to the linked list of handlers


* --------------------------------------------------------------------
*       timer on
TMON            ;lda     #4                  ;10 millisecond ticks
                ;sta     MPT                 ;enable the timer
                rts

* --------------------------------------------------------------------
*       timer off

TMOFF           ;lda     #$8F                ;disable the timer
                ;sta     MPT
                rts
* --------------------------------------------------------------------
*
*   Interrupt handler for the TIMER irq
    
IHND            lda     PTM                 ;kill IRQ flag in MPT
                inc     TICK                ;bump tick counter
                lda     TICK                ;
                cmpa    #100                ;100 ticks?
                bne     NOUP                ;no - skip to spooler
                                            ;
                clr     TICK                ;yes - reset tick counter
                inc     SECOND              ;- increment second counter
                lda     SECOND              ;see if overflow
                cmpa    #60                 ;
                bne     NOUP                ;no - don't update minutes
                                            ;
                clr     SECOND              ;yes - reset seconds
                inc     MINUTE              ;increment minute
                lda     MINUTE              ;
                cmpa    #60                 ;see if overflow
                bne     NOUP                ;no - don't update hour
                                            ;
                clr     MINUTE              ;yes - reset minute
                inc     HOUR                ;increment hours
                lda     HOUR                ;
                cmpa    #24                 ;overflow?
                bne     NOUP                ;no - don't update day
                                            ;
                clr     HOUR                ;yes - reset hours
                inc     DAY                 ;increment system day
                lda     DAY                 ;get the day
                leax    MONTHDAYS,pc        ;point to days per month table
                ldb     MONTH               ;get month in b for offset
                decb                        ;make zero based
                leax    b,x                 ;set X to point to correct entry
                inca                        ;
                cmpa    ,x                  ;
                bne     NOUP                ;not time to change month yet
                                            ;
                lda     #1                  ;reset the day to the first
                sta     DAY                 ;
                inc     MONTH               ;bump the month
                lda     MONTH               ;get the month value
                cmpa    #13                 ;see if overflow
                bne     NOUP                ;no
                                            ;
                lda     #1                  ;set back to January
                sta     MONTH               ;
                inc     YEAR                ;and bump to next year
                                            ;
NOUP            jmp     $C700               ;go to spooler code
                                            ;
MONTHDAYS       fcb     31                  ;Jan
                fcb     28                  ;Feb
                fcb     31                  ;Mar
                fcb     30                  ;Apr
                fcb     31                  ;May
                fcb     30                  ;Jun
                fcb     31                  ;Jul
                fcb     31                  ;Aug
                fcb     30                  ;Sep
                fcb     31                  ;Oct
                fcb     30                  ;Nov
                fcb     31                  ;Dec


*   READ    This routine reads the specified sector into memory at the
*           specified address. This routine should perform a seek
*           operation if necessary. A sector is 256 bytes in length.
*
*           ENTRY - (X) = Address in memory where sector is to be placed.
*                   (A) = Track Number
*                   (B) = Sector Number
*
*           EXIT -  (X) May be destroyed
*                   (A) May be destroyed
*                   (B) = Error condition
*                   (Z) = 1 if no error
*                       = 0 if an error

READ            stx     DMAAD			; store dma addr
				stb		CURSEC			; STORE ORIGINAL SECTOR
				sta		CURTRK			; STORE TRACK	

				bsr 	recalc_sector
				jsr     PROP_READ_SD
				cmpb	#$FF			; IS ERROR?
				beq		DRDSECA			; YES, ABORT	
				clrb	     			; NO ERROR		
				;jmp $f82a		
				rts

DRDSECA:
				ldb		#01				; RETURN ERROR CODE	
				rts


;*********************************************
; recalculate sector number
;*          (A) = TRACK NUMBER
;*          (B) = SECTOR NUMBER

recalc_sector:
	sta		CURTRK			; STORE TRACK	
    beq     rs_0_track  	; Track 0?
    decb                	; All of the other tracks don't have a sector 0
    bra     rs1
rs_0_track:
    tstb					; sector0 ?
    beq  	rs1    		;  
    decb					; Track 0 does not have a sector 1    
rs1:
    stb		CURSEC			; store recalculated sector
	rts

                rts

*   WRITE   This routine writes the information from the specifed memory
*           buffer area to the disk sector specified. This routine should
*           perform a seek operation if necessary. A sector is 256 bytes
*           in length.
*
*           ENTRY - (X) = Address of 256 memory buffer containing data
*                         to be written to disk
*                   (A) = Track Number
*                   (B) = Sector Number
*
*           EXIT -  (X) May be destroyed
*                   (A) May be destroyed
*                   (B) = Error condition
*                   (Z) = 1 if no error
*                       = 0 if an error

WRITE           stx     DMAAD			; store dma addr
				sta	CURTRK			; STORE TRACK	
				stb	CURSEC			; STORE ORIGINAL SECTOR
				bsr	    recalc_sector
DWRSEC1	
				jmp     PROP_WRITE_SD
DWRSECA:
				ldb	#08			; RETURN ERROR CODE		
				rts

*   VERIFY  The sector just written to the disk is to be verified to
*           determine if there are CRC errors. No seek is required as
*           this routine will only be called immediately after a write
*           single sector operation.
*
*           ENTRY - No entry parameters
*
*           EXIT -  (X) May be destroyed
*                   (A) May be destroyed
*                   (B) = Error condition
*                   (Z) = 1 if no error
*                       = 0 if an error

VERIFY          clrb                    
                rts                             

*   SEEK    Seeks to the track specified in the 'A' accumulator. In
*           double-sided systems, this routine should also select the
*           correct side depending on the sector number supplied in 'B'.
*
*           ENTRY - (A) = Track Number
*                   (B) = Sector Number
*
*           EXIT -  (X) May be destroyed (See text)
*                   (A) May be destroyed (See text)
*                   (B) = Error condition
*                   (Z) = 1 if no error
*                       = 0 if an error

SEEKIT          clrb
                rts


*   INIT    This routine performs any necessary initialization of the
*           drivers during cold start (at boot time). Actually, any
*           operation which must be done when the system is first booted
*           can be done here.
*
*           ENTRY - No parameters
*
*           EXIT - A, B, X, Y, and U may be destroyed

INITIAL        jsr      TINIT 
               ldx     #DRVTBL
               ldb     #12

INIT_2          clr     ,x+
                decb
                bne     INIT_2
                rts

*   WARM    Performs any necessary functions during FLEX warmstart. FLEX
*           calls this routine each time it goes thru the warm start
*           procedure (after every command). As an example, some
*           controllers use PIA's for communication with the processor.
*           If FLEX is exited with a CPU reset, these PIA's may also be
*           reset such that the controller would not function properly
*           upon a jump to the FLEX warm start entry point. This routine
*           could re-initialize the PIA when the warm start was executed.
*
*           ENTRY - No parameters
*
*           EXIT - A, B, X, Y, and U may be destroyed

WARM            jsr		TINIT
				tst     SCRATCH3
                bne     LREAE7
                lda     #$80
                ;sta     $E014      ; initialization of printer port?

LREAE7          rts

*   RESTORE A restore operation (also known as a "seek to track 00") is to
*           be performed on the specified drive. The drive is specified
*           in the FCB pointed to by the contents of the X register. Note
*           that the drive number is the 4th byte of the FCB. This
*           routine should select the drive before executing the restore
*           operation.
*
*           ENTRY - (X) = FCB address (3,x contains drive number)
*
*           EXIT -  (X) May be destroyed
*                   (A) May be destroyed
*                   (B) = Error condition
*                   (Z) = 1 if no error
*                       = 0 if an error

RESTORE         bsr     DRIVE
                clrb

LREAFF          rts

*   DRIVE   The specified drive is to be selected. The drive is specified
*           in the FCB pointed to by the contents of the X register. Note
*           that the drive number is the 4th byte of the FCB.
*
*           ENTRY - (X) = FCB address (3,x contains drive number)
*
*           EXIT -  (X) May be destroyed
*                   (A) May be destroyed
*                   (B) = $0F if non-existent drive
*                       = Error condition otherwise
*                   (Z) = 1 if no error
*                       = 0 if an error
*                   (C) = 0 if no error
*                       = 1 if an error

DRIVE           lda 	3,x 		; GET DRIVE NUMBER FROM FCB
				cmpa 	#$03		;
				bls 	DRVSEL1 	; ONLY DRIVES 0 TO 3 ALLOWED
				clra			;
DRVSEL1:		sta 	CURDRV		;
                andcc	#$fe
				rts

*   CHKRDY  Check for a drive ready condition. The drive number is found
*           in the specified FCB (at 3,x). If the user's controller turns
*           the drive motors off after some time delay, this routine
*           should first check for a drive ready condition and if it is
*           not ready, should delay long enough for the motors to come up
*           to speed, then check again. This delay should be done ONLY if
*           not ready on the first try and ONLY if necessary for the
*           particular drives and controller! If the hardware always
*           leaves the drive motors on, this routine should perform a
*           single check for drive ready and immediately return the
*           resulting status. Systems which do not have the ability to
*           check for a drive ready condition should simply always return
*           a ready status if the drive number is valid.
*
*           ENTRY - (X) = FCB address (3,x contains drive number)
*
*           EXIT -  (X) May be destroyed
*                   (A) May be destroyed
*                   (B) = Error condition
*                   (Z) = 1 if drive ready
*                       = 0 if not ready
*                   (C) = 0 if drive ready
*                       = 1 if not ready

CHKRDY          
                

*   QUICK   This routine performs a "quick" drive ready check. Its
*           function is exactly like the CHKRDY routine above except that
*           no delay should be done. If the drive does not give a ready
*           condition on the first check, a not ready condition is
*           immediately returned. Entry and exit are as above.

QUICK           clrb
                andcc	#$fe
                rts

SEBB8           bsr     *+2
                bsr     *+2
                bsr     *+2
                rts

                end     $C850
