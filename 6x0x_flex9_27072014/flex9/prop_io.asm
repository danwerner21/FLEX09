;* PROP i/O driver
;* idea by PROP I/O N8VEM

;* porta [0..7] - 	PROP D[0..7]
;* PB0				PROP CS           out
;* PB1            SD_WP             in
;* PB2				!RST              out
;* PB3            SD_CD             in
;* PB4				PROP A0           out
;* PB5				PROP A1           out
;* PB6				PROP !RD          out
;* PB7				MRDY/!BUSY        in

;* functionality
;* PS2 kbd in
;* VGA out
;* SD card i/o

;* definition of registers:
;*                    +------/WAIT
;*                    |+-----/RD
;*                    ||+---- A1
;*                    |||+--- A0
;*                    ||||+--/CS
;*                    |||||
;*                    |||||
;*   P15..P0  -->  xxxxxxxx_xxxxxxxx
;*                          +------+
;*                          D7....D0
;*
;*
;*    /wait /rd  A1  A0 CD RES WP CS  
;*     i      0   0   0  i  0	  i 1   Status Port
;*     i      0   0   1  i  0	  i 1   Keyboard receive
;*     i      1   0   1  i  0   i 1   vga out port
;*     i      1   1   0  i  0   i 1   Disk command port
;*     i      0   1   0  i  0   i 1   Disk command status port
;*     i      1   1   1  i  0   i 1   Disk write
;*     i      0   1   1  i  0   i 1   Disk read 
;*     i      x   x   x  i  1   i x   Reset Prop        
;*     i      x   x   x  i  0   i 0   Unselect Prop 
       
;*     0      1   1   1  0  1   0 1   Data Direction            
            

PROP_UNSELECT:	   equ   $00
PROP_STAT_C_RD:	equ   $01
PROP_KEYB_D_RD:	equ   $11
PROP_VGA_D_WR:	   equ   $51
PROP_SD_C_WR:	   equ   $61
PROP_SD_C_RD:	   equ   $21
PROP_SD_D_WR:	   equ   $71
PROP_SD_D_RD:	   equ   $31
PROP_RST:		   equ   $04

iobase:			   equ   $ee0c
;porta:			   equ	$00		; pa0 - mrdy
;portb:			   equ	$04		; pb1 - cs, pb2 - a0, pb3 - a1, pb4 - !rd

portain:		      equ	$00
portaout:		   equ	$ff
portbdd:		      equ   $75
selectbdr		   equ	4

porta			      equ	iobase
portacr			   equ   porta+1
portb			      equ   iobase+2
portbcr			   equ   portb+1

via1               equ  $ed00

;* mrdy is connected to pb7
mrdy:			      equ   $80

;* disk commands
DISK_READ: 	 	   equ   $01			; COMMAND FOR READING FROM SD DISK
DISK_WRITE: 	   equ   $02			; COMMAND FOR WRITING TO SD DISK
DISK_REQ: 	 	   equ   $EE			; COMMAND TO SEE IF DISK CONTROLLER IS READ
DISK_IODONE:	   equ   $AA        	; WRITE IS COMPLETE

*
* PORT A ASSIGMENTS
*               
;       EQU     IC3     ;                       PA0 
;io_src  equ     IC2     ;                       PA1
;       EQU     IC1     ;                       PA2 
;       EQU     I4O5    ;                       PA3
;       EQU     0C4     ;                       PA4 
;       EQU     OC3     ;                       PA5
;RST     equ     OC2     ;Enable DS1302          PA6
;       EQU     OC1     ;                       PA7
*
* PORT D ASSIGNMENTS
*
I_O     equ     %00001000       ;DS1302 I/O    PD3
SCLK    equ     %00010000       ;DS1302 CLOCK  PD4
*


;*****************************************************************
;	PropIO vectors
;
;*****************************************************************
FLEXBOOT		   jmp		FLEXBOOT_IMPL
PROP_WRITE_SD	jmp		PROP_WRITE_SD_IMPL
PROP_READ_SD	jmp		PROP_READ_SD_IMPL
PROP_CONIN		jmp		PROP_CONIN_IMPL
PROP_CONOUT		jmp		PROP_CONOUT_IMPL
PROP_CONST		jmp		PROP_CONST_IMPL
CONIN			jmp		CONIN_IMPL
CONOUT			jmp		CONOUT_IMPL
CONSTAT			jmp		CONSTAT_IMPL
PROP_RESET		jmp		Reset_Prop

         ;org   $bee0
;test2    jsr prop_init
         
;done2    jsr CONSTAT
			;;bita #$01
			;beq   done2
         ;jsr   CONIN
    		;jsr   CONOUT   
         ;jmp   done2   
         
      				
			org		FLEXBOOT+$40
test     jsr prop_init
         
done     jsr prop_stat_rd
			bita #$01
			beq   done
         jsr   prop_keyb_rd
    		;lda 	#$41	; output control signals
			jsr   prop_vga_wr   
         jmp   done   
         
;*******************************************************************			
;
;
;   HANDLE prop WRITE CALL
;
;*******************************************************************
PROP_WRITE_SD_IMPL:
			lda #DISK_REQ
			jsr prop_dsk_cmd_wr
			
REQUEST_LOOP_WR:
			jsr prop_dsk_stat_rd
			bita #$04
			beq REQUEST_LOOP_WR

			lda #DISK_WRITE
			jsr  prop_dsk_cmd_wr

			lda CURDRV
			jsr prop_dsk_cmd_wr

			lda CURSEC
			jsr prop_dsk_cmd_wr

			lda CURTRK
			jsr prop_dsk_cmd_wr

			clra					; hi track is 0
			jsr prop_dsk_cmd_wr

			lda DMAAD+1
			jsr prop_dsk_cmd_wr

			lda DMAAD
			jsr prop_dsk_cmd_wr
			
PROP_DISK_READY1:
			jsr prop_dsk_stat_rd
			bita #$10
			beq PROP_DISK_READY1

			ldx DMAAD
			clrb				; one sector is 256 bytes
wl:			lda 0,x
			jsr prop_dsk_data_wr
			leax 1,x
			decb
			bne wl

			lda #DISK_IODONE
			jsr  prop_dsk_cmd_wr
			
PROP_DISK_READY2:
			jsr prop_dsk_stat_rd
			bita #$20
			beq PROP_DISK_READY2

			clrb
			rts
;*******************************************************************			
PROP_CONST_IMPL:					; CONSOLE STATUS, RETURN 0FFH IF CHARACTER READY, 00H IF NOT
			jsr prop_stat_rd
			anda #$01
			beq	NOT_READY
		
			lda	#$ff
NOT_READY:				;
			rts			;
;***********************************************************************
		

			
;***********************************************************************		
PROP_CONIN_IMPL:					; KBD CHARACTER INTO REGISTER A
					;
			jsr		prop_stat_rd

			tsta
			beq	PROP_CONIN_IMPL

			jsr prop_keyb_rd
			rts			;
;***********************************************************************
PROP_CONOUT_IMPL:				
			jsr	prop_vga_wr
			rts			;
;***********************************************************************
SER_CONIN_IMPL   jmp VINCH

SER_CONOUT_IMPL	 jmp VOUTCH

SER_CONSTAT_IMP  jmp STAT_

;****************************+*******************************************
INCHNE_IMPL     pshs    b
                ldb     VIA1+DRB
                bitb    #%00000010           ;pd1                
                beq     ci1
                puls    b
                jmp     INCHNE_
ci1             puls    b
ci4             jsr     prop_stat_rd
                bita    #$01                
                beq     ci4                
                lbra    prop_keyb_rd 

CONIN_IMPL      pshs    b
                ldb     VIA1+DRB
                bitb    #%00000010           ;pd1                
                beq     ci5
                puls    b
                bra     SER_CONIN_IMPL
ci5             puls    b
ci6             jsr     prop_stat_rd
                bita    #$01                
                beq     ci6                
                lbsr    prop_keyb_rd
                
CONOUT_IMPL     pshs    b
                ldb     VIA1+DRB
                bitb    #%00000010
                beq     ci2
                puls    b
                bra     SER_CONOUT_IMPL
ci2             puls    b
                lbra    prop_vga_wr
                
CONSTAT_IMPL    pshs    a
                lda     VIA1+DRB
                bita    #%00000010
                beq     ci3
                
                puls    a
                bra     SER_CONSTAT_IMP
                
ci3             jsr     prop_stat_rd
                bita    #$01                
                bne     cs2
                
                puls	   a
                orcc	   #$04					; set zero flag
                rts
cs2  			    puls	   a
                andcc	#$fb					; clear zero
                rts

;***********************************************************************
;			prop communication subroutines
;*****************************************************************
				
waitmrdy: 	lda   #mrdy	
wm1:			bita  portb
				beq  	wm1   ; wait for mrdy
				rts
;prepare ports for output
prepare_prop_wr
				;ldx		#iobase
				ldb 	#portaout		;porta to output
				stb 	porta
				ldb		#selectbdr		; enable data
				stb     portacr
				sta 	porta			; output data on porta
				rts
; unselect prop port				
prop_unslct
				ldb 	#PROP_UNSELECT			; reset control
				stb 	portb			; reset control
				clr		portacr
				clr     porta
				rts
            
;* A contains command
prop_dsk_cmd_wr: pshs	b        ;
				ldb 	#portaout		;porta to output
				stb 	porta
				ldb		#selectbdr		; enable data
				stb     portacr
				sta 	porta			; output data on porta
				ldb 	#PROP_SD_C_WR	; output control signals
				stb 	portb
            bsr 	waitmrdy
				ldb 	#PROP_UNSELECT			; reset control
				stb 	portb			; reset control
				clr		portacr
				clr     porta
				puls    b
				rts
			
;* A contains data
prop_dsk_data_wr:
			    pshs	b
				ldb 	#portaout		;porta to output
				stb 	porta
				ldb		#selectbdr		; enable data
				stb     portacr
				sta 	porta			; output data on porta
				ldb 	#PROP_SD_D_WR	; output control signals
				stb 	portb
            bsr 	waitmrdy
				ldb 	#PROP_UNSELECT			; reset control
				stb 	portb			; reset control
				clr		portacr
				clr     porta
				puls    b
				rts

;*************************************************
;* Write to prop vga 
;* A contains data
prop_vga_wr:
				pshs	a,b
				ldb 	#portaout		;porta to output
				stb 	porta
				ldb	#selectbdr		; enable data
				stb   portacr
				sta 	porta			; output data on porta
				ldb 	#PROP_VGA_D_WR	; output control signals
				stb 	portb
				jsr 	waitmrdy
				ldb 	#PROP_UNSELECT			; reset control
				stb 	portb			; reset control
				clr	portacr
				clr   porta
				puls  a,b
			rts
;* reset Propeller
Reset_Prop: pshs     b
            lbsr      prepare_prop_wr
            ldb		#PROP_RST
            stb 	   portb
            clrb
reset_dly	decb
            bne		reset_dly
            lbsr      prop_unslct		
            puls	   b
            rts	
;* A contains status
prop_dsk_stat_rd:
			   pshs	   b
				;ldx		#iobase
				ldb		#selectbdr		; enable data
				stb      portacr
				ldb 	   #PROP_SD_C_RD	; output control signals
				stb 	   portb
            jsr 	   waitmrdy
				lda 	   porta
				ldb 	#PROP_UNSELECT			; reset control
				stb 	portb			; reset control
				clr		portacr
				clr     porta
				puls     b
				rts
			
;* A contains data
prop_dsk_data_rd:
				pshs	   b
				ldb		#selectbdr		; enable data
				stb      portacr
				ldb 	   #PROP_SD_D_RD	; output control signals
				stb 	   portb
            jsr 	   waitmrdy			
				lda 	   porta
				ldb 	   #PROP_UNSELECT			; reset control
				stb 	   portb			; reset control
				clr		portacr
				clr      porta
				puls     b
				rts
			
;* A contains status
prop_stat_rd:
				pshs	   b
            ldb      LASTCHAR
            cmpb     #$0d
            beq      psr
				;ldx		#iobase
				ldb		#selectbdr		; enable data
				stb      portacr
				ldb 	   #PROP_STAT_C_RD	; output control signals
				stb 	   portb
				jsr 	   waitmrdy
				lda 	   porta
				lbsr     prop_unslct
      		puls     b
			   rts
psr         lda      #$1	
      		puls     b
			   rts
   
;* A contains data
prop_keyb_rd:
				pshs     b
            ldb      LASTCHAR
            cmpb     #$0d
            beq      pkbr
				;ldx		#iobase
				ldb		#selectbdr		; enable data
				stb      portacr
				ldb      #PROP_KEYB_D_RD	; output control signals
				stb      portb
				jsr      waitmrdy
			;brclr  	   portb,x,mrdy,*
				lda 	   porta
				lbsr     prop_unslct
pkbr1  		puls     b
            sta      LASTCHAR
    			rts
pkbr        lda      #$0a
            bra      pkbr1

   
prop_init:  
            ;clr     portacr
            ;clr     porta
            clr      portbcr
            lda      #portbdd
            sta	   portb
            lda 	   #selectbdr
            sta		portbcr		
            lbra     prop_unslct
;*******************************************************************	

PROP_READ_SD_IMPL:
			;sei
			lda #DISK_REQ		; REQUEST TO SEE IF DISK IS READY TO RECEIVE
			jsr  prop_dsk_cmd_wr						
			
REQUEST_LOOP_RD:
			jsr prop_dsk_stat_rd
			bita #$04
			beq REQUEST_LOOP_RD
	
			lda #DISK_READ
			jsr  prop_dsk_cmd_wr
	
			lda CURDRV
			jsr prop_dsk_cmd_wr
			lda CURSEC
			jsr	prop_dsk_cmd_wr

 			lda CURTRK
			jsr	prop_dsk_cmd_wr
			clra			; hi track is 0
			jsr	prop_dsk_cmd_wr

			lda DMAAD+1
			jsr	prop_dsk_cmd_wr

			lda DMAAD
			jsr	prop_dsk_cmd_wr


PROP_DISK_READY:
			jsr prop_dsk_stat_rd
			
			bita #$10
			beq PROP_DISK_READY
	
			ldx DMAAD
			clrb				; one sector is 256 bytes
rl:			jsr prop_dsk_data_rd
			sta ,x
			leax 1,x
			decb
			bne rl

			rts
;*****************************************************************
; BOOTLOADER
; Loads Flex executable from SD card 
; File is FLEX.SYS
; Prop opens this file as drive 15
; treat as single track
;*****************************************************************
FLEXBOOT_IMPL:	
			jsr     prop_init
fb1      lda      portb
         bita     #$80
         beq     fb1
			ldx		#STARTOFFLEX
clearmem:   clr		0,x
			leax 1,x
			cmpx		#ENDOFFLEX
			bne     clearmem
			lda 	#$0f
			sta 	CURDRV
			clra 
			sta	CURSEC
			clr		CURTRK
			ldx		#LOADADDR
			stx		DMAAD
			ldb	    #27			; 26 consecutive sectors
BootLoop:	pshs	b
			pshs	a
			jsr		PROP_READ_SD
			stx		DMAAD
			lda 	#$0f
			sta 	CURDRV
			puls	a
			inca
			sta		CURSEC
			puls	b
			decb
			bne		BootLoop
			clr 	CURSEC
			clr 	CURDRV
			jmp		COLDS
;***********************************************************************
