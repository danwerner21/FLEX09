* N8VEM 6x0x (6809) driver for 
* DS1302 on VIA1
* as9 rtc_driver.asm -l s19 now

VIA1           equ   $ED00

CE             equ   $10      ;PD4=CE
NCE            equ   $EF      ;PD4=CE
DQ             equ   $20      ;PD5=DQ
NDQ            equ   $DF      ;PD5=DQ
SCK            equ   $40      ;SCLK = PD6
NSCK           equ   $BF      ;SCLK = PD6



DRA            equ   1
DDRA           equ   3
DRB            equ   0
DDRB           equ   2
PCR            equ   12
ACR            equ   11

RTC_BURST_RD   equ   $BF
RTC_BURST_WR   equ   $BE
RAM_BURST_RD   equ   $FF
RAM_BURST_WR   equ   $FE
WP             equ   $00
CHARGE         equ   $A5      ;1 DIODE, 2k
RTC_WP_WR      equ   $8E
RTC_WP_RD      equ   $8F

;INCHNP equ 0   ;INPUT CHAR IN A REG - NO PARITY
;OUTCH equ 1    ;OUTPUT CHAR FROM A REG
;PDATA1 equ 2   ;OUTPUT STRING
;PDATA equ 3    ;OUTPUT CR/LF THEN STRING
;OUT2HS equ 4   ;OUTPUT TWO HEX AND SPACE
;OUT4HS equ 5   ;OUTPUT FOUR HEX AND SPACE
;PCRLF equ 6    ;OUTPUT CR/LF
;SPACE equ 7    ;OUTPUT A SPACE
;MONITR equ 8   ;ENTER ASSIST09 MONITOR
;VCTRSW equ 9   ;VECTOR EXAMINE/SWITCH
;BRKPT equ 10   ;USER PROGRAM BREAKPOINT
;PAUSE equ 11   ;TASK PAUSE FUNCTION

;GETCHR      equ   $CD15
;OUTCH       equ   $CD0F
;OUTHEX      equ   $CD3C
;PCRLF       equ   $CD24
;PSTRNG      equ   $CD1E
;WARMS       equ   $CD03
;PUTCHR      equ   $CD18

LATCH_DIS           equ     0
FLEXMONTH           equ     $CC0E
FLEXDATE            equ     $CC0F
FLEXYEAR            equ     $CC10
                        
FLEXHOURS           equ     $D370               
FLEXMINS            equ     $D371
FLEXSECS            equ     $D372
          
      

START_WR_RTC   jsr   ASKDAT
               jsr   WRITE_WP_OFF
               jsr   WRITE_BURST_RTC
               ;swi
               ;fcb   MONITR
               jmp   WARMS

;BACKSP         fcb   $08 ;BACKSPACE CHARACTER
;DELETE         fcb   $18 ;CONTROL-X
;ENDLIN         fcc   ':' ;END OF LINE
;TEMP           rmb   3 ;TEMPORARY IN FCS4, DEC-HEXIN, CMDPROC

;MONTH          rmb   1
;DATE           rmb   1
;YEAR           rmb   1
;HOURS          rmb   1
;MINUTES        rmb   1
;SECONDS        rmb   1
;DAY            rmb   1

START_RD_RTC   jsr   WRITE_WP_OFF
               jsr   READ_BURST_RTC
               jsr   WRITEDATE
               jsr   FLEXDATETIME
               rts



************************************************************************
* write bit in C               
WRITE_BIT      bcc   BIT0_0
               ldb   #CE+DQ
               bra   BIT0
BIT0_0         ldb   #CE               
BIT0           stb   VIA1+DRB
               orb   #SCK
               stb   VIA1+DRB
               rts
               
************************************************************************               
* write A to bitbang serial      
WRITE_1BYTE    lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               lsra
               bsr   WRITE_BIT
               rts
************************************************************************   
* write rtc in burst mode            
WRITE_BURST_RTC
               ldb   #CE+DQ+SCK
               stb   VIA1+DDRB
               clr   VIA1+DRB
               lda   #RTC_BURST_WR
               bsr   WRITE_1BYTE
               lda   RTCSECONDS
               bsr   WRITE_1BYTE
               lda   RTCMINUTES
               bsr   WRITE_1BYTE
               lda   RTCHOURS
               bsr   WRITE_1BYTE
               lda   RTCDATE
               bsr   WRITE_1BYTE
               lda   RTCMONTH
               bsr   WRITE_1BYTE
               lda   RTCDAY
               bsr   WRITE_1BYTE
               lda   RTCYEAR
               bsr   WRITE_1BYTE
               lda   #WP
               bsr   WRITE_1BYTE
               lda   #CHARGE
               bsr   WRITE_1BYTE
               clr   VIA1+DRB
               rts
************************************************************************
* disable write protection
WRITE_WP_OFF   ldb   #CE+DQ+SCK
               stb   VIA1+DDRB
               clr   VIA1+DRB
               lda   #RTC_WP_WR
               bsr   WRITE_1BYTE
               lda   #0
               bsr   WRITE_1BYTE
               clr   VIA1+DRB
               rts
************************************************************************     
* read single bit          
READ_BIT       pshs  a
               ldb   #CE
               stb   VIA1+DRB
               lda   VIA1+DRB
               ldb   #CE+SCK
               stb   VIA1+DRB
               anda  #DQ
               puls  a
               rts
************************************************************************               
* read byte
READ_1BYTE     clra
               bsr   READ_BIT
               beq   BIT1  
               ora   #1
BIT1           bsr   READ_BIT
               beq   BIT2  
               ora   #2
BIT2           bsr   READ_BIT
               beq   BIT3  
               ora   #4
BIT3           bsr   READ_BIT
               beq   BIT4  
               ora   #8
BIT4           bsr   READ_BIT
               beq   BIT5  
               ora   #$10
BIT5           bsr   READ_BIT
               beq   BIT6  
               ora   #$20
BIT6           bsr   READ_BIT
               beq   BIT7  
               ora   #$40
BIT7           bsr   READ_BIT
               beq   RET
               ora   #$80
RET            rts
************************************************************************
* read rtc in burst mode
READ_BURST_RTC ldb   #CE+DQ+SCK
               stb   VIA1+DDRB
               clr   VIA1+DRB
               lda   #RTC_BURST_RD
               jsr   WRITE_1BYTE   
               ldb   #CE+SCK
               stb   VIA1+DDRB
               bsr   READ_1BYTE
               sta   RTCSECONDS
               bsr   READ_1BYTE
               sta   RTCMINUTES
               bsr   READ_1BYTE
               sta   RTCHOURS
               bsr   READ_1BYTE
               sta   RTCDATE
               bsr   READ_1BYTE
               sta   RTCMONTH
               bsr   READ_1BYTE
               sta   RTCDAY
               bsr   READ_1BYTE
               sta   RTCYEAR
               bsr   READ_1BYTE     ;WP discard
               bsr   READ_1BYTE     ;CHARGE discard
               clr   VIA1+DRB
               rts
  
************************************************************************
* ask for date & time
ASKDAT         nop
ASKDA1         leax DATERQ,pc
               ;swi
               ;fcb PDATA
               jsr  PSTRNG

               ldy #RTCMONTH ;POINT TO MONTH LOCATION
               bsr GETDAT ;GET MONTH
               bsr GETDAT ;GET DATE
               bsr GETDAT ;GET YEAR
               bsr GETDAT ;GET HOUR
               bsr GETDAT ;GET MINUTES
               bsr GETDAT ;GET SECONDS
               ldx #THANKS
               ;swi
               ;fcb PDATA
               jsr  PSTRNG
               ;swi
               ;fcb PCRLF
               jsr  PCRLF
               rts  
************************************************************************                   
* GETDAT ROUTINe to INPUT AND STORE DATE
GETDAT         jsr DECIN    ;GET NEXT DATE ITEM
               bcc DATEOK  ;OK NUMBER RECEIVED
DATENG         leas 2,s    ;ELSE REMOVE RETURN ADDRESS FROM STACK
               bra ASKDA1  ;AND ASK FOR DATE AGAIN
DATEOK         cmpb #99    ;SHOULD BE LESS THAN 99
*              tfr x,d     ;MOVE DATE TO D
*              cmpd #99    ;SHOULD BE LESS THAN 99
               bhi DATENG  ;ELSE IT'S NO GOOD
               stb ,y+    ;SAVE IT
               rts
************************************************************************ 
DECIN          
GETHE1         ldx #0
               stx TEMP    ;NO NUMBER AS YET
               clrb        ;CLEAR  B FLAG FOR ERROR

* MAIN GET NUMBER LOOP
GETHD1         jsr GETNX   ;GET NEXT CHARACTER FROM INPUT BUFFER
               bcs GETSKP  ;FINISHED IF NOT ALPHA
               cmpa #'0'    ;CHECK IF A DIGIT
               blo GETFIN  ;<=9 ok, skip otherwise
               suba #$30   ;CONVERT FROM ASCII
               sta  TEMP

GETHD2         jsr GETNX  ;GET NEXT CHARACTER FROM INPUT BUFFER
               bcs GETSKP ;FINISHED IF NOT ALPHA
               cmpa #'0'   ;CHECK IF A DIGIT
               blo GETFIN ;<=9 ok, skip otherwise
               suba #$30  ;CONVERT FROM ASCII               PSHS A
               pshs a
               ldb TEMP   ;GET NUMBER SO FAR
               aslb
               aslb
               aslb
               aslb
               addb ,s+   ;ADD CURRENT DIGIT TO TEMP
               stb TEMP    ;AND SAVE IT               
               bra GETHD2 ;AND GO DO NEXT DIGIT
               
* NORMAL EXIT AT END OF NUMBER: C=0, B<>0, X=ANSWER
GETFIN         ldx TEMP   ;GET ANSWER
               andcc #$FE  ;   CLC C=0
               rts

* WHEN INVALID DIGIT IS FOUND, STEP TO NEXT CR OR SPACE
GETSKP         orcc  #$1    ;SEC C=1
               rts         ;FINALLY EXIT WITH CARRY SET

                        
DATERQ         fcc "ENTER DATE AND TIME(MM,DD,YY-HH,MI,SS): "
               fcb 4
DATEPR         fcc "DATE AND TIME(MM,DD,YY-HH,MI,SS): "
               fcb 4
THANKS         fcc "THANK YOU"
               fcb 4
************************************************************************
* GETNX - GET NEXT CHARACTER FROM INPUT, AND
* CLASF - CLASSIFY IT AS NUMERIC OR NOT

GETNX          ;swi
               ;fcb  INCHNP
               jsr   GETCHR

* IF SPACE, STEP PAST IT TO NEXT NON-SPACE
GETNX2         cmpa #$20 ;SPACE?
               beq GETNX ;YES, SO READ AND REPEAT

* CLASSIFY CHARACTER, AND RETURN CARRY SET IF NOT ALPHANUMERIC

CLASF0         cmpa #'9'
               bls CLASF1 ;DIGITS 0-9 ARE OK
CLASF2         orcc #$01  ;SEC SET CARRY
               rts        ;AND RETURN

CLASF1         andcc #$FE ;  CLC CLEAR IF OK
               rts
               
*****************************************************************               
* WRITE OUT DATE AND TIME
*
WRITEDATE   ldx  #RTCMONTH
            ;swi
            ;fcb   OUT2HS
            jsr   OUTHEX
            lda   #'/'
            ;swi
            ;fcb   OUTCH
            jsr   PUTCHR
            ldx   #RTCDATE
            ;swi
            ;fcb   OUT2HS
            jsr   OUTHEX
            lda   #'/'
            ;swi
            ;fcb   OUTCH
            jsr   PUTCHR
            ldx   #RTCYEAR
            ;swi
            ;fcb   OUT2HS
            jsr   OUTHEX
            lda   #'-'
            ;swi
            ;fcb   OUTCH
            jsr   PUTCHR
            ldx   #RTCHOURS
            ;swi
            ;fcb   OUT2HS
            jsr   OUTHEX
            lda   #':'
            ;swi
            ;fcb   OUTCH
            jsr   PUTCHR
            ldx   #RTCMINUTES
            ;swi
            ;fcb   OUT2HS
            jsr   OUTHEX
            lda   #':'
            ;swi
            ;fcb   OUTCH
            jsr   PUTCHR
            ldx   #RTCSECONDS
            ;swi
            ;fcb   OUT2HS
            jsr   OUTHEX
            ;swi
            ;fcb   PCRLF
            jsr   PCRLF
            rts

******************************************************************
* calculate date for flex9
*
CALCDATE    lda   0,x
            lsra
            lsra
            lsra
            lsra
            ldb   #10
            mul
            pshs  b
            ldb   0,x
            andb  #$0f
            addb  ,s+
            rts
            
FLEXDATETIME ldx   #RTCMONTH
            jsr   CALCDATE
            stb   FLEXMONTH
            ldx   #RTCYEAR
            jsr   CALCDATE
            stb   FLEXYEAR
            ldx   #RTCDATE
            jsr   CALCDATE
            stb   FLEXDATE
            ldx   #RTCHOURS
            jsr   CALCDATE
            stb   FLEXHOURS
            ldx   #RTCMINUTES
            jsr   CALCDATE
            stb   FLEXMINS
            ldx   #RTCSECONDS
            jsr   CALCDATE
            stb   FLEXSECS
            rts            
            
            
