        NAM     INIT.TXT
        OPT     pag
        PAG
        PRAGMA  CD
;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*
;*                                               ;*
;*       flex 2.9:1 initialization code          ;*
;*                                               ;*
;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*
STARTOFFLEX     EQU $C000
ENDOFFLEX       EQU $DEFF

;LNBUFF  equ     $C080
;STARTR  equ     $C100

;SMONTH  equ     $CC0E
;LNBUFP  equ     $CC14
;ESCRTN  equ     $CC16
;PROMPT  equ     $CC4E

;COLDST  equ     $CD00
;WARMST  equ     $CD03
;RENTER  equ     $CD06
;INCH    equ     $CD09
;INCH2   equ     $CD0C
;OUTCH   equ     $CD0F
;OUTCH2  equ     $CD12
;INBUFF  equ     $CD1B
;PSTRNG  equ     $CD1E
;PRCRLF  equ     $CD24
;INDECM  equ     $CD48
;STAT    equ     $CD4E

;ZD0F0   equ     $D0F0
;ZD0F1   equ     $D0F1

STIME           EQU $D370

;IHNDLR  equ     $D3E7
;TIMOFF  equ     $D3ED
;TIMON   equ     $D3EF
;TMINIT  equ     $D3F1
;TRMINT  equ     $D3F5
;TRMCHK  equ     $D3F7
;TIMOUT  equ     $D3F9
;TIMINE  equ     $D3FB
;ZD3FD   equ     $D3FD

;FMSCAL  equ     $D406

;ZDFD0   equ     $DFD0
;ZDFDC   equ     $DFDC
;ZDFDD   equ     $DFDD

;ZE005   equ     $E005
;ZE045   equ     $E045
;ZE085   equ     $E085
;ZE090   equ     $E090
;ZE0C5   equ     $E0C5

;ZF810   equ     $F810
;ZFFF0   equ     $FFF0
;ZFFFC   equ     $FFFC
;ZFFFD   equ     $FFFD

        PAGE

;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*


;*
;* STARTUp rouTINE
;* THIS RoutinE INITIALIZES CERTAIN PARAMETERS, GETS
;* DATE From uSER, AND EXECUTES A STARTUP.TXT FILE.

        ORG     $C400

LOADADDR
STAR
        BRA     STAR0
Vers
        FCB     $82,$2E,$89,$3A,$81

STAR0
        LDA     #$39                              ;SET UP RTS
        STA     >TSTSTR                           ;disable re-entry to this code
        LDD     #$CD03                            ;setup 'escape routine' address
        STD     >RETRNR                           ;ESCRTN

        LDD     #$BFFF
        STD     MEMEND
;
;        ldd     >TRMCHK         ;get address of terminal status check routine
;        std     >DSTAT+1         ;set in FLEX status check jump
;                                ;
;        ldd     >TIMOUT         ;get address of terminal output routine
;        std     >OUTCH+1        ;set in FLEX out char jump
;        std     >OUTCH2+1       ;set in FLEX alternate out char jump
;                                ;
;        ldd     >TIMINE         ;get address of terminal input routine
;        std     >INCH+1         ;set in FLEX in char jump
;        std     >INCH2+1        ;set in FLEX alt in char jump
;

        JSR     [TINITP]                          ;do terminal init

        LDX     #ZC810                            ;point to Flex version signon
        JSR     >PSTRNG                           ;print to terminal
        JSR     >DPCRLF                           ;and CRLF
                                                  ;

ZC43A
        LDD     >PPRMPT                           ;PROMPT get  current prompt pointer
        PSHS    b,a                               ;save it
        LDX     #ZC82E                            ;request for date - PROMPT FOR IT
        STX     >PPRMPT                           ;set new prompt pointer
        JSR     >PSTRNG                           ;do prompt
        JSR     >DINBUF                           ;get date
        PULS    b,a                               ;restore prompt pointer
        STD     >PPRMPT                           ;
        LDY     #SYSMTH                           ;SMONTH point Y reg to date regs
        BSR     ZC4A0                             ;convert month from ascii
        BCS     ZC43A                             ;no good - retry
                                                  ;
        BSR     ZC4A0                             ;convert day from ascii
        BCS     ZC43A                             ;no good - retry
                                                  ;
        BSR     ZC4A0                             ;convert year from ascii
        BCS     ZC43A                             ;no good - retry
                                                  ;
        LDY     #STIME                            ;point Y reg to system time regs
        BSR     ZC4A0                             ;
        BCS     ZC43A                             ;no good - retry
                                                  ;
        BSR     ZC4A0                             ;
        BCS     ZC43A                             ;no good - retry
                                                  ;
        BSR     ZC4A0                             ;
        BCS     ZC43A                             ;no good - retry
        CLR     STIME+3                           ;clear tick counter

        JSR     >DPCRLF                           ;do CRLF

        JSR     >WARM                             ;init the disk drivers
        LDX     #SYSFCB                           ;point to STARTUP.TXT FCB
        JSR     >CHKRDY                           ;do disk check
        LDA     #$01                              ;set for read operation
        STA     ,x                                ;
        JSR     >FMS                              ;D406
        BEQ     ZC47E                             ;no error
                                                  ;
        LDA     $01,x                             ;
        CMPA    #$04                              ;file not found error?
        BNE     ZC4B2                             ;no -
                                                  ;
        JMP     >WARMS                            ;yes - ignore file
;
ZC47E
        LDY     #LINBUF                           ;init line buffer pointer
        STY     >BUFPNT                           ;LNBUFP
        LDB     #$80                              ;set byte count to move
                                                  ;
ZC488
        JSR     >FMS                              ;get byte from startup.txt

        BNE     ZC4B2                             ;error
        DECB                                      ;decrement count
        BEQ     ZC4B2                             ;done
                                                  ;
        STA     ,y+                               ;put character in line buffer
        CMPA    #$0D                              ;see if EOL character
        BNE     ZC488                             ;no - loop
                                                  ;
        LDA     #$04                              ;yes - close file
        STA     ,x                                ;
        JSR     >FMS                              ;

        JMP     >RENTER                           ;re-enter FLEX with command in line buffer
                                                  ;
;*       convert ascii to decimal;
;
ZC4A0
        JSR     >DINDEC                           ;
        PSHS    x                                 ;
        BCS     ZC4B0                             ;
        LDA     ,y                                ;
        TSTB                                      ;see if any valid decimal digits entered
        ORCC    #$01                              ;set carry flag for error
        BEQ     ZC4AE                             ;no - return error
                                                  ;
        LDA     $01,s                             ;yes - get returned byte
        ANDCC   #$FE                              ;set carry flag = 0 if no error
ZC4AE
        STA     ,y+                               ;put in callers buffer
ZC4B0
        PULS    pc,b,a                            ;return

;*       error in startup file - report it

ZC4B2
        LDX     #ZC555                            ;can't run startup message

        JSR     >PSTRNG

        JMP     >WARMS

;*       fix up number of K of memory available message

ZC4BB
        PSHS    x,b
        LDX     #ZC82B                            ;'K' message
        LDA     #$04
        BRA     ZC4CA

;*

ZC4C4
        SUBA    #$0A
        STA     ,x
        LDA     #$01

;*

ZC4CA
        ADDA    ,-x
        ORA     #$30
        STA     ,x
        CMPA    #$39
        BHI     ZC4C4
        PULS    pc,x,b

;*       return status that no no RTC is available
;*       but mPT does exist

ZC4D6
        JSR     TMINT                             ;init timer
        LDA     #$02                              ;set flag for MPT
        RTS

;*       startup file error message

ZC555
        FCC     "Can't run STARTUP."
        FCB     $04

;*       flex startup signon message

ZC810   ;fcb     $1A               ; purpose?
        FCC     "FLEX for NHYODYNE 6809 Version 2.9:2 "
        FCB     $04,$34,$38                       ; purpose?

ZC82B
        FCC     "48K ram"
        FCB     $04                               ; purpose?
        FCB     $04

;*       date prompt

ZC82E
        FCC     "Date and TIME (MM/DD/YY HH/MM/SS)? "
        FCB     $04

;        org     SYSFCB		; same data in FLX29CPP

;        fcb     $FF
;        fcb     $00
;        fcb     $00
;        fcb     $00
;        fcc     "startup"
;        fcb     $00
;        fcc     "txt"
;        fcb     $00

;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*
;*                                               ;*
;*       flex entry point after boot             ;*
;*                                               ;*
;*         this must start at $C850              ;*
;*       N8VEM doesn't use this!                 ;*
;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*;*
;		org $c850
;COLDSTART
;SFRES1  orcc    #$50
;lds     #LNBUF
;ldd     >ZDFDC
;ldx     >ZD0F0
;pshs    x,b,a
;ldx     #$99AA
;ldy     #STARTR+256
;ldb     #$FF

;ZC867   lbsr    ZC91A
;stx     >ZD0F0
;cmpx    >ZD0F0
;bne     ZC888
;pshs    b

;ZC874   lbsr    ZC91A
;stb     >ZD0F1
;subb    #$01
;bcc     ZC874

;puls    b
;lbsr    ZC91A
;cmpb    >ZD0F1
;beq     ZC889
;ZC888   clra

;ZC889   sta     ,-y
;beq     ZC890
;lbsr    ZC4BB

;ZC890   subb    #$01
;bcc     ZC867

;puls    x,b,a
;std     >ZFFFC
;stx     >ZD0F0
;clra
;ldx     #STARTR
;ldb     >ZDFDC
;eorb    #$0F
;clr     d,x
;ldb     >ZDFDD
;eorb    #$0F
;clr     d,x
;bsr     ZC922
;tstb
;beq     ZC8B8

;ZC8B3   clr     ,y+
;decb
;bne     ZC8B3

;ZC8B8   leay    -$0C,y
;ldx     #ZFFF0
;ldb     #$10

;ZC8BF   lda     ,y+
;sta     ,x+
;decb
;bne     ZC8BF
;lda     >MEMEND
;asla
;asla
;asla
;asla
;clrb
;subd    #$0001
;std     >MEMEND
;lbsr    ZC4D6
;ora     >CPUTYP
;ldb     >ZDFD0
;bitb    #$F0
;beq     ZC8E3
;ora     #$01

;ZC8E3   ldb     >ZE005
;beq     ZC8FD
;cmpb    #$FF
;beq     ZC8FD
;cmpb    >ZE0C5
;bne     ZC8FD
;cmpb    >ZE045
;bne     ZC8FD
;cmpb    >ZE085
;bne     ZC8FD
;ora     #$04

;ZC8FD   ldx     >$E800
;pshs    x
;ldx     #$99AA
;stx     >$E800
;cmpx    >$E800
;puls    x
;bne     ZC911
;ora     #$10

;ZC911   stx     >$E800
;sta     >CPUTYP
        JMP     >STAR

;*

;ZC91A   tfr     b,a
;eora    #$0F
;sta     >ZFFFD
;rts

;;*

;ZC922   ldb     #$0C
;ldy     #ZDFD0
;;*       ldx     #STARTR+16
;ldx     #STARTR

;ZC92B   cmpx    #STARTR+256
;beq     ZC93D
;lda     ,x+
;beq     ZC92B
;sta     ,y+
;inc     >MEMEND
;decb
;bne     ZC92B
;rts

;ZC93D   ldx     #STARTR

;ZC940   cmpx    #STARTR+16
;beq     ZC951
;lda     ,x+
;beq     ZC940
;sta     ,y+
;inc     >MEMEND
;decb
;bne     ZC940

ZC951
        RTS
