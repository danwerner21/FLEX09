        OPT     PAG
        TTL     6809 FILE MANAGEMENT SYSTEM
        PAG
        PRAGMA  CD
;*
;* TSC 6809 DISK FILE MANAGEMENT SYSTEM
;*
;* COPYRIGHT (C) 1979 BY
;*
;* TECHNICAL SYSTEMS CONSULTANTS, INC.
;* BOX 2574
;* WEST LAFAYETTE, INDIANA 47906
;* (317) 463-2502
;*
;* NOTE:
;* AS OF 11/79 THE POINTS IN FMS WHICH LOAD A REGISTER
;* WITH THE SECTOR LENGTH HAVE BEEN CHANGED TO SIMPLY
;* CLEAR THE REGISTER (SINCE LDB #256 = LDB #0).
;* ALL POINTS AFFECTED BY THIS CHANGE ARE FLAGGED WITH
;* FOUR BACKSLASHES IN THE COMMENT FIELD (\\\\).
;*
;* CORRECTED WRITE FILE SECTOR MAP ROUTINE (WTFSM) 2/4/80.

;*
;* THE FILE MANAGEMENT SYSTEM (FMS) RELIES
;* ON THE USER DEFINITIONS OF FILE CONTROL
;* BLOCKS (FCB) FOR EACH FILE OPENED.
;* THE FCB STRUCTURE IS AS FOLLOWS:
;*
;* EACH FCB CONSUMES 320 BYTES OF RAM.
;* EACH BYTE IS USED AS FOLLOWS:
;*
;* NAME  OFFSET FUNCTION
;* ----  ------ --------

FFC             EQU 0                             ;FUNCTION OP CODE
FES             EQU 1                             ;ERROR STATUS BYTE
FAS             EQU 2                             ;ACTIVITY STATUS
FDN             EQU 3                             ;DRIVE NUMBER
FFN             EQU 4                             ;- 11 FILE NAME
FNE             EQU 12                            ;- 14 NAME EXTENSION
FID             EQU 15                            ;IDENTIFIER BYTE
FNU             EQU 16                            ;FILE NUMBER
FSA             EQU 17                            ;- 18 START DISK ADR.
FEA             EQU 19                            ;- 20 END DISK ADDRESS
FSZ             EQU 21                            ;- 22 FILE SIZE
FMP             EQU 23                            ;- 24 FILE SECTOR MAP
FDT             EQU 25                            ;- 27 CREATION DATE
FLP             EQU 28                            ;- 29 FCB LIST POINTER
FCS             EQU 30                            ;- 31 CURRENT SECTOR
FRN             EQU 32                            ;- 33 RECORD NUMBER
FDI             EQU 34                            ;DATA INDEX
FRI             EQU 35                            ;RANDOM INDEX
FWB             EQU 36                            ;- 46 NAME WORK BUFFER
FCD             EQU 47                            ;- 49 CURRENT DIR. ADR.
FFD             EQU 50                            ;- 52 FIRST DELETED DIR.
FLR             EQU 53                            ;- 55 LAST RECORD PNTR
FNK             EQU 56                            ;- 58 NEXT KEY POINTER
FSC             EQU 59                            ;SPACE COMP. CNTR
FSP             EQU 60                            ;- 63 SPARE BYTES
FSB             EQU 64                            ;- 319 SECTOR BUFFER

FMX             EQU FSP                           ;MAX SECTOR NUMBER
SBC             EQU FLR+2                         ;SEQ. BLOCK COUNT

;*
;* ALL DRIVER ROUTINES ARE REFERENCED
;* THROUGH THIS TABLE.

;DREAD   equ     $DE00       ;DISK READ ROUTINE
;DWRITE  equ     $DE03       ;DISK WRITE ROUTINE
;RESTOR  equ     $DE09       ;HEAD RESTORE ROUTINE
;VERIFY  equ     $DE06       ;WRITE VERIFY ROUTINE
;DRIVE   equ     $DE0C       ;DRIVE SELECT
;CHECK   equ     $DE0F       ;CHECK READY
;QUKCHK  equ     $DE12       ;QUICK READY CHECK
;DINIT   equ     $DE15       ;INITIALIZE DRIVERS
;DWARM   equ     $DE18       ;USER WARMSTART ROUTINE
;DSEEK   equ     $DE1B       ;SEEK TO TRACK ROUTINE

;* ALL LOW LEVEL SYSTEM VARIABLES
;* AND CONSTANTS ARE IN THIS AREA.
;*
;* SYSTEM CONSTANTS

SL              EQU 256                           ;SECTOR LENGTH
NL              EQU 11                            ;NAME LENGTH
RTC             EQU 5                             ;RETRY COUNT
RSC             EQU 7                             ;SEEK RETRY COUNT
RS              EQU 4                             ;RECORD START
IRS             EQU 16                            ;INFO RECORD START
IRL             EQU 24                            ;INFO RECORD LENGTH
IRFAS           EQU $04                           ;INFO MASK
LSTFC           EQU 22                            ;LAST FUNCTION CODE
MAXSP           EQU $7F                           ;MAX SPACE COUNT
SPC             EQU $20                           ;ASCII SPACE
SCC             EQU 9                             ;SPACE COMPRESSION CHAR.
DELC            EQU $18                           ;DELETE CHARACTER
NODR            EQU 4                             ;NUMBER OF DRIVES
DTRK            EQU 0                             ;DIRECTORY START TRACK
DSEC            EQU 5                             ;DIR. START SECTOR
ISEC            EQU 3                             ;INFO SECTOR NUMBER
TRMSK           EQU $7F                           ;TRACK MASK
SCMSK           EQU $7F                           ;SECTOR MASK
LSTTRK          EQU 76                            ;LAST TRACK NUMBER
LSTSEC          EQU 15                            ;LAST SECTOR NUMBER
RNFMSK          EQU $10                           ;REC NOT FOUND MASK
MAIND           EQU $0005                         ;MAIN DIRECTORY ADDRESS

;* DOS CONSTANTS

DATE            EQU $CC0E                         ;SYSTEM DATA
LOCK            EQU $C709                         ;LOCK FMS
UNLOCK          EQU $C70C                         ;UNLOCK FMS
;PR1     equ     $CCFC       ;PROCESS 1 REGISTER

;* SYSTEM ERROR NUMBER DEFINITIONS
;*
;* NAME  NUMBER MEANING
;* ----  ------ -------

NOER            EQU 0                             ;NO ERROR
ICER            EQU 1                             ;ILLEGAL FUNCTION CODE
FBER            EQU 2                             ;FILE BUSY
FEER            EQU 3                             ;FILE EXISTS
NFER            EQU 4                             ;NO SUCH FILE
DRER            EQU 5                             ;DIRECTORY ERROR
TMER            EQU 6                             ;TOO MANY FILES
DFER            EQU 7                             ;DISK FULL
EFER            EQU 8                             ;END OF FILE
RDER            EQU 9                             ;READ ERROR (CRC)
WTER            EQU 10                            ;WRITE ERROR (CRC)
WPER            EQU 11                            ;WRITE PROTECTED
DPER            EQU 12                            ;DELETE PROTECTED
IFER            EQU 13                            ;ILLEGAL FCB
DAER            EQU 14                            ;ILLEGAL DISK ADDRESS
DNER            EQU 15                            ;DRIVE NUMBER ERROR
NRER            EQU 16                            ;NOT READY
ADER            EQU 17                            ;ACCESS DENIED
STER            EQU 18                            ;STATUS ERROR
IRER            EQU 19                            ;INDEX RANGE ERROR
FIER            EQU 20                            ;FMS INACTIVE
INER            EQU 21                            ;ILLEGAL FILE NAME
CLER            EQU 22                            ;CLOSE ERROR
FSER            EQU 23                            ;FSM OVERFLOW ERROR
RRER            EQU 24                            ;RECORD RANGE ERROR
RMER            EQU 25                            ;RECORD MATCH ERROR

;
; CONSOLE I/O DRIVER VECTOR TABLE
;_____________________________________________________________________________________________________
        ORG     $D3E1                             ; TABLE STARTS AT $D3E1

LD3E1
        FDB     ADDDEV                            ; add an IRQ handler to table
        FDB     DELDEV                            ; delete an IRQ handler from table

INCHNEP
        FDB     INCHNE                            ; INPUT CHARACTER W/O ECHO
IHNDLRP
        FDB     IHNDLR                            ; IRQ INTERRUPT HANDLER
SWIVECP
        FDB     SWIVEC                            ; SWI3 VECTOR LOCATION
IRQVECP
        FDB     IRQVEC                            ; IRQ VECTOR LOCATION
        FDB     TMOFF                             ; TIMER OFF ROUTINE
        FDB     TMON                              ; TIMER ON ROUTINE
        FDB     TMINT                             ; TIMER INITIALIZATION
MONITRP
        FDB     MONITR                            ; MONITOR ENTRY ADDRESS
TINITP
        FDB     TINIT                             ; TERMINAL INITIALIZATION
        FDB     STAT                              ; CHECK TERMINAL STATUS
        FDB     VOUTCH                            ; OUTPUT CHARACTER
        FDB     VINCH                             ; INPUT CHARACTER W/ ECHO


        ORG     $D3FD
TSTSTR
        JMP     STAR

        ORG     $D400

;* FMS JUMP TABLES
;*
;* ALL CALLS TO THE FMS SHOULD ENTER
;* THROUGH ONE OF THESE THREE POINTS.

FMSINT
        JMP     INIT                              ;FMS INITIALIZATION  $D400
FMSCLS
        JMP     EXCLS                             ;FMS CLOSURE         $D403
FMS
        JMP     CMND                              ;FMS COMMAND ENTRY   $D406

;* GLOBAL VARIABLE STORAGE

FCBBAS
        RMB     2                                 ;FCB BASE POINTER        $D409
FCBSTR
        RMB     2                                 ;CURRENT FCB             $D40B
TEMP
        RMB     2                                 ;TEMPORARY INDEX         $D40D
DATAPT
        RMB     2                                 ;DATA POINTER            $D40F
ETRIES
        RMB     1                                 ;ERROR TRY COUNT         $D411
STRIES
        RMB     1                                 ;SEEK TRY COUNT          $D412
CLD
        RMB     2                                 ;CURRENT LOOK UP DIR     $D413
CUD
        RMB     2                                 ;CURRENT USER DIR        $D415
DIRDN
        RMB     1                                 ;DIRECTORY DRIVE NUM     $D417
BKLN
        RMB     2                                 ;BACK LINK               $D418
SINDIR
        RMB     1                                 ;SINGLE DIR SEARCH       $D41A
AVLPNT
        RMB     2                                 ;AVAIL POINTER           $D41B
SECMAP
        RMB     NODR                              ;*6      ;SECTOR MAPS             $D41D

        ORG     $D435

VRFYFG
        FCB     $FF                               ;VERIFY FLAG

DRVINFO
        FCB     $00,$00,$00,$00

;* SYSTEM ENTRY ROUTINES
;*
;* THE FOLLOWING THREE ROUTINES SHOULD
;* BE ACCESSED THROUGH THE SYSTEM JUMP
;* TABLE.

;* INIT
;*
;* INIT IS THE FMS INITIALIZATION ROUTINE.
;* NO ERRORS CAN OCCUR FROM THIS ROUTINE
;* AND THE SYSTEM ASSUMES NO FILES ARE OPEN.

INIT
        JSR     CINIT                             ;INITIALIZE DRIVERS
        LDX     #FCBBAS                           ;SET POINTER
        LDB     #10                               ;SET COUNT
        BSR     INIT4                             ;CLEAR SPACE
        LDX     #MAIND                            ;SET MAIN DIRECTORY
        STX     CLD                               ;
        STX     CUD                               ;
        CLR     SINDIR                            ;CLEAR SINGLE DIR
                                                  ;
INIT2
        LDX     #AVLPNT                           ;POINT TO FMS SPC
        LDB     #26                               ;SET COUNTER
                                                  ;
INIT4
        CLR     ,x+                               ;CLEAR BYTE
        DECB                                      ;DEC THE COUNT
        BNE     INIT4                             ;REPEAT?
        JMP     UNLOCK

;* EXCLS
;*
;* EXCLS IS THE FMS EXIT ROUTINE.
;* EXECUTION OF THIS ROUTINE TELLS THE
;* SYSTEM TO CLOSE ALL OPEN FILES.

EXCLS
        JSR     LOCK                              ;LOCK FMS
EXCLS1
        LDX     FCBBAS                            ;GET LINK BASE
        BEQ     INIT2                             ;ANY FCBS LEFT?
        LEAX    -FLP,x                            ;
        STX     FCBSTR                            ;SET ACT. FCB
        PSHS    y                                 ;SAVE REGISTER
        JSR     CLOSE                             ;GO CLOSE FILE
        PULS    y                                 ;RESTORE REGISTER
        BCC     EXCLS1                            ;REPEAT
                                                  ;
        LDX     FCBSTR                            ;SET TO FCB
        CLR     2,x                               ;CLEAR FAS
        JSR     UNLOCK                            ;UNLOCK FMS
        LDB     #$FF                              ;SET ERROR
        RTS

;* CMND
;*
;* CMND IS THE FMS COMMAND INTERPRETER.
;* ALL COMMAND CALLS TO FMS SHOULD BE
;* THROUGH THIS ROUTINE.

CMND
        TST     PR1                               ;PROCESS ACTIVE?
        BEQ     CMND1                             ;
        JSR     LOCK                              ;LOCK FMS
                                                  ;
CMND1
        PSHS    b,y                               ;SAVE REGISTERS
        STX     FCBSTR                            ;SET FCB STORAGE
        CLR     FES,x                             ;CLEAR ERRORS
        LDB     FFC,x                             ;GET FUNCTION CODE
        BNE     CMND4                             ;IO CODE?
                                                  ;
        LDB     FAS,x                             ;GET ACTIVITY BYTE
        BEQ     CMND3                             ;
                                                  ;
        CMPB    #2                                ;IS IT WRITE?
        BEQ     CMND2                             ;
        JSR     SRDSEQ                            ;GO DO READ
CMND15
        LDX     FCBSTR                            ;RESTORE X
        BCS     CMND7                             ;ERROR?
        TST     PR1                               ;PR 1 ACTIVE?
        BNE     CMND8                             ;
        CLRB                                      ;CLEAR ERRORS
        PULS    b,y                               ;RESTORE REGISTERS
        RTS                                       ;
                                                  ;jmp  $f82a

CMND2
        JSR     SWTSEQ                            ;GO DO WRITE
        BRA     CMND15                            ;
                                                  ;
CMND3
        LDB     #STER                             ;SET STATUS ERROR
        BRA     CMND7                             ;REPORT ERROR
                                                  ;
CMND4
        CMPB    #LSTFC                            ;CHECK CODE
        BLS     CMND5                             ;
        LDB     #ICER                             ;SET CODE ERROR
        BRA     CMND7                             ;REPORT IT
                                                  ;
CMND5
        DECB                                      ;DEC THE CODE
        ASLB                                      ;CODE TIMES 2
        LDX     #CODTBL                           ;POINT TO TABLE
        JSR     [b,x]                             ;GO DO ROUTINE
        LDX     FCBSTR                            ;RESTORE FCB PNTR
        BCC     CMND8                             ;ERRORS?
                                                  ;
CMND7
        STB     FES,x                             ;SET ERROR
                                                  ;
CMND8
        JSR     UNLOCK                            ;
        TST     FES,x                             ;TEST FOR ERROR
        PULS    b,y                               ;RESTORE REGISTERS
        RTS

;* CODTBL
;*
;* CODTBL IS THE SYSTEM TABLE OF
;* FUNCTION CODE ROUTINE ADDRESSES.

CODTBL
        FDB     OPNRD                             ;OPNRD   OPEN FOR READ
        FDB     OPNWT                             ;OPNWT   OPEN FOR WRITE
        FDB     OPNRW                             ;OPNRW   OPEN FOR READ WRITE
        FDB     CLOSE                             ;CLOSE   CLOSE FILE
        FDB     REWIND                            ;REWIND  REWIND FILE
        FDB     OPNDIR                            ;OPNDIR  OPEN DIRECTORY
        FDB     GETIR                             ;GETIR   GET INFO RECORD
        FDB     PUTIR                             ;PUTIR   PUT INFO RECORD
        FDB     READSS                            ;READSS  READ SINGLE SECTOR
        FDB     WRITSS                            ;WRITSS  WRITE SINGLE SECTOR
        FDB     WRTDIR                            ;WRTDIR  WRITE DIRECTORY
        FDB     DELETE                            ;DELETE  DELETE FILE
        FDB     RENAME                            ;RENAME  RENAME FILE
        FDB     RETRY4                            ;RETRY4  APPEND FILES
        FDB     NEXTS                             ;NEXTS   NEXT SEQU. SECTOR
        FDB     OPNSIR                            ;OPNSIR  OPEN SYSTEM INFO
        FDB     GETRAN                            ;GETRAN  GET RANDOM CHARACTER
        FDB     PUTRAN                            ;PUTRAN  WRITE RANDOM CHARACTER
        FDB     WTAPP                             ;WTAPP   OPEN WRITE APPEND
        FDB     NXTRDY                            ;NXTRDY  FIND NEXT READY DRIVE
        FDB     POSIT                             ;POSIT   POSITION TO RECORD N
        FDB     BKREC                             ;BKREC   BACKUP ONE RECORD

;*
;* THE FOLLOWING ROUTINES ARE THE SYSTEM
;* LEVEL ROUTINES USED BY THE FMS.

;* SETFCB
;*
;* SETFCB IS USED TO TELL THE SYSTEM
;* WHERE THE NEW FCB IS LOCATED.
;* IT SETS UP A CHAINED STRUCTURE
;* WITH FCBBAS AS THE BASE OF THE CHAIN.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF FCB EXISTS
;*          ALL REGISTERS CHANGED

SETFCB
        BSR     FNDFCB                            ;FIND FCB
        BNE     SETFC2                            ;ERROR?
        LDB     #FBER                             ;FILE BUSY
        ORCC    #1                                ;SEC SHOW ERROR
        RTS                                       ;
                                                  ;
SETFC2
        STD     ,x                                ;SET FCB
        LDX     ,x                                ;GET FCB POS
        CLR     ,x                                ;CLEAR LAST LINK
        CLR     1,x                               ;ALSO SHOWS NO ERROR
        RTS

;* REMFCB
;*
;* REMFCB IS USED TO REMOVE AN ACTIVE
;* FCB FROM THE SYSTEM FCB LIST.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF FCB NOT FOUND
;*          ALL REGISTERS CHANGED

REMFCB
        BSR     FNDFCB                            ;FIND FCB
        BEQ     REMFC2                            ;ERROR?
        LDB     #IFER                             ;SET ERROR CODE
        ORCC    #1                                ;SEC SHOW ERROR
        RTS                                       ;
                                                  ;
REMFC2
        LDD     [,x]                              ;GET NEXT LINK
        STD     ,x                                ;SAVE NEW VALUE
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS

;* FNDFCB
;*
;* FNDFCB TRIES TO FIND THE FCB IN
;* FCBSTR IN THE SYSTEM TABLE.
;*
;*   ENTRY: NONE
;*   EXIT:  NE IF NOT FOUND
;*          A & B DESTROYED
;*          X POINTS TO FCB

FNDFCB
        LDD     FCBSTR                            ;PICKUP FCB
        ADDD    #FLP                              ;SET TO LIST PNTR
        LDX     #FCBBAS                           ;GET BASE LOC.
FNDFC3
        LDY     ,x                                ;CHECK FOR LIST END
        BNE     FNDFC4                            ;
        ANDCC   #$FB                              ;SET NE BIT
        RTS                                       ;
                                                  ;
FNDFC4
        CMPD    ,x                                ;COMPARE VALUE
        BNE     FNDFC6                            ;
        RTS                                       ;RET WITH EQ
                                                  ;
FNDFC6
        LDX     ,x                                ;MOVE TO NEXT FCB
        BRA     FNDFC3                            ;REPEAT

;* CLRFCB
;*
;* CLRFCB CLEARS SELECTED SECTIONS OF
;* THE CURRENT FCB.
;*
;*   ENTRY: NONE
;*   EXIT:  ALL REGISTERS CHANGED

CLRFCB
        LDX     FCBSTR                            ;GET FCB
        CLRA                                      ;CLEAR A BYTE
        CLRB                                      ;GET SECTOR LENGTH \\\\
        BSR     CLRFC2                            ;CLEAR OUT
        LDB     #FSB-FSA                          ;
CLRFC2
        STA     FSA,x                             ;CLEAR BYTE
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNT
        BNE     CLRFC2                            ;LOOP TIL DONE
        RTS

;* COPNAM
;*
;* COPNAM WILL COPY THE NAME (NL BYTES)
;* FROM FCB AREA FFN (FILE NAME) TO THE
;* AREA FWB (WORK BUFFER).
;*
;*   ENTRY: NONE
;*   EXIT:  ALL REGISTERS CHANGED

COPNAM
        LDX     FCBSTR                            ;PICKUP FCB
        LDB     #NL                               ;SET NAME LENGTH
COPNA2
        LDA     FFN,x                             ;GET CHARACTER
        STA     FWB,x                             ;MOVE IT
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNTER
        BNE     COPNA2
        RTS

;* CMPNAM
;*
;* CMPNAM WILL COMPARE THE NAME (NL
;* BYTES LONG) IN THE FCB AREA FWB
;* (WORK BUFFER) TO THE CONTENTS OF
;* FCB AREA FFN (FILE NAME).
;*
;*   ENTRY: NONE
;*   EXIT:  NE IF NOT EQUAL
;*          ALL REGISTERS CHANGED

CMPNAM
        LDX     FCBSTR                            ;PICKUP FCB
        LDB     #NL                               ;SET LENGTH
                                                  ;
CMPNA1
        LDA     FFN,x                             ;GET CHARACTER
        ORA     #$20                              ;MAKE LOWERCASE
        PSHS    a                                 ;
        LDA     FWB,x                             ;GET 2ND CHARACTER
        ORA     #$20                              ;MAKE LOWERCASE
        CMPA    ,s+                               ;COMPARE THEM
        BNE     CMPNA4                            ;NOT EQUAL?
                                                  ;
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNTER
        BNE     CMPNA1                            ;REPEAT
CMPNA4
        RTS

;* GETRAN   ;* FFC #17 ;*
;*
;* GETRAN GETS A RANDOM CHARACTER FROM
;* THE CURRENT FSB.
;*
;*   ENTRY: FRI CONTAINS DESIRED INDEX
;*   EXIT:  CS IF FRI OUT OF RANGE
;*          A CONTAINS CHARACTER
;*          B & X CHANGED

GETRAN
        LDX     FCBSTR                            ;GET FCB
        LDB     FAS,x                             ;GET STATUS
        LSRB                                      ;CHECK IF R OR RW
        BCC     REWIN2                            ;GO SET ERROR
        LDB     FRI,x                             ;GET RANDOM INDEX
        JMP     RDSEQ0

;* PUTNXT
;*
;* PUTNXT PUTS THE CHARACTER IN A INTO
;* THE NEXT AVAILABLE FSB LOCATION
;* POINTED TO BY THE FDI.
;*
;*   ENTRY: A CONTAINS CHARACTER
;*   EXIT:  CS IF LAST FSB POS. USED
;*          B & X CHANGED

PUTNXT
        LDX     FCBSTR                            ;PICKUP FCB
        LDB     FDI,x                             ;GET DATA INDEX
        INC     FDI,x                             ;BUMP FDI
        ABX                                       ;ADD IN INDEX
        STA     FSB,x                             ;PUT THE CHARACTER
        INCB                                      ;BUMP IT
        BNE     PUTRA2                            ;OVER END OF FSB?
        ORCC    #1                                ;SEC OVER END!
        RTS

;* PUTRAN   ;* FFC #18 ;*
;*
;* PUTRAN PUTS THE CHARACTER IN A
;* IN THE FSB LOCATION INDEXED BY
;* THE FRI.
;*
;*   ENTRY: A CONTAINS CHARACTER
;*          FRI CONTAINS INDEX
;*   EXIT:  CS IF FRI OUT OF RANGE
;*          B & X CHANGED

PUTRAN
        LDX     FCBSTR                            ;GET FCB
        LDB     FAS,x                             ;CHECK IF RW
        ANDB    #3                                ;MASK OFF
        CMPB    #3                                ;IS IT RW?
        BNE     REWIN2                            ;SKIP IF ERROR
        ORB     #$80                              ;SET UPDATE BIT
        STB     FAS,x                             ;SAVE IT
        LDB     FID,x                             ;CHECK WP
        BITB    #$80                              ;
        BNE     PUTRA4                            ;
        LDB     FRI,x                             ;GET RANDOM INDEX
        ABX                                       ;ADD IN INDEX
        STA     64,x                              ;FSB,x PUT CHARACTER
                                                  ;
PUTRA2
        ANDCC   #$FE                              ;CLC CLEAR ERROR
        RTS                                       ;
                                                  ;
PUTRA4
        LDB     #WPER                             ;SET WP ERROR
        ORCC    #1                                ;SEC SET ERROR
        RTS

;* SRDSEQ
;*
;* SRDSEQ IS THE HIGH LEVEL READ
;* SEQUENTIAL CHARACTER ROUTINE.
;* CONTROL CHARACTERS AND SPACE
;* EXPANSION ARE HANDLED HERE UNLESS
;* FSC IS NEGATIVE.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR
;*          B HAS ERROR NUMBER
;*          A & X CHANGED

SRDSEQ
        LDA     FSC,x                             ;CHECK FOR SP. EXP.
        BMI     RDSEQ                             ;CONTROL IGNORE?
        BEQ     SRDSE2                            ;ACTIVE EXPANSION?
        DEC     FSC,x                             ;DEC THE SPACE COUNT
        LDA     #SPC                              ;SETUP SPACE
        BRA     SRDSE7                            ;FINISH UP
                                                  ;
SRDSE2
        BSR     RDSEQ                             ;READ NEXT CHAR
        BCS     SRDSE8                            ;ERRORS?
        CMPA    #DELC                             ;IS IT DELETED?
        BHI     SRDSE7                            ;SKIP ALL SPECIALS
        BEQ     SRDSE2                            ;
        CMPA    #SCC                              ;SPACE COMP CHAR?
        BNE     SRDSE6                            ;
        BSR     RDSEQ                             ;GO GET COUNT
        BCS     SRDSE8                            ;ERROR?
        LDX     FCBSTR                            ;RESTORE POINTER
        STA     FSC,x                             ;SAVE COUNT
        BRA     SRDSEQ                            ;REPEAT
                                                  ;
SRDSE6
        TSTA                                      ;IS CHAR NULL?
        BEQ     SRDSE2                            ;IGNORE IF SO
                                                  ;
SRDSE7
        ANDCC   #$FE                              ;CLC CLEAR ERROR
SRDSE8
        RTS

;* REWIND   ;* FFC #5 ;*
;*
;* REWIND WILL LOGICALLY REWIND THE
;* FILE SPECIFIED IN THE FCB.
;*
;*   ENTRY: NONE
;*   EXIT:  SAME AS RDNEXT

REWIND
        JSR     DOSTAT                            ;CHECK STATUS
        BCS     REWIN2                            ;ERROR?
        BITA    #1                                ;CHECK FOR R BIT
        BEQ     REWIN2                            ;ERROR?
        STA     FFC,x                             ;SET FFC
        JMP     OPNRD1                            ;GO SETUP FILE
                                                  ;
REWIN2
        LDB     #STER                             ;SET ERROR CODE
        ORCC    #1                                ;SEC SHOW ERROR
        RTS

;* RDSEQ
;*
;* RDSEQ IS THE LOW LEVEL GET
;* SEQUENTIAL CHARACTER ROUTINE.
;*
;*   ENTRY: NONE
;*   EXIT:  CHAR IN A
;*          CS IF ERROR
;*          B & X DESTROYED

RDSEQ
        LDX     FCBSTR                            ;PICKUP FCB
        LDB     FDI,x                             ;GET DATA INDEX
        BEQ     RDSEQ1                            ;
        INC     FDI,x                             ;BUMP DATA INDEX
                                                  ;
RDSEQ0
        ABX                                       ;ADD IN OFFSET
        LDA     FSB,x                             ;GET CHARACTER
        ANDCC   #$FE                              ;CLC
        RTS                                       ;
                                                  ;
RDSEQ1
        BSR     RDNEXT                            ;GET NEXT RECORD
        BCC     RDSEQ                             ;ERRORS?
        RTS

;* RDNEXT
;*
;* RDNEXT READS THE NEXT SEQUENTIAL
;* RECORD IF IT EXISTS.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR
;*          B HAS ERROR NUMBER
;*          A & X CHANGED

RDNEXT
        LDX     FCBSTR                            ;PICKUP FCB
        LDD     FSB,x                             ;GET TRACK & SECTOR
        INC     FRN+1,x                           ;BUMP REC NUM
        BNE     RDNEX1                            ;
        INC     FRN,x                             ;
                                                  ;
RDNEX1
        CMPD    #$0000                            ;TEST IF 0 LINK
        BEQ     RDNEX4                            ;END OF FILE?
RDNEX2
        STD     FCS,x                             ;SET CURRENT ADR.
        PSHS    a                                 ;
        LDA     #RS                               ;SET RECORD START
        STA     FDI,x                             ;SAVE IT
        PULS    a                                 ;
        BSR     READSS                            ;GO DO READ
        BCC     RDNEX8                            ;ERRORS?
        BITB    #$80                              ;CHECK NOT READY
        BEQ     RDNEX3                            ;
        LDB     #NRER                             ;SET ERROR
        BRA     RDNEX6                            ;
                                                  ;
RDNEX3
        LDB     #RDER                             ;SET READ ERROR
        BRA     RDNEX6                            ;
                                                  ;
RDNEX4
        LDB     #EFER                             ;SET EOF ERROR
RDNEX6
        ORCC    #1                                ;SEC SHOW ERROR
RDNEX8
        RTS

;* READSS   ;* FFC #9 ;*
;*
;* READSS READS A SINGLE RECORD (SECTOR)
;* FROM THE DISK.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF READ ERROR
;*          ALL REGISTERS CHANGED

READSS
        BSR     CLRTRY                            ;CLEAR TRY COUNTERS
        LDX     FCBSTR                            ;SET POINTER
        JSR     DRIVE                             ;DO DRIVE SEL
        BCS     READS6                            ;
                                                  ;
READS2
        BSR     GETCUR                            ;GET DISK ADDRESS
        JSR     READ                              ;GO READ RECORD
        BNE     READS4                            ;ERRORS?
        ANDCC   #$FE                              ;CLC CLEAR ERROR
        RTS                                       ;
                                                  ;
READS4
        PSHS    b                                 ;SAVE B
        BSR     RETRY                             ;CHECK IF RETRY
        PULS    b                                 ;RESTORE B
        BCC     READS2                            ;TRY AGAIN?

READS6
        RTS

;* GETCUR
;*
;* GETCUR GETS THE CURRENT RECORD ADDRESS
;* (TRACK AND SECTOR) INTO A & B.
;* IF ILLEGAL ADDRESS, CARRY IS SET.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ILLEGAL ADR.
;*          A = TRACK NUMBER
;*          B = SECTOR NUMBER
;*          X POINTS TO FSB

GETCUR
        LDX     FCBSTR                            ;PICKUP FCB
        LDD     FCS,x                             ;GET TRACK & SECTOR
        LEAX    FSB,x                             ;ADD FSB BIAS
        RTS

;* CLRTRY
;*
;* CLRTRY CLEARS THE ERROR TRY COUNTERS
;*
;*   ENTRY: NONE
;*   EXIT:  A CLEARED

CLRTRY
        CLRA                                      ;CLEAR A
        STA     ETRIES                            ;CLEAR COUNTERS
        STA     STRIES
        RTS

;* RETRY
;*
;* RETRY WILL TEST THE TRY COUNTERS
;* ETRIES AND STRIES TO SEE IF THEY
;* ARE AT MAXIMUM.
;* A RESTORE OPERATION IS PERFORMED
;* IF NECESSARY.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF NO MORE TRIES LEFT

RETRY
        BITB    #$10                              ;CHECK IF SEEK ERROR
        BNE     RETRY2                            ;
        BITB    #$80                              ;CHECK NOT READY
        BNE     RETRY6                            ;
        LDB     ETRIES                            ;CHECK ERROR CNTR
        INCB                                      ;BUMP IT ONE
        CMPB    #RTC                              ;IS IT MAXIMUM?
        BEQ     RETRY2                            ;
                                                  ;
        STB     ETRIES                            ;SAVE COUNT
        BRA     RETRY4                            ;
                                                  ;
RETRY2
        CLR     ETRIES                            ;CLEAR COUNTER
        LDB     STRIES                            ;CHECK SEEK CNTR
        INCB                                      ;BUMP IT
        CMPB    #RSC                              ;IS IT MAXIMUM?
        BEQ     RETRY6                            ;
        STB     STRIES                            ;SAVE COUNTER
        LDX     FCBSTR                            ;
        JSR     RESTORE                           ;GO RESTORE
                                                  ;
RETRY4
        ANDCC   #$FE                              ;CLC CLEAR ERROR
        RTS                                       ;
                                                  ;
RETRY6
        ORCC    #1                                ;SEC SET ERROR
        RTS

;* WRITSS   ;* FFC #10 ;*
;*
;* WRITSS IS THE SYSTEM WRITE SINGLE
;* SECTOR ROUTINE.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF WRITE ERROR
;*          ALL REGISTERS CHANGED

WRITSS
        BSR     CLRTRY                            ;CLEAR TRY COUNTERS
        LDX     FCBSTR                            ;SET POINTER
        JSR     DRIVE                             ;DO DRIVE SEL
        BCS     WRITS6                            ;
                                                  ;
WRITS2
        LDX     FCBSTR                            ;SET POINTER
        BSR     GETCUR                            ;GET CURRENT SEC
        JSR     WRITE                             ;DO ACTUAL WRITE
        BNE     WRITS4                            ;ERRORS?
                                                  ;
        LDA     VRFYFG                            ;VERIFY SECTOR?
        BEQ     SWTSE6                            ;NO VERIFY
                                                  ;
        JSR     VERIFY                            ;GO DO VERIFY
        BEQ     SWTSE6                            ;ERROR?
                                                  ;
WRITS4
        BITB    #$40                              ;CHECK IF W.P.
        BNE     WRITS8                            ;
                                                  ;
        PSHS    b                                 ;SAVE STATUS
        BSR     RETRY                             ;RETRY?
        PULS    b                                 ;RESTORE STATUS
        BCC     WRITS2                            ;TRY AGAIN
WRITS6
        RTS                                       ;ERROR RETURN
                                                  ;
WRITS7
        LDB     #$20                              ;SET ERROR
WRITS8
        ORCC    #1                                ;SHOW ERROR
        RTS                                       ;ERROR RETURN

;* SWTSEQ
;*
;* SWTSEQ IS THE HIGH LEVEL WRITE
;* SEQUENTIAL CHARACTER ROUTINE.
;* SPACE COMPRESSION IS HANDLED HERE
;* UNLESS FSC IS NEGATIVE.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR
;*          B HAS ERROR NUMBER
;*          A & X CHANGED

SWTSEQ
        LDX     FCBSTR                            ;PICKUP FCB
        LDB     FSC,x                             ;GET SPC COUNT
        BMI     WTSEQ                             ;WRITE IF NEG.
        CMPA    #SPC                              ;IS CHARACTER SPACE?
        BNE     SWTSE4                            ;
        INCB                                      ;BUMP THE COUNT
        STB     FSC,x                             ;SAVE IT
        CMPB    #MAXSP                            ;MAXIMUM?
        BNE     SWTSE6                            ;
        BRA     PUTSPC                            ;;*;* FIX FOR 128 SPACES ;*;*
                                                  ;
SWTSE2
        BSR     PUTSPC                            ;GO PUT SPACES
        BCC     SWTSEQ                            ;ERRORS?
        RTS                                       ;
                                                  ;
SWTSE4
        TSTB                                      ;COUNT ZERO?
        BEQ     WTSEQ                             ;
        BRA     SWTSE2                            ;DO SPACES
                                                  ;
SWTSE6
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS

;* PUTSPC
;*
;* PUTSPC IS THE ROUTINE WHICH WRITES
;* THE SPACE COMPRESSION CODE AND THE
;* SPACE COUNT.
;*
;*   ENTRY: B HAS COUNT
;*   EXIT:  A PRESERVED

PUTSPC
        PSHS    a                                 ;SAVE CHAR
        CMPB    #1                                ;IS COUNT 1?
        BNE     PUTSP2                            ;
        LDA     #$20                              ;SETUP SPACE
        BRA     PUTSP4                            ;GO WRITE
                                                  ;
PUTSP2
        LDA     #SCC                              ;SETUP SCC
        BSR     WTSEQ                             ;WRITE IT OUT
        PULS    a                                 ;
        BCS     PUTSP6                            ;ERROR?
        PSHS    a                                 ;SAVE CHAR
        LDX     FCBSTR                            ;PICKUP FCB
        LDA     FSC,x                             ;GET COUNT
PUTSP4
        CLR     FSC,x                             ;CLEAR COUNT
        BSR     WTSEQ                             ;WRITE COUNT
        PULS    a                                 ;RESTORE CHAR
PUTSP6
        RTS

;* WTSEQ
;*
;* WTSEQ WRITES THE NEXT CHARACTER TO
;* THE FSB AREA OF THE FCB.
;*
;*   ENTRY: A HAS CHARACTER
;*   EXIT:  CS IF ERROR
;*          B HAS ERROR NUMBER
;*          X CHANGED

WTSEQ
        LDX     FCBSTR                            ;GET FCB POINTER
        LDB     FAS,x                             ;GET ACTIVE STATUS
        CMPB    #2                                ;IS IT WRITE?
        LBNE    REWIN2                            ;ERROR IF NOT
        LDB     FDI,x                             ;GET INDEX
        CMPB    #RS                               ;IS IT BEGINNING?
        BNE     WTSEQ2                            ;
        PSHS    a                                 ;SAVE CHAR.
        BSR     WTNEXT                            ;GO WRITE RECORD
        PULS    a                                 ;RESTORE CHAR.
        BCS     WTSEQ4                            ;ERRORS?
                                                  ;
WTSEQ2
        JSR     PUTNXT                            ;PUT CHARACTER
        BCC     WTSEQ4                            ;LAST SPACE USED?
        LDB     #RS                               ;SET RECORD START
        LDX     FCBSTR                            ;POINT TO FCB
        STB     FDI,x                             ;SET INDEX
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
WTSEQ4
        RTS

;* CLRLRN

CLRLRN
        LDX     FCBSTR                            ;GET POINTER
        CLRA                                      ;
        CLRB                                      ;
        STD     FRN,x                             ;CLEAR LRN
        STD     FSB+2,x                           ;CLEAR ACTUAL RN
        BRA     WTNEX1                            ;GO WRITE NEXT

;* WTNEXT
;*
;* WTNEXT WRITES THE NEXT RECORD.
;*
;*   ENTRY: X = FCB
;*   EXIT:  CS IF ERROR
;*          REGISTERS CHANGED

WTNEXT
        LDB     FSA+1,x                           ;FIRST RECORD?
        BNE     WTNEX1                            ;
        LDB     FMP,x                             ;CHECK FOR RANDOM
        BEQ     ASNNXT                            ;
        CLR     FMP,x                             ;CLEAR FLAG
        BSR     ASNNXT                            ;GO ASSIGN
        BCS     GETFS2                            ;ERROR?
        BSR     CLRLRN                            ;CLEAR LRN
        BCS     GETFS2                            ;ERROR?
        BSR     CLRLRN                            ;CLEAR NEXT
        BCS     GETFS2                            ;ERROR?
        LDX     FCBSTR                            ;GET FCB
        LDB     #2                                ;SET FSM COUNT
        STB     FMP,x                             ;
        LDD     FSA,x                             ;GET START ADDR
        JMP     UPDF75                            ;GO DO UPDATE
                                                  ;
WTNEX1
        BSR     GETFST                            ;GET FIRST AVAIL
        LDX     FCBSTR                            ;SET FCB PNTR
        STD     FSB,x                             ;SET FORWARD LINK
        JSR     WRITSS                            ;WRITE RECORD
        BCC     ASNNXT                            ;ERROR?
        JMP     WRTERR                            ;REPORT ERROR

;* GETFST
;*
;* GETFST LOADS THE FIRST AVAILABLE
;* SECTOR ADDRESS INTO A & B.
;*
;*   ENTRY: NONE
;*   EXIT:  EQ IF FSTAVL = 00
;*          X UNCHANGED
;*          A & B = FSTAVL H & L

GETFST
        BSR     FSECMP                            ;FIND SECTOR MAP
        LDD     ,x                                ;GET FIRST AVAIL.
GETFS2
        RTS

;* FSECMP
;*
;* FSECMP FINDS THE SECTOR MAP
;* CORRESPONDING TO THE CURRENT
;* DRIVE SELECTED IN THE FCB.
;*
;*   ENTRY: NONE
;*   EXIT:  EQ IF AVAIL NOT SET
;*          X POINTS TO SECTOR MAP

FSECMP
        LDX     FCBSTR                            ;PICKUP FCB
        LDB     FDN,x                             ;GET DRIVE NUMBER
        LDA     #6                                ;MULTIPLY TIMES 6
        MUL                                       ;
        LDX     #SECMAP                           ;POINT TO MAPS
        ABX                                       ;FIX POINTER
        STX     AVLPNT                            ;SET POINTER
        TST     ,x                                ;IS AVAIL EMPTY?
        RTS

;* ASNNXT
;*
;* ASNNXT ASSIGNS THE NEXT AVAILABLE
;* RECORD TO THE CURRENT OPEN WRITE
;* FILE POINTED TO BY THE FCB.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR
;*          ALL REGISTERS CHANGED

ASNNXT
        BSR     GETFST                            ;GET FSTAVL
        BNE     ASNNX2                            ;IS IT ZERO?
        LDB     #DFER                             ;DISK FULL!
ASNNX1
        ORCC    #1                                ;SEC SET ERROR
        RTS                                       ;
                                                  ;
ASNNX2
        LDX     FCBSTR                            ;GET FCB POINTER
        STD     FEA,x                             ;SET END ADDRESS
        TST     FSA+1,x                           ;IS THIS FIRST?
        BNE     ASNNX4                            ;JUMP AHEAD IF NOT
        STD     FSA,x                             ;SET START ADDRESS
                                                  ;
ASNNX4
        INC     FSZ+1,x                           ;BUMP FILE SIZE
        BNE     ASNNX6                            ;
        INC     FSZ,x                             ;FIX MSB
                                                  ;
ASNNX6
        TST     FMP,x                             ;CHECK FOR RANDOM
        BEQ     ASNN65                            ;
        JSR     UPDFSM                            ;UPDATE FSM ENTRY
        BCS     ASNNX1                            ;ERROR?
        LDX     FCBSTR                            ;GET FCB
        LDD     FEA,x                             ;GET END ADDRESS
                                                  ;
ASNN65
        JSR     RDNEX2                            ;READ NEXT RECORD
        BCS     ASNNX1                            ;READ ERROR?
        LDX     FCBSTR                            ;POINT TO FCB
        LDD     FSB,x                             ;GET FOR. LINK
        PSHS    a,b                               ;SAVE LINK
        BSR     FSECMP                            ;FIND SECTOR MAP
        PULS    a,b                               ;RESTORE LINK
        STD     ,x                                ;SET FIRST AVAIL
        BNE     ASNNX7                            ;FULL DISK??
                                                  ;
        CLR     2,x                               ;CLEAR OUT REST
        CLR     3,x                               ;
        CLR     4,x                               ;
        CLR     5,x                               ;
        BRA     ASNNX8                            ;GO AHEAD
                                                  ;
ASNNX7
        LDY     4,x                               ;GET SEC COUNT
        LEAY    -1,y                              ;DEC BY 1
        STY     4,x                               ;SAVE RESULT
                                                  ;
ASNNX8
        CLRA                                      ;CLEAR REGISTER
        LDX     FCBSTR                            ;SET POINTER
        INC     FRN+1,x                           ;INC LRN
        BNE     ASNN85                            ;
        INC     FRN,x                             ;
                                                  ;
ASNN85
        CLRB                                      ;GET SECTOR LENGTH \\\\
ASNNX9
        STA     FSB,x                             ;CLEAR LOCATION
        LEAX    1,x                               ;BUMP POINTER
        DECB                                      ;DEC THE COUNTER
        BNE     ASNNX9                            ;REPEAT?
                                                  ;
        LDX     FCBSTR                            ;GET FCB PNTR
        LDD     FRN,x                             ;GET LRN
        STD     FSB+2,x                           ;SAVE IN DATA
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS

;* OPN - SIR, DIR
;*
;* OPNIR OPENS EITHER THE SYSTEM IR
;* OR THE DIRECTORY IR, DEPENDING ON
;* THE ENTRY POINT.
;*
;*   ENTRY: NONE
;*   EXIT:  B & X DESTROYED

OPNSIR
        CLRB                                      ;SET TRACK 0
        PSHS    b
        LDB     #ISEC                             ;GET SECTOR
        BRA     OPNIR

;* ------- No Path to this code

OPNCUD
        LDX     CUD                               ;GET CUD
        STX     CLD                               ;SAVE AS LOOKUP
                                                  ;
;* -------                   ;

OPNDIR: ;
OPNCLD
        LDB     CLD                               ;GET TRACK
        PSHS    b                                 ;SAVE IT
        LDB     CLD+1                             ;GET SECTOR
                                                  ;
                                                  ;
OPNIR
        LDX     FCBSTR                            ;SET FCB POINTER
        STB     FSB+1,x                           ;SAVE SECTOR
        PULS    b                                 ;GET TRACK
        STB     FSB,x                             ;SET SECTOR PNTR
        CLR     BKLN                              ;CLEAR BACK LINK
        CLRB                                      ;GET SECTOR LENGTH \\\\
        STB     FDI,x                             ;SAVE IT
        RTS

;* GETIR
;*
;* GETIR GETS THE NEXT INFORMATION
;* RECORD (IR) FROM THE FSB.
;*
;*   ENTRY: X = FCB
;*   EXIT:  CS IF ERROR

GETIR
        LDX     FCBSTR                            ;SET FCB POINTER
        LDB     FDI,x                             ;GET DATA INDEX
        BNE     GETIR2                            ;NEXT SECTOR?
                                                  ;
        JSR     RDNEXT                            ;GET NEXT SECTOR
        BCS     GETIR8                            ;ERROR?
                                                  ;
        LDX     FCBSTR                            ;GET FCB POINTER
        TST     BKLN                              ;TEST BACK LINK
        BNE     GETIR1                            ;IS IT SET?
                                                  ;
        LDD     #$0005                            ;< OLD Code
;*       ldd     FSB+4,x    	;GET NEW BL  <- CORRECTED
        STD     BKLN                              ;SAVE IT
                                                  ;
GETIR1
        LDA     #IRS                              ;SET START INDEX
        STA     FDI,x                             ;
        LDD     FCS,x                             ;GET CURRENT SEC ADR
        STD     FCD,x                             ;SET CURRENT IR
                                                  ;
GETIR2
        LDA     FDI,x                             ;GET CURRENT INDEX
        STA     FCD+2,x                           ;SAVE IT
        LDB     #IRL                              ;SET LENGTH
                                                  ;
GETIR4
        PSHS    b,x                               ;SAVE VALUES
        JSR     RDSEQ                             ;READ NEXT CHAR
        PULS    b,x                               ;RESTORE VALUES
        STA     FFN,x                             ;PUT THE CHAR
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNT
        BNE     GETIR4                            ;FINISHED?
;*       clrb                ;CLEAR ERRORS
        ANDCC   #$FE                              ;CLC CLEAR ERROR
GETIR8
        RTS

;* PUTIR
;*
;* PUTIR PUTS THE IR INTO THE FSB.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR

PUTIR
        LDX     FCBSTR                            ;SET FCB POINTER
        LDA     FCD+2,x                           ;GET INDEX
        STA     FDI,x                             ;SET IT
        LDB     #IRL                              ;SET LENGTH COUNT
                                                  ;
PUTIR2
        PSHS    b,x                               ;SAVE POINTERS
        LDA     FFN,x                             ;GET THE CHAR
        JSR     WTSEQ                             ;GO WRITE CHAR
        PULS    b,x                               ;RESTORE POINTERS
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNT
        BNE     PUTIR2                            ;REPEAT?
                                                  ;
        JMP     WRITSS                            ;GO WRITE SECTOR

;* FNDNAM
;*
;* FNDNAM TRIES TO FIND THE NAME IN
;* FFN IN THE DIRECTORY.
;*
;*   ENTRY: NAME IN FFN
;*   EXIT:  EQ IF FOUND
;*          CS IF ERROR ( IN B )
;*          REGISTERS CHANGED

FNDNAM
        LDX     FCBSTR                            ;GET FCB
        LDA     FDN,x                             ;GET DRIVE NUM
        STA     FRI,x                             ;SAVE IT IN TEMP
        LDA     DIRDN                             ;GET DIR DRIVE NUM
        TST     SINDIR                            ;SINGLE DIR ?
        BNE     FNDNA1                            ;
                                                  ;
        STA     FDN,x                             ;SET NEW DRIVE NUM
        LDX     CUD                               ;GET DIR POINTER
        STX     CLD                               ;SET CLD
                                                  ;
FNDN04
        CMPX    #MAIND                            ;IS IT MAIN?
        BEQ     FNDN06                            ;
        BSR     FNDNA1                            ;SEARCH DIR
        BLS     FNDNA3                            ;FIND OR ERROR?
        LDX     BKLN                              ;GET BACK LINK
        STX     CLD                               ;SET CLD
        BRA     FNDN04                            ;REPEAT
                                                  ;
FNDN06
        LDX     FCBSTR                            ;SET POINTER
        LDA     FRI,x                             ;RESTORE DN
        STA     FDN,x                             ;
        BPL     FNDNA1                            ;DRIVE SPECIFIC?
                                                  ;
FNDN08
        JSR     NXTRDY                            ;GET NEXT RDY DRV
        BCS     FNDNA9                            ;ERROR?
        BSR     FNDNA1                            ;DO SEARCH
        BLS     FNDNA3                            ;FIND OR ERROR?
        JSR     RSTNAM                            ;RESTORE NAME
        BRA     FNDN08                            ;
                                                  ;
FNDNA1
        LDX     FCBSTR                            ;SET POINTER
        CLR     SINDIR                            ;CLEAR MODE
        JSR     COPNAM                            ;COPY NAME TO FWB
        JSR     OPNDIR                            ;OPEN DIRECTORY
                                                  ;
FNDNA2
        JSR     GETIR                             ;GET RECORD
        BCC     FNDNA4                            ;ERROR?
        CMPB    #EFER                             ;END OF FILE?
        BEQ     FNDNA9                            ;
        ORCC    #1                                ;SEC SET ERROR
FNDNA3
        RTS                                       ;ERROR RETURN
                                                  ;
FNDNA4
        LDX     FCBSTR                            ;POINT TO FCB
        LDA     FFN,x                             ;GET CHAR
        BEQ     FNDNA8                            ;NO MORE?
        BPL     FNDNA6                            ;DELETED NAME?
        BSR     SETFD                             ;SET DELETED
                                                  ;
FNDNA6
        JSR     CMPNAM                            ;COMPARE NAME
        BNE     FNDNA2                            ;EQUAL?
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS                                       ;
                                                  ;
FNDNA8
        BSR     SETFD                             ;
FNDNA9
        ANDCC   #$FB                              ;SHOW NO FIND
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS                                       ;
                                                  ;
SETFD
        LDA     FFD+1,x                           ;FIRST DELETED?
        BNE     SETFD2                            ;
        LDD     FCD,x                             ;GET CURRENT ADR
        STD     FFD,x                             ;SET FIRST DELETED
        LDA     FCD+2,x                           ;GET INDEX
        STA     FFD+2,x                           ;SAVE IT
SETFD2
        RTS

;* GETAVL
;*
;* GETAVL SETS THE SECTOR MAP POINTERS
;* IF THEY HAVE NOT BEEN SET.
;*
;*   ENTRY: NONE
;*   EXIT:  CS SET IF ERROR
;*          REGISTERS CHANGED

GETAVL
        JSR     FSECMP                            ;FIND MAP
        BNE     GETAV3                            ;SET YET?
        BSR     GETDIS                            ;READ IN DIS
        BCS     GETAV4                            ;ERROR?
                                                  ;
        LDB     #6                                ;SET COUNTER
        LDY     FCBSTR                            ;GET FCB
        LDX     AVLPNT                            ;POINT TO MAP

GETAV2
        LDA     FSB+FSA+IRS-4,y
        LEAY    1,y
        STA     ,x+
        DECB                                      ;DEC THE COUNT
        BNE     GETAV2                            ;FINISHED?

GETAV3
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
GETAV4
        RTS

;* GETDIS
;*
;* GETDIS READS IN THE DIS SECTOR.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR
;*          REGISTERS CHANGED

GETDIS
        JSR     OPNSIR                            ;OPEN THE DIS
        JSR     RDNEXT                            ;READ NEXT BLOCK
        BCS     GETDI2                            ;ERROR?
                                                  ;
        LDX     FCBSTR                            ;SET POINTER
        LDB     #IRS                              ;SET START POINT
        STB     FDI,x                             ;SET INDEX
GETDI2
        RTS

;* PUTAVL
;*
;* PUTAVL UPDATES THE DIS SECTOR.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR
;*          REGISTERS CHANGED

PUTAVL
        JSR     FSECMP                            ;FIND SECTOR MAP
        BSR     GETDIS                            ;GO GET DIS
        BCS     GETDI2                            ;ERROR?
        LDB     #6                                ;SET UP COUNTER
        LDY     FCBSTR                            ;GET FCB POINTER
        LDX     AVLPNT                            ;POINT TO MAP

PUTAV2
        LDA     ,x+
        STA     FSB+FSA+IRS-4,y
        LEAY    1,y
        DECB                                      ;DEC THE COUNT
        BNE     PUTAV2                            ;FINISHED?
                                                  ;
        JSR     WRITSS                            ;WRITE SECTOR
        BCC     GETDI2                            ;ERROR?
        JMP     WRTERR                            ;REPORT ERROR

;* WRTDIR
;*
;* WRTDIR UPDATES THE DISK DIRECTORY.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR

WRTDIR
        LDX     FCBSTR                            ;POINT TO FCB
        LDA     #2                                ;SET FOR WRITE
        STA     FAS,x                             ;
        LDD     FCD,x                             ;GET CURRENT DIR
        STD     FCS,x                             ;SET CURRENT SECTOR
        JSR     READSS                            ;READ IN DIR
        BCS     WRTDI2                            ;ERROR?
        JSR     PUTIR                             ;GO WRITE DIR
        BCC     WRTDI4                            ;
        JMP     WRTERR                            ;CHECK FOR WP ERR
                                                  ;
WRTDI2
        LDB     #WTER                             ;SET ERROR
WRTDI4
        RTS                                       ;ERROR RETURN

;* OPNRD
;*
;* OPNRD IS THE HIGH LEVEL SYSTEM
;* ROUTINE WHICH OPENS A FILE FOR
;* A READ OPERATION.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR (IN B)
;*          REGISTERS CHANGED

OPNRD
        JSR     SETFCB                            ;SET FCB POINTER
        BCS     OPNRD2                            ;ERROR?
        JSR     FNDNAM                            ;LOOK UP NAME
        BCS     OPNRD2                            ;ERRORS?
                                                  ;
        BNE     LD9D6                             ;WAS IT FOUND?
                                                  ;
        LDX     FCBSTR                            ;POINT TO FCB
        TST     SINDIR                            ;SINGLE DIR?
        BEQ     OPNRD1                            ;
        LDA     FID,x                             ;CHECK RP BIT
        BITA    #$20                              ;IS IT SET?
        BNE     OPNRD3                            ;REPORT ERROR
                                                  ;
OPNRD1
        JSR     SETMAX                            ;SET MAX SEC
        BCS     OPNERR                            ;ERROR?
        LDD     FSA,x                             ;GET ADDRESS
        STD     FSB,x                             ;SET FRWD LINK
        JSR     SETST                             ;SET STATUS
        LDB     FMP,x                             ;CHECK RANDOM
        BEQ     OPNR15                            ;
                                                  ;
OPNR12
        PSHS    b                                 ;SAVE COUNT
        JSR     RDNEXT                            ;GET NEXT SECTOR
        PULS    b                                 ;RESTORE COUNT
        BCS     OPNRD2                            ;ERROR?
                                                  ;
        DECB                                      ;DEC THE COUNT
        BNE     OPNR12                            ;
                                                  ;
        LDX     FCBSTR                            ;SET FCB PNTR
        CLRB                                      ;GET SECTOR LENGTH \\\\
        STB     FDI,x                             ;
                                                  ;
OPNR15
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
OPNRD2
        RTS                                       ;
                                                  ;
OPNRD3
        LDB     #ADER                             ;READ ACC DENIED
        BRA     OPNERR                            ;
                                                  ;
LD9D6
        LDB     #NFER                             ;FILE NOT FOUND

;* OPEN ERROR

OPNERR
        PSHS    b                                 ;SAVE ERROR
        JSR     REMFCB                            ;REMOVE FCB
        PULS    b                                 ;
        ORCC    #1                                ;SEC  SET ERROR
        RTS

;* OPNWT
;*
;* OPNWT OPENS A FILE FOR WRITE.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR (IN B)
;*          REGISTERS CHANGED

OPNWT
        LDX     FCBSTR                            ;SET POINTER
        TST     FDN,x                             ;CHECK FOR ALL DRIVES
        BPL     OPNWT3                            ;
        JSR     NXTRDY                            ;FIND READY
        BCC     OPNWT3                            ;FOUND ONE
        LDB     #NRER                             ;NONE READY
        RTS                                       ;
                                                  ;
OPNWT3
        JSR     SETFCB                            ;SET FCB LINK
        BCS     OPNERR                            ;ERROR?
        JSR     CLRFCB                            ;CLEAR OUT FCB
        JSR     GETAVL                            ;SETUP SECTOR MAP
        BCS     OPNERR                            ;ERROR?
        JSR     FNDNAM                            ;GO LOOK FOR NAME
        BCS     OPNERR                            ;ERROR?
        BNE     OPNWT4                            ;FIND?
        LDB     #FEER                             ;FOUND - ERROR
        BRA     OPNERR                            ;
                                                  ;
OPNWT4
        JSR     SETMAX                            ;SET MAX SEC
        BCS     OPNERR                            ;ERROR?
        LDX     FCBSTR                            ;POINT TO FCB
        LDB     #10                               ;SET COUNT
OPNWT5
        CLR     FID,x                             ;CLEAR BLOCK
        LEAX    1,x                               ;
        DECB                                      ;DEC THE COUNT
        BNE     OPNWT5                            ;
                                                  ;
        LDX     FCBSTR                            ;
        LDD     FFD,x                             ;GET FIRST DELETED
        BEQ     OPNWT8                            ;EOF ER?
        STD     FCD,x                             ;SET CURRENT DIR
        LDA     FFD+2,x                           ;GET INDEX
        STA     FCD+2,x                           ;SAVE IT
        LDD     DATE                              ;GET DATE
        STD     FDT,x                             ;SET DATE
        LDA     DATE+2
        STA     FDT+2,x

;*       -- not in UniFLEX version

        LDA     FDN,x                             ;GET DRIVE
        LDX     #DRVINFO                          ;GET TABLE POINTER
        LDA     a,x                               ;GET DRIVE INFO
        LDX     FCBSTR                            ;POINT TO FCB
        STA     24,x                              ;
                                                  ;
;*       --                  ;
;
        JSR     RSTNAM                            ;RESTORE NAME
        JSR     WRTDIR                            ;SET DIRECTORY
        BCS     OPNERR                            ;ERROR?
        BSR     SETST                             ;SET STATUS
        LDA     #RS                               ;SET DATA POINTER
        STA     FDI,x                             ;
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS                                       ;
                                                  ;
OPNWT8
        LDX     FCBSTR                            ;POINT TO FCB
        CLR     FMP,x                             ;CLEAR FLAG
        INC     FSA+1,x                           ;SET FSA NON 0
        LDD     FCD,x                             ;GET POSITION
        JSR     RDNEX2                            ;READ SECTOR
        BCS     OPNW85                            ;ERROR?
                                                  ;
        JSR     WTNEX1                            ;GO WRITE NEW
        BCS     OPNW85                            ;ERROR?
                                                  ;
        JSR     WRITSS                            ;WRITE NEW SECTOR
        BCC     OPNWT9                            ;ERROR?
        JSR     WRTERR                            ;REPORT ERROR
                                                  ;
OPNW85
        JMP     OPNERR                            ;
                                                  ;
OPNWT9
        LDX     FCBSTR                            ;SET POINTER
        LDD     FCS,x                             ;GET CURRENT
        STD     FFD,x                             ;SET FIRST DELETED
        LDA     #IRS                              ;SET INDEX
        STA     FFD+2,x                           ;
        JSR     PUTAVL                            ;UPDATE AVLS
        BCS     OPNW85                            ;
        JMP     OPNWT4                            ;FINISH UP

;* SETST
;*
;* SETST SETS THE FCB STATUS AFTER
;* AN OPEN FILE COMMAND.
;*
;*   ENTRY: NONE
;*   EXIT:  A & X CHANGED

SETST
        LDX     FCBSTR                            ;POINT TO FCB
        LDA     FFC,x                             ;GET FUNCTION CODE
        STA     FAS,x                             ;SET ACTIVITY STATUS
        CLR     FFC,x                             ;CLEAR FUNCTION CODE
        CLR     FSC,x                             ;CLEAR SPC COMP
        CLRA                                      ;GET SECTOR LENGTH \\\\
        STA     FDI,x                             ;SET INDEX
        RTS

;* NEXTS
;*
;* NEXTS IS THE SYSTEM ROUTINE TO
;* ADVANCE TO THE NEXT SECTOR.
;*
;*   ENTRY: NONE
;*   EXIT:  ALL CHANGED

NEXTS
        BSR     DOSTAT                            ;CHECK STATUS
        BCS     NEXTS4                            ;ERROR?
        CLR     ,x                                ;
        LSRA                                      ;READING?
        LBCS    RDNEXT                            ;READ NEXT
                                                  ;
        LDB     #RS                               ;SET START
        STB     FDI,x                             ;SET INDEX
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
NEXTS4
        RTS

;* CHKWT
;*
;* CHECK FOR WRITE SECTOR NECESSITY
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR

CHKWT
        LDX     FCBSTR                            ;GET FCB POINTER
        LDA     FAS,x                             ;GET STATUS
        CMPA    #$83                              ;NEED WRITING?
        BNE     CHKWT4                            ;
                                                  ;
        LDA     #3                                ;RESET STATUS
        STA     FAS,x                             ;
                                                  ;
CHKWT2
        JSR     WRITSS                            ;WRITE SECTOR
        LBCS    WRTERR                            ;REPORT ERROR
                                                  ;
CHKWT4
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
        RTS

;* DOSTAT
;*
;* DOSTAT DOES FILE STATUS CHECKING
;*
;*   ENTRY: NONE
;*   EXIT:  ALL CHANGED
;*          CS IF ERROR

DOSTAT
        BSR     CHKWT                             ;CHECK FOR WRITE
        BCS     DOSTA4                            ;ERRORS?
                                                  ;
        LDX     FCBSTR                            ;SET POINTER
        LDA     FAS,x                             ;GET STATUS
        CMPA    #3                                ;IS IT RW?
        BLS     CHKWT4                            ;ERROR?
                                                  ;
        LDB     #STER                             ;SET ERROR
        ORCC    #1                                ;SEC
DOSTA4
        RTS                                       ;ERROR RETURN

;* CLOSE
;*
;* CLOSE A DISK FILE
;*
;*   ENTRY: NONE
;*   EXIT:  ALL CHANGED

CLOSE
        BSR     DOSTAT                            ;CHECK STATUS
        BCS     CLOSE4                            ;ERROR?
        CMPA    #2                                ;IS IT WRITE?
        BEQ     CLOSE2                            ;
                                                  ;
CLOSE1
        LDX     FCBSTR                            ;GET FCB
        CLR     FAS,x                             ;CLEAR STATUS
        JMP     REMFCB                            ;REMOVE FCB
                                                  ;
CLOSE2
        LDA     FSA+1,x                           ;CHECK IF EMPTY
        BNE     CLOSE3                            ;EMPTY?
        JSR     DELNAM                            ;DELETE NAME
        BRA     CLOS35                            ;
                                                  ;
CLOSE3
        BSR     CHKWT2                            ;WRITE SECTOR
        BCS     CLOSE4                            ;ERROR?
        LDX     FCBSTR                            ;GET POINTER
        TST     FMP,x                             ;RANDOM?
        BEQ     CLOS32                            ;
                                                  ;
        JSR     WTFSM                             ;WRITE FSM
        BCS     CLOSE4                            ;ERROR?
                                                  ;
CLOS32
        JSR     WRTDIR                            ;WRITE DIRECTORY
        BCS     CLOSE4                            ;ERROR?
                                                  ;
        JSR     PUTAVL                            ;SET AVL MAP
                                                  ;
CLOS35
        BCC     CLOSE1                            ;ERRORS?
CLOSE4
        RTS                                       ;ERROR RETURN

;* OPNRW
;*
;* OPNRW OPENS A FILE FOR UPDATE
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR

OPNRW
        JSR     OPNRD                             ;OPEN AS READ
        BCS     WTAPP4                            ;ERROR?
        JSR     RDNEXT                            ;READ FIRST SEC
        BCS     WTAPP4                            ;ERRORS?

        LDA     #3                                ;SET RW STATUS
        BRA     WTAPP2                            ;FINISH UP

;* WTAPP
;*
;* WTAPP IS THE SYSTEM OPEN FILE
;* FOR WRITE APPEND. THE FILE MUST
;* EXIST AND NEW DATA IS WRITTEN ON
;* THE END OF THE FILE.
;*
;*   ENTRY: NONE
;*   EXIT:  CS IF ERROR

WTAPP
        JSR     OPNRD                             ;OPEN AS READ
        BCS     WTAPP4                            ;ERRORS?
                                                  ;
        LDX     FCBSTR                            ;GET FCB POINTER
        LDA     FID,x                             ;GET ATT BYTE
        BITA    #$80                              ;CHECK WP BIT
        BNE     WTAPP6                            ;
                                                  ;
        LDD     FEA,x                             ;GET END ADDRESS
        JSR     RDNEX2                            ;READ IN LAST
        BCS     WTAPP4                            ;ERRORS?
                                                  ;
        LDA     #2                                ;SET WRITE STATUS
                                                  ;
WTAPP2
        LDX     FCBSTR                            ;SET POINTER
        STA     FAS,x                             ;SET STATUS
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
WTAPP4
        RTS                                       ;
                                                  ;
WTAPP6
        LDB     #WPER                             ;SET ERROR
        ORCC    #1                                ;SEC
        RTS

;* RENAME
;*
;* RENAME IS THE SYSTEM FILE RENAME
;* ROUTINE.  THE NEW NAME MUST BE IN
;* FCB+FLR.
;*
;*   ENTRY: SEE ABOVE
;*   EXIT:  CS IF ERROR

RENAME
        BSR     SWAP                              ;SWAP NAMES
        JSR     FNDNAM                            ;GO LOOK FOR IT
        BCS     RENAM5                            ;ERROR?
        BEQ     RENAM4                            ;ALREADY EXISTS?
                                                  ;
        LDX     FCBSTR                            ;
        LDB     #NL                               ;SET COUNTER
                                                  ;
RENAM1
        LDA     FWB,x                             ;GET CHAR
        STA     FFN,x                             ;MOVE BACK
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNT
        BNE     RENAM1                            ;
        BSR     SWPNM                             ;SWAP AND FIND
        BCS     RENAM5                            ;ERROR?
        LDX     FCBSTR                            ;GET POINTER
        LDA     FID,x                             ;GET ATT BYTE
        BITA    #$80                              ;
        BNE     WTAPP6                            ;
        BITA    #$60                              ;CHECK DP BIT
        BNE     RENAM6                            ;
        BSR     SWAP                              ;SWAP NAMES
        BRA     DELNA2                            ;WRITE DIRECTORY
                                                  ;
RENAM4
        LDB     #FEER                             ;SET ERROR
        ORCC    #1                                ;SEC SHOW ERROR
RENAM5
        RTS                                       ;
                                                  ;
RENAM6
        LDB     #DPER                             ;SET ERROR
        ORCC    #1                                ;SEC SHOW ERROR
        RTS

;* SWAP
;*
;* SWAP THE NAME IN FLR WITH FFN.
;* IF FLR HAS NULL EXT SET AS FFN.

SWAP
        LDX     FCBSTR                            ;SET FCB POINTER
        LDA     #NL                               ;SET COUNT
        STA     ETRIES                            ;SAVE IT
                                                  ;
SWAP2
        LDA     FFN,x                             ;GET CHARACTER
        LDB     FLR,x                             ;GET OTHER
        STA     FLR,x                             ;SWAP THEM
        STB     FFN,x                             ;
        LEAX    1,x                               ;BUMP TO NEXT
        DEC     ETRIES                            ;DEC THE COUNT
        BNE     SWAP2                             ;AGAIN?
                                                  ;
        LDX     FCBSTR                            ;RESTORE POINTER
        LDA     FNE,x                             ;GET IST CHAR
        BNE     SWAP6                             ;IS IT NULL?
        LDB     #3                                ;SET COUNT
SWAP4
        LDA     FLR+8,x                           ;GET CHAR
        STA     FNE,x                             ;SAVE IT
        LEAX    1,x                               ;BUMP TO NEXT
        DECB                                      ;DEC THE COUNT
        BNE     SWAP4                             ;
SWAP6
        LDX     FCBSTR                            ;RESTORE POINTER
        RTS

;* SWPNM
;*
;* SWAP NAMES AND DO FNDNAM

SWPNM
        BSR     SWAP                              ;GO DO SWAP
SWPNM2
        JSR     FNDNAM                            ;FIND NAME
        BCS     SWPNM4                            ;ERROR?
        BNE     SWPNM5                            ;NO FIND?
                                                  ;
        LDX     FCBSTR                            ;RESTORE POINTER
        ANDCC   #$FE                              ;CLC CLEAR ERRORS
SWPNM4
        RTS                                       ;
                                                  ;
SWPNM5
        LDB     #NFER                             ;SET ERROR
        ORCC    #1                                ;SEC
        RTS

;* DELNAM
;*
;* DELETE FILE NAME IN DIR

DELNAM
        LDX     FCBSTR                            ;POINT TO FCB
        LDA     #$FF                              ;SET NEGATIVE
        STA     FFN,x                             ;SET VALUE
                                                  ;
DELNA2
        JSR     WRTDIR                            ;WRITE DIRECTORY
        LDX     FCBSTR                            ;SET POINTER
        LDA     #0                                ;CLEAR STATUS
        STA     FAS,x
        RTS

;* WRITIT
;*
;* WRITIT PUTS AND WRITES NEW
;* FORWARD LINK IN SECTOR.

WRITIT
        STD     FSB,x                             ;SET NEW LINK
        JSR     WRITSS                            ;WRITE SECTOR
        BCC     WRTER4                            ;
                                                  ;
WRTERR
        BITB    #$40                              ;W.P. ?
        BNE     WRTER1                            ;
        BITB    #$80                              ;
        BEQ     WRTER3                            ;
        LDB     #NRER                             ;SET NOT READY
        BRA     WRTER3                            ;
                                                  ;
WRTER1
        LDB     #WPER                             ;SET WP ERROR
        BRA     WRTER3                            ;
                                                  ;
;* ---- No path to this code ;
;
WRTER2
        LDB     #WTER                             ;SET WRITE ERROR

;* ----

WRTER3
        ORCC    #1
WRTER4
        RTS

;* DELETE
;*
;* DELETE A SYSTEM FILE RETURNING ITS
;* SECTORS BACK TO THE LIST OF AVAIL.
;*
;*   ENTRY: NAME IN FFN
;*   EXIT:  ALL CHANGED
;*          CS IF ERROR

DELETE
        JSR     GETAVL                            ;GET SEC MAP
        BCS     DELET6                            ;ERROR?
        BSR     SWPNM2                            ;FIND NAME
        BCS     DELET6                            ;ERROR?
                                                  ;
        LDX     FCBSTR                            ;GET POINTER
        LDA     FID,x                             ;GET ATT BYTE
        BITA    #$80                              ;CHECK WP BIT
        BNE     DELET7                            ;
                                                  ;
        BITA    #$60                              ;CHECK DP BIT
        BNE     DELET8                            ;
                                                  ;
        JSR     FSECMP                            ;FIND SEC MAP
        LDX     AVLPNT                            ;GET MAP POINTER
        LDD     2,x                               ;GET LAST AVAIL
        BNE     DELET2                            ;IS IT NULL?
        LDX     FCBSTR                            ;RESTORE POINTER
        LDD     FSA,x                             ;GET START ADR
        BEQ     DELET5                            ;
        LDX     AVLPNT                            ;POINT TO AVAILS
        STD     ,x                                ;SET NEW
        BRA     DELET4                            ;JUMP AHEAD
                                                  ;
DELET2
        LDX     FCBSTR                            ;SET POINTER
        JSR     RDNEX2                            ;READ SECTOR
        BCS     DELET6                            ;ERRORS?
        LDX     FCBSTR                            ;RESTORE POINTER
        LDD     FSA,x                             ;GET START ADR
        BEQ     DELET5                            ;
        BSR     WRITIT                            ;SET LINK
        BCS     DELET6                            ;ERROR?
                                                  ;
DELET4
        LDX     FCBSTR                            ;SET FCB PNTR
        LDD     FEA,x                             ;GET END ADR
        LDX     AVLPNT                            ;POINT TO AVAILS
        STD     2,x                               ;SET NEW LAST
        LDX     FCBSTR                            ;SET POINTER
        LDD     FSZ,x                             ;GET SIZE
        LDX     AVLPNT                            ;POINT TO AVAILS
        ADDD    4,x                               ;ADD IN SECTORS
        STD     4,x                               ;SAVE NEW COUNT
                                                  ;
DELET5
        JSR     DELNAM                            ;DELETE NAME
        BCS     DELET6                            ;ERROR?
        JSR     PUTAVL                            ;WRITE AVAIL SEC
DELET6
        RTS                                       ;
                                                  ;
DELET7
        LDB     #WPER                             ;SET ERROR
        BRA     DELET9                            ;
                                                  ;
DELET8
        LDB     #DPER                             ;SET ERROR
DELET9
        ORCC    #1                                ;SEC SHOW ERROR
        RTS

;* UPDFSM
;*
;* UPDATE FILE SECTOR MAP

UPDFSM
        LDD     FCS,x                             ;GET CURRENT SEC
        INCB                                      ;CHECK IF SEQUENTIAL
        CMPB    FMX,x                             ;CHECK MAX
        BLS     UPDFS2                            ;
        LDB     #1                                ;SET SECTOR 1
        INCA                                      ;BUMP TRACK
UPDFS2
        CMPD    FEA,x                             ;CHECK END
        BNE     UPDFS4                            ;
        LDA     SBC,x                             ;CHECK MAX COUNT
        CMPA    #$FF                              ;IS IT MAX?
        BEQ     UPDFS4                            ;
        INCA                                      ;BUMP COUNT
        STA     SBC,x                             ;SAVE IT
        ANDCC   #$fe                              ;clc CLEAR ERRORS
        RTS                                       ;RETURN
                                                  ;
UPDFS4
        BSR     WTFSM                             ;WRITE FSM
        BCS     UPDFS9                            ;ERROR?
        LDX     FCBSTR                            ;SET POINTER
        LDA     FNK+2,x                           ;GET OFFSET
        ADDA    #3                                ;BUMP TO NEXT ENTRY
        BNE     UPDFS8                            ;END OF SECTOR?
        LDD     FCS,x                             ;GET CURRENT
        CMPD    FSA,x                             ;START ADDR?
        BEQ     UPDFS7                            ;
UPDFS6
        LDB     #FSER                             ;SET ERROR
        ORCC    #1                                ;sec
        RTS                                       ;RETURN
                                                  ;
UPDFS7
        LDD     FSB,x                             ;GET LINK
UPDF75
        STD     FNK,x                             ;SET POINTER
        LDA     #4                                ;SET INITIAL OFFSET
UPDFS8
        STA     FNK+2,x                           ;
        LDD     FEA,x                             ;GET END ADDR
        STD     FLR,x                             ;MARK POSITION
        LDA     #1                                ;SET COUNT
        STA     SBC,x                             ;
        ANDCC   #$fe                              ;clc CLEAR ERRORS
UPDFS9
        RTS                                       ;RETURN

;* WTFSM
;*
;* WRITE FILE SECTOR MAP

WTFSM
        LDD     FNK,x                             ;GET RECORD
        JSR     RDNEX2                            ;READ SECTOR
        BCS     UPDFS9                            ;ERROR?
        LDX     FCBSTR
        TFR     x,y
        LDB     FNK+2,x

;* LEAX    B,x   < original code
;* Add NOP and ABX

        NOP
        ABX                                       ;CORRECTED 2/4/80
        LDB     #3
WTFSM2
        LDA     FLR,y
        LEAY    1,y
        STA     FSB,x
        LEAX    1,x
        DECB                                      ;DEC THE COUNT
        BNE     WTFSM2                            ;
        JSR     WRITSS                            ;WRITE SECTOR
        BCC     UPDFS9                            ;ERROR?
        JMP     WRTERR                            ;SET ERROR

;* SETMAX
;*
;* SET MAX SECTOR NUMBER

SETMAX
        JSR     OPNSIR                            ;GET SECTOR
        JSR     RDNEXT                            ;
        BCS     POSI05                            ;ERROR?
        LDX     FCBSTR                            ;SET FCB PNTR
        CLRA                                      ;
        CLRB                                      ;
        STD     FRN,x                             ;CLEAR REC NUM
        LDA     FSB+39,x                          ;GET MAX
        STA     FMX,x                             ;SAVE MAX
        CLRB                                      ;GET SECTOR LENGTH \\\\
SETMA2
        CLR     FSB,x                             ;CLEAR BYTES
        LEAX    1,x                               ;
        DECB                                      ;DEC THE COUNTER
        BNE     SETMA2                            ;
        LDX     FCBSTR                            ;RESTORE POINTER
        ANDCC   #$fe                              ;clc CLEAR ERRORS
        RTS                                       ;RETURN

;* BKREC
;*
;* BACK UP ONE RECORD

BKREC
        LDX     FCBSTR                            ;GET FCB
        LDA     FMP,x                             ;RANDOM?
        BEQ     POSIT0                            ;
        LDD     FRN,x                             ;GET REC NUMBER
        SUBD    #1                                ;DEC BY ONE
        BPL     BKREC2                            ;UNDERFLOW?
        JMP     POSIT8                            ;
BKREC2
        STD     FRN,x                             ;SAVE NEW

;* POSIT
;*
;* POSITION TO FRN RECORD NUMBER

POSIT
        JSR     DOSTAT                            ;CHECK STATUS
        BCS     POSI05                            ;ERROR?
        RORA                                      ;
        BCC     POSIT0                            ;ERROR?
        CLR     ,x                                ;CLEAR FFC
        LDA     FMP,x                             ;CHECK RANDOM
        BNE     POSIT1                            ;ERROR?
                                                  ;
POSIT0
        LDB     #STER                             ;SET ERROR
        ORCC    #1                                ;sec
POSI05
        RTS                                       ;RETURN
                                                  ;
POSIT1
        CLR     ETRIES                            ;CLEAR COUNT
        LDD     FSA,x                             ;GET START ADDR
        LDY     FRN,x                             ;CHECK FOR 0
        BEQ     POSIT7                            ;GO DO ZERO
POSIT2
        JSR     GETFSM                            ;GET FSM
        BCS     POSI05                            ;
        CLRA                                      ;CLEAR COUNT
        CLRB                                      ;
POSIT3
        TST     2,x                               ;CHECK FOR EOF
        BEQ     POSIT8                            ;
        ADDB    2,x                               ;ADD IN NEW
        ADCA    #0                                ;
        STX     DATAPT                            ;
        LDX     FCBSTR                            ;
        CMPD    FRN,x                             ;CHECK NUMBER
        BHS     POSIT6                            ;
POSIT4
        LDX     DATAPT                            ;RESTORE POINTER
        LEAX    3,x                               ;BUMP TO NEXT
        PSHS    a                                 ;SAVE COUNT
        LDA     ETRIES                            ;
        INCA                                      ;BUMP POSITION
        STA     ETRIES                            ;SAVE RESULT
        CMPA    #84                               ;LAST RECORD?
        BEQ     POSIT5                            ;
        CMPA    #168                              ;
        PULS    a                                 ;RESTORE TOTAL
        BEQ     POSIT8                            ;ERROR?
        BRA     POSIT3                            ;REPEAT
POSIT5
        PSHS    b                                 ;
        LDX     FCBSTR                            ;SET POINTER
        LDD     FSB,x                             ;
        BSR     GETFSM                            ;GET FSM
        BCS     POSIT8                            ;ERROR?
        PULS    b                                 ;
        PULS    a                                 ;RESTORE TOTAL
        BRA     POSIT3                            ;REPEAT
POSIT6
        SUBD    FRN,x                             ;SUB REC NUM
        LDX     DATAPT                            ;RESTORE POINTER
        LDA     2,x                               ;
        PSHS    b                                 ; sba
        SUBA    ,s+                               ;
        DECA                                      ;FIX UP COUNT
        TFR     a,b                               ;
        LDA     ,x                                ;GET TRACK
        ADDB    1,x                               ;ADD IN SECTOR
        LDX     FCBSTR                            ;
        BCS     POSI68                            ;
POSI65
        CMPB    FMX,x                             ;MAX?
        BLS     POSIT7                            ;
POSI68
        SUBB    FMX,x                             ;FIX IF SO
        INCA                                      ;BUMP TRACK
        BRA     POSI65                            ;
POSIT7
        JSR     RDNEX2                            ;READ NEXT
        BCS     POSI85                            ;ERROR?
        LDX     FCBSTR                            ;
        LDD     FSB+2,x                           ;GET LRN
        CMPD    FRN,x                             ;COMPARE TO FRN
        BEQ     GETS1                             ;
POSI75
        LDB     #RMER                             ;SET ERROR
        BRA     POSI82                            ;
POSIT8
        LDB     #RRER                             ;SET ERROR
POSI82
        ORCC    #01                               ;sec
POSI85
        RTS                                       ;RETURN

;* GET FSM SECTOR

GETFSM
        JSR     RDNEX2                            ;READ NEXT SEC
        BCS     GETS2                             ;ERROR?
        LDX     FCBSTR                            ;SET INDEX
        LDB     #FSB+4                            ;SET OFFSET
        ABX                                       ;
GETS1
        ANDCC   #$fe                              ; clc CLEAR ERRORS
GETS2
        RTS

;* RSTNAM
;*
;* RESTORE NAME FROM FWB TO FFN.

RSTNAM
        LDX     FCBSTR                            ;SET FCB
        LDB     #NL                               ;SET COUNTER
RSTNA2
        LDA     FWB,x                             ;GET CHARACTER
        STA     FFN,x                             ;PUT IT
        LEAX    1,x                               ;BUMP THE POINTER
        DECB                                      ;DEC THE COUNT
        BNE     RSTNA2                            ;
        RTS                                       ;RETURN

;* NXTRDY
;*
;* NXTRDY RETURNS THE DRIVE NUMBER IN
;* FCB+FDN OF THE NEXT READY DRIVE.
;* CS IF NO MORE READY DRIVES.

NXTRDY
        LDX     FCBSTR                            ;GET FCB
        LDA     FDN,x                             ;GET DRIVE NUMBER
        INCA                                      ;BUMP BY ONE
        CMPA    #4                                ;PAST RANGE?
        BHS     NXTRD6                            ;
        STA     FDN,x                             ;SAVE NEW NUMBER
        BNE     NXTRD2                            ;DRIVE 0 ?
        JSR     CHKRDY                            ;CHECK IF READY
        BRA     NXTRD4                            ;
NXTRD2
        JSR     QUICK                             ;QUICK CHECK
NXTRD4
        BCS     NXTRDY                            ;CHECK NEXT DRIVE
        RTS                                       ;RETURN
NXTRD6
        LDB     #NRER                             ;SET ERROR
        ORCC    #1                                ;sec
        RTS                                       ;RETURN
