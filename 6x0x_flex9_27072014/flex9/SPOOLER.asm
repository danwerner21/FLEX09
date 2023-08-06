*        NAM     SPOOLER.TXT
*        OPT     pag
*       LEN     96
*        page

*************************************************
*                                               *
*       FLEX Spooler (for task switching)       *
*                                               *
*************************************************

;ZC810   equ     $C810
;SYSFCB  equ     $C840
ZC980   equ     $C980

;FMSBUSY equ     $CC30
;CP      equ     $CC31
;ZCC34   equ     $CC34
;PRDYCH  equ     $CCD8
;PTROUT  equ     $CCE4
;PR0     equ     $CCF8
;PR1     equ     $CCFC

;IHNDLR  equ     $D3E7
;TIMOFF  equ     $D3ED

;FMSCAL  equ     $D406
        page

        org     $C700

        jmp     >ZC723          ;scheduler
        jmp     >ZC75C          
        jmp     >ZC75D          ;queue handler
        jmp     >ZC74C		    ; LOCK
        jmp     >ZC757			; UNLOCK
        jmp     >ZC71F          ;IRQ ISR
                                ;
ZC712   fcb     $0C             ;TOF character
        fcb     $00,$00,$00,$00 ;
        fdb     ZC810           ;first queue entry pointer
ZC719   fdb     ZC810           ;current queue entry pointer
ZC71B   fcb     $00             ;queue count
ZC71C   fcb     $00             ;<cr> flag
ZC71D   fcb     $00             ;qcheck 'kill' flag
ZC71E   fcb     $00             ;qcheck 'suspend' flag
                                ;
ZC71F   jmp     [IHNDLRP]        ;handle IRQ
                                ;
*       scheduler               ;
                                ;
ZC723   orcc    #$50            ;disable IRQ and FIRQ
        ldx     >CP             ;get stack address
        sts     $02,x           ;save current system stack
        tst     >PRCNT          ;see if spooling is active
        bne     ZC741           ;yes - go toggle task
                                ;
        ldx     #PR1            ;point to background task stack pointer
        inc     >PRCNT          ;bump spooling active flag
        tst     ,x              ;see if we are printing
        beq     ZC741           ;no - clear spooling flag
                                ;
ZC73A   stx     >CP             ;set task stack pointer
        lds     $02,x           ;restore stack pointer
        rti                     ;return from interrupt
                                ;
*       toggle background and fo;rground tasks
                                ;
ZC741   ldx     #PR0            ;load foreground task stack pointer
        clr     >PRCNT          ;set spooling inactive
        bra     ZC73A           ;go set the foreground task active
                                ;
*                               ;
                                ;
ZC749   swi3                    ;
        nop                     ;
                                ;
ZC74C   orcc    #$50            ;disable IRQ and FIRQ
        tst     >FMSBSY        ; 
        bne     ZC749           ;
        inc     >FMSBSY        ; 
        rts                     ;
                                ;
ZC757   clr     >FMSBSY        ;
        andcc   #$AF            ;
                                ;
ZC75C   rts                     ;
                                ;
*       queue handler           ;
                                ;
ZC75D   orcc    #$50            ;disable IRQ and FIRQ
        tst     >ZC71B          ;see if queue count = 0
        beq     ZC78D           ;yes - queue is empty
        ldx     >ZC719          ;no - get queue control block address
        lda     ,x              ;get drive number
        pshs    a               ;save it
        ldd     $01,x           ;get next track and sector link
        ldx     #ZC980          ;point to queue file control block
        std     $40,x           ;set link in FCB
        puls    a               ;get drive
        sta     $03,x           ;set in FCB
        clr     ,x              ;set function = open for read
        lda     #$01            ;set activity status of FCB
        sta     $02,x           ;
        clr     $22,x           ;clear data index
        clr     $3B,x           ;and set space compression flag on
                                ;
ZC783   tst     >ZC71E          ;are we suspended?
        beq     ZC79B           ;no - output the file to printer
        swi3                    ;yes - go to scheduler
        nop                     ;
        bra     ZC783           ;loop
                                ;
*       turn timer off and exit ;to foreground task
                                ;
ZC78D   clr     >PR1            ;say that background task is done
        andcc   #$AF            ;allow IRQ and FIRQ
        swi3                    ;go to scheduler
        nop                     ;
        bra     ZC78D           ;loop
                                ;
*       output the current file ;in the queue
                                ;
ZC79B   tst     >ZC71D          ;kill flag set?
        bne     ZC7C7           ;yes - set up next queued entry
                                ;
        ldx     #ZC980          ;point to queue FCB
        jsr     >FMS         ;read a byte of data
        bne     ZC7C7           ;error or end of file
                                ;
        tst     >ZC71C          ;<cr> flag set?
        beq     ZC7BC           ;no -
                                ;
        clr     >ZC71C          ;yes - clear it and do a linefeed
        cmpa    #$0A            ;was the character already a linefeed?
        beq     ZC7C3           ;yes - just do one
        pshs    a               ;no - save character
        lda     #$0A            ;and do a line feed
        bsr     ZC7F9           ;output character
        puls    a               ;restore character
                                ;
ZC7BC   cmpa    #$0D            ;is character a <cr>?
        bne     ZC7C3           ;no - then just output it
        sta     >ZC71C          ;yes - set <cr> flag
                                ;
ZC7C3   bsr     ZC7F9           ;output the character and loop
        bra     ZC79B           ;
                                ;
*       got to end of file or FM;S error - do next queued entry
                                ;
ZC7C7   lda     #$0D            ;close output file with a <cr>
        bsr     ZC7F9           ;
        lda     #$0A            ;and line feed
        bsr     ZC7F9           ;
        lda     >ZC712          ;get TOF character
        bsr     ZC7F9           ;output it also
        clr     >ZC71D          ;reset kill flag
        ldx     >ZC719          ;point to current entry control block
        tst     $03,x           ;see if all copies have been done
        beq     ZC7E3           ;yes - do next entry in queue
        dec     $03,x           ;no - decrement count
        jmp     >ZC75D          ;loop on queue handler
                                ;
*       bump control pointer to ;next entry
                                ;
ZC7E3   leax    $04,x           ;bump pointer
        cmpx    #SYSFCB         ;overflow?
        bne     ZC7ED           ;no - set pointer to next
        ldx     #ZC810          ;yes - reset queue pointer
                                ;
ZC7ED   stx     >ZC719          ;set pointer to next entry
        dec     >ZC71B          ;decrement queue count
        jmp     >ZC75D          ;loop on queue handler
                                ;
*                               ;
                                ;
ZC7F6   swi3                    ;
        nop                     ;
                                ;
*       output to printer       ;
                                ;
ZC7F9   jsr     >PCHK           ;PRDYCH printer ready?
        bpl     ZC7F6           ;no - go to scheduler
        jmp     >POUT           ;PTROUT -yes - ouput byte

        end
