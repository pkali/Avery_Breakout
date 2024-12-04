.IF *>0 ;this is a trick that prevents compiling this file alone

;--------------------------------------------------
.proc open
;--------------------------------------------------
; OPEN #1,4,0,"D:LEVEL000.DAT"
 
    ldx #$10            ;IOCB #1
    lda #$03            ;komenda: OPEN
    sta iccmd,x
    lda #<fname         ;adres nazwy pliku
    sta icbufa,x
    lda #>fname
    sta icbufa+1,x
    lda #04            ;kod dostępu: $04 odczyt, $08 zapis, $09 dopisywanie, $0c odczyt/zapis
    sta icax1,x
    lda #$00            ;dodatkowy parametr, $00 jest zawsze dobre
    sta icax2,x 
    jmp ciov
fname .byte "D:LEVEL000.DAT",$9b
.endp
;--------------------------------------------------
.proc bget 
;--------------------------------------------------
; BGET #1,LevelFileBuff,LevelFileBuffLen-2

    ldx #$10            ;IOCB #1
    lda #$07            ;komenda: GET BYTES / BINARY READ
    sta iccmd,x
    lda #<LevelFileBuff        ;adres w pamieci, gdzie maja trafic dane
    sta icbufa,x
    lda #>LevelFileBuff
    sta icbufa+1,x
    lda #<(LevelFileBuffLen-2)        ;wielkosc bloku danych w bajtach
    sta icbufl,x
    lda #>(LevelFileBuffLen-2)
    sta icbufl+1,x
    jmp ciov
.endp
;--------------------------------------------------
.proc close 
;--------------------------------------------------
; CLOSE #1
    ldx #$10            ;IOCB #1
    lda #$0c            ;komenda: CLOSE
    sta iccmd,x
    jmp ciov
.endp


.ENDIF