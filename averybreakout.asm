;Acid Breakout - a break from the daily acid
;build 006, 2010-05-12
;CLEAR!
;あめでと
;---------------------------------------------------
.IFNDEF TARGET
    .def TARGET = 800 ; 5200
.ENDIF
;---------------------------------------------------

         OPT r+  ; saves 10 bytes, and probably works :) https://github.com/tebe6502/Mad-Assembler/issues/10

;---------------------------------------------------
.macro build
    dta d"1.00" ; number of this build (4 bytes)
.endm

.macro RMTSong
      lda #:1
      jsr RMTSongSelect
.endm

;---------------------------------------------------
    icl 'lib/ATARISYS.ASM'
    icl 'lib/MACRO.ASM'

display = $a000
screenWidth = 80   ;in pixels
screenBytes = screenWidth/2   ; in bytes
maxLines = 55      ; number of lines on the screen (must be odd)
spawnProbability = (256*1/5)
margin = 2         ; top and bottom screen safety margin
racquetPosMin = $2 ; min position of the paddle moved by the user
racquetPosMax = screenWidth-8 ; max position of the paddle moved by the user
racquetSize = 10
maxSpeed = 2       ; maximum speed of a ball. must be power of 2 ('and #' used)
maxBalls = 40      ; maximum number of moving balls, <$80 (bpl used!) 
maxMemory = 7      ; number of saved pixel positions 
                   ; Beware! For easier calc somewhere it uses "modulo maxMemory"
                   ; calculations and therefore this value must be a power of 2 -1 (?)!
maxBrickLines = 14 ; maximum number of lines of bricks to be eradicated

    .zpvar xpos ypos .word = $80 ; position of the ball
    .zpvar deXpos deYpos .byte ;position for deletion
    .zpvar dX dY .word ;main loop shortcuts to the table values of delta 
    ;xpos, dx - "static point precision" - [dx+1].[dx] (big endian!)
    ;this static point precision is emulated with .word calcs, just a result is the high byte
    .zpvar color .byte ; color of the pixel to plot
    .zpvar currBall collisionCheck racquetPos MyClok eXistenZstackPtr .byte
    .zpvar xMemAddr yMemAddr .word ; address where to store memories of the current ball
    .zpvar temp .word
    .zpvar displayposition .word
    .zpvar inlevel .word
    .zpvar clearCount clearBallNr .byte
    .zpvar DLI_A DLI_X dliCount .byte
    .zpvar RMT_blocked noSfx SFX_EFFECT .byte
    .zpvar AutoPlay .byte   ; Auto Play flag ($80 - auto)
RMT_zpvars = AutoPlay+1  ; POZOR!!! RMT vars go here
;---------------------------------------------------
    org $2000
MODUL
    ins 'art/muzyka_stripped.rmt',+5  ; my RMT 1.28 on WINE is apparently broken. I lost some hair here (5, not 6)
    .align $100
    icl 'art/rmtplayr.a65'
    ;---------------------------------------------------
    .align $400
font
    ins 'art/Mild West.fnt'  ; https://damieng.com/typography/zx-origins/mild-west/
dl 
    .by SKIP3
    dta MODE2+LMS,a(statusBuffer)
    ;.by $80+$50  # fancy shmancy vscroll square pixels
    ;dta $4f+$20,a(display)	 ;VSCROLL
    ;:((maxlines-1)/2) dta a($2f8f)	
    .by SKIP1+DLII
    .rept (maxlines-1), #
    :3 dta MODEF+LMS, a(display+screenBytes*:1)
    dta MODEF+LMS+DLII, a(display+screenBytes*:1)
    .endr
    ;----    
    .by MODE2+LMS+SCH ;Hscroll
DLracquetAddr0
    .wo racquetDisp
    .by JVB
    .wo dl
;---------------------------------------------------
dl_level 
    :8 .by SKIP8
    dta 6+LMS,a(LevelText)
    dta SKIP8
    dta 6,SKIP4,6
    .by JVB
    .wo dl_level
;---------------------------------------------------
dl_start
    :6 .by SKIP3
    dta 6+LMS,a(StartText)
    dta 6,SKIP4
    dta 6,SKIP8,SKIP8
    dta 6
    .by SKIP1+DLII
    .rept 20, #
    :3 dta MODEF+LMS, a(display+screenBytes*:1)
    dta MODEF+LMS+DLII, a(display+screenBytes*:1)
    .endr
    ;----   
    .by JVB
    .wo dl_start
;---------------------------------------------------
dl_over 
    :8 .by SKIP8
    dta 6+LMS,a(OverText)
    .by SKIP8
    dta 6
    .by JVB
    .wo dl_over
;---------------------------------------------------
racquetDisp
    :42 .byte $0
    .byte $80, $80, $80, $80, $80
    :36 .byte $0

;--------------------------------------------------
statusBuffer
    dta d" Lives: 5    HS: 000000   Score: 000000 "
score   = statusBuffer+33
HiScore = statusBuffer+17
Lives   = statusBuffer+8
LevelText
    dta d" entering level 000 "
    dta d"  HIT       OUT OF  "
    dta d"          BLOCKS    "
OverText
    dta d"     GAME OVER      "
    dta d" YOUR SCORE: 000000 "
StartText
    dta d"                   ,"
    dta d"GAME by PIRX & PECUS"
    dta d"   MUSIC by ALEX    "
    dta d"press start to START"
BlocksToHit=LevelText+26
AllBlocks=LevelText+44
;--------------------------------------------------
    icl 'lib/fileio.asm'
;--------------------------------------------------

;--------------------------------------------------
.proc vint
;--------------------------------------------------
    ;------------JOY-------------
    ;happy happy joy joy
    ;check for joystick now

    ldy PORTA
    tya
    and #$04 ;left
    bne jNotLeft
    ldx racquetPos
    cpx #racquetPosMin+1
    bcc jNotLeft
    dex
    stx racquetPos

jNotLeft
    tya
    and #$08 ;right
    bne jNotRight
    ldx racquetPos
    cpx #racquetPosMax
    bcs jNotRight
    inx
    stx racquetPos
jNotRight
/*
    ;fire
    lda TRIG0
    bne JNotFire
    ...
JNotFire
*/
  
    ;lda racquetPos

    sec
    lda #screenWidth-1
    sbc racquetPos
    lsr
    clc
    adc #<racquetDisp
    sta dlracquetAddr0
    lda #>racquetDisp
    adc #0
    sta dlracquetAddr0+1

    lda racquetPos
    lsr 
    and #$01
    ;sta HSCROL
    
/*  ;pos print
    lda racquetPos
    :4 lsr
    clc
    adc #'0'
    sta hexDump
 
    lda racquetPos
    and #$0F
    clc
    adc #'0'
    sta hexDump+1
*/ 

    mva #0 dliCount
    ; mva #13 VSCROL  ; FOX gfx mode only

/*
    bit RMT_blocked
    bmi SkipRMTVBL
    ; ------- RMT -------
    lda sfx_effect
    bmi lab2
    asl @                       ; * 2
    tay                         ;Y = 2,4,..,16  instrument number * 2 (0,2,4,..,126)
    ldx #0                      ;X = 0          channel (0..3 or 0..7 for stereo module)
    lda #0                      ;A = 0          note (0..60)
    bit noSfx
    smi:jsr RASTERMUSICTRACKER+15   ;RMT_SFX start tone (It works only if FEAT_SFX is enabled !!!)

    lda #$ff
    sta sfx_effect              ;reinit value
lab2
    jsr RASTERMUSICTRACKER+3    ;1 play
    ; ------- RMT -------
SkipRMTVBL

*/
    ;sfx
    lda sfx_effect
    bmi lab2
    asl                         ; * 2
    tay                         ;Y = 2,4,..,16  instrument number * 2 (0,2,4,..,126)
    ldx #3                      ;X = 3          channel (0..3 or 0..7 for stereo module)
    lda #12                     ;A = 12         note (0..60)
    jsr RASTERMUSICTRACKER+15   ;RMT_SFX start tone (It works only if FEAT_SFX is enabled !!!)
;
    lda #$ff
    sta sfx_effect              ;reinit value
;
lab2
    jsr RASTERMUSICTRACKER+3
skipSoundFrame

    jmp XITVBV
.endp
;--------------------------------------------------
.proc DLI
;--------------------------------------------------
/*  # fancy shmancy vscroll screen shenanigangs to get the square pixels
	sta DLI_A
	stx DLI_X
    mva #$80 PRIOR

	ldx dliCount
	sta WSYNC
 
	lda #13
	sta VSCROL

	lda #3
	sta VSCROL
	
	txa
	asl
	asl 
	;lda brickcolorTab,x
	sta COLBAK
	
	inx
	stx dliCount
	ldx DLI_X
	lda DLI_A
	rti
*/
    sta DLI_A
    mva #$80 PRIOR

    lda VCOUNT
    asl 
    asl
    sta WSYNC
    sta COLBAK
    
    lda DLI_A
    rti
.endp
;--------------------------------------------------
main
;--------------------------------------------------
    jsr wait_for_depress
    jsr MakeDarkScreen
    jsr initialize
    RMTsong song_main_menu
    jsr StartScreen
    jsr MakeDarkScreen
    
    mva #$0 AutoPlay    
    jsr ScoreClear
    mva #"5" Lives
    jsr clearscreen
    mva #$0 LevelType
    jsr LoadLevelData.level000  ; set visible number to 000
    jsr BuildLevelFromBuffer
    jsr LevelScreen
    RMTSong song_ingame
gameloop
    jsr initialize.ClearTables
    jsr MainScreen
    jsr PlayLevel
    bit EndLevelFlag    ; reason for end level
    bmi EndOfLife   ; end of life :)
    ; end of level (level up)
    jsr MakeDarkScreen
    jsr NextLevel
    jsr LevelScreen
    ; RMTSong song_ingame
    jsr AudioInit   ; after I/O
    jmp gameloop
EndOfLife
    dec Lives   ; decrease Lives
    lda Lives
    cmp #"0"
    beq gameOver    ; if no lives - game over
    jsr NextLife
    jmp gameloop
gameOver
    ;game over
    RMTSong song_game_over 
    jsr HiScoreCheckWrite
    jsr GameOverScreen
@   lda RANDOM
    and #%00001110
    sta COLPF0
    lda CONSOL
    and #@consol(start) ; START
    beq main
    lda TRIG0   ; fire
    jeq main
    jmp @-

;--------------------------------------------------
.proc StartScreen
;--------------------------------------------------
    jsr MakeDarkScreen
    mva #$ff AutoPlay
    sta LevelType   ; Title
    mva #"9" Lives
    jsr clearscreen
    jsr BuildLevelFromBuffer
    mwa #dl_start dlptrs
    lda #$0 ;+GTIACTLBITS
    sta GPRIOR
    sta COLBAKS
    lda #@dmactl(standard|dma) ; normal screen width, DL on, P/M off
    sta dmactls
    pause 1
StartLoop
    jsr PlayLevel
    bit EndLevelFlag    ; reason for end level
    bmi EndOfStartScreen
    ; end of level (level up)
    jsr NextLevel
    jmp StartLoop
EndOfStartScreen
    rts
.endp
;--------------------------------------------------
.proc NextLife
;--------------------------------------------------
    ldy #maxBalls
    sty eXistenZstackPtr
    ;OK, one ball starts!
    lda eXistenZstack,Y
    dey
    sty eXistenZstackPtr
    tax
    jsr randomStart ;just one random pixxxel 
                    ;previously the whole band of ballz
    rts
.endp
;--------------------------------------------------
.proc NextLevel
;--------------------------------------------------
    lda LevelType
    beq level000
    bmi levelTitle
    ; load level from disk
loadNext
    jsr FileUp
    jsr LoadLevelData
levelTitle
    jsr clearscreen
    jsr BuildLevelFromBuffer
    jsr initialize.ClearTables
    rts ; start level
level000
    mva #1 LevelType    ; switch to files
    ; reset file number to 000
    ldx #2
@   lda StartLevelNumber,x
    sta LevelNumber,x
    dex
    bpl @-
    jmp loadNext
.endp

;--------------------------------------------------
.proc LevelScreen
;--------------------------------------------------
    jsr MakeDarkScreen
    ldx #2
@     lda LevelNumber,x
      sec
      sbc #$20
      sta LevelText+16,x
      dex
    bpl @-
    mwa #dl_level dlptrs
    lda #@dmactl(standard|dma)  ; normal screen width, DL on, P/M off
    sta dmactls
    pause 100
    rts
.endp
;--------------------------------------------------
.proc GameOverScreen
;--------------------------------------------------
    jsr MakeDarkScreen
    ldx #5
@     lda score,x
      sta OverText+33,x
      dex
    bpl @-
    mwa #dl_over dlptrs
    lda #%00110010  ; normal screen width, DL on, P/M off
    sta dmactls
    pause 20
    
    rts
.endp
;--------------------------------------------------
.proc MainScreen
;--------------------------------------------------
    jsr MakeDarkScreen
    mwa #dl dlptrs
    lda #$0 ;+GTIACTLBITS
    sta GPRIOR
    sta COLBAKS
    lda #@dmactl(standard|dma)  ; normal screen width, DL on, P/M off
    sta dmactls
    pause 1
    rts
.endp
;--------------------------------------------------
.proc MakeDarkScreen
;--------------------------------------------------
    mva #0 dmactls             ; dark screen
    ; and wait one frame :)
    pause 1
    rts
.endp
;--------------------------------------------------
.proc PlayLevel
;--------------------------------------------------
loop
    mva #maxBalls-1 currBall
    jsr cyclecolors

flight

    ldx currBall
    lda BalleXistenZ,x
    jeq ballDoesNotexist
    lda xposTableL,x
    sta xpos
    lda xposTableH,x
    sta xpos+1
    
    lda yposTableL,x
    sta ypos
    lda yposTableH,x
    sta ypos+1

    lda dxTableL,x
    sta dX
    lda dxTableH,x
    sta dX+1
    
    lda dyTableL,x
    sta dY
    lda dYTableH,x
    sta dY+1

    ; now, delete the oldest pixel
    ;
    lda memCycleTable,x
    clc
    adc #1 ;next position in the table
    cmp #maxMemory
    bne notMaxMem
    lda #0
notMaxMem    
    sta memCycleTable,x ; memCycleTable saved
    tax
    lda xposMemTableAdrL,x
    sta xMemAddr
    lda xposMemTableAdrH,x
    sta xMemAddr+1
    lda yposMemTableAdrL,x
    sta yMemAddr
    lda yposMemTableAdrH,x
    sta yMemAddr+1
    ;now on zero page I've got the addressess to store the old xPos and yPos
    ldy currBall
    lda (yMemAddr),y
    tax
    lda (xMemAddr),y
    sta dexpos
 
    
    ; and erase the last point in the "snake"
    ;jsr deplot
;--------------------------------------------------
;deplot
; moved here for the speeeeeed
; deyxpos, deypos (.byte) - pixel position
;--------------------------------------------------
   	; let's calculate coordinates from xpos and ypos
    ;lda dexpos
    lsr
    tay
;---
    ;ldx deypos
    lda lineAdrL,x
    sta temp
    lda lineAdrH,x
    sta temp+1

    lda dexpos
    and #$01
    tax
	
    lda (temp),y
    and debittable,x
    sta (temp),y

    ;move the ball!!!
    adw xpos dX xpos

    adw ypos dY ypos



;top bounce
    ; if ypos<margin then bounce
    lda ypos+1
    cmp #margin
    bcs noTop
    ; assuming that here a plot can get only from below, so it is enough to switch dy sign 
    ; sbw #$ffff dy dy ;this does not compile :(
    negw dY
    mva #margin+1 ypos+1  
noTop
    
;bottom bounce
    ; if ypos>maxLines+margin then bounce
    lda ypos+1
    cmp #maxLines+margin
    bcc noBottom

    ; check if the ball hits the racquette
    bit AutoPlay
    bmi GoAuto
    lda CONSOL
    and #@consol(option) ; OPTION
    bne bounceNormally
GoAuto
	jmp bottomBounce ; turns off the ball kill
bounceNormally

    lda ypos+1
    cmp #maxLines+margin*2+maxSpeed*2 ; that makes the ball below the racquet
    bcs flyDown2 ;kinda lame optimisation as A carries ypos+1
    


    lda racquetPos
    sec
    sbc #racquetPosMin+1
    bpl racquettePlus
    lda #0
racquettePlus    
    cmp xpos+1

    jcs flyDown

    clc
    adc #racquetSize-1
    cmp xpos+1
    jcs bottomBounce

flyDown
    lda ypos+1
flyDown2
    cmp #maxLines+margin*6+maxSpeed*2 ;maximum depth
    bcc noBottom
  

ballOut

    lda currBall

eXistenZdEstroy
    ;destroys ball number A
    ;pushes one free slot to the eXistenZstack
    ;ends with !number of balls in X (maxBalls == end of the game)
    ;txa
    ldy eXistenZstackPtr
    iny
    sta eXistenZstack,y
    sty eXistenZstackPtr

    ;ldx currBall
    tax
    lda #0
    sta balleXistenZ,x
 
    jmp flightLoopEnd

bottomBounce
    ; assuming that here a plot can get only from below, 
    ; so it is enough to switch dy sign 
    ; sbw #$ffff dy dy ;this does not compile :(
    negw dY
    mva #maxLines+margin-2 ypos+1
    bit LevelType
    bmi noPingSFX   ; no SFX on title screen
    mva #sfx_ping sfx_effect
noPingSFX    


noBottom

;left bounce
    lda xpos+1
    cmp #margin
    bcs noLeft
    negw dX
    mva #margin+1 xpos+1

noLeft


;right border bounce
    lda xpos+1
    cmp #screenWidth-margin
    bcc noRight
    negw dX
    mva #screenWidth-margin-1 xpos+1
noRight

    ;jsr plot
    ; the full plot copied here to get few cycles and collision
   	; high byte is the integer position
   	; low byte is the "fractional" part
    ; let's calculate coordinates from xpos and ypos
    lda xpos+1
    lsr 
    tay
;---
    ldx ypos+1
    lda lineAdrL,x
    sta temp
    lda lineAdrH,x
    sta temp+1

    ldx color
    
    lda xpos+1
    and #$01
    bne pRightNibble
pLeftNibble
    lda (temp),y
    sta collisionCheck
    and #$0F
    ora LNColtable,x
    sta (temp),y
    lda collisionCheck
    and #$F0
    cmp #%10000000
    jne noCollision
    jmp plotEnd ;unconditional branch
	
pRightNibble
    lda (temp),y
    sta collisionCheck
    and #$F0
    ora RNColtable,x
    sta (temp),y
    lda collisionCheck
    and #$0F
    cmp #%00001000
    jne noCollision

plotEnd
	
    lda ypos+1
    cmp #maxLines+margin-2-1
    jcs noCollision ;ball is outside the screen!

    ;switch direction, Charles
    ; an idea for assuming which direction to switch - dX or dY?
    ; on a diagram below in the middle there is an approached brick
/*
     \      /
      \-dY /
       \  /
    -dX [] -dX
       /  \
      /-dY \  
     /      \
*/    
    ; it means:
    ; if |dX|>|dY| then dX == -dX
    ;   else  dY == -dY
    
    ; get absolute values
    lda dX+1
    bpl dXpositive
;dX is negative here
    lda dY+1
    bpl dXneg_dYpos

;dX and dY are negative here
    cmp dX+1
    bcc dX_gr_dY__dX_dYneg
    ; |dY| >= |dX| ; hour 5
    negw dY
    jmp bounceDone
dX_gr_dY__dX_dYneg
    ; hour 4
    negw dX
    jmp bounceDone

dXneg_dYpos
    ; dY in A
    clc
    adc dX+1
    bpl dY_gr_dX__dXneg_dYpos 
    ; |dX| > |dy|; hour 2
    negw dX
    jmp bounceDone
dY_gr_dX__dXneg_dYpos 
    ; hour 1
    negw dY
    jmp bounceDone

dXpositive
    lda dY+1
    bpl dX_dYpositive
;dX positive, dY negative
    clc
    adc dX+1
    bpl dX_gr_dY__dXpos_dYneg
    ; hour 7
    negw dY
    jmp bounceDone
    
dX_gr_dY__dXpos_dYneg
    ; hour 8
    negw dX
    jmp bounceDone
    
dX_dYpositive
    ;(dY+1)* is in A
    cmp dX+1
    bcc dX_gr_dY__dX_dYpos
    ; dY > dX ; hour 11
    negw dY
    jmp bounceDone
dX_gr_dY__dX_dYpos
    ; dY < dX ; hour 10
    negw dX

bounceDone
    ; if Auto Play or OPTION key presset - no score
    bit AutoPlay
    bmi NoScoreUp
    lda CONSOL
    and #%00000100 ; OPTION
    beq NoScoreUp
    jsr ScoreUp
NoScoreUp
    dew BricksInLevel
    lda BricksInLevel
    ora BricksInLevel+1
    bne NoLevelEnd
    ; all bricks gone - level ended!
    jmp GoNextLevel
NoLevelEnd
    ;spawn the new bally
    ; if there is still an empty slot for a new ball somewhere...
    ;lda RANDOM
    ;cmp #spawnProbability
    ;bcs noCollision 
    lda color
    cmp #1
    bne noCollision

eXistenZcReate
    ;creates a new ball
    ;removes one free slot from the eXistenZstack
    ;ends with ball number in X
    ;ends with zero when there is no free slot for a ball 
    ldy eXistenZstackPtr
    beq noMoreSlots 
    lda eXistenZstack,Y
    dey
    sty eXistenZstackPtr
    tax

    ;OK, in X there is an empty slot for a ball

    ;spawn it
    lda #1
    sta balleXistenZ,x
    lda xPos
    sta xPosTableL,x
    lda xPos+1
    sta xPosTableH,x
    lda yPos
    sta yPosTableL,x
    lda yPos+1
    sta yPosTableH,x

; random initial speed and direction
    lda random
    bpl dXplus
    
    lda #-1
    sta dxTableH,x
    bne dXlower

dXplus
    lda #1	
    sta dxTableH,x
dXlower    
    lda random
    sta dxTableL,x


    ;randomize 1 maxSpeed-1 ;dy can not be too small or the game would take forever
    lda #1
    sta dyTableH,x
    lda random
    sta dyTableL,x
 
    ;pong
    bit LevelType
    bmi noPongSFX   ; no SFX on title screen
    mva #sfx_pong sfx_effect
noPongSFX
noCollision
noMoreSlots


flightLoopEnd 
;end of the cycle for one ball
    ;save the changes now
    ldx currBall

    ; let's save the position for the future erase
    ; old position of ball(currBall) is saved here
    ; in table nr memCycleTable(currBall)
    ldy currBall
    lda xpos+1            ;high byte is the integer position
    sta (xMemAddr),y
    lda ypos+1
    sta (yMemAddr),y
    ; saved

    lda xpos
    sta xposTableL,x
    lda xpos+1
    sta xposTableH,x
    
    lda ypos
    sta yposTableL,x
    lda ypos+1
    sta yposTableH,x

    lda dX
    sta dxTableL,x
    lda dX+1
    sta dxTableH,x
   
    lda dY
    sta dyTableL,x
    lda dY+1
    sta dYTableH,x
 


endOfBallzLoop
       
    dec currBall
    jpl flight


    pause 1 ;all balls
    bit AutoPlay
    bpl NoAuto
    pause 1 ;additional pause if auto play mode (slower)
    lda CONSOL
    and #@consol(start) ; START
    beq LevelOver   ; Start pressed in Auto Play - exit
    lda TRIG0
    beq LevelOver
    
NoAuto
    lda eXistenZstackPtr
    cmp #maxBalls
    jne loop
LevelOver
    ; level over
    mva #$ff EndLevelFlag
    jsr wait_for_depress
    rts
    

;-------------------
ballDoesNotexist
    ;a delay loop for a ball that does not really exist (yet)
    ldx #70
delayLoop
    dex    
    bne delayLoop
    jmp endOfBallzLoop

GoNextLevel
    mva #0 EndLevelFlag     ; level ended!
    rts
    
.endp
;--------------------------------------------------
.proc fatplot
; xpos, ypos (.byte) - pixel position
; xpos<80
; pixel color in "color"
;--------------------------------------------------
   	; let's calculate coordinates from xpos and ypos
    lda xpos
    lsr 
    tay
;---
    ldx ypos
    lda lineAdrL,x
    sta temp
    lda lineAdrH,x
    sta temp+1
    
    ldx color

    lda xpos
    and #$01
    bne fpRightNibble
fpLeftNibble
    lda (temp),y
    and #$0F
    ora LNColtable,x
    sta (temp),y
    rts
	
fpRightNibble
    lda (temp),y
    and #$F0
    ora RNColtable,x
    sta (temp),y
    rts
.endp
;--------------------------------------------------
.proc fatdeplot
; deyxpos, deypos (.byte) - pixel position
;--------------------------------------------------
   	; let's calculate coordinates from xpos and ypos
    lda dexpos
    lsr
    tay
;---
    ldx deypos
    lda lineAdrL,x
    sta temp
    lda lineAdrH,x
    sta temp+1

    lda dexpos
    and #$01
    tax
	
    lda (temp),y
    and debittable,x
    sta (temp),y
    rts
.endp
;--------------------------------------------------
.proc clearDeadBall
;--------------------------------------------------
;dead ball in clearBallNr
   
   ldx #maxMemory-1
   stx clearCount

clearDeadLoop
    ldx clearCount
    lda xposMemTableAdrL,x
    sta xMemAddr
    lda xposMemTableAdrH,x
    sta xMemAddr+1
    lda yposMemTableAdrL,x
    sta yMemAddr
    lda yposMemTableAdrH,x
    sta yMemAddr+1
    ;now on zero page I've got the addressess to store the old xPos and yPos
    ldy clearBallNr
    lda (xMemAddr),y
    sta dexpos
    lda (yMemAddr),y
    sta deypos
    
    jsr fatdeplot

    dec clearCount
    bpl clearDeadLoop    

    rts
.endp
;--------------------------------------------------
.proc ScoreUp
;--------------------------------------------------
    inc score+5
    lda score+5
    cmp #"9"+1  ; 9+1 character code
    bne ScoreReady
    lda #"0"    ; 0 character code
    sta score+5
    inc score+4
    lda score+4
    cmp #"9"+1  ; 9+1 character code
    bne ScoreReady
    lda #"0"    ; 0 character code
    sta score+4
    inc score+3
    lda score+3
    cmp #"9"+1  ; 9+1 character code
    bne ScoreReady
    lda #"0"    ; 0 character code
    sta score+3
    inc score+2
    ; bonus !!! :)
    lda Lives
    cmp #"9"
    beq noLivesUP
    inc Lives
    mva #05 sfx_effect
    mva #$ff COLBAKS
    pause 2 ; sorry
    inc COLBAKS
noLivesUP
    ;----------
    lda score+2
    cmp #"9"+1  ; 9+1 character code
    bne ScoreReady
    lda #"0"    ; 0 character code
    sta score+2
    inc score+1
    lda score+1
    cmp #"9"+1  ; 9+1 character code
    bne ScoreReady
    lda #"0"    ; 0 character code
    sta score+1
    inc score
ScoreReady
    rts
.endp
;--------------------------------------------------
.proc ScoreClear
;--------------------------------------------------
    lda #"0"
    ldx #4
@   sta score,x
    dex
    bpl @-
    rts
.endp
;--------------------------------------------------
.proc HiScoreCheckWrite
; It checks if the score is greater than hiscore.
; If yes - rewrites the score to hiscore.
;--------------------------------------------------
    lda HiScore
    cmp score
    bcc higher1
    bne lower
    lda HiScore+1
    cmp score+1
    bcc higher2
    bne lower
    lda HiScore+2
    cmp score+2
    bcc higher3
    bne lower
    lda HiScore+3
    cmp score+3
    bcc higher4
    bne lower
    lda HiScore+4
    cmp score+4
    bcc higher5
    bne lower
    lda HiScore+5
    cmp score+5
    bcc higher6
lower
    rts
higher1
    lda score
    sta HiScore
higher2
    lda score+1
    sta HiScore+1
higher3
    lda score+2
    sta HiScore+2
higher4
    lda score+3
    sta HiScore+3
higher5
    lda score+4
    sta HiScore+4
higher6
    lda score+5
    sta HiScore+5
    rts
.endp
;--------------------------------------------------
.proc clearScreen
;--------------------------------------------------
    lda #0
    tax
@
    :(maxLines*40/256+1) sta display+$100*#,x
    inx
    bne @-
    rts
.endp
;--------------------------------------------------
.proc cyclecolorsReset
;--------------------------------------------------
    ldy #6
cycleRloop
    lda colorCycleTabReset,y
    sta colorCycleTab,y
    dey
    bpl cycleRloop
    mva #0 color
.endp
;--------------------------------------------------
.proc cyclecolors
;--------------------------------------------------
    inc color
    lda color
    cmp #8
    bne nocolorReset
    mva #1 color
nocolorReset
    ldy #6
cycleCloop
    lda colorCycleTab,y
    ;sta COLPM1,y
    sta PCOLR1,y
    dey
    bpl cycleCloop

;shift colors
/*
            2
        	2	4
        2	4	6
    	2	4	6	8
    2	4	6	8	10
	2	4	6	8	10	12
2	4	6	8	10	12	14
4	6	8	10	12	14	2
6	8	10	12	14	2	4
8	10	12	14	2	4	6
10	12	14	2	4	6	8
12	14	2	4	6	8	10
14	2	4	6	8	10	12
2	4	6	8	10	12	14
4	6	8	10	12	14	
6	8	10	12	14    
8	10	12	14    	
10	12	14        
12	14        	
14            
261	262	263	264	265	266	267
*/
cct = colorCycleTab
    ldx cct+6
    mva cct+5 cct+6
    mva cct+4 cct+5
    mva cct+3 cct+4
    mva cct+2 cct+3
    mva cct+1 cct+2
    mva cct+0 cct+1
    stx cct+0

    rts
.endp
;--------------------------------------------------
colorCycleTab

    .by 14,2,4,6,8,10,12

colorCycleTabReset
    .by 14,2,4,6,8,10,12
brickcolorTab
    .by 0
    
;--------------------------------------------------
.proc AudioInit
;--------------------------------------------------
    ; pokeys init
    lda #3
    sta skctl ; put Pokey into Init
    sta skctl+$10
    ldx #8
    lda #0
@   
      sta $D200,x ; clear all voices, set AUDCTL to 00
      sta $D210,x ; clear all voices, set AUDCTL to 00
      dex
    bpl @-
    rts
.endp
;--------------------------------------------------
.proc initialize
;--------------------------------------------------
     
    mva #>font CHBAS
    mva #$00 PCOLR0 ; = $02C0 ;- - rejestr-cień COLPM0

    mva #$7C COLBAKS

    mva #screenWidth/2-racquetSize/4 racquetPos    

    mva #0 dliCount
    sta RMT_blocked
    
    lda #$ff
    sta sfx_effect

    JSR AudioInit
    
    ;RMT INIT
    ldx #<MODUL                 ;low byte of RMT module to X reg
    ldy #>MODUL                 ;hi byte of RMT module to Y reg
    lda #0                      ;starting song line 0-255 to A reg
    jsr RASTERMUSICTRACKER      ;Init
    
    
    lda #@dmactl(standard|dma)
    sta dmactls
    mwa #dl dlptrs
    vdli DLI

ClearTables
    jsr cyclecolorsReset
    mwa #clear_vars_start temp
    ldy #0
@
    tya
    sta (temp),y
    inw temp
    cpw temp #clear_vars_end
    bne @-


; prepare mem address tables (for "snake" routine)
    
    ;first address initialized
    mva #<xposMemTable xposMemTableAdrL
    mva #>xposMemTable xposMemTableAdrH
    mva #<yposMemTable yposMemTableAdrL
    mva #>yposMemTable yposMemTableAdrH
    ;now add maxBalls to the following addresses
    ;just take the previous one and add "maxBalls"
    ldx #0
initLoop1
    clc
    lda xposMemTableAdrL,x
    adc #<maxBalls ; maxBalls <$80
    sta xposMemTableAdrL+1,x
    lda xposMemTableAdrH,x
    adc #>maxBalls ; maxBalls <$80, so it is == 0
    sta xposMemTableAdrH+1,x
    clc
    lda yposMemTableAdrL,x
    adc #<maxBalls ; maxBalls <$80
    sta yposMemTableAdrL+1,x
    lda yposMemTableAdrH,x
    adc #>maxBalls ; maxBalls <$80, so it is == 0
    sta yposMemTableAdrH+1,x
    inx
    cpx #maxMemory-1
    bne initLoop1

    ldx #maxBalls-1
    txa
eXistenZstackFill
    sta eXistenZstack+1,x
    dex
    txa
    bne eXistenZstackFill
    

    ldy #maxBalls
    sty eXistenZstackPtr
    ;sty clearPtr


    ;OK, one ball starts!
    
    ;ldy eXistenZstackPtr
    lda eXistenZstack,Y
    dey
    sty eXistenZstackPtr
    tax

    jsr randomStart ;just one random pixxxel 
                    ;previously the whole band of ballz
                    
    ;VBI

    vmain vint,7
    
    mva #1 color

    rts
.endp
;--------------------------------------------------
.proc drawBricks
;--------------------------------------------------

; solid maxBrickLines field
; for x=margin to screenWidth-margin:
;   for y=margin to maxBrickLines+margin:
;     fatplot(x,y)
	 	mva #8 color
    mva #margin*2 ypos
drawBricksLoopY
    mva #margin*3 xpos
drawBricksLoop
    jsr fatplot
    inc xpos
    lda xpos
    cmp #screenWidth-margin*3
    bne drawBricksLoop
    inc ypos
    lda ypos
    cmp #maxBrickLines+margin*2
    bne drawBricksLoopY

; set number of bricks in this level
        mwa #952 BricksInLevel
    rts
.endp
;--------------------------------------------------
.proc randomStart
; X - ball number
;--------------------------------------------------
    lda #1
    sta balleXistenZ,x

    ;randomize 10 70
    lda racquetPos
    adc #2  ; do not care about curry, just move the baby to the right
    sta xposTableH,x
    ;randomize margin*2+maxBrickLines maxLines-margin*4
    lda #54
    sta yposTableH,x

; random initial speed and direction
    ;randomize 0 maxSpeed-1
    lda random
    and #%1
    beq xneg
    lda #1 ;easy start
    bne @+ ; jmp
xneg
    lda #-1
@ 
    sta dxTableH,x
    lda random
    sta dxTableL,x
 
    ;randomize 1 maxSpeed-1 ;dy can not be too small or the game would take forever
    lda #-1 ;easy start
    sta dyTableH,x
    lda #1
    sta dyTableL,x
    rts
.endp
;--------------------------------------------------
.proc FileUp
;--------------------------------------------------
    inc LevelNumber+2
    lda LevelNumber+2
    cmp #'9'+1  ; 9+1 character code
    bne NumberReady
    lda #'0'    ; 0 character code
    sta LevelNumber+2
    inc LevelNumber+1
    lda LevelNumber+1
    cmp #'9'+1  ; 9+1 character code
    bne NumberReady
    lda #'0'    ; 0 character code
    sta LevelNumber+1
    inc LevelNumber
NumberReady
    rts
.endp
;--------------------------------------------------
.proc LoadLevelData
;--------------------------------------------------
    lda LevelType
    beq level000
    bmi levelTitle
    ; load level from disk
    ; prepare number in filename
    ldx #2
@   lda LevelNumber,x
    sta fname+7,x
    dex
    bpl @-
    ; clear buffer
    mwa #LevelFileBuff temp
    ldy #0
@   tya
    sta (temp),y
    inw temp
    cpw temp #LevelFileBuffEnd
    bne @-
    ; try to load file
    jsr close
    jsr open
    bmi open_error
    jsr bget
    bmi bget_error
go_close    jsr close
    rts
bget_error
    cpy #136 ; EOF
    beq go_close
open_error
    mva #0 LevelType    ; set level to internal 000
level000
    ; reset file number to 000
    ldx #2
@   lda StartLevelNumber,x
    sta LevelNumber,x
    dex
    bpl @-
levelTitle
    rts 
.endp   
;--------------------------------------------------
.proc BuildLevelFromBuffer
;--------------------------------------------------
    lda LevelType
    beq level000
    bmi levelTitle
    mwa #LevelFileBuff inlevel
    jmp PrepareLevel
levelTitle
    mwa #Menu_data inlevel
    jmp PrepareLevel
level000
    mwa #Level000_data inlevel
PrepareLevel
    ldy #0
    sty BricksInLevel
    sty BricksInLevel+1
nextnumber
    lda (inlevel),y
    inw inlevel
    cmp #CR_PC  ; skip PC CR
    beq nextnumber
    cmp #EOL   ; Atari LF
    beq nextnumber2
    cmp #LF_PC  ; PC LF
    beq nextnumber2
    ; check valid characters
    ldx #9
@   cmp Numbers,x
    beq valid1
    dex
    bpl @-
    jmp LevelDataError
valid1  ; value in X register
    ; now we must multiply BricksInLevel by 10
    asl BricksInLevel
    rol BricksInLevel+1
    mwa BricksInLevel temp
    asl BricksInLevel
    rol BricksInLevel+1
    asl BricksInLevel
    rol BricksInLevel+1
    adw temp BricksInLevel BricksInLevel
    ; and add value
    clc
    txa
    adc BricksInLevel
    sta BricksInLevel
    bcc @+
    inc BricksInLevel+1
@   jmp nextnumber

nextnumber2
    sty BigBrickFlag    ; #0
    lda (inlevel),y
    inw inlevel
    cmp #'1'
    beq singlepixel
    cmp #'2'
    jne LevelDataError
doublepixel
    dec BigBrickFlag    ; #$ff
singlepixel
    lda (inlevel),y
    inw inlevel
    cmp #CR_PC  ; skip PC CR
    beq singlepixel
    cmp #EOL   ; Atari LF
    beq makeBricks
    cmp #LF_PC  ; PC LF
    bne singlepixel
; make bricks
makeBricks
    mwa #0 temp2
    mva #margin*2 ypos
drawBricksLoopY
    mva #0 xpos
drawBricksLoop
    ; get data
    ldy #0
    lda (inlevel),y
    beq LevelDataEnd    ; if end of data
    inw inlevel
    cmp #CR_PC  ; skip PC CR
    beq drawBricksLoop
    cmp #EOL   ; Atari LF
    beq EndOfLine
    cmp #LF_PC  ; PC LF
    beq EndOfLine   ; next line
    cmp #' '
    beq NoBrick     ; if no brick
    ldy #8
    inw temp2    ; real number of bricks
    bit BigBrickFlag
    bpl OnePixel
    inw temp2    ; real number of bricks
OnePixel
NoBrick
    sty color
    jsr fatplot
    inc xpos
    bit BigBrickFlag
    bpl SmallBrick
    jsr fatplot ; second bixel of big brick
    inc xpos
SmallBrick
    lda xpos
    cmp #screenWidth
    bne drawBricksLoop
    ; if screenwidth is reached we skip all buffer characters up to EOL.
    jsr skipToEOL
EndOfLine
    inc ypos
    lda ypos
    cmp #maxlines
    bne drawBricksLoopY    
LevelDataEnd
    cpw BricksInLevel temp2
    bcc BricksOK    ; if defined bricks number is bigger tan real
    mwa temp2 BricksInLevel  ; set to real brick number
BricksOK
    mwa #AllBlocks displayposition
    mwa temp2 decimal
    jsr displaydec5
    mwa #BlocksToHit displayposition
    mwa BricksInLevel decimal
    jsr displaydec5
    jsr cyclecolorsReset
    rts
LevelDataError
    ; errer in data - set level to o (internal) and draw level
    mva #0 LevelType
    jmp level000
skipToEOL
    ldy #0
    lda (inlevel),y
    beq skipped    ; if end of data
    inw inlevel
    cmp #EOL   ; Atari LF
    beq skipped
    cmp #LF_PC  ; PC LF
    bne skipToEOL   ; next data character
skipped
    rts
.endp
;--------------------------------------------------
.proc displaydec5 ;decimal (word), displayposition  (word)
;--------------------------------------------------
; displays decimal number as in parameters (in text mode)
; leading zeroes are removed
; the range is (00000..65565 - two bytes)

    ldy #4  ; there will be 5 digits
NextDigit
    ldx #16 ; 16-bit dividee so Rotate 16 times
    lda #$00
Rotate000
    aslw decimal
    rol  ; scroll dividee
    ; (as highest byte - additional - byte is A)
    cmp #10  ; divider
    bcc TooLittle000 ; if A is smaller than divider
    ; there is nothing to substract
    sbc #10  ; divider
    inc decimal     ; lowest bit set to 1
    ; because it is 0 and this is the fastest way
TooLittle000 dex
    bne Rotate000 ; and Rotate 16 times, Result will be in decimal
    tax  ; and the rest in A
    ; (and it goes to X because
    ; it is our decimal digit)
    lda digits,x
    sta decimalresult,y
    dey
    bpl NextDigit ; Result again /10 and we have next digit

;rightnumber
    ; displaying without leading zeroes (if zeroes exist then display space at this position)
    ldy #0
    ldx #0    ; digit flag (cut leading zeroes)
displayloop
    lda decimalresult,y
    cpx #0
    bne noleading0
    cpy #4
    beq noleading0    ; if 00000 - last 0 must stay
    cmp zero
    bne noleading0
    lda #space
    beq displaychar    ; space = 0 !
noleading0
    inx        ; set flag (no leading zeroes to cut)
displaychar
    sta (displayposition),y
nexdigit
    iny
    cpy #5
    bne displayloop

    rts
.endp
;--------------------------------------------------
.proc RmtSongSelect
;  starting song line 0-255 to A reg
;--------------------------------------------------
/*
    cmp #song_main_menu
    beq noingame               ; noMusic blocks only ingame songs
    bit noMusic
    spl:lda #song_silencio
noingame
*/
    mvx #$ff RMT_blocked
    ldx #<MODUL                ; low byte of RMT module to X reg
    ldy #>MODUL                ; hi byte of RMT module to Y reg
    jsr RASTERMUSICTRACKER     ; Init
    mva #0 RMT_blocked
    rts
.endp
;--------------------------------------------------
.proc wait_for_depress  ; ion
;--------------------------------------------------
    lda CONSOL
    and:cmp #%00000111
    bne wait_for_depress
    lda TRIG0
    beq wait_for_depress
    rts
.endp
;--------------------------------------------------
Menu_data
    .byte '200',EOL ; number of bricks in ATASCII before success
    .byte '1',EOL   ; brick size in pixels
    ;      0         1         2         3         4         5         6         7            
    ;      01234567890123456789012345678901234567890123456789012345678901234567890123456789
    .byte EOL
    .byte '                   ####    ##    ##  #######  ######    ##    ##',EOL
    .byte '                  ######   ##    ##  ##       ##   ##    ##  ##',EOL
    .byte '                 ##    ##   ##  ##   #####    ######      ####',EOL
    .byte '                 ########    ####    ##       ##  ##       ##',EOL
    .byte '                 ##    ##     ##     #######  ##   ##      ##',EOL
    .byte EOL
    .byte '   #####    ######   #######    ####    ##    ##   ######   ##    ##  ########',EOL
    .byte '   ##  ##   ##   ##  ##        ######   ##  ##    ##    ##  ##    ##     ##',EOL
    .byte '   #####    ######   #####    ##    ##  ####      ##    ##  ##    ##     ##',EOL
    .byte '   ##   ##  ##  ##   ##       ########  ##  ##    ##    ##  ##    ##     ##',EOL
    .byte '   ######   ##   ##  #######  ##    ##  ##    ##   ######    ######      ##',EOL
    .byte EOL
    .byte 0
Level000_data
    .byte '486',EOL   ; '952',EOL ; number of bricks (pixes) in ATASCII to be taken out before success
    .byte '2',EOL   ; brick size in pixels
    ;          0         1         2         3
    ;          0123456789012345678901234567890123456789
    .byte EOL,EOL,EOL
    :14 .byte '   ##################################',EOL
    .byte 0
LevelFileBuff
LevelFileBuffLen=(screenWidth*maxLines)+20
    .ds LevelFileBuffLen   ; Buffer for data from the level file
LevelFileBuffEnd
LevelNumber
    .byte '000'
StartLevelNumber
    .byte '000'
fname
    .byte 'D:LEVEL000.DAT',EOL
;--------------------------------------------------
EndLevelFlag
    .byte 0 ; $ff - level over, $00 - level ended
BigBrickFlag
    .byte 0
BricksInLevel
    .word 0
temp2
    .word 0
LevelType
    .byte 0 ; level type $00 - first level, $01 - level from buffer, $ff - title screen  
Numbers
    .byte '0123456789'
digits
zero
    .byte "0123456789"
space = 0
    .byte " "
decimal
    .word 0
decimalresult
    .byte "     "
lineAdrL
    :margin .byte <marginLine ;8 lines of margin space
    :maxLines .byte <(display+screenBytes*#)
    :256-maxLines-1*margin .by <marginLine  ; (display+40*#) ;just to let the plot smear on full .byte ypos
lineAdrH
    :margin .byte >marginLine
    :maxLines .byte >(display+screenBytes*#)
    :256-maxLines-1*margin .by >marginLine  ; (display+40*#) ;just to let the plot smear on full .byte ypos
bittable
    .byte %11110000
debittable
    .byte %00001111
    .byte %11110000
RNColtable ; Right Nibble color Table
    .byte %00000000
    .byte %00000001
    .byte %00000010
    .byte %00000011
    .byte %00000100
    .byte %00000101
    .byte %00000110
    .byte %00000111
    .byte %00001000
LNColtable ; Left Nibble color Table
    .byte %00000000
    .byte %00010000
    .byte %00100000
    .byte %00110000
    .byte %01000000
    .byte %01010000
    .byte %01100000
    .byte %01110000
    .byte %10000000
;--------------------------------
clear_vars_start
dxTableL    :maxBalls .byte 0
dxTableH    :maxBalls .byte 0
dyTableL    :maxBalls .byte 0
dyTableH    :maxBalls .byte 0
; xpos and ydaw are "decimal" parts of static point precision .word
xposTableL  :maxBalls .byte 0 ; "fractional" part
xposTableH  :maxBalls .byte 0 ; "fractional" part
yposTableL  :maxBalls .byte 0 ; 
yposTableH  :maxBalls .byte 0 ; 
; ball position memory tables - the ball trace works like a "snake" 
; (one set, one erased)
; there are "maxMemory" number of tables, "maxballs" length each
; too bad their addressess are not known in advance, 
; so a short subrourine must calculate them and place to XposMemTableAdrL, etc. 
balleXistenZ  :maxBalls .byte 0 ; 0-dead, 1-alive!
balleXistenZcatch
    .byte 0 ; catch last ball byte
eXistenZstack ; goes from index [1..maxBalls]. maxBalls[0] is unused
    ; keeps the list of free slots for balls
    ; goes from down to top. ptr==0 means stack is empty (all balls playing)
    :maxBalls+1 .byte 0    
xposMemTable
    :maxBalls*maxMemory .byte 0
yposMemTable
    :maxBalls*maxMemory .byte 0
;addressess of the tables with snake pixels
xposMemTableAdrL
    :maxMemory .byte 0
xposMemTableAdrH
    :maxMemory .byte 0
yposMemTableAdrL
    :maxMemory .byte 0
yposMemTableAdrH
    :maxMemory .byte 0
;table for keeping the count on the last position to be deleted from the "snake"
memCycleTable
    :maxBalls .byte 0
clear_vars_end
;--------------------------------
statusBar
    dta d"rc$"
hexDump
    dta d"   dx$"
dxDisp
    dta d"   dy$"
dyDisp
    dta d"   balls$"
ballDisp
    dta d"  "
marginLine  .ds screenBytes
;--------------------------------
; names of RMT instruments (sfx)
;--------------------------------
sfx_ping = $07
sfx_pong = $08
;--------------------------------
; RMT songs (lines)
;--------------------------------
song_main_menu  = $00
song_ingame     = $07
song_game_over  = $12


    RUN main
