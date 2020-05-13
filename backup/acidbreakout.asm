;Acid Breakout - a break from the daily acid
;build 006, 2010-05-12
;CLEAR!
;あめでと
;---------------------------------------------------

    icl '../lib/atari.hea'
    icl '../lib/system.hea'

display=$a000
screenWidth = 256 ;in pixels
maxLines = 210 ; number of lines on the screen
spawnProbability = (256*1/5)
margin = 8 ; top and bottom screen safety margin
racquetPosMin = $8 ; min position of the paddle moved by the user
racquetPosMax = $e8 ; max position of the paddle moved by the user
racquetSize = 4*8 
maxSpeed = 4; maximum speed of a ball. must be power of 2 ('and #' used)
maxBalls = 64 ; maximum number of moving balls, <$80 (bpl used!) 
maxMemory = 8 ; number of saved pixel positions 
              ;Beware! For easier calc somewhere it uses "modulo maxMemory"
              ;calculations and therefore this value must be a power of 2!
maxBrickLines = 80 ; maximum number of lines of bricks to be eradicated

    .zpvar xpos ypos .word = $80 ; position of the ball
    .zpvar deXpos deYpos .byte ;position for deletion
    .zpvar dX dY .word ;main loop shortcuts to the table values 
    ;.zpvar dx dy .word ;delta
    ;xpos, dx - "static point precision" - [dx+1].[dx] (big endian!)
    ;this static point precision is emulated with .word calcs, just a result is the high byte
    .zpvar currBall collisionCheck racquetPos MyClok eXistenZstackPtr .byte
    .zpvar xMemAddr yMemAddr .word ; address where to store memories of the current ball
    .zpvar temp .word 
    .zpvar clearCount clearBallNr .byte
    
    org $2000
;---------------------------------------------------
dl 
		.by $70,$30
		;.by $42
		;.wo statusBar
		;.wo eXistenZstack+1
		;.by $02
		;.by 0
		.by $4f    ; 1 line
		.wo display
		:127 .by $0f ;128 lines here
		.by $4f
		.wo display+$1000
		.by $0f ; 130 lines here
		:maxlines-130 .byte $0f ; maxLines lines total
		.by $4f+$10 ;Hscroll
DLracquetAddr0
		.wo racquetDispEven 
		.by $4f+$10 ;Hscroll
DLracquetAddr1
		.wo racquetDispEven 
		.by $4f+$10 ;Hscroll
DLracquetAddr2
		.wo racquetDispEven 
		.by $4f+$10 ;Hscroll
DLracquetAddr3
		.wo racquetDispEven 
		.by $41
		.wo dl
;---------------------------------------------------
racquetDispEven
    :34 .byte $0
    .byte $ff, $ff, $ff, $ff
    :28 .byte $0

main
    jsr initialize

loop

    mva #maxBalls-1 currBall

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
    and #maxMemory-1 ; this is the tricky part (mod #maxMemory) 
                     ; due to this AND maxMemory MUST be power of 2!!!
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
		lsr 
		lsr 
		tay
;---
		;ldx deypos
		lda lineAdrL,x
		sta temp
		lda lineAdrH,x
		sta temp+1

		lda dexpos
		and #$07
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
    ;jmp noBallOut ; turns off the ball kill

    lda ypos+1
    cmp #maxLines+margin+maxSpeed*2 ; that makes the ball below the racquet
    bcs flyDown2 ;kinda lame optimisation as A carries ypos+1
    


    lda racquetPos
    sec
    sbc #racquetPosMin
    cmp xpos+1

    jcs flyDown

    clc
    adc #racquetSize-1
    cmp xpos+1
    jcs bottomBounce

flyDown
    lda ypos+1
flyDown2
    cmp #255-margin ;maximum depth
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


noBottom

;left bounce
    lda xpos+1
    cmp #0+maxSpeed+1
    bcs noLeft
    negw dX
    mva #0+maxSpeed+3 xpos+1

noLeft


;right border bounce
    lda xpos+1
    .if $100-maxSpeed > $ff 
      .error "maxSpeed too low!!!"
    .endif
    cmp #$100-maxSpeed-1
    bcc noRight
    negw dX
    mva #$ff-maxSpeed-3 xpos+1
noRight

    ;jsr plot
    ; the full plot copied here to get few cycles and collision
   	; high byte is the integer position
   	; low byte is the "fractional" part
    ; let's calculate coordinates from xpos and ypos
		lda xpos+1
		lsr
		lsr 
		lsr 
		tay
;---
		ldx ypos+1
		lda lineAdrL,x
		sta temp
		lda lineAdrH,x
		sta temp+1

		lda xpos+1
		and #$07
		tax
	
		lda (temp),y
		sta collisionCheck
		ora bittable,x ;eor
		sta (temp),y
		cmp collisionCheck
		; when final byte and the original bytes are equal, it means it is a collision (plot in the place of a dot)
		jne noCollision
		
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
;spawn the new bally
		; if there is still an empty slot for a new ball somewhere...
		lda RANDOM
		cmp #spawnProbability
		bcs noCollision 

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

    ;A carries a random number
    and #maxSpeed-1 ;a variable speed limiter
    clc
    adc #$FF-(maxSpeed-1)
    sta dxTableH,x
    bne dXlower

dXplus
    clc
    ;A carries a random number
    and #maxSpeed-1 ;a variable speed limiter
    adc #1
    sta dxTableH,x
dXlower    
    lda random
    sta dxTableL,x


    randomize 1 maxSpeed-1 ;dy can not be too small or the game would take forever
    sta dyTableH,x
    lda random
    sta dyTableL,x
 


    ; sound
    ;lda random
    ;and #%00001000
    lda #%00000000
    sta consol


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

    ;deaD Ball erasing
    ; if clearPtr (pointer for lazy crearing)
    ; is lower than eXistenZstackPtr
    ; then erase 1 memory ball and increase the pointer
    ; the pointer gets lowered with eXistenZstackPtr

/*    
    ldx clearPtr
    dex
    cpx eXistenZstackPtr
    bcS clearPrt_GREQ_eXistenZstackPtr
    
    lda eXistenZstack,x
    sta clearBallNr
    jsr clearDeadBall
    inc clearPtr

clearPrt_GREQ_eXistenZstackPtr
*/


    lda eXistenZstackPtr
    cmp #maxBalls
    bne gameIsNotOver
    ;game over
zzz 
    inc colbak
    jmp zzz
   
gameIsNotOver
    jmp loop
;-------------------
ballDoesNotexist
    ;a delay loop for a ball that does not really exist (yet)
    ldx #75
delayLoop
    dex    
    bne delayLoop
    jmp endOfBallzLoop
;--------------------------------------------------
plot
; xpos, ypos (.byte) - pixel position
;--------------------------------------------------
   	; let's calculate coordinates from xpos and ypos
		lda xpos
		lsr
		lsr 
		lsr 
		tay
;---
		ldx ypos
		lda lineAdrL,x
		sta temp
		lda lineAdrH,x
		sta temp+1

		lda xpos
		and #$07
		tax
	
		lda (temp),y
		ora bittable,x ;eor
		sta (temp),y
		rts
;--------------------------------------------------
deplot
; deyxpos, deypos (.byte) - pixel position
;--------------------------------------------------
   	; let's calculate coordinates from xpos and ypos
		lda dexpos
		lsr
		lsr 
		lsr 
		tay
;---
		ldx deypos
		lda lineAdrL,x
		sta temp
		lda lineAdrH,x
		sta temp+1

		lda dexpos
		and #$07
		tax
	
		lda (temp),y
		and debittable,x
		sta (temp),y
		rts
;--------------------------------------------------
byteDePlot
; deyxpos, deypos (.byte) - byte-pixel position 32xmaxlines screen
;--------------------------------------------------
   	; let's calculate coordinates from xpos and ypos
;---
		ldx deypos
		lda lineAdrL,x
		sta temp
		lda lineAdrH,x
		sta temp+1

		ldy dexpos
    lda #0
		sta (temp),y
		rts
;--------------------------------------------------
clearDeadBall
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
    
    jsr deplot

    dec clearCount
    bpl clearDeadLoop    

    rts
;--------------------------------------------------
clearScreen
;--------------------------------------------------
		lda #0
		tax
Loopi1
    :(maxLines*32/256+1) sta display+$100*#,x
		inx
		bne Loopi1
		rts
;--------------------------------------------------
drawBricks
;--------------------------------------------------

; solid maxBrickLines field
		lda #%11111111
		ldx #0
loopi2
    :(maxBrickLines+margin)*32/256-1 sta display+screenWidth+$100*#,x
		inx
		bne loopi2

; empty borders
    mva #margin deypos
loopi3
    mva #0      dexpos
    jsr byteDePlot
    mva #1      dexpos
    jsr byteDePlot
    mva #30      dexpos
    jsr byteDePlot
    mva #31      dexpos
    jsr byteDePlot
    inc deypos
    lda deypos
    cmp #maxBrickLines+margin*2
    bne loopi3

		rts
;--------------------------------------------------
randomStart
; X - ball number
;--------------------------------------------------
    lda #1
    sta balleXistenZ,x

    randomize margin $ff-margin
    sta xposTableH,x
    randomize margin*2+maxBrickLines maxLines-margin*4
    sta yposTableH,x

; random initial speed and direction
    ;randomize 0 maxSpeed-1
    lda #1 ;easy start
    sta dxTableH,x
    lda random
    sta dxTableL,x
 
    ;randomize 1 maxSpeed-1 ;dy can not be too small or the game would take forever
    lda #-2 ;easy start
    sta dyTableH,x
    lda random
    sta dyTableL,x
    rts
    

;--------------------------------------------------
initialize
;--------------------------------------------------
    ;mva 0 COLPF2S
    jsr clearscreen
    jsr drawBricks

    lda dmactls
    and #$fc
    ;ora #$02     ; normal screen width
    ora #$01     ; narrow screen width
    sta dmactls
    mwa #dl dlptrs
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
		;snake memory addressess initialized!
		
		;clear the balleXistenZ (nothing is bouncing!)
		;and other tables
		ldx #0
		txa
eXistenZclearLoop
		sta balleXistenZ,x
		sta dxTableL,x
    sta dxTableH,x
    sta dyTableL,x
    sta dyTableH,x
    sta xposTableL,x
    sta xposTableH,x
    sta yposTableL,x
    sta yposTableH,x
    sta memCycleTable,x

		inx
		cpx #maxBalls
		bne eXistenZclearLoop
    sta balleXistenZcatch		


    dex
    ; X == maxBalls-1
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
		mva #$b0 racquetPos
    vmain vint,7
		rts
;--------------------------------------------------
vint
    ;------------JOY-------------
    ;happy happy joy joy
    ;check for joystick now
/*
    inc MyClok
    lda MyClok
    and #$07
    bne jNotRight
*/    
    ldy PORTA
/*
    tya
    and #$01 ;up
    bne jNotUp
    ldx joystickConversion ;up
    lda #1
    sta keyboardGrid,x
jNotUp
    tya
    and #$02 ;down
    bne jNotDown
    ldx joystickConversion+1 ;up
    lda #1
    sta keyboardGrid,x
jNotDown
*/
    tya
    and #$04 ;left
    bne jNotLeft
    ldx racquetPos
    cpx #racquetPosMin+1
    bcc jNotLeft
    dex
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
  


    lda racquetPos

    sec
    lda #screenWidth-1
    sbc racquetPos
    :3 lsr
    clc
    adc #<racquetDispEven
    sta dlracquetAddr0
    lda #>racquetDispEven
    adc #0
    sta dlracquetAddr0+1

    lda dlracquetAddr0
    sta dlracquetAddr1
    sta dlracquetAddr2
    sta dlracquetAddr3
    lda dlracquetAddr0+1
    sta dlracquetAddr1+1
    sta dlracquetAddr2+1
    sta dlracquetAddr3+1
    
    lda racquetPos
    lsr 
    and #$03
    sta HSCROL
    
    lda racquetPos
    :4 lsr
    tax
    lda hexconv,x
    sta hexDump

    lda racquetPos
    and #$0F
    tax
    lda hexconv,x
    sta hexDump+1


    jmp XITVBV

;--------------------------------------------------
hexConv
    dta d"0123456789abcdef"
marginLine :32 .byte 0

lineAdrL
    :margin .byte <marginLine ;8 lines of margin space
    :maxLines .byte <(display+32*#)
    ;:margin .byte <marginLine ;8 lines of margin space
    :256-maxLines-1*margin .by <marginLine; (display+32*#) ;just to let the plot smear on full .byte ypos
lineAdrH
    :margin .byte >marginLine
    :maxLines .byte >(display+32*#)
    ;:margin .byte >marginLine
    :256-maxLines-1*margin .by >marginLine; (display+32*#) ;just to let the plot smear on full .byte ypos
    ; $E000 is an address in ROM - the trick to avoid spawning new balls!    
bittable
    .byte $80,$40,$20,$10,$08,$04,$02,$01
debittable
    .byte %01111111
    .byte %10111111
    .byte %11011111
    .byte %11101111
    .byte %11110111
    .byte %11111011
    .byte %11111101
    .byte %11111110

dxTableL    :maxBalls .byte 0
dxTableH    :maxBalls .byte 0
dyTableL    :maxBalls .byte 0
dyTableH    :maxBalls .byte 0
; xpos and ydaw are "decimal" parts of static point precision .word
xposTableL  :maxBalls .byte 0 ; "fractional" part
xposTableH  :maxBalls .byte 0 ; "fractional" part
yposTableL  :maxBalls .byte 0 ; 
yposTableH  :maxBalls .byte 0 ; 
;ball position memory tables - the ball trace works like a "snake" 
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
;addressess of the tables with 
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

    RUN main

   