;Acid Breakout - a break from the daily acid
;build 006, 2010-05-12
;CLEAR!
;あめでと
;---------------------------------------------------

    icl '../lib/atari.hea'
    icl '../lib/macro.hea'

display=$a000
screenWidth = 80 ;in pixels
maxLines = 57 ; number of lines on the screen (must be odd)
spawnProbability = (256*1/5)
margin = 2 ; top and bottom screen safety margin
racquetPosMin = $2 ; min position of the paddle moved by the user
racquetPosMax = screenWidth-8 ; max position of the paddle moved by the user
racquetSize = 10
maxSpeed = 2; maximum speed of a ball. must be power of 2 ('and #' used)
maxBalls = 100 ; maximum number of moving balls, <$80 (bpl used!) 
maxMemory = 7 ; number of saved pixel positions 
              ;Beware! For easier calc somewhere it uses "modulo maxMemory"
              ;calculations and therefore this value must be a power of 2!
maxBrickLines = 14 ; maximum number of lines of bricks to be eradicated

    .zpvar xpos ypos .word = $80 ; position of the ball
    .zpvar colour .byte ; colour of the pixel to plot
    .zpvar deXpos deYpos .byte ;position for deletion
    .zpvar dX dY .word ;main loop shortcuts to the table values 
    ;.zpvar dx dy .word ;delta
    ;xpos, dx - "static point precision" - [dx+1].[dx] (big endian!)
    ;this static point precision is emulated with .word calcs, just a result is the high byte
    .zpvar currBall collisionCheck racquetPos MyClok eXistenZstackPtr .byte
    .zpvar xMemAddr yMemAddr .word ; address where to store memories of the current ball
    .zpvar temp .word 
    .zpvar clearCount clearBallNr .byte
    .zpvar DLI_A DLI_X dliCount .byte
    org $2000
;---------------------------------------------------
dl 
		.by $80+$20		
		dta $4f+$20,a(display)	 ;VSCROLL
		:((maxlines-1)/2) dta a($2f8f)	

    ;----		
		.by $42+$10 ;Hscroll
DLracquetAddr0
		.wo racquetDisp
		.by $41
		.wo dl
;---------------------------------------------------
racquetDisp
    :42 .byte $0
    .byte $80, $80, $80, $80, $80
    :36 .byte $0

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
  


    lda racquetPos

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
    
  ;pos print
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
 
		mva #0 dliCount
		mva #13 VSCROL
    jmp XITVBV

;--------------------------------------------------
DLI
	sta DLI_A
	stx DLI_X
	ldx dliCount
	sta WSYNC
 
	lda #13
	sta VSCROL

	lda #3
	sta VSCROL
	
	txa
	asl
	asl 
	;lda brickColourTab,x
	sta COLBAK
	
	inx
	stx dliCount
	ldx DLI_X
	lda DLI_A
	rti
;--------------------------------------------------
main
    jsr initialize

loop

    mva #maxBalls-1 currBall
		jsr cycleColours

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
    lda CONSOL
    and #%00000100 ; OPTION
    bne bounceNormally
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

		ldx colour
		
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
;spawn the new bally
		; if there is still an empty slot for a new ball somewhere...
			;lda RANDOM
			;cmp #spawnProbability
			;bcs noCollision 
		lda colour
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
 


    ; sound
    ;lda random
    ;and #%00001000
    ;lda #%00000000
    ;sta consol


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
		;pause
		   
    dec currBall
    jpl flight


		pause 0;all balls

    lda eXistenZstackPtr
    cmp #maxBalls
    jne loop

    ;game over
gameOver
    lda RANDOM
    and #$07
		sta COLPM0
    jmp gameOver

;-------------------
ballDoesNotexist
    ;a delay loop for a ball that does not really exist (yet)
    ldx #70
delayLoop
    dex    
    bne delayLoop
    jmp endOfBallzLoop
;--------------------------------------------------
fatplot
; xpos, ypos (.byte) - pixel position
; xpos<80
; pixel colour in "colour"
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
		
		ldx colour

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
;--------------------------------------------------
fatdeplot
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
    
    jsr fatdeplot

    dec clearCount
    bpl clearDeadLoop    

    rts
;--------------------------------------------------
clearScreen
;--------------------------------------------------
		lda #0
		tax
Loopi1
    :(maxLines*40/256+1) sta display+$100*#,x
		inx
		bne Loopi1
		rts
;--------------------------------------------------
cycleColoursReset
		ldy #6
cycleRloop
		lda colourCycleTabReset,y
		sta colourCycleTab,y
		dey
		bpl cycleRloop
		mva #0 colour
;--------------------------------------------------
cycleColours
;--------------------------------------------------
		inc colour
		lda colour
		cmp #8
		bne noColourReset
		mva #1 colour
noColourReset
		ldy #6
cycleCloop
		lda colourCycleTab,y
		;sta COLPM1,y
		sta COLPM1S,y
		dey
		bpl cycleCloop

;shift colours
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
cct = colourCycleTab
		ldx cct+6
		mva cct+5 cct+6
		mva cct+4 cct+5
		mva cct+3 cct+4
		mva cct+2 cct+3
		mva cct+1 cct+2
		mva cct+0 cct+1
		stx cct+0

		rts
colourCycleTab

		.by 14,2,4,6,8,10,12

colourCycleTabReset
		.by 14,2,4,6,8,10,12
brickColourTab
		.by 0
;--------------------------------------------------
initialize
;--------------------------------------------------
		 
		mva #$00 COLPM0S ; = $02C0 ;- - rejestr-cień COLPM0

		jsr cycleColoursReset
		
		mva #$7C COLBAKS
		
    mva #0 dliCount
		jsr clearscreen
    jsr drawBricks
		
    lda dmactls
    and #$fc
    ora #$02     ; normal screen width
    ;ora #$01     ; narrow screen width
    sta dmactls
    mwa #dl dlptrs
		vdli DLI



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
		mva #screenWidth/2 racquetPos
    vmain vint,7
    lda #$80 ;+GTIACTLBITS
    sta GTIACTL
    sta GTICTLS
    
    mva #1 colour

		rts
;--------------------------------------------------
drawBricks
;--------------------------------------------------

; solid maxBrickLines field
; for x=margin to screenWidth-margin:
;   for y=margin to maxBrickLines+margin:
;     fatplot(x,y)
	 	mva #8 colour
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

		rts
;--------------------------------------------------
randomStart
; X - ball number
;--------------------------------------------------
    lda #1
    sta balleXistenZ,x

    ;randomize margin $ff-margin
    lda #40
		sta xposTableH,x
    ;randomize margin*2+maxBrickLines maxLines-margin*4
    lda #30
		sta yposTableH,x

; random initial speed and direction
    ;randomize 0 maxSpeed-1
    lda #1 ;easy start
    sta dxTableH,x
    lda random
    sta dxTableL,x
 
    ;randomize 1 maxSpeed-1 ;dy can not be too small or the game would take forever
    lda #-1 ;easy start
    sta dyTableH,x
    lda #1
    sta dyTableL,x
    rts
;--------------------------------------------------

lineAdrL
    :margin .byte <marginLine ;8 lines of margin space
    :maxLines .byte <(display+40*#)
    ;:margin .byte <marginLine ;8 lines of margin space
    :256-maxLines-1*margin .by <marginLine; (display+40*#) ;just to let the plot smear on full .byte ypos
lineAdrH
    :margin .byte >marginLine
    :maxLines .byte >(display+40*#)
    ;:margin .byte >marginLine
    :256-maxLines-1*margin .by >marginLine; (display+40*#) ;just to let the plot smear on full .byte ypos
    ; $E000 is an address in ROM - the trick to avoid spawning new balls!    
bittable
    .byte %11110000
    .byte %00001111
RNColtable ; Right Nibble Colour Table
    .byte %00000000
    .byte %00000001
    .byte %00000010
    .byte %00000011
    .byte %00000100
    .byte %00000101
    .byte %00000110
    .byte %00000111
    .byte %00001000
LNColtable ; Left Nibble Colour Table
    .byte %00000000
    .byte %00010000
    .byte %00100000
    .byte %00110000
    .byte %01000000
    .byte %01010000
    .byte %01100000
    .byte %01110000
    .byte %10000000
debittable
    .byte %00001111
    .byte %11110000

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
marginLine :40 .byte 0

    RUN main
