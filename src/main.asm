INCLUDE "hardware.inc"

SECTION "vblank",ROM0[$40]
    jp VBlankHandler

SECTION "Header", ROM0[$100]

EntryPoint:
	di
	jp Start

; This apparently gets fixed by rgbfix
REPT $150 - $104
	db 0
ENDR

BLOCK_TILE EQU $0013
BLANK_TILE EQU $0020

PADDLE_POS EQU $FF80
BALL_POS_X EQU $FF81
BALL_POS_Y EQU $FF82
BALL_VEL_X EQU $FF83
BALL_VEL_Y EQU $FF84
SCORE_HIGH EQU $FF85
SCORE_LOW EQU $FF86

IS_GAME_OVER EQU $FF90

PADDLE_START_POS EQU 24
MAX_PADDLE_POS EQU 128
MAX_BALL_POS EQU 152
MIN_PADDLE_POS EQU 16
PADDLE_VPOS EQU 128
SCORE_PER_HIT EQU 1

PADDLE_SPRITE EQU $FE00
BALL_SPRITE EQU $FE40 ; 20 sprites should be enough for the paddle?

SECTION "Game code", ROM0

Start:
.waitVBlank
	ld a, [rLY]
	cp 144
	jr c, .waitVBlank

	xor a
	ld [rLCDC], a

	ld hl, _VRAM8000
	ld de, Tiles
	ld bc, TilesEnd - Tiles

.copyTiles
	ld a, [de]
	ld [hli], a
	inc de
	dec bc
	ld a, b
	or c
	jr nz, .copyTiles

.drawFrame
	ld hl, $9800

.drawTopBorder
	
	ld a, BLOCK_TILE
	ld [hli], a
	ld a, $14
	ld [hli], a
	inc a
	ld [hli], a
	inc a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld a, BLOCK_TILE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld a, $17
	ld [hli], a
	ld a, BLOCK_TILE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a

	ld b, $F ; 15 rows

.drawSides
	; add 12 to HL
	ld a, l
	add a, 12
	ld l, a
	jr nc, .leftWall
	inc h ; If we're here, L overflowed so we need to inc h

.leftWall
	ld a, BLOCK_TILE ;; TODO should already be set?
	ld [hli], a

.eighteenBlanks
	ld a, BLANK_TILE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a

.rightWall
	ld a, BLOCK_TILE
	ld [hli], a

	dec b
	jr nz, .drawSides

	ld b, $2

.clearBottom
	; add 12 to HL
	ld a, l
	add a, 12
	ld l, a
	jr nc, .twentyBlanks
	inc h ; If we're here, L overflowed so we need to inc h

.twentyBlanks
	ld a, BLANK_TILE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a

	dec b
	jr nz, .clearBottom

.initPaddleSprites
	ld hl, PADDLE_SPRITE

	ld a, PADDLE_VPOS
	ld [hli], a ; y coord
	ld a, $50
	ld [hli], a ; x coord
	ld a, $B
	ld [hli], a ; sprite index
	ld a, %00000000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS
	ld [hli], a ; y coord
	ld a, $58
	ld [hli], a ; x coord
	ld a, $C
	ld [hli], a ; sprite index
	ld a, %00000000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS
	ld [hli], a ; y coord
	ld a, $60
	ld [hli], a ; x coord
	ld a, $C
	ld [hli], a ; sprite index
	ld a, %00000000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS
	ld [hli], a ; y coord
	ld a, $68
	ld [hli], a ; x coord
	ld a, $B
	ld [hli], a ; sprite index
	ld a, %00100000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS + 8
	ld [hli], a ; y coord
	ld a, $50
	ld [hli], a ; x coord
	ld a, $D
	ld [hli], a ; sprite index
	ld a, %00000000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS + 8
	ld [hli], a ; y coord
	ld a, $58
	ld [hli], a ; x coord
	ld a, $E
	ld [hli], a ; sprite index
	ld a, %00000000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS + 8
	ld [hli], a ; y coord
	ld a, $60
	ld [hli], a ; x coord
	ld a, $E
	ld [hli], a ; sprite index
	ld a, %00000000
	ld [hli], a ; sprite attributes
	
	ld a, PADDLE_VPOS + 8
	ld [hli], a ; y coord
	ld a, $68
	ld [hli], a ; x coord
	ld a, $D
	ld [hli], a ; sprite index
	ld a, %00100000
	ld [hli], a ; sprite attributes


; zero out the rest of the sprites I think
; all sprites are between $FE00 and $FE9F
; if l is ever higher than 9F, we're done

	ld b, $9F

.zeroOutASprite
	ld a, l ; load the bottom bit into A
	cp b
	jr nc, .doneZeroingSprites ; if A is above $9F, we've gone far enough
	xor a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	jr .zeroOutASprite	

.doneZeroingSprites

	

; +---------------------------------------------------------+
; |                                                         |
; |                 INITIALIZE VARIABLES		    |
; |                                                         |
; +---------------------------------------------------------+

	ld a, PADDLE_START_POS
	ld [PADDLE_POS], a ; paddle go in middle
	ld a, 1
	ld [BALL_VEL_Y], a
	ld a, 24
	ld [BALL_POS_X], a
	ld [BALL_POS_Y], a
	xor a
	ld [SCORE_LOW], a
	ld [SCORE_HIGH], a
	ld [BALL_VEL_X], a
	ld [IS_GAME_OVER], a


	; init display!
	ld a, %11100100
	ld [rBGP], a
	ld [rOBP0], a

	xor a
	ld [rSCY], a
	ld [rSCX], a

	; no sound
	ld [rNR52], a

	; screen, sprites, and bg on
	ld a, %10010011
	ld [rLCDC], a

	ld a, %00000001 ; Interrupts on
	ld [$FFFF], a

.lockup
	ei
	halt 
	jr .lockup

; +---------------------------------------------------------+
; |                                                         |
; |                      GAME LOOP                          |
; |                                                         |
; +---------------------------------------------------------+

VBlankHandler:
	call .setControlsInA

	ld b, a
	and a, %00000010
	jr nz, .leftPressed
	ld a, b
	and a, %00000001
	jr nz, .rightPressed
	jr .endInput

.rightPressed
	call .movePaddleRight
	jr .endInput

.leftPressed
	call .movePaddleLeft
	jr .endInput

.endInput

  call .moveBall
	call .collideBall

	call .drawPaddle
	call .drawBall
	call .drawScore
	;call .drawHighScore
	call .drawGameOver
	reti

.setControlsInA
	ld hl, rP1 ; controls
	ld a, %00010000 ; 5 low = buttons
	ld [hl], a
	ld [hl], a
	ld [hl], a
	ld [hl], a ; jump! pogo pogo pogo pogo 
	ld a, [hl]
	cpl ; 1 = pressed
	and a, %00001111 ; mask for just buttons
	swap a ; move to high nibble
	ld b, a ; store high nibble

	ld a, %00100000 ; 4 low = arrows
	ld [hl], a
	ld [hl], a
	ld [hl], a
	ld [hl], a
	ld [hl], a
	ld [hl], a
	ld [hl], a ; bounce! pogo pogo pogo pogo 
	ld a, [hl]
	cpl ; 1 = pressed
	and a, %00001111 ; mask for just buttons
	or b
	ret 

.movePaddleRight
	ld hl, PADDLE_POS
	inc [hl]
	ld a, [hl]
	sub a, MAX_PADDLE_POS
	jr c, .notAtRightEdge
	ld [hl], MAX_PADDLE_POS
.notAtRightEdge
	ret

.movePaddleLeft
	ld hl, PADDLE_POS
	dec [hl]
	ld a, [hl]
	sub a, MIN_PADDLE_POS
	jr nc, .notAtLeftEdge
	ld [hl], MIN_PADDLE_POS
.notAtLeftEdge
	ret

.moveBall
	ld a, [BALL_VEL_X]
	ld b, a
	ld a, [BALL_POS_X]
	add a, b
	ld [BALL_POS_X], a

	ld a, [BALL_VEL_Y]
	ld b, a
	ld a, [BALL_POS_Y]
	add a, b
	ld [BALL_POS_Y], a
	ret 

.collideBall
	ld a, [BALL_POS_X]
	ld b, a
	sub a, MIN_PADDLE_POS
	jr nc, .ballNotAtLeftEdge
	ld hl, BALL_VEL_X
	xor a
	sub [hl]
	ld [BALL_VEL_X], a

.ballNotAtLeftEdge
	ld a, b
	sub a, MAX_BALL_POS
	jr c, .ballNotAtRightEdge
	ld hl, BALL_VEL_X
	xor a
	sub [hl]
	ld [BALL_VEL_X], a

.ballNotAtRightEdge
	ld a, [BALL_POS_Y]
	ld b, a
	sub a, MIN_PADDLE_POS + 8
	jr nc, .ballNotAtTopEdge
	ld hl, BALL_VEL_Y
	xor a
	sub [hl]
	ld [BALL_VEL_Y], a

.ballNotAtTopEdge

	; Okay we have to bounce off the side of the paddle I think


	ld a, b
	sub a, PADDLE_VPOS - 8
	jr c, .ballNotAtBottomEdge
	
	ld a, [PADDLE_POS] 
	ld b, a ; get the paddle's X position into b
	ld a, [BALL_POS_X] ; get the ball's X position into a
	add 8 ; right edge of the ball
	cp b ; Ball x - paddle x. If it's negative, the ball is to the left
	jr c, .ballNotAtBottomEdge

	ld a, b
	add $20 ; paddle right edge
	ld b, a ; into b
	ld a, [BALL_POS_X] ; ball's left edge
	cp b ; Ball x - paddle x. If it's positive, the ball is to the right
	jr nc, .ballNotAtBottomEdge

	; The ball is bouncing off the top of the paddle
	; Set the Y velocity to negative, so it doesn't get stuck
	
	ld a, [BALL_VEL_Y]
	ld b, a
	and $80 ; sign bit
	cp $80 ; set Z if it's negative, so the ball is traveling up
	jr z, .ballNotAtBottomEdge
	; invert the velocity
	xor a ; a = 0
	sub b ; a = 0 - b
	ld [BALL_VEL_Y], a

.incrementScore
	ld b, SCORE_PER_HIT
	ld a, [SCORE_LOW]
	add b
	daa ; Put it in BCD
	ld [SCORE_LOW], a
	jr nc, .incrementScoreHigh
	ld b, 1
	ld a, [SCORE_HIGH]
	add b
	daa 
	ld [SCORE_HIGH], a
.incrementScoreHigh

.ballNotAtBottomEdge

	;; TODO collide with the sides of the paddle

	ret 

.drawPaddle
	ld a, [PADDLE_POS]
	ld [PADDLE_SPRITE + 1], a
	ld [PADDLE_SPRITE + 17], a
	add a, 8
	ld [PADDLE_SPRITE + 5], a
	ld [PADDLE_SPRITE + 21], a
	add a, 8
	ld [PADDLE_SPRITE + 9], a
	ld [PADDLE_SPRITE + 25], a
	add a, 8
	ld [PADDLE_SPRITE + 13], a
	ld [PADDLE_SPRITE + 29], a
	ret

.drawBall
	ld hl, BALL_SPRITE
	ld a, [BALL_POS_Y]
	ld [hli], a
	ld a, [BALL_POS_X]
	ld [hli], a
	ld a, $A
	ld [hli], a
	xor a
	ld [hli], a
	ret

.drawScore
	ld a, [SCORE_LOW]
	ld b, a ; store it in b
	and $0f ; just the lower digit
	ld [$9807], a ; put this in the ones digit
	ld a, b
	swap a ; tens nybl is now the low nybl
	and $0f ; mask out again
	ld [$9806], a ; put this in the tens digit

	ld a, [SCORE_HIGH]
	ld b, a ; store it in b
	and $0f ; just the lower digit
	ld [$9805], a ; put this in the hundreds digit
	ld a, b
	swap a ; thousands nybl is now the low nybl
	and $0f ; mask out again
	ld [$9804], a ; put this in the thousands digit
	ret

.drawHighScore
	xor a ;; TODO get the high score
	;ld a, [IS_GAME_OVER]
	ld hl, $980F
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ret

.drawDebug
	ld a, [PADDLE_POS]
	ld b, a
	ld a, [BALL_POS_X]
	add 8
	cp b
	ld a, 1
	jr c, .notAbovePaddle

	ld a, [PADDLE_POS]
	add 32
	ld b, a
	ld a, [BALL_POS_X]
	cp b
	ld a, 3
	jr nc, .notAbovePaddle
	
	ld a, 2
.notAbovePaddle
	ld [$9800], a
	ret

.drawGameOver
	ld a, [IS_GAME_OVER]
	and %10000000
	cp  %10000000
	jr z, .isGameOver
	jr .eraseGameOverMessage

.isGameOver
	ld a, [IS_GAME_OVER]
	inc a
	cp  %10100000
	jr nz, .fullCycle
	sub %00100000

.fullCycle
	ld [IS_GAME_OVER], a
	and %00010000
	cp  %00010000
	jr nz, .gameOverMessageVisible

.eraseGameOverMessage
	ld hl, $9905
	ld a, BLANK_TILE
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ld [hli], a
	ret 

.gameOverMessageVisible
	ld hl, $9905
	ld a, $18
	ld [hli], a ; G
	inc a 
	ld [hli], a ; A
	inc a 
	ld [hli], a ; M
	inc a 
	ld [hli], a ; E
	ld a, BLANK_TILE
	ld [hli], a ; space
	ld a, $1C 
	ld [hli], a ; O
	inc a 
	ld [hli], a ; V
	ld a, $1B
	ld [hli], a ; E again
	ld a, $1E
	ld [hli], a ; R
	ld a, $1F
	ld [hli], a ; !!
	ret


SECTION "Tiles", ROM0

Tiles:
INCBIN "tiles.2bpp"
TilesEnd:

