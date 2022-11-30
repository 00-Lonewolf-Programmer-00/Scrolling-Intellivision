    INCLUDE "constants.bas"
    'CONST GW =   BG00 - GRAM + FG_BLUE  + BG_ORANGE
    CONST GW =  BG00 + FG_BLUE  + BG_ORANGE

    MODE SCREEN_FOREGROUND_BACKGROUND
    WAIT



    'misc
    DIM #line
	DIM bits

    'DEF FN jumpRestrainer = bits AND &00000001
    'DEF FN movingUp = bits AND &00000010
    'DEF FN movingDown = bits AND &00000100

	DIM jumpRestrainer
	DIM movingUp

    'background collission stuff
    DIM overlap

    CONST tileHeightMask = &00000111
    CONST tileHeight = 8
    CONST tileWidthMask = &00000111
    CONST tileWidth = 8
    CONST pfDiff = 8
    CONST spriteHeightMinusOne = 3
    CONST spriteWidthMinusOne = 7


    'top and bottom row and left and right column of sprite
    DEF FN top = ((#Y0 AND 255) - pfDiff) / tileHeight
    DEF FN bottom = (((#Y0 AND 255) - pfDiff) + spriteHeightMinusOne) / tileHeight
    DEF FN left = (((#X0 AND 255) - scrollCounter) - pfDiff) / tileWidth
    DEF FN right = ((((#X0 AND 255) - scrollCounter) - pfDiff) + spriteWidthMinusOne) / tileWidth

    'DEF FN bellow = (((#Y0 AND 255) - pfDiff) + 8) / tileHeight
    'DEF FN above = ((((#Y0 AND 255) - pfDiff) - 1) - (#0YVelocity AND 255)) / tileHeight
    'DEF FN above = (((#Y0 AND 255) - pfDiff) - 1) / tileHeight


   'copy gfx to GRAM
    DEFINE DEF00,1,backgroundGFX
    WAIT
    DEFINE DEF16,2,spriteGFX
    WAIT



    'SPRITE STUFF
    DIM #spriteFrame
	#spriteFrame = 0.0
    CONST spriteFrameMask = &00000001
    DIM #X0
    DIM #Y0
    #X0 = 32.0
    #Y0 = 70.0

    'SCROLL STUFF
    BORDER BORDER_BLACK,BORDER_HIDE_LEFT_EDGE
	CONST dataColumns = 38
	DIM scrollCounter
    dim #x
    #x=20
    DIM temp


    FOR y=0 TO 11
        FOR x=0 TO 19
            #BACKTAB(y*20+x)  =  level1(y*dataColumns+x)
        NEXT
    NEXT



    'RESTORE level1
    'FOR offset = 0 TO 239
        'READ #line
        '#BACKTAB(offset) = #line
    'NEXT offset
    'WAIT



    jumpRestariner = 0
	movingUp = 0
	DIM #0YVelocity
    #0YVelocity = 0.0
	DIM zeroVelRestrainer
	zeroVelRestrainer = 0
	CONST acceleration = 0.125
    '16 = 0625 för högt
	'32 = 125
	DIM Counter
loop:


    IF jumpRestariner = 0 THEN
        IF CONT1.up THEN
	        jumpRestariner = 1
			movingUp = 1
            #0YVelocity = 2.50
        END IF
	END IF

    IF movingUp = 1 THEN GOSUB moveUp ELSE GOSUB moveDown





    IF CONT1.DOWN = 0 THEN GOTO skipDown
 
skipDown:


    IF CONT1.LEFT = 0 THEN GOTO skipLeft
    #X0 = #X0 -. 3.5
    overlap = tileWidth - (#X0 AND tileWidthMask)

    #line = #backtab(left + top * 20)
    IF #line = GW THEN #X0 = #X0 +. overlap : GOTO skipLeft

    #line = #backtab(left + bottom * 20)
    IF #line = GW THEN #X0 = #X0 +. overlap


    IF #x-21 = -1 THEN GOTO skipLeft
	IF (#X0 AND 255) <= 16 THEN Counter = -31
skipLeft:




    IF CONT1.RIGHT = 0 THEN GOTO skipRight
    #X0 = #X0 +. 2.5
    overlap = #X0 AND tileWidthMask

    #line = #backtab(right + top * 20)
    IF #line = GW THEN #X0 = #X0 -. overlap : GOTO skipRight

    #line = #backtab(right + bottom * 20)
    IF #line = GW THEN #X0 = #X0 -. overlap


    IF #x = dataColumns + 1 THEN GOTO skipRight
    IF (#X0 AND 255) >= 159 THEN Counter = 31
skipRight:


    
	

	IF Counter = 0 THEN GOTO skipScroll
    IF Counter < 128 THEN GOSUB scroll_Left ELSE GOSUB scroll_Right
skipScroll:



    #spriteFrame = #spriteFrame +. 0.125
    SPRITE 0,   (#X0 AND 255)-scrollCounter+HIT+VISIBLE,    (#Y0 AND 255),    SPR16+(#spriteFrame AND spriteFrameMask)*8+SPR_YELLOW
    WAIT





    GOTO loop

backgroundGFX:
    'BITMAP "########"
    'BITMAP ".......#"
    'BITMAP ".......#"
    'BITMAP ".......#"
    'BITMAP "########"
    'BITMAP "...#...."
    'BITMAP "...#...."
    'BITMAP "...#...."


    BITMAP "########"
    BITMAP "#......#"
    BITMAP "#......#"
    BITMAP "#......#"
    BITMAP "#......#"
    BITMAP "#......#"
    BITMAP "#......#"
    BITMAP "########"

spriteGFX:
	REM Invader
	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "####..##"
	BITMAP "########"
	BITMAP "########"
	BITMAP "########"
	BITMAP ".######."
	BITMAP "..####.."

	BITMAP "..####.."
	BITMAP ".######."
	BITMAP "####..##"
	BITMAP "######.."
	BITMAP "####...."
	BITMAP "######.."
	BITMAP ".######."
	BITMAP "..####.."

level1:
    DATA 00,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,GW,GW,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,GW,GW,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,00,GW
    DATA 00,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW,GW

moveUp: PROCEDURE
    #0YVelocity = #0YVelocity -. acceleration
    #Y0 = #Y0 -. #0YVelocity
    IF #0YVelocity = 0.0 then movingUp = 0 'if you reached peak jump height, start checking for ground instead

    'overlap = tileHeight - (#Y0 AND tileHeightMask)

    #line = #backtab(right + top * 20)
    IF #line = GW THEN GOTO hitHead

    #line = #backtab(left + top * 20)
    IF #line = GW THEN GOTO hitHead
    RETURN

hitHead:
    #Y0 = #Y0 +. tileHeight - (#Y0 AND tileHeightMask)
    #0YVelocity = 0.0
    movingUp = 0
    RETURN
END

moveDown: PROCEDURE
    #0YVelocity = #0YVelocity -. acceleration
    #Y0 = #Y0 -. #0YVelocity

    'overlap = (#Y0 AND tileHeightMask) - 4

    #line = #backtab(right + bottom * 20)
    IF #line = GW THEN GOTO onTheGround

    #line = #backtab(left + bottom * 20)
    IF #line = GW THEN GOTO onTheGround
    RETURN

onTheGround:
    #Y0 = #Y0 -. ((#Y0 AND tileHeightMask) - 4)
    jumpRestariner = 0    'so you can jump again after touching ground
	#0YVelocity = 0.0
	'#0YVelocity = 0.0 ': zeroVelRestrainer = 1
END


scroll_Left: PROCEDURE
    FOR Counter = 143 TO 0 STEP -1
	IF scrollCounter = 0 THEN scrollCounter = 7 : temp = 2 ELSE scrollCounter = scrollCounter - 1 : temp = 0
    SCROLL scrollCounter,0,temp
	SPRITE 0,   (#X0 AND 255) - scrollCounter + HIT + VISIBLE,    (#Y0 AND 255),    SPR16 + (#spriteFrame AND spriteFrameMask) * 8 + SPR_YELLOW
    WAIT
	IF temp = 2 THEN
        FOR y=0 TO 11
           #BACKTAB(  19+(y*20)  ) = level1(  #x+(y*dataColumns)  )
        NEXT
	    #x = #x + 1
		temp = 0
	END IF
	#X0 = #X0 -. 1.0
	NEXT Counter
	Counter = 0
END


scroll_Right: PROCEDURE
    FOR Counter = 143 TO 0 STEP -1
	IF scrollCounter = 7 THEN scrollCounter = 0 : temp = 1 ELSE scrollCounter = scrollCounter + 1 : temp = 0
    SCROLL scrollCounter,0,temp
	SPRITE 0,   (#X0 AND 255) - scrollCounter + HIT + VISIBLE,    (#Y0 AND 255),    SPR16 + (#spriteFrame AND spriteFrameMask) * 8 + SPR_YELLOW
    WAIT
	IF temp = 1 THEN
        FOR y=0 TO 11
           #BACKTAB(  y*20  ) = level1(  (#x-21)+(y*dataColumns)  )
        NEXT
	    #x = #x - 1
		temp = 0
	END IF
	#X0 = #X0 +. 1.0
	NEXT
	Counter = 0
END