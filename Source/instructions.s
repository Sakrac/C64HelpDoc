const COLUMNS = 32
const ROWS = 16
const MAX_GAPS = 8
const MAX_IMAGES = 8

const NumSticks = 2
const zpCharShift = $b0 ; 16 bytes, must be even

const VRAM = $4000
const VRAM_SET = 2 ; 0:$c000, 1:$8000, 2:$4000, 3:$0000

const TITLE_VRAM_SET = 0

const ScreenTitle = $e000
const BitmapTitle = $c000

const DecompressBuffer = $f800

const Bitmap = VRAM
const Screen = VRAM+$2000
const OverlaySprites = VRAM + $2800
const ColorRam = $d800

enum PageData {
	None,
	Image,
	Fence,
	Block,
	Color,
	Divider,
	Paragraph,
	Style,
	Sprites,
	End
}

; Change zero page usage here, 
pool zpGlobal $c0-$100
zpGlobal pool zpLocal 36
zpGlobal pool zpUtility 16


; global zero page usage. can be minimized by making some variable not use zero page if desired
zpLocal zpPage.w
zpLocal zpUnpack.w
zpLocal zpUnpackInt.w
zpLocal zpUnpackSrc.w

zpLocal zpPosX.w
zpLocal zpPosY
zpLocal zpWidth ; maybe allow for >255 pixels wide?
zpLocal zpHeight
zpLocal zpAlign
zpLocal zpLineSpace
zpLocal zpSpaceWidth
zpLocal zpCharSpacing
zpLocal zpColor
zpLocal zpFontPtr.w
zpLocal zpFontWid.w
zpLocal zpSprCount
zpLocal zpWideChar

enum Stick {
    up = 1,        ; Bit 0 (weight 1) goes low if the "up" switch is activated,
    down = 2,    ; Bit 1 (weight 2) goes low if the "down" switch is activated,
    left = 4,    ; Bit 2 (weight 4) goes low if the "left" switch is activated,
    right = 8,    ; Bit 3 (weight 8) goes low if the "right" switch is activated,
    fire = 16    ; Bit 4 (weight 16) goes low if the fire button is pressed.
}

if disk_ver
	org $3000	; make code start at this address if making the final
else
    org $801	; include a sys command and start in basic
; 1 SYS 2064
dc.b $0b, $08, $01, $00, $9e, $32, $30, $36, $34, $00, $00, $00, $00, $00, $00
endif

ifndef borderCol
borderCol = 6
endif

ifndef backCol
backCol=6
endif

Instructions:
	sei
	lda #0
	sta $d011

	lda #$7f
	sta $dc0d
	sta $dd0d
	lda $dc0d
	lda $dd0d
	lda #<Intrpt
	sta $fffe
	lda #>Intrpt
	sta $ffff
	lda #<RandomRTI
	sta $fffa
	lda #>RandomRTI
	sta $fffb
	lda #$35
	sta 1

	jsr InitStick
	lda #$01
	sta $d01a
	lda #$00
	sta $d01b
	lda #$20
	sta $d012

    ; set up screen mode
    lda $dd00     ; vic bank = $4000
    and #$fc
    ora #VRAM_SET
    sta $dd00
    lda #$c8
    sta $d016
    lda #((Screen>>6)&$f0) | ((VRAM>>10)&$0e)
    sta $d018
    lda #borderCol
    sta $d020
    lda #backCol
    sta $d021

	cli

	ldx #0
	jsr Setup
	jsr DrawPage

    lda #$3b    ; charmode, extended color
    sta $d011

	{
		sta IntWait
		{
			cmp IntWait
			beq !
		}
		jsr ReadStick

		{	; space
			lda RawSticksHit+1
			and #Stick.right
			beq .incr
			lda KeyboardBits + 1
			and #$80
			bne .shiftR
			lda KeyboardBits + 6
			and #$10
			bne .shiftR
			lda KeyboardBitsChange
			and #$04
			bne .incr
.shiftR		lda KeyboardBitsChange + 7
			and #$10
			beq %
.incr
			ldx CurrPage
			inx
			cpx PageCount
if disk_ver
			bcs BackToTitle ;%
else
			bcs %
endif
			stx CurrPage
			jsr Setup
			jsr DrawPage
		}

		{	; C=
			lda RawSticksHit+1
			and #Stick.left
			beq .decr
			lda KeyboardBits + 1
			and #$80
			bne .shiftL
			lda KeyboardBits + 6
			and #$10
			beq .noShift
.shiftL		lda KeyboardBitsChange
			and #$04
			bne .decr
.noShift	lda KeyboardBitsChange + 7
			and #$20
			beq %
.decr
			ldx CurrPage
			dex
			bmi %
			stx CurrPage
			jsr Setup
			jsr DrawPage
		}

		ldy #NumPageKeys-1
		{
			ldx PageKeyIndex,y
			{
				lda KeyboardBitsChange,x
				and PageKeyMask,y
				beq %
				ldx PageKeyPage,y
				bpl SetPage
			}
			dey
			bpl !
		}

if disk_ver	; This code checks for menu exit, but there isn't anything to do
		{ ; run/stop, fire
			lda KeyboardBitsChange + 7
			and #$80
			bne BackToTitle
			lda RawSticksHit+1
			and #Stick.fire
			beq BackToTitle
		}
endif
		jmp !
SetPage:
		{
			cpx CurrPage
			beq %
			stx CurrPage
			jsr Setup
			jsr DrawPage
		}
		jmp !

	}

if disk_ver
; NOTE: This code won't compile, implement your own Back To Game code here!
BackToTitle:
{
	sei
	lda #<LoadMusicInterrupt
	sta $fffe
	lda #>LoadMusicInterrupt
	sta $ffff
	cli

    ; set up screen mode
    lda $dd00     ; vic bank = $4000
    and #$fc
    sta $dd00
    lda #$d8
    sta $d016
    lda #((ScreenTitle>>6)&$f0) | ((BitmapTitle>>10)&$0e)
    sta $d018
    lda #0
    sta $d020
    lda #0
    sta $d021

    lda #$3b    ; charmode, extended color
    sta $d011

	jmp $120
}
endif

Intrpt:
	pha
	txa
	pha
	tya
	pha

	inc IntWait

	ldx #15
	{
		lda SpritePositions,x
		sta $d000,x
		dex
		bpl !
	}
	ldx #7
	{
		lda SpriteColors,x
		sta $d027,x
		clc
		txa
		adc #OverlaySprites>>6
		sta Screen + $3f8,x
		dex
		bpl !
	}
	ldx zpSprCount
	lda BitSets,x
	sta $d015
	lda #0
	sta $d017
	sta $d01b
	sta $d01c
	sta $d01d

	lda SpriteHiBit
	sta $d010

	lda #<Intrpt2
	sta $fffe
	lda #>Intrpt2
	sta $ffff

	; IRQ ACKNOWLEDGE
	lda #$ff
	sta $d019

	lda #$1c + 52
	sta $d012
	lda $d011
	and #$7f
	sta $d011
if disk_ver
	cli	; Allow music call to take longer than next interrupt
	jsr CheckPlayMusic ; call music update
endif
	pla
	tay
	pla
	tax
	pla
RandomRTI:
	rti

Intrpt2:
	pha
	txa
	pha
	tya
	pha

	lda #<Intrpt3
	sta $fffe
	lda #>Intrpt3
	sta $ffff

	; IRQ ACKNOWLEDGE
	lda #$ff
	sta $d019

	lda #$1c + 52 * 2
	sta $d012
	lda $d011
	and #$7f
	sta $d011

if disk_ver
	cli
	jsr CheckPlayMusic
endif

	pla
	tay
	pla
	tax
	pla
	rti

Intrpt3:
	pha
	txa
	pha
	tya
	pha

	lda #<Intrpt4
	sta $fffe
	lda #>Intrpt4
	sta $ffff

	; IRQ ACKNOWLEDGE
	lda #$ff
	sta $d019

	lda #$1c + 52 * 3
	sta $d012
	lda $d011
	and #$7f
	sta $d011

if disk_ver
	cli
	jsr CheckPlayMusic
endif

	pla
	tay
	pla
	tax
	pla
	rti

Intrpt4:
	pha
	txa
	pha
	tya
	pha

	lda #<Intrpt5
	sta $fffe
	lda #>Intrpt5
	sta $ffff

	; IRQ ACKNOWLEDGE
	lda #$ff
	sta $d019

	lda #$1c + 52 * 4
	sta $d012
	lda $d011
	and #$7f
	sta $d011

if disk_ver
	cli
	jsr CheckPlayMusic
endif

	pla
	tay
	pla
	tax
	pla
	rti

Intrpt5:
	pha
	txa
	pha
	tya
	pha

	lda #<Intrpt
	sta $fffe
	lda #>Intrpt
	sta $ffff

	; IRQ ACKNOWLEDGE
	lda #$ff
	sta $d019

	lda #$1c
	sta $d012
	lda $d011
	and #$7f
	sta $d011

if disk_ver
	cli
	jsr CheckPlayMusic
endif

	pla
	tay
	pla
	tax
	pla
	rti



BitSets:
	rept 9 { dc.b (1<<rept)-1 }

DrawPage:
{
	ldy #0
	lda (zpPage),y
	{
		cmp #PageData.End
		bne %
		rts
	}
	{
		cmp #PageData.Image
		bne %
		jsr DrawImage
		lda #1+10
		jmp NextPageData
	}
	{
		cmp #PageData.Sprites
		beq .doSpr
		jmp %
.doSpr	zpLocal .zpSprSrc.w
		zpLocal .zpSprDst.w
		zpLocal .zpSprComp
		lda #0
		sta .zpSprDst
		lda zpSprCount
		lsr
		ror .zpSprDst
		lsr
		ror .zpSprDst
		adc #>OverlaySprites
		sta .zpSprDst+1
		iny
		lda (zpPage),y
		sta .zpSprSrc
		iny
		lda (zpPage),y
		sta .zpSprSrc+1
		iny
		{
			lda (zpPage),y
			beq %
			tya
			pha
			lda .zpSprSrc
			sta zpUnpack
			lda .zpSprSrc+1
			sta zpUnpack+1
			jsr Unpack
			lda #<DecompressBuffer
			sta .zpSprSrc
			lda #>DecompressBuffer
			sta .zpSprSrc+1
			pla
			tay
		}
		iny
		zpLocal .zpLeft
		lda (zpPage),y
		sta .zpLeft
		iny
		lda (zpPage),y
		{
			ldx zpSprCount
			beq %
			{
				asl
				dex
				bne !
			}
		}
		ora SpriteHiBit
		sta SpriteHiBit
		{
			ldx zpSprCount
			iny
			lda (zpPage),y
			sta SpriteColors,x
			txa
			asl
			tax
			iny
			lda (zpPage),y
			sta SpritePositions,x
			iny
			inx
			lda (zpPage),y
			sta SpritePositions,x
			tya
			pha
			ldy #$40-2
			{
				lda (.zpSprSrc),y
				sta (.zpSprDst),y
				dey
				bpl !
			}
			clc
			lda .zpSprSrc
			adc #$40
			sta .zpSprSrc
			{
				bcc %
				inc .zpSprSrc+1
			}
			clc
			lda .zpSprDst
			adc #$40
			sta .zpSprDst
			{
				bcc %
				inc .zpSprDst+1
			}
			pla
			tay
			inc zpSprCount
			dec .zpLeft
			bne !
		}
		iny
		tya
		jmp NextPageData
	}
	{
		cmp #PageData.Block
		beq .block
		jmp %
.block	iny
		lda (zpPage),y
		sta zpPosX
		iny
		lda (zpPage),y
		sta zpPosX+1
		iny
		lda (zpPage),y
		sta zpPosY
		iny
		lda (zpPage),y
		sta zpWidth
		iny
		lda (zpPage),y
		sta zpHeight
		iny
		tya
HandleBlockData:
		{	; handle block
			clc
			adc zpPage
			sta zpPage
			{
				bcc %
				inc zpPage+1
			}
			ldy #0
			lda (zpPage),y
			{
				cmp #PageData.End
				bne %
				lda #1
				jmp NextPageData
			}
			{
				cmp #PageData.Color
				bne %
				iny
				lda (zpPage),y
				sta zpColor
				lda #2
				jmp HandleBlockData
			}
			{
				cmp #PageData.Divider
				bne %
				jsr DrawDivider
				lda #1
				jmp HandleBlockData
			}
			{
				cmp #PageData.Style
				bne %
				zpLocal .zpStyle.w
				iny
				lda (zpPage),y ; style #, y=1
				tax
				lda StylesLo,x
				sta .zpStyle
				lda StylesHi,x
				sta .zpStyle+1
				ldy #0
				lda (.zpStyle),y ; font #, y=0
				tax
				lda FontBitmapLo,x
				sta zpFontPtr
				lda FontBitmapHi,x
				sta zpFontPtr+1
				lda FontWidthLo,x
				sta zpFontWid
				lda FontWidthHi,x
				sta zpFontWid+1
				iny
				lda (.zpStyle),y ; line spacing, y = 1
				sta zpLineSpace
				iny
				lda (zpPage),y ; align, y = 2
				sta zpAlign
				lda (.zpStyle),y ; space width, y = 2
				sta zpSpaceWidth
				iny
				lda (.zpStyle),y ; char spacing, y = 3
				sta zpCharSpacing
				lda #3
				jmp HandleBlockData
			}
			{
				cmp #PageData.Paragraph
				bne %
				{
					zpLocal .zpLen
					ldy #1
					lda (zpPage),y
					sta .zpLen
					iny
					lda (zpPage),y
					pha
					iny
					lda (zpPage),y
					tax
					pla
					ldy .zpLen
					jsr PrintString ; a = str lo, x = str hi, y = str len
				}
				lda #4
				jmp HandleBlockData
			}
			dc.b 2
		}
	}

NextPageData:
	clc
	adc zpPage
	sta zpPage
	{
		bcc %
		inc zpPage+1
	}
	jmp !
}

DrawDivider:
{
	clc
	lda zpPosY
	adc #8
	sta zpPosY
	rts
}

struct DrawImg {
	byte Compressed
	word Bitmap
	word Screen
	word posX
	byte posY
	byte width
	byte height
}

const DrawImgSize = DrawImg.bytes

DrawImage:
{
	zpUtility .zpHgt
	zpUtility .zpWid
	zpUtility .zpPosY
	zpUtility .zpPosX.w
	zpUtility .zpScrn.w
	zpUtility .zpBitmap.w
	zpUtility .zpCompreseed
	ldy #0
	{
		iny
		lda (zpPage),y
		sta .zpCompreseed-1,y ; pools are allocated in reverse
		cpy #DrawImg.bytes
		bcc !
	}
	{
		ldx CurrImageCount
		cpx #MAX_IMAGES
		bcs %
		inc CurrImageCount
		lda .zpPosX
		sta ImageXLo,x
		;clc
		adc .zpWid
		sta ImageXRLo,x
		lda .zpPosX+1
		sta ImageXHi,x
		adc #0
		sta ImageXRHi,x
		lda .zpPosY
		sta ImageY,x
		;clc
		adc .zpHgt
		sta ImageYBot,x
	}
	{	; decompress bitmap?
		lda .zpCompreseed
		and #1
		beq %
		lda .zpBitmap
		sta zpUnpack
		lda .zpBitmap+1
		sta zpUnpack+1
		jsr Unpack
		lda #<DecompressBuffer
		sta .zpBitmap
		lda #>DecompressBuffer
		sta .zpBitmap+1
	}
	{
		;*40*8 = *5*64 = *$100 - $40
		zpUtility .zpTrgBitmap.w
		lda #0
		sta .zpTrgBitmap+1
		lda .zpPosY ; already x8
		asl
		rol .zpTrgBitmap+1
		asl
		rol .zpTrgBitmap+1
		adc .zpPosY
		{
			bcc %
			inc .zpTrgBitmap+1
		}
		asl
		rol .zpTrgBitmap+1
		asl
		rol .zpTrgBitmap+1
		asl
		rol .zpTrgBitmap+1
		sta .zpTrgBitmap
		lda .zpTrgBitmap+1
		adc #>Bitmap
		sta .zpTrgBitmap+1
		lda .zpTrgBitmap
		adc .zpPosX
		sta .zpTrgBitmap
		lda .zpTrgBitmap+1
		adc .zpPosX+1
		sta .zpTrgBitmap+1

		lda .zpHgt
		lsr
		lsr
		lsr
		tax
		{
			ldy #0
			{
				lda (.zpBitmap),y
				sta (.zpTrgBitmap),y
				iny
				cpy .zpWid
				bcc !
			}
			clc
			lda .zpBitmap
			adc .zpWid
			sta .zpBitmap
			{
				bcc %
				inc .zpBitmap+1
			}
			clc
			lda .zpTrgBitmap
			adc #<(40 * 8)
			sta .zpTrgBitmap
			lda .zpTrgBitmap+1
			adc #>(40 * 8)
			sta .zpTrgBitmap+1
			dex
			bne !
		}
	}
	{	; decompress screen?
		lda .zpCompreseed
		and #2
		beq %
		lda .zpScrn
		sta zpUnpack
		lda .zpScrn+1
		sta zpUnpack+1
		jsr Unpack
		lda #<DecompressBuffer
		sta .zpScrn
		lda #>DecompressBuffer
		sta .zpScrn+1
	}
	{
		zpUtility .zpTrgScreen.w
		lda #0
		sta .zpTrgScreen+1
		lda .zpPosY ; / 8 * 40 = * 5
		asl
		rol .zpTrgScreen+1
		asl
		rol .zpTrgScreen+1
		adc .zpPosY
		sta .zpTrgScreen
		{
			bcc %
			inc .zpTrgScreen+1
		}
		clc
		lda .zpTrgScreen+1
		adc #>Screen
		sta .zpTrgScreen+1
		{
			zpUtility .zpShiftX
			lda .zpPosX+1
			sta .zpShiftX
			lda .zpPosX
			lsr .zpShiftX
			ror
			lsr .zpShiftX
			ror
			lsr .zpShiftX
			ror
			adc .zpTrgScreen
			sta .zpTrgScreen
			{
				bcc %
				inc .zpTrgScreen+1
			}
		}
		{
			zpUtility .zpCols
			lda .zpWid
			lsr
			lsr
			lsr
			sta .zpCols
			lda .zpHgt
			lsr
			lsr
			lsr
			tax
			{
				ldy .zpCols
				dey
				{
					lda (.zpScrn),y
					sta (.zpTrgScreen),y
					dey
					bpl !
				}
				clc
				lda .zpScrn
				adc .zpCols
				sta .zpScrn
				{
					bcc %
					inc .zpScrn+1
				}
				clc
				lda .zpTrgScreen
				adc #40
				sta .zpTrgScreen
				{
					bcc %
					inc .zpTrgScreen+1
				}
				dex
				bne !
			}
		}
	}
	rts
	; compressed data in .zpComp.w
}

Unpack:
{
	ldy #0
	clc
	lda (zpUnpack),y
	sta zpUnpackInt
	iny
	lda (zpUnpack),y
	adc #>DecompressBuffer
	sta zpUnpackInt+1
	iny
	jsr .stepY
	{
		ldy #0
		lda (zpUnpack),y
		{
			bne %
			rts
		}
		bmi .copy
		{
			tax
			eor #$ff
			sec
			adc zpUnpackInt
			sta zpUnpackInt
			sta % + 5
			lda zpUnpackInt+1
			sbc #0
			sta zpUnpackInt+1
			sta % + 6
		}
		{
			iny
			dex
			lda (zpUnpack),y
			sta $f800,x
			cpx #0
			bne !
		}
		iny
		jsr .stepY
		jmp !
.copy	pha
		clc
		adc zpUnpackInt
		sta zpUnpackInt
		lda zpUnpackInt+1
		sbc #0
		sta zpUnpackInt+1
		iny
		{
			clc
			lda (zpUnpack),y
			adc zpUnpackInt
			sta zpUnpackSrc
			lda zpUnpackInt+1
			adc #0
			sta zpUnpackSrc+1
			iny
			jsr .stepY
			pla
			eor #$ff ; length - 1
			tay
			{
				lda (zpUnpackSrc),y
				sta (zpUnpackInt),y
				dey
				bpl !
			}
		}
		bmi !
	}
.stepY
	clc
	tya
	adc zpUnpack
	sta zpUnpack
	{
		bcc %
		inc zpUnpack+1
	}
	rts
}


; a = char
; x = shift
ShiftChar:
{
;	zpUtility .zpCharShift
	zpUtility .zpSrc.w

	tay
	pha

	txa
	and #7
	tax


	{
		clc
		adc (zpFontWid),y
		cmp #9
		bcs %
		pla
		{
			zpUtility .zpChr
			asl
			rol .zpChr
			asl
			rol .zpChr
			asl
			rol .zpChr
			clc
			adc zpFontPtr
			sta .zpSrc
			lda .zpChr
			and #7
			adc zpFontPtr+1
			sta .zpSrc+1
		}
		{
			zpUtility .zpSft
			stx .zpSft
			ldy #7
			{
				lda (.zpSrc),y
				{
					ldx .zpSft
					beq %
					{
						lsr
						dex
						bne !
					}
				}
				sta zpCharShift,y
				dey
				bpl !
			}
		}
		lda #0
		sta zpWideChar
		rts
	}
	pla
	{
		zpUtility .zpChr
		asl
		rol .zpChr
		asl
		rol .zpChr
		asl
		rol .zpChr
		clc
		adc zpFontPtr
		sta CharSrc
		lda .zpChr
		and #7
		adc zpFontPtr+1
		sta CharSrc+1
	}

	{
		lda #zpCharShift
		ldy #zpCharShift+8
		cpx #4
		bcc %
		tya
		ldy #zpCharShift
	}
	sta CharDst
	sty CharClr

	txa
	pha

	ldx #7
	stx zpWideChar
	{
const CharSrc = *+1
		lda $1234,x
const CharDst = *+1
		sta.z zpCharShift,x
		lda #0
const CharClr = *+1
		sta zpCharShift+8,x
		dex
		bpl !
	}

	pla
	tay
	beq .noRol
	cpy #4
	bcs .doRor
.doRol
	{
		ldx #7
		{
			lsr zpCharShift + rept,x
			ror zpCharShift + rept + 8,x
			dex
			bpl !
		}
		dey
		bne !
	}
	beq .noRol
.doRor
	{
		ldx #7
		{
			asl zpCharShift + rept + 8,x
			rol zpCharShift + rept,x
			dex
			bpl !
		}

		iny
		cpy #8
		bne !
	}
.noRol
	rts
}

; x = page
Setup:
{
	stx CurrPage
	lda PagesLo,x
	sta zpPage
	lda PagesHi,x
	sta zpPage+1

	lda #0	; remove old image fences
	sta CurrImageCount
	sta zpSprCount
	sta SpriteHiBit

	ldy #>Bitmap
	ldx #>$2000
	lda #0
	jsr ClearPages

	ldy #>Screen
	ldx #4
	lda #$e0 | backCol
	jsr ClearPages

	rts
}


; x = x lo
; a = x hi
; y = y
DrawChar:
{
	zpUtility .zpCharY ; 0-7, current vertical pos within char
	zpUtility .zpX.w
	zpUtility .zpDst.w ; address
	stx .zpX
	sta .zpX+1
	tya
	and #7
	sta .zpCharY
	lda #0
	sta .zpDst
	tya ; * 40 * 8 => *5 * 64 = * (5 * 256)>>2 = *256 + *64
	lsr
	lsr
	lsr
	sta .zpDst+1
	lsr
	ror .zpDst
	lsr
	ror .zpDst
	clc
	adc .zpDst+1
	adc #>Bitmap
	sta .zpDst+1
	lda .zpX
	and #$f8
	adc .zpDst
	sta .zpDst
	lda .zpDst+1
	adc .zpX+1
	sta .zpDst+1
	ldx #0
	ldy .zpCharY
	{
		lda zpWideChar
		beq %
		{
			lda zpCharShift,x
			ora (.zpDst),y
			sta (.zpDst),y
			tya
			ora #8
			tay
			lda zpCharShift+8,x
			ora (.zpDst),y
			sta (.zpDst),y
			tya
			and #7
			tay
			iny
			{
				cpy #8
				bcc %
				lda .zpDst
				adc #<(40*8-1) ; C set
				sta .zpDst
				lda .zpDst+1
				adc #>(40*8-1)
				sta .zpDst+1
				ldy #0
			}
			inx
			cpx #8
			bcc !
		}
		rts
	}
	{
		{
			lda zpCharShift,x
			ora (.zpDst),y
			sta (.zpDst),y
			iny
			{
				cpy #8
				bcc %
				lda .zpDst
				adc #<(40*8-1) ; C set
				sta .zpDst
				lda .zpDst+1
				adc #>(40*8-1)
				sta .zpDst+1
				ldy #0
			}
			inx
			cpx #8
			bcc !
		}
		rts
	}
}

;zpLocal zpPosX.w
;zpLocal zpPosY
;zpLocal zpWidth

; a = str lo
; x = str hi
; y = str len
PrintString:
{
	zpLocal .zpStr.w
	zpLocal .zpLeft

	; a,x: string
	; y: length

	sta .zpStr
	stx .zpStr+1
	sty .zpLeft
	{
		zpLocal .zpXPosClip.w
		zpLocal .zpWidthClip

; x, y, w, x hi...
CheckLineImages:
		{
			lda zpPosX
			sta .zpXPosClip
			lda zpPosX+1
			sta .zpXPosClip+1
			lda zpWidth
			sta .zpWidthClip
			ldy CurrImageCount
			{
				bne %
				jmp FillColor
			}
			{
				{
					dey
					; y>=(imageY-zpLineHeight) => (y+zpLineHeight) >= imageY
					clc
					lda zpPosY
					adc zpLineSpace
					cmp ImageY,y
					{
						bcs %
						jmp NextImageFence
					}

					lda zpPosY
					cmp ImageYBot,y
					{
						bcc %
						jmp NextImageFence
					}

					; a=xpos, b=xrgt, c=imgx, d=imgrgt
					; intersect if d>a && c<b => d-a>0 && b-c>0 (c-b<=0)
					; a<c => opt 1 = ac
					; b>d => opt 2 = db
					; ac = imgx-xpos
					; db = xrgt-imgrgt

					zpLocal .zpXRgt.w
					clc
					lda .zpXPosClip
					adc .zpWidthClip
					sta .zpXRgt
					lda .zpXPosClip+1
					adc #0
					sta .zpXRgt+1

					; c<b
					lda ImageXLo,y ; b
					cmp .zpXRgt    ; c
					lda ImageXHi,y
					sbc .zpXRgt+1
					bcs NextImageFence

					; d>a
					lda .zpXPosClip
					cmp ImageXRLo,y
					lda .zpXPosClip+1
					sbc ImageXRHi,y
					bcs NextImageFence

					zpLocal .zpAC.w
					zpLocal .zpDB.w
					; calculate ac and db, pick whichever is greater
					sec ; AC
					lda ImageXLo,y
					sbc .zpXPosClip
					sta .zpAC
					lda ImageXHi,y
					sbc .zpXPosClip+1
					sta .zpAC+1

					sec	; DB
					lda .zpXRgt
					sbc ImageXRLo,y
					sta .zpDB
					lda .zpXRgt+1
					sbc ImageXRHi,y
					sta .zpDB+1

					lda .zpDB+1
					bne .useAC
					lda .zpAC+1
					bne .useDB

					lda .zpAC ; AC > DB ?
					cmp .zpDB
					lda .zpAC+1
					sbc .zpDB+1
					bcc .useDB ; DB was greater
.useAC
					lda .zpAC+1
					bne .no_fit
					lda .zpAC
					cmp #24
					bcc .no_fit
					sta .zpWidthClip
					jmp NextImageFence

.useDB				lda .zpDB+1
					bne .no_fit
					clc
					lda ImageXRLo,y
					adc #2
					sta .zpXPosClip
					lda ImageXRHi,y
					adc #0
					sta .zpXPosClip+1
					lda .zpDB
					sbc #4
					sta .zpWidthClip
					jmp NextImageFence

.no_fit				clc
					lda zpPosY
					adc zpLineSpace
					sta zpPosY
					cmp zpHeight
					{
						bcs %
						rts
					}
					jmp CheckLineImages
				}
NextImageFence:
				cpy #0
				beq %
				jmp !
			}
		}
FillColor:
		{
			zpLocal .zpScrn.w
			zpLocal .zpXChar
			zpLocal .zpYChar
			lda .zpXPosClip+1
			sta .zpXChar
			lda .zpXPosClip
			lsr .zpXChar
			ror
			lsr
			lsr
			sta .zpXChar
			lda #0
			sta .zpScrn+1
			lda zpPosY ; x 8
			and #$f8
			sta .zpYChar
			asl
			rol .zpScrn+1
			asl
			rol .zpScrn+1
			adc .zpYChar ; x 5
			{
				bcc %
				inc .zpScrn+1
			}
			clc
			adc .zpXChar
			sta .zpScrn
			lda .zpScrn+1
			adc #>Screen
			sta .zpScrn+1
			ldx #0
			{
				clc
				lda zpPosY
				adc #7
				cmp zpHeight
				bcc .heightOk
				lda zpHeight
.heightOk		and #$f8
				cmp .zpYChar
				beq %
				inx
			}

			clc
			lda .zpXPosClip
			and #7
			adc .zpWidthClip
			clc
			adc #7
			lsr
			lsr
			lsr
			zpLocal .zpBgChars
			sta .zpBgChars
			{
				ldy .zpBgChars
				beq %
				dey
				lda zpColor
				{
					sta (.zpScrn),y
					dey
					bpl !
				}
				clc
				lda .zpScrn
				adc #40
				sta .zpScrn
				bcc .noInc
				inc .zpScrn+1
.noInc			dex
				bmi %
				jmp !
			}
		}
CharsPerLine:
		{ ; determine how many pixels fit
			zpLocal .zpPixelsLeft
			zpLocal .zpPixelsLeftLastSpace
			zpLocal .zpLastSpace
			lda #$ff
			sta .zpLastSpace
			lda .zpWidthClip
			sta .zpPixelsLeft
			ldy #0
			{
				lda (.zpStr),y
				bne .not_space
				sty .zpLastSpace
				sec
				lda .zpPixelsLeft
				sta .zpPixelsLeftLastSpace
				sbc zpSpaceWidth
				bcc %
				bcs .next
.not_space
				zpLocal .zpY
				sty .zpY
				tay
				sec
				lda .zpPixelsLeft
				sbc zpCharSpacing
				bcs .okChrSpc
				ldy .zpY
				bcc %
.okChrSpc		dey
				sbc (zpFontWid),y
				ldy .zpY
				bcc %
.next			sta .zpPixelsLeft
				iny
				cpy .zpLeft
				bcc !
			}
			{
				cpy .zpLeft ; check if entire remainder (no need to clip at space)
				beq %
				lda .zpLastSpace
				cmp #$ff
				beq %
				tay
				lda .zpPixelsLeftLastSpace
				sta .zpPixelsLeft
			}
			lda #0
			ldx zpAlign
			bmi %
			lda .zpPixelsLeft
			cpx #1
			beq %
			lsr
		}
DrawLine:
		; a = pixl offset for align
		; y = number of chars on line
		{
			zpLocal .zpCurr
			zpLocal .zpLine
			zpLocal .zpLineX.w
			sty .zpLine
			clc
			adc .zpXPosClip
			sta .zpLineX
			lda .zpXPosClip+1
			adc #0
			sta .zpLineX+1
			ldy #0
			{
				sty .zpCurr
				lda (.zpStr),y
				bne .notSpace
				lda zpSpaceWidth
				bne .stepForward
.notSpace		sec
				sbc #1
				pha
				ldx .zpLineX ; only need lower 3 bits
				jsr ShiftChar
				ldx .zpLineX
				lda .zpLineX+1
				ldy zpPosY
				jsr DrawChar
				pla
				tay
				clc
				lda (zpFontWid),y
				adc zpCharSpacing
.stepForward
				clc
				adc .zpLineX
				sta .zpLineX
				{
					bcc %
					inc .zpLineX+1
				}
				ldy .zpCurr
				iny
				cpy .zpLine
				bcc !
			}
			clc
			lda .zpStr
			adc .zpLine
			sta .zpStr
			{
				bcc %
				inc .zpStr+1
			}
			sec
			lda .zpLeft
			sbc .zpLine
		}
		pha
		clc
		lda zpPosY
		adc zpLineSpace
		sta zpPosY
		pla
		beq %
		sta .zpLeft
		ldy #0	; skip
.skipLeadingSpaces
		lda .zpLeft
		beq %
		lda (.zpStr),y
		bne .skipToNextLine
		dec .zpLeft
		inc .zpStr
		bne .skipLeadingSpaces
		inc .zpStr+1
		bne .skipLeadingSpaces
.skipToNextLine
		jmp !
	}
	rts
}



ClearPages:
{
    sty ClearPagesPage
    ldy #0
    {
const ClearPagesPage = * + 2
        sta VRAM,y
        iny
        bne !
        inc !+2
        dex
        bne !
    }
    rts
}

if disk_ver == 0

; KEYBOARD MATRIX
;   |   38 |   30 |   28 |   20 |   18 |   10 |   8  |   0  |
;---+------+------+------+------+------+------+------+------+
; 0 | DOWN |  F5  |  F3  |  F1  |  F7  |RIGHT |  RET |  DEL |
; 1 |L-SHFT|   e  |   s  |   z  |   4  |   a  |   w  |   3  |
; 2 |   x  |   t  |   f  |   c  |   6  |   d  |   r  |   5  |
; 3 |   v  |   u  |   h  |   b  |   8  |   g  |   y  |   7  |
; 4 |   n  |   o  |   k  |   m  |   0  |   j  |   i  |   9  |
; 5 |   ,  |   @  |   :  |   .  |   -  |   l  |   p  |   +  |
; 6 |   /  |   ^  |   =  |R-SHFT| HOME |   ;  |   *  |   £  |
; 7 | STOP |   q  |  C=  |SPACE |   2  | CTRL |  <-  |   1  |
;---+------+------+------+------+------+------+------+------+
;   |  80  |  40  |  20  |  10  |   8  |   4  |   2  |   1  |
; KEYBOARD MASK

ReadStick:
{
	ldx #0
	stx $dc02
	stx $dc03
	lda $dc01 // read port1
	and #$1f
	sta IntSticks

	lda $dc00 // read port2
	sta IntSticks+1

	lda #$00	; Set to input
	sta $dc03	; Port B data direction register
	ldx #$ff	; Set to output
	stx $dc02	; Port A data direction register

	ldx #7
	{
		lda BitShiftInv,x ; ~(1<<x), KeyboardColumnMasks
		sta $dc00
		lda $dc01
		sta KeyboardBitsInt,x
		dex
		bpl !
	}


	ldx #NumSticks - 1
	{
		lda RawSticks,x
		sta RawSticksPrev,x
		lda IntSticks,x
		sta RawSticks,x
		dex
		bpl !
	}

	ldx #7
	{
		lda KeyboardBitsInt,x
		pha
		ora KeyboardBits,x
		eor #$ff
		sta KeyboardBitsChange,x
		pla
		eor #$ff
		sta KeyboardBits,x
		dex
		bpl !
	}

	ldx #NumSticks-1
	{
		lda RawSticks,x ; 0
		eor #$ff
		and RawSticksPrev,x ; 1
		eor #$ff
		sta RawSticksHit,x
		dex
		bpl !
	}
	rts
}

InitStick:
ClearInput:
{
	lda #0
	ldx #ClearButtonLen
	{
		dex
		sta InputBSSStart,x
		bne !
	}
	rts
}
BitShiftInv:
	rept 8 { dc.b 255 - (1<<rept) }

endif


include "include_hack.s"

PageKeyIndex:
	dc.b $01, $03, $02, $02, $01, $02, $03, $03, $04, $06
const NumPageKeys = * - PageKeyIndex
PageKeyMask:
	dc.b $04, $10, $10, $04, $40, $20, $04, $20, $02, $08
PageKeyPage:
	dc.b $00, $01, $02, $03, $04, $05, $06, $06, $07, $00

SECTION BSS, bss
org $e800

CurrPage:
	ds 1

CurrImageCount:
	ds 1

ImageXLo:
	ds MAX_IMAGES

ImageXHi:
	ds MAX_IMAGES

ImageXRLo:
	ds MAX_IMAGES

ImageXRHi:
	ds MAX_IMAGES

ImageY:
	ds MAX_IMAGES

ImageYBot:
	ds MAX_IMAGES

SpritePositions:
	ds 16
SpriteColors:
	ds 8
SpriteHiBit:
	ds 1

if disk_ver == 0
IntWait:
	ds 1
endif
if disk_ver == 0
InputBSSStart:
IntSticks:
	ds NumSticks
RawSticksPrev:
	ds NumSticks
RawSticks:
	ds NumSticks
RawSticksHit:
	ds NumSticks

KeyboardBitsInt:
	ds 8
KeyboardBits:
	ds 8
KeyboardBitsChange:
	ds 8

ClearButtonLen = * - InputBSSStart
endif