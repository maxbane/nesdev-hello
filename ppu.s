; NES Picture Processing Unit module
; Useful constants, macros, and subroutines

.fileopt    comment, "NES Picture Processing Unit module, v1.0"
.fileopt    author,  "Max Bane"

;
; PPU Register Address Constants
;

.export PPU_CTRL     := $2000
; Writable
; 7654 3210
; |||| ||||
; |||| ||++- Base nametable address
; |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
; |||| |+--- VRAM address increment per CPU read/write of PPUDATA
; |||| |     (0: add 1, going across; 1: add 32, going down)
; |||| +---- Sprite pattern table address for 8x8 sprites
; ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
; |||+------ Background pattern table address (0: $0000; 1: $1000)
; ||+------- Sprite size (0: 8x8; 1: 8x16)
; |+-------- PPU master/slave select
; |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
; +--------- Generate an NMI at the start of the
;            vertical blanking interval (0: off; 1: on)
; (Diagram from wiki.nesdev.com)

.export PPU_MASK     := $2001
; Writable
; 76543210
; ||||||||
; |||||||+- Grayscale (0: normal color; 1: produce a monochrome display)
; ||||||+-- 1: Show background in leftmost 8 pixels of screen; 0: Hide
; |||||+--- 1: Show sprites in leftmost 8 pixels of screen; 0: Hide
; ||||+---- 1: Show background
; |||+----- 1: Show sprites
; ||+------ Intensify reds (and darken other colors)
; |+------- Intensify greens (and darken other colors)
; +-------- Intensify blues (and darken other colors)
; (Diagram from wiki.nesdev.com)

.export PPU_STATUS   := $2002
; Readable
; 7654 3210
; |||| ||||
; |||+-++++- Least significant bits previously written into a PPU register
; |||        (due to register not being updated for this address)
; ||+------- Sprite overflow. The intent was for this flag to be set
; ||         whenever more than eight sprites appear on a scanline, but a
; ||         hardware bug causes the actual behavior to be more complicated
; ||         and generate false positives as well as false negatives; see
; ||         PPU sprite evaluation. This flag is set during sprite
; ||         evaluation and cleared at dot 1 (the second dot) of the
; ||         pre-render line.
; |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
; |          a nonzero background pixel; cleared at dot 1 of the pre-render
; |          line.  Used for raster timing.
; +--------- Vertical blank has started (0: not in VBLANK; 1: in VBLANK).
;            Set at dot 1 of line 241 (the line *after* the post-render
;            line); cleared after reading $2002 and at dot 1 of the
;            pre-render line.
; (Diagram from wiki.nesdev.com)

.export PPU_OAM_ADDR := $2003
; Writable

.export PPU_OAM_DATA := $2004
; Read/Write

.export PPU_OAM_DMA  := $4014
; Fancy Write

.export PPU_SCROLL   := $2005
; Write x2

.export PPU_ADDR     := $2006
; Write x2

.export PPU_DATA     := $2007
; Read/Write

;
; Buffers reserved in ram for updating the PPU
;
.segment "RAM"

; nametable update entry buffer for PPU update
ppu_nmt_buffer: .res 256
.export ppu_nmt_buffer

; palette buffer for PPU update
ppu_palette_buffer:    .res 32
.export ppu_palette_buffer

; Reserve a page for an object attribute map buffer
.segment "OAM"
; sprite OAM data to be uploaded by DMA
ppu_oam_buffer: .res 256
.export ppu_oam_buffer

;
; Zeropage variables used by our PPU routines
;
.segment "ZEROPAGE"

; prevents NMI re-entry
ppu_nmi_lock:       .res 1 
.exportzp ppu_nmi_lock

; is incremented every NMI
ppu_nmi_count:      .res 1 
.exportzp ppu_nmi_count

; set to 1 to push a PPU frame update, 2 to turn rendering off next NMI
ppu_nmi_ready:      .res 1 
.exportzp ppu_nmi_ready

; number of bytes in nmt_update buffer
ppu_nmt_buffer_len: .res 1 
.exportzp ppu_nmt_buffer_len

; x,y scroll positions
ppu_scroll_x:       .res 1 
ppu_scroll_y:       .res 1 
.exportzp ppu_scroll_x, ppu_scroll_y

; nametable select (0-3 = PPU_CTRL,$2400,$2800,$2C00)
ppu_scroll_nmt:     .res 1 
.exportzp ppu_scroll_nmt

_ppu_temp:           .res 1 ; temporary variable


;
; PPU routines
;
.segment "CODE"

; Generic PPU reset/startup routine. Disables rendering and NMI, waits for two
; vblanks, and finally enables the NMI.
.export ppu_reset
ppu_reset:
    lda #0
    sta PPU_CTRL ; disable NMI
    sta PPU_MASK ; disable rendering
    ; wait for first vblank
    bit PPU_STATUS
    :
        bit PPU_STATUS
        bpl :-
    ; place all sprites offscreen at Y=255
    lda #255
    ldx #0
    :
        sta ppu_oam_buffer, X
        inx
        inx
        inx
        inx
        bne :-
    ; wait for second vblank
    :
        bit PPU_STATUS
        bpl :-
    ; PPU is initialized
    ; enable the NMI for graphical updates
    lda #%10001000
    sta PPU_CTRL
    rts

;
; Generic buffer-copying, scrolling NMI routine
;
.export ppu_nmi_buffered
ppu_nmi_buffered:
    ; save registers
    pha
    txa
    pha
    tya
    pha
    ; prevent NMI re-entry
    lda ppu_nmi_lock
    beq :+
        jmp @nmi_end
    :
    lda #1
    sta ppu_nmi_lock
    ; increment frame counter
    inc ppu_nmi_count
    ;
    lda ppu_nmi_ready
    bne :+ ; ppu_nmi_ready == 0 not ready to update PPU
        jmp @ppu_update_end
    :
    cmp #2 ; ppu_nmi_ready == 2 turns rendering off
    bne :+
        lda #%00000000
        sta PPU_MASK
        ldx #0
        stx ppu_nmi_ready
        jmp @ppu_update_end
    :
    ; sprite OAM DMA
    ldx #0
    stx PPU_OAM_ADDR
    lda #>ppu_oam_buffer
    sta PPU_OAM_DMA
    ; palettes
    lda #%10001000
    sta PPU_CTRL ; set horizontal nametable increment
    lda PPU_STATUS
    lda #$3F
    sta PPU_ADDR
    stx PPU_ADDR ; set PPU address to $3F00
    ldx #0
    :
        lda ppu_palette_buffer, X
        sta PPU_DATA
        inx
        cpx #32
        bcc :-
    ; nametable update
    ldx #0
    cpx ppu_nmt_buffer_len
    bcs @scroll
    @nmt_update_loop:
        lda ppu_nmt_buffer, X
        sta PPU_ADDR
        inx
        lda ppu_nmt_buffer, X
        sta PPU_ADDR
        inx
        lda ppu_nmt_buffer, X
        sta PPU_DATA
        inx
        cpx ppu_nmt_buffer_len
        bcc @nmt_update_loop
    lda #0
    sta ppu_nmt_buffer_len
@scroll:
    lda ppu_scroll_nmt
    and #%00000011 ; keep only lowest 2 bits to prevent error
    ora #%10001000
    sta PPU_CTRL
    lda ppu_scroll_x
    sta PPU_SCROLL
    lda ppu_scroll_y
    sta PPU_SCROLL
    ; enable rendering
    lda #%00011110
    sta PPU_MASK
    ; flag PPU update complete
    ldx #0
    stx ppu_nmi_ready
@ppu_update_end:
    ; if this engine had music/sound, this would be a good place to play it
    ; unlock re-entry flag
    lda #0
    sta ppu_nmi_lock
@nmi_end:
    ; restore registers and return
    pla
    tay
    pla
    tax
    pla
    rti

;
; ppu_update: intended to be called once per logical game frame. Waits until
; next NMI, where the NMI handler, if set to ppu_nmi_buffered, will turn
; rendering on (if not already), upload OAM, palette, and nametable update to
; PPU.
;
.export ppu_update
ppu_update:
    lda #1
    sta ppu_nmi_ready
    :
        lda ppu_nmi_ready
        bne :-
    rts

; ppu_skip: waits until next NMI, ppu_nmi_buffered will not update PPU
.export ppu_skip
ppu_skip:
    lda ppu_nmi_count
    :
        cmp ppu_nmi_count
        beq :-
    rts

; ppu_off: waits until next NMI, ppu_numi_buffered will turn rendering off; now
; safe to write PPU directly via PPU_DATA after ppu_off returns.
.export ppu_off
ppu_off:
    lda #2
    sta ppu_nmi_ready
    :
        lda ppu_nmi_ready
        bne :-
    rts

; ppu_address_tile: use with rendering off, sets memory address to tile at X/Y, ready for a PPU_DATA write
;   Y =  0- 31 nametable $2000
;   Y = 32- 63 nametable $2400
;   Y = 64- 95 nametable $2800
;   Y = 96-127 nametable $2C00
.export ppu_address_tile
ppu_address_tile:
    lda PPU_STATUS ; reset latch
    tya
    lsr
    lsr
    lsr
    ora #$20 ; high bits of Y + $20
    sta PPU_ADDR
    tya
    asl
    asl
    asl
    asl
    asl
    sta _ppu_temp
    txa
    ora _ppu_temp
    sta PPU_ADDR ; low bits of Y + X
    rts

; ppu_update_tile: can be used with rendering on, sets the tile at X/Y to tile A next time you call ppu_update
.export ppu_update_tile
ppu_update_tile:
    pha ; temporarily store A on stack
    txa
    pha ; temporarily store X on stack
    ldx ppu_nmt_buffer_len
    tya
    lsr
    lsr
    lsr
    ora #$20 ; high bits of Y + $20
    sta ppu_nmt_buffer, X
    inx
    tya
    asl
    asl
    asl
    asl
    asl
    sta _ppu_temp
    pla ; recover X value (but put in A)
    ora _ppu_temp
    sta ppu_nmt_buffer, X
    inx
    pla ; recover A value (tile)
    sta ppu_nmt_buffer, X
    inx
    stx ppu_nmt_buffer_len
    rts

; ppu_update_byte: like ppu_update_tile, but X/Y makes the high/low bytes of the PPU address to write
;    this may be useful for updating attribute tiles
.export ppu_update_byte
ppu_update_byte:
    pha ; temporarily store A on stack
    tya
    pha ; temporarily store Y on stack
    ldy ppu_nmt_buffer_len
    txa
    sta ppu_nmt_buffer, Y
    iny
    pla ; recover Y value (but put in Y)
    sta ppu_nmt_buffer, Y
    iny
    pla ; recover A value (byte)
    sta ppu_nmt_buffer, Y
    iny
    sty ppu_nmt_buffer_len
    rts

;
; ppu_clear_background: clears all nametable tile entries and attributes to 0.
; Writes directly to PPU_DATA, so call when rendering is off.
;
.export ppu_clear_background
ppu_clear_background:
    ; first nametable, start by clearing to empty
    lda PPU_STATUS ; reset latch
    lda #$20
    sta PPU_ADDR
    lda #$00
    sta PPU_ADDR
    ; empty nametable
    lda #0
    ldy #30 ; 30 rows
    :
        ldx #32 ; 32 columns
        :
            sta PPU_DATA
            dex
            bne :-
        dey
        bne :--
    ; set all attributes to 0
    ldx #64 ; 64 bytes
    :
        sta PPU_DATA
        dex
        bne :-
    rts

