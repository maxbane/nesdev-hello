; imports

.include "ines.inc"
.include "ppu.inc"

;
; iNES header
;

.segment "HEADER"

INES_PRG_BANK_COUNT = 2 ; 16k PRG bank count
INES_CHR_BANK_COUNT = 1 ; 8k CHR bank count
INES_MAPPER = 0         ; 0 = NROM (iNES standard mapper number)
INES_MIRROR = 1         ; 0 = horizontal mirroring, 1 = vertical mirroring
INES_SRAM   = 0         ; 1 = battery backed SRAM at $6000-7FFF

; INES_HEADER macro constructs the header bytes given arguments
INES_HEADER INES_PRG_BANK_COUNT, INES_CHR_BANK_COUNT, INES_MAPPER, INES_MIRROR, INES_SRAM

;
; CHR ROM
;

.segment "TILES"
.incbin "westminster-ascii.chr"

;
; interrupt vectors 
;

.segment "VECTORS"
.word ppu_nmi_buffered
.word reset
.word irq

;
; reset routine
;

.segment "CODE"
reset:
    sei       ; mask interrupts
    cld       ; disable decimal mode

    lda #0
    sta $4015 ; disable APU sound
    sta $4010 ; disable DMC IRQ
    lda #$40
    sta $4017 ; disable APU IRQ

    ldx #$FF
    txs       ; initialize stack

    ; clear all RAM to 0 (except $100 stack area)
    lda #0
    ldx #0
    :
        sta $0000, X
        sta $0200, X
        sta $0300, X
        sta $0400, X
        sta $0500, X
        sta $0600, X
        sta $0700, X
        inx
        bne :-

    jsr ppu_reset
    jmp main

;
; irq
;

.segment "CODE"
irq:
    rti

;
; main
;

.segment "RODATA"
example_palette:
.byte $0F,$15,$26,$37 ; bg0 purple/pink
.byte $0F,$09,$19,$29 ; bg1 green
.byte $0F,$01,$11,$21 ; bg2 blue
.byte $0F,$00,$10,$30 ; bg3 greyscale
.byte $0F,$18,$28,$38 ; sp0 yellow
.byte $0F,$14,$24,$34 ; sp1 purple
.byte $0F,$1B,$2B,$3B ; sp2 teal
.byte $0F,$12,$22,$32 ; sp3 marine

.segment "ZEROPAGE"
; general purpose zp locations at which to store addresses for indirect
; referencing
addr0:     .res 2

.segment "RODATA"
str_hello:  .asciiz "Hello, world!" 

.segment "CODE"

; string drawing functions

draw_str_to_addressed_row:
    ; Draw characters of AY-pointed, null-terminated ascii string characters to
    ; row of nametable, starting at tile currently addressed; rendering should
    ; be off. Assumes tile number of a character glyph equals the ASCII value
    ; of the character.
    ; Performs no nametable bounds checking! If your string is too long,
    ; weird things may happen.
    sta addr0
    tya
    sta addr0+1
    ldy #0
    @each_char:
        lda (addr0), Y
        beq @done ; A == 0, i.e., we found the null string terminator
        sta PPU_DATA
        iny
        bne @each_char ; Note: won't draw more than 255 chars
    @done:
        rts

; TODO: another function for use when rendering is on, which updates tiles in
; our cpu copy of the nametables, to be copied to the PPU on next NMI.


main:
    ; setup 
    ldx #0
    :
        lda example_palette, X
        sta ppu_palette_buffer, X
        inx
        cpx #32
        bcc :-

    jsr ppu_clear_background

    ; address a tile toward the middle of the screen
    ldy #15
    ldx #9
    jsr ppu_address_tile

    ; draw row of characters starting at addressed tile
    lda #<str_hello
    ldy #>str_hello
    jsr draw_str_to_addressed_row

    ; enable nmi-handler
    jsr ppu_update
    ; loop forever
    @infiniloop:
        jmp @infiniloop


