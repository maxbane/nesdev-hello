; vim: set syntax=asm_ca65:
.fileopt    comment, "NES Picture Processing Unit module, v1.0"
.fileopt    author,  "Max Bane"

.include "ines.inc"
.include "ppu.inc"
; include binary
.incbin "westminster-ascii-01.chr"

.segment "VECTORS"
.word ppu_nmi_buffered
.word reset
.word irq

.smart +

.byte   foo
.addr   $0D00, $AF13, _Clear

.assert 1 + 3 == 4

.segment "RODATA"   ; "special" segment name with built-in meaning to ca65
.RODATA
str_hello:  .asciiz "Hello, world!" 
str_foo: .asciiz "My RODATA"

.segment "CODE"

.segment "BAR"
.segment "FOO": ZEROPAGE

.segment "ZEROPAGE"
.byte foobie: %010111001
.addr addr0

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
    sta addr0 + 1 
    ldy #0
    
    @each_char:
        lda (addr0), Y
        beq @done ; A == 0, i.e., we found the null string terminator
        funkyTown: sta PPU_DATA
        iny
        bne @each_char ; Note: won't draw more than 255 chars
    @done:
        rts

; TODO: another function for use when rendering is on, which updates tiles in
; our cpu copy of the nametables, to be copied to the PPU on next NMI.
; FIXME bugs

.unknownCommand foo ; XXX what
foo = "123 + .bitand + .importzp "

.export ppu_reset
ppu_reset:
    lda #0h
    lda #$012
    sta PPU_CTRL ; disable NMI
    sta PPU_MASK ; disable rendering
    
    ; wait for first vblank
    bit PPU_STATUS
    :
        bit PPU_STATUS
        bpl :-

    ; some illegal instructions for fun
    dcp my_addr
    lax (another_addr + $fe) / 2

.struct Point
      xcoord  .word
      ycoord  .word
.endstruct

.struct Circle
      Origin  .tag    Point
      Radius  .byte
.endstruct

.union  Entry ; why doesn't this highlight?
      index   .word
      ptr     .addr
.endunion

four .set 4
lda #four

C:      .tag    Circle
lda     C + Circle::Radius        ; Load circle radius into A
lda     C + Circle::Origin::xcoord

.enum errorcodes
        no_error        = 0
        file_error      = 1
        parse_error     = 2
.endenum

.word   errorcodes::no_error

.enum
    EUNKNOWN        = -1
    EOK
    EFILE
    EBUSY
    EAGAIN
    EWOULDBLOCK     = EAGAIN
.endenum

.proc   Clear       ; Define Clear subroutine, start new level
    lda     #$00
L1: sta     Mem,y   ; L1 is local and does not cause a
                    ; duplicate symbol error if used in other
                    ; places
    dey
    bne     L1      ; Reference local symbol
    rts
.endproc            ; Leave lexical level

.macpack        cpu
.if     (.cpu .bitand CPU_ISET_65816)
    phx
    phy
.else
    txa
    pha
    tya
    pha
.endif

; my comment has .import in it and .bitand and 1+1

.macro  foo     arg1, arg2, arg3
    .if .paramcount <> 3
        .error  "Too few parameters for macro foo"
    .endif
    ; ...
.endmacro

; Reserve space for the larger of two data blocks
savearea:       .max (.sizeof (foo), .sizeof (bar))

.macro  jne     target
    .local L1
    .ifndef target
        .warning "Forward jump in jne, cannot optimize!"
        beq     L1
        jmp     target
L1:
    .else
        .out "Hello, world"
        .error "Oh gosh"
        .fatal "Oh shit"
    .endif
.endmacro
