; NES Gamepad module
; Useful constants and subroutines
; TODO: support multiple gamepads, efficiently (macros?)

.export JOY_PAD0_ADDR = $4016
.export JOY_PAD1_ADDR = $4017

.export JOY_A      = $01
.export JOY_B      = $02
.export JOY_SELECT = $04
.export JOY_START  = $08
.export JOY_UP     = $10
.export JOY_DOWN   = $20
.export JOY_LEFT   = $40
.export JOY_RIGHT  = $80

.segment "ZEROPAGE"
joy_pad0: .res 1
.exportzp joy_pad0

.segment "CODE"
; joy_poll: this reads the gamepad state into the variable labelled "gamepad"
;   This only reads the first gamepad, and also if DPCM samples are played they can
;   conflict with gamepad reading, which may give incorrect results.
.export joy_poll
joy_poll:
    ; strobe the gamepad to latch current button state
    lda #1
    sta $4016
    lda #0
    sta $4016
    ; read 8 bytes from the interface at $4016
    ldx #8
    :
        pha
        lda $4016
        ; combine low two bits and store in carry bit
        and #%00000011
        cmp #%00000001
        pla
        ; rotate carry into gamepad variable
        ror
        dex
        bne :-
    sta joy_pad0
    rts

