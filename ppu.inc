; PPU Register Address Constants
.import PPU_CTRL
.import PPU_MASK
.import PPU_STATUS
.import PPU_OAM_ADDR
.import PPU_OAM_DATA
.import PPU_OAM_DMA
.import PPU_SCROLL
.import PPU_ADDR
.import PPU_DATA

; Buffer addresses
.import ppu_nmt_buffer
.import ppu_palette_buffer
.import ppu_oam_buffer

; Zeropage variables used by PPU routines
.importzp ppu_nmi_lock
.importzp ppu_nmi_count
.importzp ppu_nmi_ready
.importzp ppu_nmt_buffer_len
.importzp ppu_scroll_x, ppu_scroll_y
.importzp ppu_scroll_nmt

; PPU Routines
.import ppu_reset
.import ppu_nmi_buffered
.import ppu_update
.import ppu_skip
.import ppu_off
.import ppu_address_tile
.import ppu_update_tile
.import ppu_update_byte
.import ppu_clear_background
