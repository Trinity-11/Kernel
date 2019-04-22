; page_00.asm
; Direct Page Addresses
;
;* Addresses are the byte AFTER the block. Use this to confirm block locations and check for overlaps
BANK0_BEGIN      = $000000 ;Start of bank 0 and Direct page
unused_0000      = $000000 ;12 Bytes unused
SCREENBEGIN      = $00000C ;3 Bytes Start of screen in video RAM. This is the upper-left corrner of the current video page being written to. This may not be what's being displayed by VICKY. Update this if you change VICKY's display page.
COLS_VISIBLE     = $00000F ;2 Bytes Columns visible per screen line. A virtual line can be longer than displayed, up to COLS_PER_LINE long. Default = 80
COLS_PER_LINE    = $000011 ;2 Bytes Columns in memory per screen line. A virtual line can be this long. Default=128
LINES_VISIBLE    = $000013 ;2 Bytes The number of rows visible on the screen. Default=25
LINES_MAX        = $000015 ;2 Bytes The number of rows in memory for the screen. Default=64
CURSORPOS        = $000017 ;3 Bytes The next character written to the screen will be written in this location.
CURSORX          = $00001A ;2 Bytes This is where the blinking cursor sits. Do not edit this direectly. Call LOCATE to update the location and handle moving the cursor correctly.
CURSORY          = $00001C ;2 Bytes This is where the blinking cursor sits. Do not edit this direectly. Call LOCATE to update the location and handle moving the cursor correctly.
CURCOLOR         = $00001E ;2 Bytes Color of next character to be printed to the screen.
CURATTR          = $000020 ;2 Bytes Attribute of next character to be printed to the screen.
STACKBOT         = $000022 ;2 Bytes Lowest location the stack should be allowed to write to. If SP falls below this value, the runtime should generate STACK OVERFLOW error and abort.
STACKTOP         = $000024 ;2 Bytes Highest location the stack can occupy. If SP goes above this value, the runtime should generate STACK OVERFLOW error and abort.
; OPL2 Library Variable (Can be shared if Library is not used)
OPL2_OPERATOR    = $000026 ;
OPL2_CHANNEL     = $000027 ;
OPL2_REG_REGION  = $000028 ; Offset to the Group of Registers
OPL2_REG_OFFSET  = $00002A ; 2 Bytes (16Bits)
OPL2_IND_ADDY_LL = $00002C ; 2 Bytes Reserved (Only need 3)
OPL2_IND_ADDY_HL = $00002E ; 2 Bytes Reserved (Only need 3)
OPL2_NOTE        = $000030 ; 1 Byte
OPL2_OCTAVE      = $000031 ; 1 Byte
OPL2_PARAMETER0  = $000032 ; 1 Byte
OPL2_PARAMETER1  = $000033 ; 1 Byte
OPL2_PARAMETER2  = $000034 ; 1 Byte
OPL2_PARAMETER3  = $000035 ; 1 Byte
OPL2_LOOP        = $000036 ;
; SD Card (CH376S) Variables
SDCARD_FILE_PTR  = $000038 ; 3 Bytes Pointer to Filename to open
SDCARD_BYTE_NUM  = $00003C ; 2Bytes
SDCARD_PRSNT_MNT = $00003F ; 1 Byte, Indicate that the SDCard is Present and that it is Mounted
; BMP File Parser Variables (Can be shared if BMP Parser not used)
BMP_X_SIZE       = $000040 ; 2 Bytes
BM_CLEAR_SCRN_X  = $000040
BMP_Y_SIZE       = $000042 ; 2 Bytes
BM_CLEAR_SCRN_Y  = $000042
BMP_PRSE_SRC_PTR = $000044 ; 3 Bytes
BMP_PRSE_DST_PTR = $000048 ; 3 Bytes
BMP_COLOR_PALET  = $00004C ; 2 Bytes
SCRN_X_STRIDE    = $00004E ; 2 Bytes, Basically How many Pixel Accross in Bitmap Mode
BMP_FILE_SIZE    = $000050 ; 4 Bytes
BMP_POSITION_X   = $000054 ; 2 Bytes Where, the BMP will be position on the X Axis
BMP_POSITION_Y   = $000056 ; 2 Bytes Where, the BMP will be position on the Y Axis
BMP_PALET_CHOICE = $000058 ;
; Not Defined Yet
KERNEL_TEMP      = $0000D0 ;32 Bytes Temp space for kernel
USER_TEMP        = $0000F0 ;32 Bytes Temp space for user programs

GAVIN_BLOCK      = $000100 ;256 Bytes Gavin reserved, overlaps debugging registers at $1F0

MULTIPLIER_0     = $000100 ;0 Byte  Unsigned multiplier
M0_OPERAND_A     = $000100 ;2 Bytes Operand A (ie: A x B)
M0_OPERAND_B     = $000102 ;2 Bytes Operand B (ie: A x B)
M0_RESULT        = $000104 ;4 Bytes Result of A x B

MULTIPLIER_1     = $000108 ;0 Byte  Signed Multiplier
M1_OPERAND_A     = $000108 ;2 Bytes Operand A (ie: A x B)
M1_OPERAND_B     = $00010A ;2 Bytes Operand B (ie: A x B)
M1_RESULT        = $00010C ;4 Bytes Result of A x B

DIVIDER_0        = $000108 ;0 Byte  Unsigned divider
D0_OPERAND_A     = $000108 ;2 Bytes Divider 0 Dividend ex: A in  A/B
D0_OPERAND_B     = $00010A ;2 Bytes Divider 0 Divisor ex B in A/B
D0_RESULT        = $00010C ;2 Bytes Quotient result of A/B ex: 7/2 = 3 r 1
D0_REMAINDER     = $00010E ;2 Bytes Remainder of A/B ex: 1 in 7/2=3 r 1

DIVIDER_1        = $000110 ;0 Byte  Signed divider
D1_OPERAND_A     = $000110 ;2 Bytes Divider 1 Dividend ex: A in  A/B
D1_OPERAND_B     = $000112 ;2 Bytes Divider 1 Divisor ex B in A/B
D1_RESULT        = $000114 ;2 Bytes Signed quotient result of A/B ex: 7/2 = 3 r 1
D1_REMAINDER     = $000116 ;2 Bytes Signed remainder of A/B ex: 1 in 7/2=3 r 1
; Reserved
ADDER_SIGNED_32  = $000120 ; The 32 Bit Adders takes 12Byte that are NOT RAM Location
; Reserved
INT_CONTROLLER   = $000140 ; $000140...$00015F Interrupt Controller

; Interrupt Bit Definition
; Register Block 0
FNX0_INT00_ALLONE = $01  ; Not Used - Always 1
FNX0_INT01_SOF    = $02  ;Start of Frame @ 60FPS
FNX0_INT02_SOL    = $04  ;Start of Line (Programmable)
FNX0_INT03_TMR0   = $08  ;Timer 0 Interrupt
FNX0_INT04_TMR1   = $10  ;Timer 1 Interrupt
FNX0_INT05_TMR2   = $20  ;Timer 2 Interrupt
FNX0_INT06_RTC    = $40  ;Real-Time Clock Interrupt
FNX0_INT07_FDC    = $80  ;Floppy Disk Controller
; Register Block 1
FNX1_INT00_KBD    = $01  ;Keyboard Interrupt
FNX1_INT01_SC0    = $02  ;Sprite 2 Sprite Collision
FNX1_INT02_SC1    = $04  ;Sprite 2 Tiles Collision
FNX1_INT03_COM2   = $08  ;Serial Port 2
FNX1_INT04_COM1   = $10  ;Serial Port 1
FNX1_INT05_MPU401 = $20  ;Midi Controller Interrupt
FNX1_INT06_LPT    = $40  ;Parallel Port
FNX1_INT07_SDCARD = $80  ;SD Card Controller Interrupt
; Register Block 2
FNX2_INT00_OPL2R  = $01  ;OPl2 Right Channel
FNX2_INT01_OPL2L  = $02  ;OPL2 Left Channel
FNX2_INT02_BTX_INT= $04  ;Beatrix Interrupt (TBD)
FNX2_INT03_SDMA   = $08  ;System DMA
FNX2_INT04_VDMA   = $10  ;Video DMA
FNX2_INT05_DACHP  = $20  ;DAC Hot Plug
FNX2_INT06_EXT    = $40  ;External Expansion
FNX2_INT07_ALLONE = $80  ; Not Used - Always 1

TIMER_CONTROLLER = $000160 ; $000160...$00017F Timer0/Timer1/Timer2 Block
TIMER_CTRL_REGLL = $000160 ;
TIMER_CTRL_REGLH = $000161 ;
TIMER_CTRL_REGHL = $000162 ;
TIMER_CTRL_REGHH = $000163 ;
VECTOR_STATE     = $0001FF ;1 Byte  Interrupt Vector State. See VECTOR_STATE_ENUM

CPU_REGISTERS    = $000240 ; Byte
CPUPC            = $000240 ;2 Bytes Program Counter (PC)
CPUPBR           = $000242 ;2 Bytes Program Bank Register (K)
CPUA             = $000244 ;2 Bytes Accumulator (A)
CPUX             = $000246 ;2 Bytes X Register (X)
CPUY             = $000248 ;2 Bytes Y Register (Y)
CPUSTACK         = $00024A ;2 Bytes Stack Pointer (S)
CPUDP            = $00024C ;2 Bytes Direct Page Register (D)
CPUDBR           = $00024E ;1 Byte  Data Bank Register (B)
CPUFLAGS         = $00024F ;1 Byte  Flags (P)

MONITOR_VARS     = $000250 ; Byte  MONITOR Variables. BASIC variables may overlap this space
MCMDADDR         = $000250 ;3 Bytes Address of the current line of text being processed by the command parser. Can be in display memory or a variable in memory. MONITOR will parse up to MTEXTLEN characters or to a null character.
MCMP_TEXT        = $000253 ;3 Bytes Address of symbol being evaluated for COMPARE routine
MCMP_LEN         = $000256 ;2 Bytes Length of symbol being evaluated for COMPARE routine
MCMD             = $000258 ;3 Bytes Address of the current command/function string
MCMD_LEN         = $00025B ;2 Bytes Length of the current command/function string
MARG1            = $00025D ;4 Bytes First command argument. May be data or address, depending on command
MARG2            = $000261 ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.
MARG3            = $000265 ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.
MARG4            = $000269 ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.
MARG5            = $00026D ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.
MARG6            = $000271 ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.
MARG7            = $000275 ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.
MARG8            = $000279 ;4 Bytes First command argument. May be data or address, depending on command. Data is 32-bit number. Address is 24-bit address and 8-bit length.

LOADFILE_VARS    = $000300 ; Byte
LOADFILE_NAME    = $000300 ;3 Bytes (addr) Name of file to load. Address in Data Page
LOADFILE_LEN     = $000303 ;1 Byte  Length of filename. 0=Null Terminated
LOADPBR          = $000304 ;1 Byte  First Program Bank of loaded file ($05 segment)
LOADPC           = $000305 ;2 Bytes Start address of loaded file ($05 segment)
LOADDBR          = $000307 ;1 Byte  First data bank of loaded file ($06 segment)
LOADADDR         = $000308 ;2 Bytes FIrst data address of loaded file ($06 segment)
LOADFILE_TYPE    = $00030A ;3 Bytes (addr) File type string in loaded data file. Actual string data will be in Bank 1. Valid values are BIN, PRG, P16
BLOCK_LEN        = $00030D ;2 Bytes Length of block being loaded
BLOCK_ADDR       = $00030F ;2 Bytes (temp) Address of block being loaded
BLOCK_BANK       = $000311 ;1 Byte  (temp) Bank of block being loaded
BLOCK_COUNT      = $000312 ;2 Bytes (temp) Counter of bytes read as file is loaded

; $00:0320 to $00:06FF - Reserved for CH376S SDCard Controller
SDOS_BLOCK_BEGIN = $000320 ;
SDOS_LOAD_ADDY   = $000324 ; 4 Bytes (Uses 3 Only)
SDOS_FILE_SIZE   = $000328 ;
SDOS_BYTE_NUMBER = $00032C ; Number of Byte to Read or Write before changing the Pointer
SDOS_REG_WR32_AD = $000330 ; 4 Bytes (Used to read and Write Values in/from CH376S)
SDOS_BYTE_PTR    = $000334
SDOS_FILE_NAME   = $000380 ; // Max of 128 Chars
SDOS_BLK_BEGIN   = $000400 ; 512 Bytes to Store SD Card Incoming or Outcoming Block
SDOS_BLK_END     = $0006FF ;

STEF_BLOB_BEGIN  = $000E00 ; Temp Buffer for Testing
STEF_BLOB_END    = $000EFF ;

KEY_BUFFER       = $000F00 ;64 Bytes keyboard buffer
KEY_BUFFER_SIZE  = $40 ;64 Bytes (constant) keyboard buffer length
KEY_BUFFER_END   = $000F3F ;1 Byte  Last byte of keyboard buffer
KEY_BUFFER_RPOS  = $000F40 ;2 Bytes keyboard buffer read position
KEY_BUFFER_WPOS  = $000F42 ;2 Bytes keyboard buffer write position
KEYBOARD_SC_FLG  = $000F43 ;1 Bytes that indicate the Status of Left Shift, Left CTRL, Left ALT, Right Shift
KEYBOARD_SC_TMP  = $000F44 ;1 Byte, Interrupt Save Scan Code while Processing
TEST_BEGIN       = $001000 ;28672 Bytes Test/diagnostic code for prototype.
TEST_END         = $007FFF ;0 Byte

STACK_BEGIN      = $008000 ;32512 Bytes The default beginning of stack space
STACK_END        = $00FEFF ;0 Byte  End of stack space. Everything below this is I/O space

ISR_BEGIN        = $18FF00 ; Byte  Beginning of CPU vectors in Direct page
HRESET           = $18FF00 ;16 Bytes Handle RESET asserted. Reboot computer and re-initialize the kernel.
HCOP             = $18FF10 ;16 Bytes Handle the COP instruction. Program use; not used by OS
HBRK             = $18FF20 ;16 Bytes Handle the BRK instruction. Returns to BASIC Ready prompt.
HABORT           = $18FF30 ;16 Bytes Handle ABORT asserted. Return to Ready prompt with an error message.
HNMI             = $18FF40 ;32 Bytes Handle NMI
HIRQ             = $18FF60 ;32 Bytes Handle IRQ
Unused_FF80      = $18FF80 ;End of direct page Interrrupt handlers

VECTORS_BEGIN    = $18FFE0 ;0 Byte  Interrupt vectors
JMP_READY        = $00FFE0 ;4 Bytes Jumps to ROM READY routine. Modified whenever alternate command interpreter is loaded.
VECTOR_COP       = $00FFE4 ;2 Bytes Native COP Interrupt vector
VECTOR_BRK       = $00FFE6 ;2 Bytes Native BRK Interrupt vector
VECTOR_ABORT     = $00FFE8 ;2 Bytes Native ABORT Interrupt vector
VECTOR_NMI       = $00FFEA ;2 Bytes Native NMI Interrupt vector
VECTOR_RESET     = $00FFEC ;2 Bytes Unused (Native RESET vector)
VECTOR_IRQ       = $00FFEE ;2 Bytes Native IRQ Vector
RETURN           = $00FFF0 ;4 Bytes RETURN key handler. Points to BASIC or MONITOR subroutine to execute when RETURN is pressed.
VECTOR_ECOP      = $00FFF4 ;2 Bytes Emulation mode interrupt handler
VECTOR_EBRK      = $00FFF6 ;2 Bytes Emulation mode interrupt handler
VECTOR_EABORT    = $00FFF8 ;2 Bytes Emulation mode interrupt handler
VECTOR_ENMI      = $00FFFA ;2 Bytes Emulation mode interrupt handler
VECTOR_ERESET    = $00FFFC ;2 Bytes Emulation mode interrupt handler
VECTOR_EIRQ      = $00FFFE ;2 Bytes Emulation mode interrupt handler
VECTORS_END      = $200000 ;*End of vector space
BANK0_END        = $00FFFF ;End of Bank 00 and Direct page
;
