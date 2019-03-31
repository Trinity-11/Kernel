.cpu "65816"
.include "macros_inc.asm"
.include "simulator_inc.asm"
.include "page_00_inc.asm"
.include "page_00_data.asm"
.include "page_00_code.asm"
.include "dram_inc.asm"
.include "vicky_def.asm"
.include "super_io_def.asm"
.include "keyboard_def.asm"
.include "SID_def.asm"
.include "RTC_def.asm"
.include "Math_def.asm"
.include "io_def.asm"
.include "monitor.asm"
.include "SDOS.asm"
.include "OPL2_Library.asm"
; C256 Foenix / Nu64 Kernel
; Loads to $F0:0000

;Kernel.asm
;Jump Table

.include "kernel_jumptable.asm"

* = $190400

IBOOT           ; boot the system
                CLC           ; clear the carry flag
                XCE           ; move carry to emulation flag.
                SEI
                setaxl
                LDA #STACK_END   ; initialize stack pointer
                TAS
                setdp 0
                setas
                LDX #$0000
                LDA #$00
CLEAR_MEM_LOOP
                STA $0000, X
                INX
                CPX #$0400
                BNE CLEAR_MEM_LOOP
                NOP
                ; Setup the Interrupt Controller
                ; For Now all Interrupt are Falling Edge Detection (IRQ)
                LDA #$FF
                STA @lINT_EDGE_REG0
                STA @lINT_EDGE_REG1
                STA @lINT_EDGE_REG2
                ; Mask all Interrupt @ This Point
                STA @lINT_MASK_REG0
                STA @lINT_MASK_REG1
                STA @lINT_MASK_REG2

                setaxl
                LDA #<>SCREEN_PAGE0      ; store the initial screen buffer location
                STA SCREENBEGIN
                setas
                LDA #`SCREEN_PAGE0
                STA SCREENBEGIN+2
                setaxl
                LDA #<>SCREEN_PAGE0      ; store the initial screen buffer location
                STA CURSORPOS
                setas
                LDA #`SCREEN_PAGE0
                STA CURSORPOS+2

                LDA #$00
                STA KEYBOARD_SC_FLG     ; Clear the Keyboard Flag

                ; Set screen dimensions. There more columns in memory than
                ; are visible. A virtual line is 128 bytes, but 80 columns will be
                ; visible on screen.
                LDX #72
                STX COLS_VISIBLE
                LDY #56
                STY LINES_VISIBLE
                LDX #128
                STX COLS_PER_LINE
                LDY #64
                STY LINES_MAX
;                LDA #$05 ; TOTAL Reset of CH376S
;                STA SDCARD_CMD;
                ;JSL RESETCODEC
                setas
                LDA JOYSTICK0
                STA @lSTEF_BLOB_BEGIN
                LDA JOYSTICK1
                STA @lSTEF_BLOB_BEGIN+1
                LDA JOYSTICK2
                STA @lSTEF_BLOB_BEGIN+2
                LDA JOYSTICK3
                STA @lSTEF_BLOB_BEGIN+3
                LDA DIPSWITCH
                STA @lSTEF_BLOB_BEGIN+4
;                LDA #$01 ; GET IC CH376S Version
;                STA SDCARD_CMD;
                setaxl
                ; Initialize Super IO Chip
                ;JSL INITCODEC
                ;LDA SDCARD_DATA;

                ;JSL INITSUPERIO

                ; Init the RTC (Test the Interface)
                ;JSL INITRTC
                ; INIT The FONT Memory

                ; Init Globacl Look-up Table
                JSL INITGAMMATABLE

                ; Init All the Graphic Mode Look-up Table
                JSL INITALLLUT

                ; Initialize the Character Color Foreground/Background LUT First
                JSL INITCHLUT

                ; Go Enable and Setup the Cursor's Position
                JSL INITCURSOR

                ; Init the Basic Value for the Graphic Mode
                JSL INITVKYGRPMODE

                ; Init the Vicky Text MODE
                JSL INITVKYTXTMODE
                JSL INITTILEMODE  ; This is to Test Tiles`

                ; Now, clear the screen and Setup Foreground/Background Bytes, so we can see the Text on screen
                JSL ICLRSCREEN  ; Clear Screen and Set a standard color in Color Memory
                JSL ICOLORFLAG

                setal
                ; Write the Greeting Message Here, after Screen Cleared and Colored
greet           setdbr `greet_msg       ;Set data bank to ROM
                LDX #<>greet_msg
                JSL IPRINT       ; print the first line
                 ; Let's Change the Color Memory For the Logo
                LDX #<>version_msg
                JSL IPRINT       ; print the first line
                LDX #<>init_rtc_msg
                JSL IPRINT       ; print the RTC Init Message
                LDX #<>init_lpc_msg
                JSL IPRINT       ; print the Init
                setdp 0
                JSL ITESTMATH
                ; Init KeyBoard
                LDX #<>init_kbrd_msg
                JSL IPRINT       ; print the Keybaord Init Message

                ;
                ; Test the disassembler
                ;
                setal
                LDA #<>SAMPLE
                STA MARG1
                setas
                LDA #`SAMPLE
                STA MARG1+2

                setal
                LDA #<>SAMPLE + 32
                STA MARG2
                setas
                LDA #`SAMPLE
                STA MARG2+2

                setas
                LDA #2
                STA MARG_LEN

                LDA #$00
                STA @lMCPUSTAT    ; Clear the M and X bits in the assembler's processor status flags

                JSL DISASSEMBLE
LOCK            JMP LOCK

                ;JSL INITKEYBOARD ;
                ;JSL ITESTSID

                setaxl
                LDX #<>OPL2_test_msg
                JSL IPRINT       ; print the first line
                ;JSL OPL2_TONE_TEST

                JSL SDOS_INIT;       // Go Init the CH376S in SDCARD mod
                JSL SDOS_DIR

                setaxs
                LDX #$00
                ; Transfer the name in the Right location
COPYFILENAME;
                LDA @lsplashfilename, X
                STA @lSDOS_FILE_NAME, X
                INX
                CMP #$00
                BNE COPYFILENAME;
                ;; Load the File @ $10:0000  (128K Boundary)
                LDA #$00
                STA SDCARD_FILE_PTR
                STA BMP_PRSE_SRC_PTR
                STA SDCARD_FILE_PTR+1
                STA BMP_PRSE_SRC_PTR+1
                STA SDCARD_FILE_PTR+3
                STA BMP_PRSE_SRC_PTR+3
                LDA #$10
                STA SDCARD_FILE_PTR+2
                STA BMP_PRSE_SRC_PTR+2
                JSL SDOS_LOAD;  Go Load the BMP File for the Splash Screen

                setas
                LDA #~Mstr_Ctrl_Text_Mode_En     ;Okay, this Enables the Text Mode (Video Display)
                STA MASTER_CTRL_REG_L
                ; Go Fill the Screen with Zeros
                setal
                LDA #640
                STA BM_CLEAR_SCRN_X
                LDA #480
                STA BM_CLEAR_SCRN_Y
                LDA #$C000
                STA BMP_PRSE_DST_PTR
                LDA #$00B0
                STA BMP_PRSE_DST_PTR+2
                JSL BM_FILL_SCREEN

                LDA #$0280
                STA SCRN_X_STRIDE
                ;; Let's put the image in the middle of the Screen
                LDA #$00AA
                STA BMP_POSITION_X
                LDA #$005A
                STA BMP_POSITION_Y
                setas
                setxl
                LDA #$00
                STA BMP_PRSE_DST_PTR
                STA BMP_PRSE_DST_PTR+3
                LDA #$C0
                STA BMP_PRSE_DST_PTR+1
                LDA #$B0
                STA BMP_PRSE_DST_PTR+2


                LDX #<>bmp_parser_msg1
                JSL IPRINT       ; print the first line
                ; Now that the
                LDX #$0000 ; Load the File LUT in LUT0
                JSL BMP_PARSER

                setas
                LDA #$01          ; Enable Bit-Map and uses LUT0
                STA @lBM_CONTROL_REG
MAIN_NOTLOADED

;
; Load the Tile Set File for the Demo
;
                LDX #<>tile_msg1
                JSL IPRINT       ; print the first line
                setxl
                setas
                LDX #$0000
TRANSFER_GRAPH
                LDA @lTILE0_MAP,X
                STA @l$B80000,X
                INX
                CPX #$0000
                BNE TRANSFER_GRAPH
                NOP
                LDX #$0000
;
;Load the Tile Pattern in the Tile memory
; 29 x 64
TRANSFER_TILEMAP0
                LDA @lTILE_MAP_CONTENT0,X
                STA @lTILE_MAP0,X
                INX
                CPX #(29*64)
                BNE TRANSFER_TILEMAP0
                NOP
                LDX #$0000
;
TRANSFER_TILEMAP1
                LDA #$B3
                STA @lTILE_MAP1,X
                INX
                CPX #(11*64)
                BNE TRANSFER_TILEMAP1
                NOP
                LDX #$0000

TRANSFER_SPRITES ;
                LDA @lJUMPMAN_SPRITES,X
                STA @l$B90000,X
                INX
                CPX #$4400
                BNE TRANSFER_SPRITES
                NOP

                LDX #$0000
TRANSFER_GRAPH_LUT
                LDA @lTILE0_PALETTE,X
                STA @lGRPH_LUT1_PTR,X
                INX
                CPX #$0400
                BNE TRANSFER_GRAPH_LUT
                NOP

                setaxs
                ;First Setup Tile Layer 0
                LDA #$83    ; Enable The Layer bit[0], and we choose LUT1 bit[3:1] and bit[7] = 256x256 Tile Page stride
                STA @lTL0_CONTROL_REG
                LDA #$00    ; Set the Starting Address of Graphic Map Location
                STA @lTL0_START_ADDY_L
                LDA #$00
                STA @lTL0_START_ADDY_M
                LDA #$08    ; $B8:0000 (in graphic Mem) = $B0:6000 in Absolute Value
                STA @lTL0_START_ADDY_H

                LDA #$00    ; Set the X Stride to 256 (not implemented Yet)
                STA @lTL0_MAP_X_STRIDE_L
                LDA #$01
                STA @lTL0_MAP_X_STRIDE_H
                LDA #$00    ; Set the Y Stride to 256 (not implemented Yet)
                STA @lTL0_MAP_Y_STRIDE_L
                LDA #$01
                STA @lTL0_MAP_Y_STRIDE_H

                ;First Setup Tile Layer 1
                LDA #$83    ; Enable The Layer bit[0], and we choose LUT1 bit[3:1] and bit[7] = 256x256 Tile Page stride
                STA @lTL1_CONTROL_REG
                LDA #$00    ; Set the Starting Address of Graphic Map Location
                STA @lTL1_START_ADDY_L
                LDA #$00
                STA @lTL1_START_ADDY_M
                LDA #$08    ; $B8:0000 (in graphic Mem) = $B0:6000 in Absolute Value
                STA @lTL1_START_ADDY_H

                LDA #$00    ; Set the X Stride to 256 (not implemented Yet)
                STA @lTL1_MAP_X_STRIDE_L
                LDA #$01
                STA @lTL1_MAP_X_STRIDE_H
                LDA #$00    ; Set the Y Stride to 256 (not implemented Yet)
                STA @lTL1_MAP_Y_STRIDE_L
                LDA #$01
                STA @lTL1_MAP_Y_STRIDE_H


                ; Install the Sprites
                JSL INITSPRITE ; This is to test Sprites

                setaxl
                LDA #64+(0*32)
                STA SP00_X_POS_L
                LDA #64+(1*32)-8
                STA SP01_X_POS_L
                LDA #64+(2*32)-16
                STA SP02_X_POS_L
                LDA #64+(3*32)-24
                STA SP03_X_POS_L
                LDA #64+(4*32)-32
                STA SP04_X_POS_L
                LDA #64+(5*32)-40
                STA SP05_X_POS_L
                LDA #64+(6*32)-48
                STA SP06_X_POS_L

                LDA #80+(0*32)
                STA SP07_X_POS_L
                LDA #80+(1*32)-8
                STA SP08_X_POS_L
                LDA #80+(2*32)-16
                STA SP09_X_POS_L
                LDA #80+(3*32)-24
                STA SP10_X_POS_L
                LDA #80+(4*32)-32
                STA SP11_X_POS_L
                LDA #80+(5*32)-40
                STA SP12_X_POS_L

                LDA #64+(0*32)
                STA SP13_X_POS_L
                LDA #64+(1*32)
                STA SP14_X_POS_L
                LDA #64+(2*32)
                STA SP15_X_POS_L
                LDA #64+(3*32)
                STA SP16_X_POS_L

                LDA #64
                STA SP00_Y_POS_L  ;J
                STA SP01_Y_POS_L  ;u
                STA SP02_Y_POS_L  ;m
                STA SP03_Y_POS_L  ;p
                STA SP04_Y_POS_L  ;m
                STA SP05_Y_POS_L  ;a
                STA SP06_Y_POS_L  ;n
                LDA #96
                STA SP07_Y_POS_L  ;S
                STA SP08_Y_POS_L  ;e
                STA SP09_Y_POS_L  ;n
                STA SP10_Y_POS_L  ;i
                STA SP11_Y_POS_L  ;o
                STA SP12_Y_POS_L  ;r
                LDA #128
                STA SP13_Y_POS_L  ;n
                STA SP14_Y_POS_L  ;i
                STA SP15_Y_POS_L  ;o
                STA SP16_Y_POS_L  ;r
                setdp 0
                setaxl
                ;LDX #$0
                ;LDY #16
                ;JSL ILOCATE

                CLI

                ; reset keyboard buffer
                ;STZ KEY_BUFFER_RPOS
                ;STZ KEY_BUFFER_WPOS

                ; ; Copy vectors from ROM to Direct Page
                ; setaxl
                ; LDA #$FF
                ; LDX #$FF00
                ; LDY #$FF00
                ; MVP $00, $FF

                ; display boot message
;greet           setdbr `greet_msg       ;Set data bank to ROM
;                LDX #<>greet_msg
;                JSL IPRINT       ; print the first line
;                JSL IPRINT       ; print the second line
;                JSL IPRINT       ; print the third line
;                JSL IPRINTCR     ; print a blank line. Just because
                setas
                setdbr $01      ;set data bank to 1 (Kernel Variables)

endlessloop     NOP
                JML endlessloop



greet_done      BRK             ;Terminate boot routine and go to Ready handler.

;
; IBREAK
; ROM Break handler. This pulls the registers out of the stack
; and saves them in the "CPU" direct page locations
IBREAK          setdp 0
                PLA             ; Pull .Y and stuff it in the CPUY variable
                STA CPUY
                PLA             ; Pull .X and stuff it in the CPUY variable
                STA CPUX
                PLA             ; Pull .A and stuff it in the CPUY variable
                STA CPUA
                PLA
                STA CPUDP       ; Pull Direct page
                setas
                PLA             ; Pull Data Bank (8 bits)
                STA CPUDBR
                PLA             ; Pull Flags (8 bits)
                STA CPUFLAGS
                setal
                PLA             ; Pull Program Counter (16 bits)
                STA CPUPC
                setas
                PLA             ; Pull Program Bank (8 bits)
                STA CPUPBR
                setal
                TSA             ; Get the stack
                STA CPUSTACK    ; Store the stack at immediately before the interrupt was asserted
                LDA #<>STACK_END   ; initialize stack pointer back to the bootup value
                                ;<> is "lower word"
                TAS
                JML JMP_READY   ; Run READY routine (usually BASIC or MONITOR)

IREADY          setdbr `ready_msg
                setas
                LDX #<>ready_msg
                JSL IPRINT
;
; IREADYWAIT*
;  Wait for a keypress and display it on the screen. When the RETURN key is pressed,
;  call the RETURN event handler to process the command. Since RETURN can change, use
;  the vector in Direct Page to invoke the handler.
;
;  *Does not return. Execution in your program should continue via the RETURN direct page
;  vector.
IREADYWAIT      ; Check the keyboard buffer.
                JSL IGETCHE
                BRA IREADYWAIT

IKEYDOWN        STP             ; Keyboard key pressed
IRETURN         STP

;
;IGETCHE
; Get a character from the current input chnannel and echo it to screen.
; Waits for a character to be read.
; Return:
; A: Character read
; Carry: 1 if no valid data
;
IGETCHE         JSL IGETCHW
                JSL IPUTC
                RTL

;
;IGETCHW
; Get a character from the current input chnannel.
; Waits for a character to be read.
; Return:
; A: Character read
; Carry: 1 if no valid data
;
IGETCHW         PHD
                PHX
                PHP
                setdp $0F00
                setaxl
                ; Read from the keyboard buffer
                ; If the read position and write position are the same
                ; no data is waiting.
igetchw1        LDX KEY_BUFFER_RPOS
                CPX KEY_BUFFER_WPOS
                ; If data is waiting. return it.
                ; Otherwise wait for data.
                BNE igetchw2
                ;SEC            ; In non-waiting version, set the Carry bit and return
                ;BRA igetchw_done
                ; Simulator should wait for input
                SIM_WAIT
                JMP igetchw1
igetchw2        LDA $0,D,X  ; Read the value in the keyboard buffer
                PHA
                ; increment the read position and wrap it when it reaches the end of the buffer
                TXA
                CLC
                ADC #$02
                CMP #KEY_BUFFER_SIZE
                BCC igetchw3
                LDA #$0
igetchw3        STA KEY_BUFFER_RPOS
                PLA

igetchw_done    PLP
                PLX             ; Restore the saved registers and return
                PLD
                RTL
;
; IPRINT
; Print a string, followed by a carriage return
; DBR: bank containing string
; X: address of the string in data bank
; Modifies: X
;
IPRINT          JSL IPUTS
                JSL IPRINTCR
                RTL

; IPUTS
; Print a null terminated string
; DBR: bank containing string
; X: address of the string in data bank
; Modifies: X.
;  X will be set to the location of the byte following the string
;  So you can print multiple, contiguous strings by simply calling
;  IPUTS multiple times.
IPUTS           PHA
                PHP
                setas
                setxl
iputs1          LDA $0,b,x      ; read from the string
                BEQ iputs_done
iputs2          JSL IPUTC
iputs3          INX
                JMP iputs1
iputs_done      INX
                PLP
                PLA
                RTL

;
;IPUTC
; Print a single character to a channel.
; Handles terminal sequences, based on the selected text mode
; Modifies: none
;
IPUTC           PHD
                PHP             ; stash the flags (we'll be changing M)
                setdp 0
                setas
                CMP #$0D        ; handle CR
                BNE iputc_bs
                JSL IPRINTCR
                bra iputc_done
iputc_bs        CMP #$08        ; backspace
                BNE iputc_print
                JSL IPRINTBS
                BRA iputc_done
iputc_print     STA [CURSORPOS] ; Save the character on the screen
                JSL ICSRRIGHT
iputc_done	sim_refresh
                PLP
                PLD
                RTL

;
;IPUTB
; Output a single byte to a channel.
; Does not handle terminal sequences.
; Modifies: none
;
IPUTB
                ;
                ; TODO: write to open channel
                ;
                RTL

;
; IPRINTCR
; Prints a carriage return.
; This moves the cursor to the beginning of the next line of text on the screen
; Modifies: Flags
IPRINTCR	      PHP
                setxl
                PHX
                PHY
                PHD

                setdp 0

                LDX #0
                LDY CURSORY
                INY
                JSL ILOCATE
                
                setxl
                PLD
                PLY
                PLX
                PLP
                RTL
;
; IPRINTBS
; Prints a carriage return.
; This moves the cursor to the beginning of the next line of text on the screen
; Modifies: Flags
IPRINTBS	PHX
                PHY
                PHP
                LDX CURSORX
                LDY CURSORY
                DEX
                JSL ILOCATE
                PLP
                PLY
                PLX
                RTL
;
;ICSRRIGHT
; Move the cursor right one space
; Modifies: none
;
ICSRRIGHT	; move the cursor right one space
                PHX
                PHB
                setal
                setxl
                setdp $0
                INC CURSORPOS
                LDX CURSORX
                INX
                CPX COLS_VISIBLE
                BCC icsr_nowrap  ; wrap if the cursor is at or past column 80
                LDX #0
                PHY
                LDY CURSORY
                INY
                JSL ILOCATE
                PLY
icsr_nowrap     STX CURSORX
                PHA
                TXA
                STA @lVKY_TXT_CURSOR_X_REG_L  ;Store in Vicky's register
                PLA
                PLB
                PLX
                RTL

ISRLEFT	        RTL
ICSRUP	        RTL
ICSRDOWN	      RTL

;ILOCATE
;Sets the cursor X and Y positions to the X and Y registers
;Direct Page must be set to 0
;Input:
; X: column to set cursor
; Y: row to set cursor
;Modifies: none
ILOCATE         PHP
                PHD
                setaxl
                PHA

                setdp 0
                
ilocate_scroll  ; If the cursor is below the bottom row of the screen
                ; scroll the screen up one line. Keep doing this until
                ; the cursor is visible.
                CPY LINES_VISIBLE
                BCC ilocate_scrolldone
                JSL ISCROLLUP
                DEY
                ; repeat until the cursor is visible again
                BRA ilocate_scroll
ilocate_scrolldone
                ; done scrolling store the resultant cursor positions.
                STX CURSORX
                STY CURSORY
                LDA SCREENBEGIN
ilocate_row     ; compute the row
                CPY #$0
                BEQ ilocate_right
                ; move down the number of rows in Y
ilocate_down    CLC
                ADC COLS_PER_LINE
                DEY
                BEQ ilocate_right
                BRA ilocate_down
                ; compute the column
ilocate_right   CLC
                ADC CURSORX             ; move the cursor right X columns
                STA CURSORPOS
                LDY CURSORY
                TYA
                STA @lVKY_TXT_CURSOR_Y_REG_L  ;Store in Vicky's registers
                TXA
                STA @lVKY_TXT_CURSOR_X_REG_L  ;Store in Vicky's register

ilocate_done    setaxl
                PLA
                PLD
                PLP
                RTL
;
; ISCROLLUP
; Scroll the screen up one line
; Inputs:
;   None
; Affects:
;   None
ISCROLLUP       ; Scroll the screen up by one row
                ; Place an empty line at the bottom of the screen.
                ; TODO: use DMA to move the data
                PHA
                PHX
                PHY
                PHB
                PHP
                setaxl
                ; Set block move source to second row
                CLC
                LDA SCREENBEGIN
                TAY             ; Destination is first row
                ADC COLS_PER_LINE
                TAX             ; Source is second row
                ;TODO compute screen bottom with multiplier
                ;(once implemented)
                ; for now, should be 8064 or $1f80 bytes
                LDA #SCREEN_PAGE1-SCREEN_PAGE0-COLS_PER_LINE
                ; Move the data
                MVP $00,$00

                PLP
                PLB
                PLY
                PLX
                PLA
                RTL

;
; IPRINTH
; Prints data from memory in hexadecimal format
; Inputs:
;   X: 16-bit address of the LAST BYTE of data to print.
;   Y: Length in bytes of data to print
; Modifies:
;   X,Y, results undefined
IPRINTH         PHP
                setaxl
                PHA


iprinth1        setas
                LDA #0,b,x      ; Read the value to be printed
                LSR
                LSR
                LSR
                LSR
                JSL iprint_digit
                LDA #0,b,x
                JSL iprint_digit
                DEX
                DEY
                BNE iprinth1

                setaxl
                PLA
                PLP
                RTL

;
; iprint_digit
; This will print the low nibble in the A register.
; Inputs:
;   A: digit to print
;   x flag should be 0 (16-bit X)
; Affects:
;   P: m flag will be set to 0
iprint_digit    PHX
                PHB

                setdbr `hex_digits

                setal
                AND #$0F
                TAX

                ; Use the value in AL to
                LDA hex_digits,X
                JSL IPUTC       ; Print the digit

                PLB
                PLX
                RTL
;
; ICLRSCREEN
; Clear the screen and set the background and foreground colors to the
; currently selected colors.
ICLRSCREEN	    PHD
                PHP
                PHA
                PHX
                setas
                setxl 			; Set 16bits
                LDX #$0000		; Only Use One Pointer
                LDA #$20		; Fill the Entire Screen with Space
iclearloop0	    STA CS_TEXT_MEM_PTR, x	;
                inx
                cpx #$2000
                bne iclearloop0
                ; Now Set the Colors so we can see the text
                LDX	#$0000		; Only Use One Pointer
                LDA #$ED		; Fill the Color Memory with Foreground: 75% Purple, Background 12.5% White
iclearloop1	    STA CS_COLOR_MEM_PTR, x	;
                inx
                cpx #$2000
                bne iclearloop1
                setxl
                setal
                PLX
                PLA
                PLP
                PLD
                RTL

;
; ICOLORFLAG
; Set the colors of the flag on the welcome screen
;
ICOLORFLAG      PHA
                PHX
                PHP
                setaxs
                LDX #$00
iclearloop2	    LDA @lgreet_clr_line1,x
                STA CS_COLOR_MEM_PTR,x
                LDA @lgreet_clr_line2,x
                STA CS_COLOR_MEM_PTR + $80,x
                LDA @lgreet_clr_line3,x
                STA CS_COLOR_MEM_PTR + $100,x
                LDA @lgreet_clr_line4,x
                STA CS_COLOR_MEM_PTR + $180,x
                LDA @lgreet_clr_line5,x
                STA CS_COLOR_MEM_PTR + $200,x
                inx
                cpx #$0E
                bne iclearloop2
                PLP
                PLX
                PLA
                RTL
;
; IINITCHLUT
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Initialize VICKY's Character Color Look-Up Table;
; Inputs:
;   None
; Affects:
;   None
IINITCHLUT		  PHD
                PHP
                PHA
                PHX
                setas
                setxs 					; Set 8bits
				        ; Setup Foreground LUT First
				        LDX	#$00
lutinitloop0	  LDA @lfg_color_lut,x		; get Local Data
                STA FG_CHAR_LUT_PTR,x	; Write in LUT Memory
                inx
                cpx #$40
                bne lutinitloop0
                ; Set Background LUT Second
                LDX	#$00
lutinitloop1	  LDA @lbg_color_lut,x		; get Local Data
                STA BG_CHAR_LUT_PTR,x	; Write in LUT Memory
                INX
                CPX #$40
                bne lutinitloop1
                setal
                setxl 					; Set 8bits
                PLX
                PLA
                PLP
                PLD
                RTL

; IINITGAMMATABLE
; Author: Stefany
; Init the GAMMA Table for each R, G, B Channels
; Dec 15th, 2018 - Just Load the Gamma Table with linear Value.
; Inputs:
;   None
; Affects:
;  VICKY GAMMA TABLES
IINITGAMMATABLE setas 		; Set 8bits
                setxl     ; Set Accumulator to 8bits
                ldx #$0000
initgammaloop   LDA GAMMA_1_8_Tbl, x
                STA GAMMA_B_LUT_PTR, x
                STA GAMMA_G_LUT_PTR, x
                STA GAMMA_R_LUT_PTR, x
                inx
                cpx #$0100
                bne initgammaloop
                setaxl        ; Set Acc back to 16bits before setting the Cursor Position
                RTL

; IINITALLLUT
; Author: Stefany
;Init the Different Look-Up Table for the Graphic Mode
; The LUT are loaded with Equal Values, so the End Results will be Gray Shades
; Inputs:
;   None
; Affects:
;  VICKY INTERNAL LOOK-UP TAbles
IINITALLLUT     PHA
                PHX
                LDX #$0000
                setas
                LDA #$00
                STA $0A     ; Temp Location
iinit_lut_loop  ;
                ; Red Channel
                STX $02
                LDX $0A
                LDA GAMMA_2_2_Tbl, x
                EOR  #$55
                LDX $02
                STA @lGRPH_LUT0_PTR, x
                STA @lGRPH_LUT1_PTR, x
                STA @lGRPH_LUT2_PTR, x
                STA @lGRPH_LUT3_PTR, x
                STA @lGRPH_LUT4_PTR, x
                STA @lGRPH_LUT5_PTR, x
                STA @lGRPH_LUT6_PTR, x
                STA @lGRPH_LUT7_PTR, x
                inx
                ; Green Channel  RANDOM_LUT_Tbl
                STX $02
                LDX $0A
                LDA RANDOM_LUT_Tbl, x
                LDX $02
                STA @lGRPH_LUT0_PTR, x
                STA @lGRPH_LUT1_PTR, x
                STA @lGRPH_LUT2_PTR, x
                STA @lGRPH_LUT3_PTR, x
                STA @lGRPH_LUT4_PTR, x
                STA @lGRPH_LUT5_PTR, x
                STA @lGRPH_LUT6_PTR, x
                STA @lGRPH_LUT7_PTR, x
                inx
                STX $02
                LDX $0A
                LDA GAMMA_1_8_Tbl, x
                EOR  #$AA
                LDX $02
                STA @lGRPH_LUT0_PTR, x
                STA @lGRPH_LUT1_PTR, x
                STA @lGRPH_LUT2_PTR, x
                STA @lGRPH_LUT3_PTR, x
                STA @lGRPH_LUT4_PTR, x
                STA @lGRPH_LUT5_PTR, x
                STA @lGRPH_LUT6_PTR, x
                STA @lGRPH_LUT7_PTR, x
                inx
                ; Alpha Channel
                LDA #$FF
                STA @lGRPH_LUT0_PTR, x
                STA @lGRPH_LUT1_PTR, x
                STA @lGRPH_LUT2_PTR, x
                STA @lGRPH_LUT3_PTR, x
                STA @lGRPH_LUT4_PTR, x
                STA @lGRPH_LUT5_PTR, x
                STA @lGRPH_LUT6_PTR, x
                STA @lGRPH_LUT7_PTR, x
                inc $0A
                inx
                cpx #$0400
                beq iinit_lut_exit
                brl iinit_lut_loop
iinit_lut_exit
                setaxl        ; Set Acc back to 16bits before setting the Cursor Position
                PLX
                PLA
                RTL

; IINITVKYTXTMODE
; Author: Stefany
;Init the Text Mode
; Inputs:
;   None
; Affects:
;  Vicky's Internal Registers
IINITVKYTXTMODE PHA
                setas
                LDA #Mstr_Ctrl_Text_Mode_En      ;Okay, this Enables the Text Mode (Video Display)
                STA MASTER_CTRL_REG_L
                ; Set the Border Color
                LDA #$20
                STA BORDER_COLOR_B
                STA BORDER_COLOR_R
                LDA #$00
                STA BORDER_COLOR_G
                LDA #Border_Ctrl_Enable   ; Enable the Border
                STA BORDER_CTRL_REG
                setaxl        ; Set Acc back to 16bits before setting the Cursor Position
                PLA
                RTL
; IINITVKYTXTMODE
; Author: Stefany
;Init the Text Mode
; Inputs:
;   None
; Affects:
;  Vicky's Internal Registers
IINITVKYGRPMODE
                PHA
                setas
                LDA #$00          ; Enable Bit-Map and uses LUT0
                STA @lBM_CONTROL_REG
                ; Set the BitMap Start Address to $00C0000 ($B0C000)
                LDA #$00          ;; (L)Load Base Address of where Bitmap begins
                STA @lBM_START_ADDY_L
                LDA #$C0
                STA @lBM_START_ADDY_M
                LDA #$00
                STA @lBM_START_ADDY_H ; This address is always base from
                                      ; of starting of FRAME Buffer $B00000
                LDA #$80
                STA BM_X_SIZE_L
                LDA #$02
                STA BM_X_SIZE_H         ; $0280 = 640
                LDA #$E0
                STA BM_Y_SIZE_L
                LDA #$01
                STA BM_Y_SIZE_H         ; $01E0 = 480
                setaxl        ; Set Acc back to 16bits before setting the Cursor Position
                PLA
                RTL

IINITTILEMODE
                PHA
                setas
                setxl        ; Set Acc back to 16bits before setting the Cursor Position
                ;First Setup Tile Layer 0
                LDA #$01    ; Enable The Layer
                STA @lTL0_CONTROL_REG
                LDA #$00    ; Set the Starting Address of Graphic Map Location
                STA @lTL0_START_ADDY_L
                STA @lTL0_START_ADDY_H
                LDA #$60    ; $00:60000 (in graphic Mem) = $B0:6000 in Absolute Value
                STA @lTL0_START_ADDY_M
                LDA #$00    ; Set the X Stride to 256 (not implemented Yet)
                STA @lTL0_MAP_X_STRIDE_L
                LDA #$01
                STA @lTL0_MAP_X_STRIDE_H
                LDA #$00    ; Set the Y Stride to 256 (not implemented Yet)
                STA @lTL0_MAP_Y_STRIDE_L
                LDA #$01
                STA @lTL0_MAP_Y_STRIDE_H
                ; Set some Characters in TILE_MAP0 Memory Bank
                ; 36 Tiles x 26 Tiles (when the border is on)
                LDX #$0000
TILEMAPSET_LOOP
                LDA #$02
                STA @lTILE_MAP0+128, x   ; This ought to fill the first line
;                LDA #$02
                STA @lTILE_MAP0+1728, x
                INX
                CPX #$0036    ; 36 - 4 (because of Border)
                BNE TILEMAPSET_LOOP
                ;Just fill some tiles with Solid colors
                LDX #$0000
INITTILE_LOOP
                LDA #$00    ; This is 1 Color
                LDA @lTILE_TEST_CH00,x
                STA @l$B06000, x
                LDA @lTILE_TEST_CH01,x
                STA @l$B06100, x
                LDA @lTILE_TEST_CH02,x
                STA @l$B06200, x
                LDA #$FF
                STA @l$B06300, x
                INX
                CPX #$0100
                BNE INITTILE_LOOP
                setal
                PLA
                RTL

IINITSPRITE     PHA
                setas
                LDA #$03    ; Enable 17 Sprites
                STA SP00_CONTROL_REG
                STA SP01_CONTROL_REG
                STA SP02_CONTROL_REG
                STA SP03_CONTROL_REG
                STA SP04_CONTROL_REG
                STA SP05_CONTROL_REG
                STA SP06_CONTROL_REG
                STA SP07_CONTROL_REG
                STA SP08_CONTROL_REG
                STA SP09_CONTROL_REG
                STA SP10_CONTROL_REG
                STA SP11_CONTROL_REG
                STA SP12_CONTROL_REG
                STA SP13_CONTROL_REG
                STA SP14_CONTROL_REG
                STA SP15_CONTROL_REG
                STA SP16_CONTROL_REG
                ; Set the Pointer for the Graphic
                LDA #$09
                STA SP00_ADDY_PTR_H
                STA SP01_ADDY_PTR_H
                STA SP02_ADDY_PTR_H
                STA SP03_ADDY_PTR_H
                STA SP04_ADDY_PTR_H
                STA SP05_ADDY_PTR_H
                STA SP06_ADDY_PTR_H
                STA SP07_ADDY_PTR_H
                STA SP08_ADDY_PTR_H
                STA SP09_ADDY_PTR_H
                STA SP10_ADDY_PTR_H
                STA SP11_ADDY_PTR_H
                STA SP12_ADDY_PTR_H
                STA SP13_ADDY_PTR_H
                STA SP14_ADDY_PTR_H
                STA SP15_ADDY_PTR_H
                STA SP16_ADDY_PTR_H

                LDA #$00
                STA SP00_ADDY_PTR_M
                LDA #$04
                STA SP01_ADDY_PTR_M
                LDA #$08
                STA SP02_ADDY_PTR_M
                LDA #$0C
                STA SP03_ADDY_PTR_M
                LDA #$10
                STA SP04_ADDY_PTR_M
                LDA #$14
                STA SP05_ADDY_PTR_M
                LDA #$18
                STA SP06_ADDY_PTR_M
                LDA #$1C
                STA SP07_ADDY_PTR_M
                LDA #$20
                STA SP08_ADDY_PTR_M
                LDA #$24
                STA SP09_ADDY_PTR_M
                LDA #$28
                STA SP10_ADDY_PTR_M
                LDA #$2C
                STA SP11_ADDY_PTR_M
                LDA #$30
                STA SP12_ADDY_PTR_M
                LDA #$34
                STA SP13_ADDY_PTR_M
                LDA #$38
                STA SP14_ADDY_PTR_M
                LDA #$3C
                STA SP15_ADDY_PTR_M
                LDA #$40
                STA SP16_ADDY_PTR_M

                LDA #$00
                STA SP00_ADDY_PTR_L
                STA SP01_ADDY_PTR_L
                STA SP02_ADDY_PTR_L
                STA SP03_ADDY_PTR_L
                STA SP04_ADDY_PTR_L
                STA SP05_ADDY_PTR_L
                STA SP06_ADDY_PTR_L
                STA SP07_ADDY_PTR_L
                STA SP08_ADDY_PTR_L
                STA SP09_ADDY_PTR_L
                STA SP10_ADDY_PTR_L
                STA SP11_ADDY_PTR_L
                STA SP12_ADDY_PTR_L
                STA SP13_ADDY_PTR_L
                STA SP14_ADDY_PTR_L
                STA SP15_ADDY_PTR_L
                STA SP16_ADDY_PTR_L
                PLA
                RTL


; IINITFONTSET
; Author: Stefany
; Init the Text Mode
; Inputs:
;   None
; Affects:
;  Vicky's Internal FONT Memory
IINITFONTSET
                RTL

;
; IINITCURSOR
; Author: Stefany
; Init the Cursor Registers
; Verify that the Math Block Works
; Inputs:
; None
; Affects:
;  Vicky's Internal Cursor's Registers
IINITCURSOR     PHA
                setas
                LDA #$A0      ;The Cursor Character will be a Fully Filled Block
                STA VKY_TXT_CURSOR_CHAR_REG
                LDA #$03      ;Set Cursor Enable And Flash Rate @1Hz
                STA VKY_TXT_CURSOR_CTRL_REG ;
                setaxl        ; Set Acc back to 16bits before setting the Cursor Position
                LDA #$0000;
                STA VKY_TXT_CURSOR_X_REG_L; // Set the X to Position 1
                LDA #$0006;
                STA VKY_TXT_CURSOR_Y_REG_L; // Set the Y to Position 6 (Below)
                PLA
                RTL

;
; IINITSUPERIO
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Initialize SuperIO PME Registers
; Inputs:
;   None
; Affects:
;   None
IINITSUPERIO	  PHD
                PHP
                PHA
                setas			;just make sure we are in 8bit mode

                LDA #$01		;Default Value - C256 Doesn't use this IO Pin
                STA GP10_REG
                LDA GP10_REG
                LDA #$01		;Default Value - C256 Doesn't use this IO Pin
                STA GP11_REG
                LDA #$01		;Default Value - C256 Doesn't use this IO Pin
                STA GP12_REG
        				LDA #$01		;Default Value - C256 Doesn't use this IO Pin
        				STA GP13_REG
        				LDA #$05		;(C256 - POT A Analog BX) Bit[0] = 1, Bit[2] = 1
        				STA GP14_REG
        				LDA #$05		;(C256 - POT A Analog BY) Bit[0] = 1, Bit[2] = 1
        				STA GP15_REG
        				LDA #$05		;(C256 - POT B Analog BX) Bit[0] = 1, Bit[2] = 1
        				STA GP16_REG
        				LDA #$05		;(C256 - POT B Analog BY) Bit[0] = 1, Bit[2] = 1
        				STA GP17_REG
        				LDA #$00		;(C256 - HEADPHONE MUTE) - Output GPIO - Push-Pull (1 - Headphone On, 0 - HeadPhone Off)
        				STA GP20_REG

                ;LDA #$00		;(C256 - FLOPPY - DS1) - TBD Later, Floppy Stuff (JIM DREW)
				        ;STA GP21_REG
				        ;LDA #$00		;(C256 - FLOPPY - DMTR1) - TBD Later, Floppy Stuff (JIM DREW)
				        ;STA GP22_REG

				        LDA #$01		;Default Value - C256 Doesn't use this IO Pin
				        STA GP24_REG
				        LDA #$05		;(C256 - MIDI IN) Bit[0] = 1, Bit[2] = 1 (Page 132 Manual)
				        STA GP25_REG
			        	LDA #$84		;(C256 - MIDI OUT) Bit[2] = 1, Bit[7] = 1 (Open Drain - To be Checked)
				        STA GP26_REG

				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 1) Setup as GPIO Input for now
				        STA GP30_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 4) Setup as GPIO Input for now
				        STA GP31_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 3) Setup as GPIO Input for now
				        STA GP32_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 6) Setup as GPIO Input for now
				        STA GP33_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 5) Setup as GPIO Input for now
				        STA GP34_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 8) Setup as GPIO Input for now
				        STA GP35_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 7) Setup as GPIO Input for now
				        STA GP36_REG
				        LDA #$01		;Default Value - (C256 - JP1 Fanout Pin 10) Setup as GPIO Input for now
				        STA GP37_REG

				        ;LDA #$01		;(C256 - FLOPPY - DRVDEN0) - TBD Later, Floppy Stuff (JIM DREW)
				        ;STA GP40_REG
				        ;LDA #$01		;(C256 - FLOPPY - DRVDEN1) - TBD Later, Floppy Stuff (JIM DREW)
				        ;STA GP41_REG
				        LDA #$01		;Default Value - C256 Doesn't use this IO Pin
				        STA GP42_REG
			          LDA #$01		;(C256 - INPUT PLL CLK INTERRUPT) Default Value - Will keep it as an input for now, no real usage for now
				        STA GP43_REG
				        LDA #$05		;(C256 - UART2 - RI2) - Input - Set Secondary Function
				        STA GP50_REG
				        LDA #$05		;(C256 - UART2 - DCD2) - Input - Set Secondary Function
				        STA GP51_REG
				        LDA #$05		;(C256 - UART2 - RXD2) - Input - Set Secondary Function
				        STA GP52_REG
				        LDA #$04		;(C256 - UART2 - TXD2) - Output - Set Secondary Function
				        STA GP53_REG
				        LDA #$05		;(C256 - UART2 - DSR2) - Input - Set Secondary Function
				        STA GP54_REG
				        LDA #$04		;(C256 - UART2 - RTS2) - Output - Set Secondary Function
				        STA GP55_REG
				        LDA #$05		;(C256 - UART2 - CTS2) - Input - Set Secondary Function
				        STA GP56_REG
				        LDA #$04		;(C256 - UART2 - DTR2) - Output - Set Secondary Function
				        STA GP57_REG
				        LDA #$84		;(C256 - LED1) - Open Drain - Output
				        STA GP60_REG
				        LDA #$84		;(C256 - LED2) - Open Drain - Output
				        STA GP61_REG
			        	LDA #$00		;GPIO Data Register (GP10..GP17) - Not Used
				        STA GP1_REG
				        LDA #$01		;GPIO Data Register (GP20..GP27) - Bit[0] - Headphone Mute (Enabling it)
				        STA GP2_REG
				        LDA #$00		;GPIO Data Register (GP30..GP37) - Since it is in Output mode, nothing to write here.
				        STA GP3_REG
				        LDA #$00		;GPIO Data Register (GP40..GP47)  - Not Used
				        STA GP4_REG
				        LDA #$00		;GPIO Data Register (GP50..GP57)  - Not Used
				        STA GP5_REG
				        LDA #$00		;GPIO Data Register (GP60..GP61)  - Not Used
				        STA GP6_REG

				        LDA #$01		;LED1 Output - Already setup by Vicky Init Phase, for now, I will leave it alone
				        STA LED1_REG
				        LDA #$02		;LED2 Output - However, I will setup this one, to make sure the Code works (Full On, when Code was ran)
				        STA LED2_REG
				        setal
                PLA
				        PLP
			        	PLD
                RTL
;
; IINITKEYBOARD
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Initialize the Keyboard Controler (8042) in the SuperIO.
; Inputs:
;   None
; Affects:
;   Carry (c)
IINITKEYBOARD	  PHD
				        PHP
				        PHA
				        PHX

                setas				;just make sure we are in 8bit mode
                setxl 					; Set 8bits

				; Setup Foreground LUT First
                CLC

                JSR Poll_Inbuf ;
;; Test AA
				        LDA #$0AA			;Send self test command
				        STA KBD_CMD_BUF
								;; Sent Self-Test Code and Waiting for Return value, it ought to be 0x55.
                JSR Poll_Outbuf ;

				        LDA KBD_OUT_BUF		;Check self test result
				        CMP #$55
				        BEQ	passAAtest

                BRL initkb_loop_out

passAAtest      ;LDX #<>pass_tst0xAAmsg
                ;JSL IPRINT      ; print Message
;; Test AB
				        LDA #$AB			;Send test Interface command
				        STA KBD_CMD_BUF

                JSR Poll_Outbuf ;

				        LDA KBD_OUT_BUF		;Display Interface test results
				        CMP #$00			;Should be 00
				        BEQ	passABtest

                BRL initkb_loop_out

passABtest      ;LDX #<>pass_tst0xABmsg
                ;JSL IPRINT       ; print Message
;; Program the Keyboard & Enable Interrupt with Cmd 0x60
                LDA #$60            ; Send Command 0x60 so to Enable Interrupt
                STA KBD_CMD_BUF

                JSR Poll_Inbuf ;

                LDA #%01101001      ; Enable Interrupt
                STA KBD_DATA_BUF

                JSR Poll_Inbuf ;

                ;LDX #<>pass_cmd0x60msg
                ;JSL IPRINT       ; print Message
;; Reset Keyboard
;                LDA #$FF      ; Send Keyboard Reset command
;                STA KBD_DATA_BUF
                ; Must wait here;
;                LDX #$FFFF
;DLY_LOOP1       DEX
;                CPX #$0000
;                BNE DLY_LOOP1

;                JSR Poll_Outbuf ;

;                LDA KBD_OUT_BUF   ; Read Output Buffer

;                LDX #<>pass_cmd0xFFmsg
;                JSL IPRINT       ; print Message
;; Test Echo - Cmd$EE
                LDA #$EE      ; Send Keyboard Reset command
                STA KBD_DATA_BUF

                LDX #$4000
DLY_LOOP2       DEX
                CPX #$0000
                BNE DLY_LOOP2

                JSR Poll_Outbuf ;

                LDA KBD_OUT_BUF
                CMP #$EE
                BNE initkb_loop_out

                ;LDX #<>pass_cmd0xEEmsg
                ;JSL IPRINT       ; print Message

				        LDA #$F4			; Enable the Keyboard
				        STA KBD_DATA_BUF

                JSR Poll_Outbuf ;

				        LDA KBD_OUT_BUF		; Clear the Output buffer
                ; Unmask the Keyboard interrupt
                ; Clear Any Pending Interrupt
                LDA @lINT_PENDING_REG1  ; Read the Pending Register &
                STA @lINT_PENDING_REG1  ; Writing it back will clear the Active Bit
                ; Disable the Mask
                LDA @lINT_MASK_REG1
                AND #~FNX1_INT00_KBD
                STA @lINT_MASK_REG1



                LDX #<>Success_kb_init
                SEC
                BCS InitSuccess

initkb_loop_out LDX #<>Failed_kb_init
InitSuccess     JSL IPRINT       ; print Message
                setal 					; Set 16bits
                setxl 					; Set 16bits

                PLX
                PLA
				        PLP
				        PLD
                RTL

Poll_Inbuf	    .as
                LDA STATUS_PORT		; Load Status Byte
				        AND	#<INPT_BUF_FULL	; Test bit $02 (if 0, Empty)
				        CMP #<INPT_BUF_FULL
				        BEQ Poll_Inbuf
                RTS

Poll_Outbuf	    .as
                LDA STATUS_PORT
                AND #OUT_BUF_FULL ; Test bit $01 (if 1, Full)
                CMP #OUT_BUF_FULL
                BNE Poll_Outbuf
                RTS
; IINITRTC
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Initialize the Real Time Clock
; Inputs:
;   None
                ; Affects:
                ;   None
IINITRTC        PHA
                setas				    ;just make sure we are in 8bit mode
                LDA #$00
                STA RTC_SEC     ;Set the Time to 10:10AM
                LDA #10
                STA RTC_MIN
                STA RTC_HRS
                LDA #12
                STA RTC_DAY
                LDA #04
                STA RTC_MONTH   ; April 12th, 2018 - Begining of the Project
                LDA #04
                STA RTC_MONTH   ; Thursday
                LDA #18
                STA RTC_YEAR    ; Thursday

                LDA RTC_DAY     ; Read the Day Registers
                STA RTC_DAY     ; Store it back

                setal 					; Set 16bits
                PLA
                RTL
;
; ITESTSID
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Initialize the Real Time Clock
; Inputs:
; None
ITESTSID        PHA
                setas				    ;just make sure we are in 8bit mode

                ; Set the Volume to Max
                LDA #$0F
                STA SID0_MODE_VOL
                ; Left SID
                ; Voice
                LDA #$BE
                STA SID0_V1_ATCK_DECY
                LDA #$F8
                STA SID0_V1_SSTN_RLSE

                LDA #$11
                STA SID0_V1_FREQ_HI
                LDA #$25
                STA SID0_V1_FREQ_LO

                LDA #$11
                STA SID0_V1_CTRL


                ;LDA #$08
                ;STA SID0_V1_PW_HI   ;G1
                ;LDA #$00
                ;STA SID0_V1_FREQ_HI

                ;LDA #$C6
                ;STA SID0_V1_SSTN_RLSE

                ;LDA #$08
                ;STA SID0_V2_PW_HI   ;G1
                ;LDA #$00
                ;STA SID0_V2_FREQ_HI
                ;LDA #$08
                ;STA SID0_V2_ATCK_DECY
                ;LDA #$C6
                ;STA SID0_V2_SSTN_RLSE

                ;LDA #$08
                ;STA SID0_V3_PW_HI   ;G1
                ;LDA #$00
                ;STA SID0_V3_FREQ_HI
                ;LDA #$08
                ;STA SID0_V3_ATCK_DECY
                ;LDA #$C6
                ;STA SID0_V3_SSTN_RLSE


                ;LDA #$36              ;Left Side (Rev A of Board)
                ;STA SID0_V1_FREQ_LO
                ;LDA #$01
                ;STA SID0_V1_FREQ_HI   ;G1
                ;LDA #$00              ;Left Side (Rev A of Board)
                ;STA SID0_V1_PW_LO
                ;LDA #$08
                ;STA SID0_V1_PW_HI   ;G1
                ;LDA #$08
                ;STA SID0_V1_CTRL    ; Reset
                ; Voice 2
                ;LDA #$0C
                ;STA SID0_V2_FREQ_LO
                ;LDA #$04
                ;STA SID0_V2_FREQ_HI   ;B1
                ;LDA #$00              ;Left Side (Rev A of Board)
                ;STA SID0_V2_PW_LO
                ;LDA #$08
                ;STA SID0_V2_PW_HI   ;G1
                ;LDA #$08
                ;STA SID0_V2_CTRL    ; Reset
                ; Voice 3
                ;LDA #$00
                ;STA SID0_V3_FREQ_LO
                ;LDA #$08
                ;STA SID0_V3_FREQ_HI   ;D
                ;LDA #$00              ;Left Side (Rev A of Board)
                ;STA SID0_V3_PW_LO
                ;LDA #$08
                ;STA SID0_V3_PW_HI   ;G1
                ;LDA #$08
                ;STA SID0_V3_CTRL    ; Reset

                ; Enable each Voices with Triangle Wave
                ;LDA #$10
                ;STA SID0_V1_CTRL    ; Triangle
                ;STA SID0_V2_CTRL    ; Triangle
                ;STA SID0_V3_CTRL    ; Triangle

                setal 					; Set 16bits
                PLA
                RTL
;
; ITESTMATH
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Verify that the Math Block Works
; Inputs:
; None
ITESTMATH       PHA
                setal 					; Set 16bits
                LDA #$1234
                STA UNSIGNED_MULT_A_LO
                LDA #$55AA
                STA UNSIGNED_MULT_B_LO
                ; Results Ought to be : $06175A88
                LDA UNSIGNED_MULT_AL_LO
                STA STEF_BLOB_BEGIN

                LDA UNSIGNED_MULT_AH_LO
                STA STEF_BLOB_BEGIN + 2
                setxl 					; Set 16bits
                setal 					; Set 16bits
                PLA
                RTL
;
; ITESTMATH
; Author: Stefany
; Note: We assume that A & X are 16Bits Wide when entering here.
; Verify that the Math Block Works
; Inputs:
; None
IINITCODEC      PHA
                setal
                LDA #%0001101000000000     ;R13 - Turn On Headphones
                STA CODEC_DATA_LO
                LDA #$0001
                STA CODEC_WR_CTRL             ; Execute the Write
                JSR CODEC_TRF_FINISHED
                ;
                LDA #%0010101000000011       ;R21 - Enable All the Analog In
                STA CODEC_DATA_LO
                LDA #$0001
                STA CODEC_WR_CTRL             ; Execute the Write
                JSR CODEC_TRF_FINISHED

                LDA #%0010001100000001      ;R17 - Enable All the Analog In
                STA CODEC_DATA_LO
                LDA #$0001
                STA CODEC_WR_CTRL             ; Execute the Write
                JSR CODEC_TRF_FINISHED

                LDA #%0010110000000111      ;R22 - Enable all Analog Out
                STA CODEC_DATA_LO
                LDA #$0001
                STA CODEC_WR_CTRL             ; Execute the Write
                JSR CODEC_TRF_FINISHED
                PLA
                RTL

IRESETCODEC     setal
                LDA #$2E00      ;R22 - Enable all Analog Out
                STA CODEC_DATA_LO
                LDA #$0001
                STA CODEC_WR_CTRL             ; Execute the Write
                JSR CODEC_TRF_FINISHED
                RTL

CODEC_TRF_FINISHED
                setas
; This is about waiting for the Serial Transfer to CODEC to be finished
CODEC_LOOP      LDA CODEC_WR_CTRL
                AND #$01
                CMP #$01
                BEQ CODEC_LOOP
                setal
                RTS

; Clear Bitmap Screen
; This is Done with Software for Now, will be done by DMA later

IBM_FILL_SCREEN  setaxl
                LDA #$0000
                LDX #$0000
BM_FILL_SCREEN_LOOPY
                LDY #$0000
                setas
BM_FILL_SCREEN_LOOPX
                STA [BMP_PRSE_DST_PTR],Y    ; This is where the Pixel Go, Video Memory
                INY
                CPY BM_CLEAR_SCRN_X              ; Transfer the First line
                BNE BM_FILL_SCREEN_LOOPX
                JSR BM_FILL_COMPUTE_Y_DST
                INX
                CPX BM_CLEAR_SCRN_Y
                BNE BM_FILL_SCREEN_LOOPY
                setaxl
                RTL
; BMP_PRSE_SRC_PTR = BMP_PRSE_SRC_PTR + BMP_X_SIZE
BM_FILL_COMPUTE_Y_DST
                setal
                ; So just load the Actual Value so it can be substracted again from BMP_X_SIZE
                LDA BMP_PRSE_DST_PTR        ; Right now it is set @ $020000 (128K) + File Size
                STA @lADDER32_A_LL
                LDA BMP_PRSE_DST_PTR+2      ; Right now it is set @ $020000 (128K)
                STA @lADDER32_A_HL
                LDA #$280        ; Right now it is set @ $020000 (128K) + File Size
                STA @lADDER32_B_LL
                LDA #$0000
                STA @lADDER32_B_HL
                LDA @lADDER32_R_LL
                STA BMP_PRSE_DST_PTR
                LDA @lADDER32_R_HL
                STA BMP_PRSE_DST_PTR+2
                LDA #$0000
                RTS





;
; IBMP_PARSER  (indexed File Only)
; Go Parse and Update LUT and Transfer Data to Video Memory (Active Memory)
; Author: Stefany
;
; Verify that the Math Block Works
; Inputs:
; None
IBMP_PARSER     setaxl
                ; First Check the BMP Signature
                LDY #$0000
                LDA [BMP_PRSE_SRC_PTR],Y
                CMP #$4D42
                BEQ IBMP_PARSER_CONT
                BRL BMP_PARSER_END_WITH_ERROR
IBMP_PARSER_CONT
                LDY #$0002
                LDA [BMP_PRSE_SRC_PTR],Y    ; File Size Low Short
                STA @lADDER32_A_LL          ; Store in 32Bit Adder (A)
                ; File Size
                LDY #$0004
                LDA [BMP_PRSE_SRC_PTR],Y    ; File Size High Short
                STA @lADDER32_A_HL          ; Store in 32Bit Adder (A)
                LDA #$FFFF                  ; Store -1 in Adder (B)
                STA @lADDER32_B_LL
                STA @lADDER32_B_HL
                ; File Size - 1
                CLC
                LDA @lADDER32_R_LL
                STA BMP_FILE_SIZE
                LDA @lADDER32_R_HL
                STA BMP_FILE_SIZE+2
                ; If the signature is valid, Save the Size of the Image
                LDY #$0012
                LDA [BMP_PRSE_SRC_PTR],Y    ; The X SIze is 32bits in BMP, but 16bits will suffice
                STA BMP_X_SIZE
                ; Y Size
                LDY #$0016
                LDA [BMP_PRSE_SRC_PTR],Y    ; The X SIze is 32bits in BMP, but 16bits will suffice
                STA BMP_Y_SIZE
                ; Number of Indexed Color in the Image (number of colors in the LUT)
                LDY #$002E
                LDA [BMP_PRSE_SRC_PTR],Y    ; The X SIze is 32bits in BMP, but 16bits will suffice
                ;INC A; Add 1
                ASL A; Multiply by 2
                ASL A; Multiply by 2
                STA BMP_COLOR_PALET         ;
                CPX #$0000
                BNE BMP_LUT1_PICK
                JSR BMP_PARSER_UPDATE_LUT0   ; Go Upload the LUT0
                BRA DONE_TRANSFER_LUT;
  BMP_LUT1_PICK
                CPX #$0001
                BNE BMP_LUT2_PICK
                JSR BMP_PARSER_UPDATE_LUT1   ; Go Upload the LUT1
  BMP_LUT2_PICK
               ; Let's Compute the Pointer for the BITMAP (The Destination)
               ; Let's use the Internal Mutliplier to Find the Destination Address
               ; Let's Compute the Hight First
               ; Y x Stride + X
  DONE_TRANSFER_LUT
                LDA BMP_POSITION_Y
                STA @lM0_OPERAND_A
                LDA SCRN_X_STRIDE
                STA @lM0_OPERAND_B
                LDA @lM0_RESULT
                STA @lADDER32_A_LL          ; Store in 32Bit Adder (A)
                LDA @lM0_RESULT+2
                STA @lADDER32_A_HL          ; Store in 32Bit Adder (A)
                LDA BMP_POSITION_X
                STA @lADDER32_B_LL          ; Put the X Position Adder (B)
                LDA #$0000
                STA @lADDER32_B_HL
                LDA @lADDER32_R_LL          ; Put the Results in TEMP
                STA USER_TEMP
                LDA @lADDER32_R_HL          ; Put the Results in TEMP
                STA USER_TEMP+2
                ; Let's Add the X,Y Memory Point to the Actual Address where the bitmap begins
                LDA BMP_PRSE_DST_PTR
                STA @lADDER32_A_LL          ; Store in 32Bit Adder (A)
                LDA BMP_PRSE_DST_PTR+2
                STA @lADDER32_A_HL          ; Store in 32Bit Adder (A)
                LDA USER_TEMP
                STA @lADDER32_B_LL          ; Store in 32Bit Adder (B)
                LDA USER_TEMP+2
                STA @lADDER32_B_HL          ; Store in 32Bit Adder (B)
                ; Results of Requested Position (Y x Stride + X) + Start Address
                LDA @lADDER32_R_LL          ; Put the Results in BMP_PRSE_DST_PTR
                STA BMP_PRSE_DST_PTR
                LDA @lADDER32_R_HL          ; Put the Results in BMP_PRSE_DST_PTR
                STA BMP_PRSE_DST_PTR+2
                ; Let's Compute the Pointer for the FILE (The Source)
                ; My GOD I love this 32Bits ADDER ;o) Makes my life so simple...
                ; Imagine when we are going to need the 16Bit Multiplier, hum... it is going to be fun
                ; Load Absolute Location in Adder32 Bit Reg A
                LDA BMP_PRSE_SRC_PTR        ; Right now it is set @ $020000 (128K)
                STA @lADDER32_A_LL
                LDA BMP_PRSE_SRC_PTR+2        ; Right now it is set @ $020000 (128K)
                STA @lADDER32_A_HL
                ; Load File Size in Adder32bits Reg B
                LDA BMP_FILE_SIZE
                STA @lADDER32_B_LL
                LDA BMP_FILE_SIZE+2
                STA @lADDER32_B_HL
                ; Spit the Answer Back into the SRC Pointer (this should Point to last Pixel in memory)
                LDA @lADDER32_R_LL
                STA BMP_PRSE_SRC_PTR
                LDA @lADDER32_R_HL
                STA BMP_PRSE_SRC_PTR+2
                ; Now Take the Last Results and put it in Register A of ADDER32
                LDA BMP_PRSE_SRC_PTR        ; Right now it is set @ $020000 (128K) + File Size
                STA @lADDER32_A_LL
                LDA BMP_PRSE_SRC_PTR+2      ; Right now it is set @ $020000 (128K)
                STA @lADDER32_A_HL
                CLC
                LDA BMP_X_SIZE              ; Load The Size in X of the image and Make it negative
                EOR #$FFFF                  ; Inverse all bit
                ADC #$0001                  ; Add 0 ()
                STA @lADDER32_B_LL          ; Store the Results in reg B of ADDER32
                LDA #$FFFF
                STA @lADDER32_B_HL          ; Store in the Reminder of the 32Bits B Register
                                            ; We are now ready to go transfer the Image
                LDA @lADDER32_R_LL
                STA BMP_PRSE_SRC_PTR
                LDA @lADDER32_R_HL
                STA BMP_PRSE_SRC_PTR+2
                                            ; The Starting Pointer is in Results of the ADDER32
                ; Here The Pointer "BMP_PRSE_SRC_PTR" ought to point to the graphic itself (0,0)
                JSR BMP_PARSER_DMA_SHIT_OUT  ; We are going to start with the slow method
                LDX #<>bmp_parser_msg0
                BRA BMP_PARSER_END_NO_ERROR

BMP_PARSER_END_WITH_ERROR
                LDX #<>bmp_parser_err0

BMP_PARSER_END_NO_ERROR
                JSL IPRINT       ; print the first line
                RTL

; This transfer the Palette Directly
; Will have to be improved, so it can load the LUT Data in any specific LUT - TBC
BMP_PARSER_UPDATE_LUT0
                SEC
                   ; And this is offset to where the Color Palette Begins
                LDY #$007A
                LDX #$0000
                setas
BMP_PARSER_UPDATE_LOOP
                ; RED Pixel
                LDA [BMP_PRSE_SRC_PTR],Y    ; First Pixel is Red
                STA @lGRPH_LUT0_PTR+0, X      ; The look-up Table point to a pixel Blue
                INY
                ; Green Pixel
                LDA [BMP_PRSE_SRC_PTR],Y    ; Second Pixel is Green
                STA @lGRPH_LUT0_PTR+1, X      ; The look-up Table point to a pixel Blue
                INY
                ; Blue Pixel
                LDA [BMP_PRSE_SRC_PTR],Y    ; Third Pixel is Blue
                STA @lGRPH_LUT0_PTR+2, X      ; The look-up Table point to a pixel Blue
                INY
                LDA #$80
                STA @lGRPH_LUT0_PTR+3, X      ; The look-up Table point to a pixel Blue
                INY ; For the Alpha Value, nobody cares
                INX
                INX
                INX
                INX
                CPX BMP_COLOR_PALET         ; Apparently sometime there is less than 256 Values in the lookup
                BNE BMP_PARSER_UPDATE_LOOP
                setal
                RTS


;
; This transfer the Palette Directly
; Will have to be improved, so it can load the LUT Data in any specific LUT - TBC
BMP_PARSER_UPDATE_LUT1
                SEC
                   ; And this is offset to where the Color Palette Begins
                LDY #$0036
                LDX #$0000
                setas
PALETTE_LUT1_LOOP
                ; RED Pixel
                LDA [BMP_PRSE_SRC_PTR],Y    ; First Pixel is Red
                STA @lGRPH_LUT1_PTR+0, X      ; The look-up Table point to a pixel Blue
                INY
                ; Green Pixel
                LDA [BMP_PRSE_SRC_PTR],Y    ; Second Pixel is Green
                STA @lGRPH_LUT1_PTR+1, X      ; The look-up Table point to a pixel Blue
                INY
                ; Blue Pixel
                LDA [BMP_PRSE_SRC_PTR],Y    ; Third Pixel is Blue
                STA @lGRPH_LUT1_PTR+2, X      ; The look-up Table point to a pixel Blue
                INY
                LDA #$80
                STA @lGRPH_LUT1_PTR+3, X      ; The look-up Table point to a pixel Blue
                INY ; For the Alpha Value, nobody cares
                INX
                INX
                INX
                INX
                CPX BMP_COLOR_PALET         ; Apparently sometime there is less than 256 Values in the lookup
                BNE PALETTE_LUT1_LOOP
                setal
                RTS

; Let's do it the easy way first, then we will implement a DMA Controller
BMP_PARSER_DMA_SHIT_OUT
                LDX #$0000
BMP_PARSER_LOOPY
                LDY #$0000
                setas
BMP_PARSER_LOOPX
                LDA [BMP_PRSE_SRC_PTR],Y    ; Load First Pixel Y (will be linear)
                STA [BMP_PRSE_DST_PTR],Y    ; This is where the Pixel Go, Video Memory
                INY
                CPY BMP_X_SIZE              ; Transfer the First line
                BNE BMP_PARSER_LOOPX
                JSR BMP_PARSER_COMPUTE_Y_SRC
                JSR BMP_PARSER_COMPUTE_Y_DST
                INX
                CPX BMP_Y_SIZE
                BNE BMP_PARSER_LOOPY
                RTS
; BMP_PRSE_SRC_PTR = BMP_PRSE_SRC_PTR + BMP_X_SIZE
BMP_PARSER_COMPUTE_Y_SRC
                setal
                ; The 32Bit ADDER is already Setup with Reg B with -(BMP_X_SIZE)
                ; So just load the Actual Value so it can be substracted again from BMP_X_SIZE
                LDA BMP_PRSE_SRC_PTR        ; Right now it is set @ $020000 (128K) + File Size
                STA @lADDER32_A_LL
                LDA BMP_PRSE_SRC_PTR+2      ; Right now it is set @ $020000 (128K)
                STA @lADDER32_A_HL
                ; And Zooom... The new Value is calculated... Yeah, Fuck I love the 32Bit Adder
                LDA @lADDER32_R_LL
                STA BMP_PRSE_SRC_PTR
                LDA @lADDER32_R_HL
                STA BMP_PRSE_SRC_PTR+2
                RTS
;BMP_PRSE_DST_PTR = BMP_PRSE_DST_PTR + Screen_Stride
BMP_PARSER_COMPUTE_Y_DST
                setal
                CLC
                LDA BMP_PRSE_DST_PTR
                ADC SCRN_X_STRIDE        ; In Normal Circumstances, it is 640
                STA BMP_PRSE_DST_PTR
                LDA BMP_PRSE_DST_PTR+2
                ADC #$0000
                STA BMP_PRSE_DST_PTR+2
                RTS


;////////////////////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////////////////////
; Interrupt Handler
;////////////////////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////////////////////
;////////////////////////////////////////////////////////////////////////////
IRQ_HANDLER
;                LDX #<>irq_Msg
;                JSL IPRINT       ; print the Init
                setas 					; Set 8bits
                ; This Clears all Pending
                ; This is very temporary, in reality this is Where
                ; there should be some parsing to know which Interrupt is Active
                ; And process it accordingly
                ; Clear Any pending Interrupt of Block0                
                LDA @lINT_PENDING_REG0 ; Clear the Pending INTERRUPT
                STA @lINT_PENDING_REG0 ; Clear the Pending INTERRUPT
                ; Clear Any pending Interrupt of Block1
                LDA @lINT_PENDING_REG1 ; Clear the Pending INTERRUPT
                STA @lINT_PENDING_REG1 ; Clear the Pending INTERRUPT
                ; Clear Any pending Interrupt of Block2
                LDA @lINT_PENDING_REG2 ; Clear the Pending INTERRUPT
                STA @lINT_PENDING_REG2 ; Clear the Pending INTERRUPT


                ldx #$0000
IRQ_HANDLER_FETCH
                LDA KBD_INPT_BUF        ; Get Scan Code from KeyBoard
                STA KEYBOARD_SC_TMP     ; Save Code Immediately
                ; Check for Shift Press or Unpressed
                CMP #$2A                ; Left Shift Pressed
                BEQ KB_SET_SHIFT
                CMP #$AA                ; Left Shift Unpressed
                BEQ KB_CLR_SHIFT

                ; Check for CTRL Press or Unpressed
                CMP #$1D                ; Left CTRL pressed
                BEQ KB_SET_CTRL
                CMP #$9D                ; Left CTRL Unpressed
                BNE KB_CHECK_ALT
                BRL KB_CLR_CTRL

KB_CHECK_ALT    CMP #$38                ; Left ALT Pressed
                BNE KB_ALT_UNPRESED
                BRL KB_SET_ALT
KB_ALT_UNPRESED CMP #$B8                ; Left ALT Unpressed
                BNE KB_UNPRESSED
                BRL KB_CLR_ALT


KB_UNPRESSED    AND #$80                ; See if the Scan Code is press or Depressed
                CMP #$80                ; Depress Status - We will not do anything at this point
                BNE KB_NORM_SC
                BRL KB_CHECK_B_DONE

KB_NORM_SC      LDA KEYBOARD_SC_TMP       ;
                TAX
                LDA KEYBOARD_SC_FLG     ; Check to See if the SHIFT Key is being Pushed
                AND #$10
                CMP #$10
                BEQ SHIFT_KEY_ON

                LDA KEYBOARD_SC_FLG     ; Check to See if the CTRL Key is being Pushed
                AND #$20
                CMP #$20
                BEQ CTRL_KEY_ON

                LDA KEYBOARD_SC_FLG     ; Check to See if the ALT Key is being Pushed
                AND #$40
                CMP #$40
                BEQ ALT_KEY_ON
                ; Pick and Choose the Right Bank of Character depending if the Shift/Ctrl/Alt or none are chosen
                LDA @lScanCode_Press_Set1, x
                BRL KB_WR_2_SCREEN
SHIFT_KEY_ON    LDA @lScanCode_Shift_Set1, x
                BRL KB_WR_2_SCREEN
CTRL_KEY_ON     LDA @lScanCode_Ctrl_Set1, x
                BRL KB_WR_2_SCREEN
ALT_KEY_ON      LDA @lScanCode_Alt_Set1, x

                ; Write Character to Screen (Later in the buffer)
KB_WR_2_SCREEN
                JSL PUTC
                JMP KB_CHECK_B_DONE

KB_SET_SHIFT    LDA KEYBOARD_SC_FLG
                ORA #$10
                STA KEYBOARD_SC_FLG
                JMP KB_CHECK_B_DONE

KB_CLR_SHIFT    LDA KEYBOARD_SC_FLG
                AND #$EF
                STA KEYBOARD_SC_FLG
                JMP KB_CHECK_B_DONE

KB_SET_CTRL    LDA KEYBOARD_SC_FLG
                ORA #$20
                STA KEYBOARD_SC_FLG
                JMP KB_CHECK_B_DONE

KB_CLR_CTRL    LDA KEYBOARD_SC_FLG
                AND #$DF
                STA KEYBOARD_SC_FLG
                JMP KB_CHECK_B_DONE

KB_SET_ALT      LDA KEYBOARD_SC_FLG
                ORA #$40
                STA KEYBOARD_SC_FLG
                JMP KB_CHECK_B_DONE

KB_CLR_ALT     LDA KEYBOARD_SC_FLG
                AND #$BF
                STA KEYBOARD_SC_FLG

KB_CHECK_B_DONE .as
                LDA STATUS_PORT
                AND #OUT_BUF_FULL ; Test bit $01 (if 1, Full)
                CMP #OUT_BUF_FULL ; if Still Byte in the Buffer, fetch it out
                BNE KB_DONE
                JMP IRQ_HANDLER_FETCH

KB_DONE
                setaxl
                RTL





NMI_HANDLER
                LDX #<>nmi_Msg
                JSL IPRINT       ; print the Init
                RTL
;
;Not-implemented routines
;
IRESTORE        BRK ; Warm boot routine
ISCINIT         BRK ;
IIOINIT         BRK ;
IPUTBLOCK       BRK ; Ouput a binary block to the currently selected channel
ISETLFS         BRK ; Obsolete (done in OPEN)
ISETNAM         BRK ; Obsolete (done in OPEN)
IOPEN           BRK ; Open a channel for reading and/or writing. Use SETLFS and SETNAM to set the channels and filename first.
ICLOSE          BRK ; Close a channel
ISETIN          BRK ; Set the current input channel
ISETOUT         BRK ; Set the current output channel
IGETB           BRK ; Get a byte from input channel. Return 0 if no input. Carry is set if no input.
IGETBLOCK       BRK ; Get a X byes from input channel. If Carry is set, wait. If Carry is clear, do not wait.
IGETCH          BRK ; Get a character from the input channel. A=0 and Carry=1 if no data is wating
IGETS           BRK ; Get a string from the input channel. NULL terminates
IGETLINE        BRK ; Get a line of text from input channel. CR or NULL terminates.
IGETFIELD       BRK ; Get a field from the input channel. Value in A, CR, or NULL terminates
ITRIM           BRK ; Removes spaces at beginning and end of string.
IPRINTC         BRK ; Print character to screen. Handles terminal commands
IPRINTS         BRK ; Print string to screen. Handles terminal commands
IPRINTF         BRK ; Print a float value
IPRINTI         BRK ; Prints integer value in TEMP
IPRINTAI        BRK ; Prints integer value in A
IPRINTAH        BRK ; Prints hex value in A. Printed value is 2 wide if M flag is 1, 4 wide if M=0
IPUSHKEY        BRK ;
IPUSHKEYS       BRK ;
ICSRLEFT        BRK ;
ICSRHOME        BRK ;
ISCRREADLINE    BRK ; Loads the MCMDADDR/BCMDADDR variable with the address of the current line on the screen. This is called when the RETURN key is pressed and is the first step in processing an immediate mode command.
ISCRGETWORD     BRK ; Read a current word on the screen. A word ends with a space, punctuation (except _), or any control character (value < 32). Loads the address into CMPTEXT_VAL and length into CMPTEXT_LEN variables.

;
; Greeting message and other kernel boot data
;
KERNEL_DATA
greet_msg       .text $20, $20, $20, $20, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, "C256 FOENIX DEVELOPMENT SYSTEM",$0D
                .text $20, $20, $20, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $20, "8/16 Bits OPEN SOURCE COMPUTER",$0D
                .text $20, $20, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $20, $20, "PCB Revision B2",$0D
                .text $20, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $20, $20, $20, "Created by: STEFANY ALLAIRE",$0D
                .text $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $EC, $A9, $20, $20, $20, $20, "2048KB CODE RAM  4096K VIDEO MEM",$00

greet_clr_line1 .text $1D, $1D, $1D, $1D, $1D, $1D, $8D, $8D, $4D, $4D, $2D, $2D, $5D, $5D
greet_clr_line2 .text $1D, $1D, $1D, $1D, $1D, $8D, $8D, $4D, $4D, $2D, $2D, $5D, $5D, $5D
greet_clr_line3 .text $1D, $1D, $1D, $1D, $8D, $8D, $4D, $4D, $2D, $2D, $5D, $5D, $5D, $5D
greet_clr_line4 .text $1D, $1D, $1D, $8D, $8D, $4D, $4D, $2D, $2D, $5D, $5D, $5D, $5D, $5D
greet_clr_line5 .text $1D, $1D, $8D, $8D, $4D, $4D, $2D, $2D, $5D, $5D, $5D, $5D, $5D, $5D

fg_color_lut	  .text $00, $00, $00, $FF
                .text $00, $00, $80, $FF
                .text $00, $80, $00, $FF
                .text $80, $00, $00, $FF
                .text $00, $80, $80, $FF
                .text $80, $80, $00, $FF
                .text $80, $00, $80, $FF
                .text $80, $80, $80, $FF
                .text $00, $45, $FF, $FF
                .text $13, $45, $8B, $FF
                .text $00, $00, $20, $FF
                .text $00, $20, $00, $FF
                .text $20, $00, $00, $FF
                .text $20, $20, $20, $FF
                .text $40, $40, $40, $FF
                .text $FF, $FF, $FF, $FF

bg_color_lut	  .text $00, $00, $00, $FF
                .text $00, $00, $80, $FF
                .text $00, $80, $00, $FF
                .text $80, $00, $00, $FF
                .text $00, $20, $20, $FF
                .text $20, $20, $00, $FF
                .text $20, $00, $20, $FF
                .text $20, $20, $20, $FF
                .text $1E, $69, $D2, $FF
                .text $13, $45, $8B, $FF
                .text $00, $00, $20, $FF
                .text $00, $20, $00, $FF
                .text $40, $00, $00, $FF
                .text $10, $10, $10, $FF
                .text $40, $40, $40, $FF
                .text $FF, $FF, $FF, $FF

version_msg     .text $0D, "Debug Code Version 0.0.15 - Feb 22th, 2019", $00
init_lpc_msg    .text "Init SuperIO...", $0D, $00
init_kbrd_msg   .text "Init Keyboard...", $0D, $00
init_rtc_msg    .text "Init RTC...", $0D, $00
test_SID_msg    .text "Testing Right & Left SID", $0D, $00
pass_tst0xAAmsg .text "Cmd 0xAA Test passed...", $0D, $00
pass_tst0xABmsg .text "Cmd 0xAB Test passed...", $0D, $00
pass_cmd0x60msg .text "Cmd 0x60 Executed.", $0D, $00
pass_cmd0xFFmsg .text "Cmd 0xFF (Reset) Done.", $0D, $00
pass_cmd0xEEmsg .text "Cmd 0xEE Echo Test passed...", $0D, $00
Success_kb_init .text "Keyboard Present", $0D, $00
Failed_kb_init  .text "No Keyboard Attached or Failed Init...", $0D, $00
irq_Msg         .text "[IRQ Interrupt]", $0D, $00
nmi_Msg         .text "[NMI Interrupt]", $0D, $00
splashfilename  .text "/FOENIX.BMP", $00
tileset0_fname  .text "/TILE0.BMP", $00
bmp_parser_err0 .text "NO SIGNATURE FOUND.", $00
bmp_parser_msg0 .text "BMP LOADED.", $00
bmp_parser_msg1 .text "EXECUTING BMP PARSER", $00
OPL2_test_msg   .text "OPL2 TONE TEST... CAN YOU HEAR IT?", $00
tile_msg1       .text "LOADING TILE PAGE 0", $00

ready_msg       .null $0D,"READY."
hello_basic     .null "10 PRINT ""Hello World""",$0D
                .null "RUN",$0D
                .null "Hello World",$0D
                .null $0D,"READY."
hello_ml        .null "G 020000",$0D
                .null "HELLO WORLD",$0D
                .null $0D
                .null " PC     A    X    Y    SP   DBR DP   NVMXDIZC",$0D
                .null ";002112 0019 F0AA 0000 D6FF F8  0000 --M-----"
error_01        .null "ABORT ERROR"
hex_digits      .text "0123456789ABCDEF",0

;                           $00, $01, $02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F
ScanCode_Press_Set1   .text $00, $1B, $31, $32, $33, $34, $35, $36, $37, $38, $39, $30, $2D, $3D, $08, $09    ; $00
                      .text $71, $77, $65, $72, $74, $79, $75, $69, $6F, $70, $5B, $5D, $0D, $00, $61, $73    ; $10
                      .text $64, $66, $67, $68, $6A, $6B, $6C, $3B, $27, $60, $00, $5C, $7A, $78, $63, $76    ; $20
                      .text $62, $6E, $6D, $2C, $2E, $2F, $00, $2A, $00, $20, $00, $00, $00, $00, $00, $00    ; $30
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $40
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $50
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $60
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $70

ScanCode_Shift_Set1   .text $00, $00, $21, $40, $23, $24, $25, $5E, $26, $2A, $28, $29, $5F, $2B, $08, $09    ; $00
                      .text $51, $57, $45, $52, $54, $59, $55, $49, $4F, $50, $7B, $7D, $0D, $00, $41, $53    ; $10
                      .text $44, $46, $47, $48, $4A, $4B, $4C, $3A, $22, $7E, $00, $5C, $5A, $58, $43, $56    ; $20
                      .text $42, $4E, $4D, $3C, $3E, $3F, $00, $2A, $00, $20, $00, $00, $00, $00, $00, $00    ; $30
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $40
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $50
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $60
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $70

ScanCode_Ctrl_Set1    .text $00, $1B, $31, $32, $33, $34, $35, $36, $37, $38, $39, $30, $2D, $3D, $08, $09    ; $00
                      .text $71, $77, $65, $72, $74, $79, $75, $69, $6F, $70, $5B, $5D, $0D, $00, $61, $73    ; $10
                      .text $64, $66, $67, $68, $6A, $6B, $6C, $3B, $27, $60, $00, $5C, $7A, $78, $63, $76    ; $20
                      .text $62, $6E, $6D, $2C, $2E, $2F, $00, $2A, $00, $20, $00, $00, $00, $00, $00, $00    ; $30
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $40
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $50
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $60
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $70

ScanCode_Alt_Set1     .text $00, $1B, $31, $32, $33, $34, $35, $36, $37, $38, $39, $30, $2D, $3D, $08, $09    ; $00
                      .text $71, $77, $65, $72, $74, $79, $75, $69, $6F, $70, $5B, $5D, $0D, $00, $61, $73    ; $10
                      .text $64, $66, $67, $68, $6A, $6B, $6C, $3B, $27, $60, $00, $5C, $7A, $78, $63, $76    ; $20
                      .text $62, $6E, $6D, $2C, $2E, $2F, $00, $2A, $00, $20, $00, $00, $00, $00, $00, $00    ; $30
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $40
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $50
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $60
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $70

ScanCode_NumLock_Set1 .text $00, $1B, $31, $32, $33, $34, $35, $36, $37, $38, $39, $30, $2D, $3D, $08, $09    ; $00
                      .text $71, $77, $65, $72, $74, $79, $75, $69, $6F, $70, $5B, $5D, $0D, $00, $61, $73    ; $10
                      .text $64, $66, $67, $68, $6A, $6B, $6C, $3B, $27, $60, $00, $5C, $7A, $78, $63, $76    ; $20
                      .text $62, $6E, $6D, $2C, $2E, $2F, $00, $2A, $00, $20, $00, $00, $00, $00, $00, $00    ; $30
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $40
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $50
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $60
                      .text $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00    ; $70
; Gamma Table 2.2
.align 256
GAMMA_2_2_Tbl         .text  $00, $14, $1c, $21, $26, $2a, $2e, $31, $34, $37, $3a, $3d, $3f, $41, $44, $46
                      .text  $48, $4a, $4c, $4e, $50, $51, $53, $55, $57, $58, $5a, $5b, $5d, $5e, $60, $61
                      .text  $63, $64, $66, $67, $68, $6a, $6b, $6c, $6d, $6f, $70, $71, $72, $73, $75, $76
                      .text  $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $80, $81, $82, $83, $84, $85, $86, $87
                      .text  $88, $88, $89, $8a, $8b, $8c, $8d, $8e, $8f, $90, $91, $92, $93, $93, $94, $95
                      .text  $96, $97, $98, $99, $99, $9a, $9b, $9c, $9d, $9e, $9e, $9f, $a0, $a1, $a2, $a2
                      .text  $a3, $a4, $a5, $a5, $a6, $a7, $a8, $a8, $a9, $aa, $ab, $ab, $ac, $ad, $ae, $ae
                      .text  $AF, $b0, $b0, $b1, $b2, $b2, $b3, $b4, $b5, $b5, $b6, $b7, $b7, $b8, $b9, $b9
                      .text  $ba, $bb, $bb, $bc, $bd, $bd, $be, $be, $bf, $c0, $c0, $c1, $c2, $c2, $c3, $c4
                      .text  $c4, $c5, $c5, $c6, $c7, $c7, $c8, $c8, $c9, $ca, $ca, $cb, $cb, $cc, $cd, $cd
                      .text  $ce, $ce, $cf, $d0, $d0, $d1, $d1, $d2, $d2, $d3, $d4, $d4, $d5, $d5, $d6, $d6
                      .text  $d7, $d8, $d8, $d9, $d9, $da, $da, $db, $db, $dc, $dc, $dd, $de, $de, $df, $df
                      .text  $e0, $e0, $e1, $e1, $e2, $e2, $e3, $e3, $e4, $e4, $e5, $e5, $e6, $e6, $e7, $e7
                      .text  $e8, $e8, $e9, $e9, $ea, $ea, $eb, $eb, $ec, $ec, $ed, $ed, $ee, $ee, $ef, $ef
                      .text  $f0, $f0, $f1, $f1, $f2, $f2, $f3, $f3, $f4, $f4, $f5, $f5, $f6, $f6, $f7, $f7
                      .text  $f8, $f8, $f9, $f9, $f9, $fa, $fa, $fb, $fb, $fc, $fc, $fd, $fd, $fe, $fe, $ff
.align 256
GAMMA_1_8_Tbl         .text  $00, $0b, $11, $15, $19, $1c, $1f, $22, $25, $27, $2a, $2c, $2e, $30, $32, $34
                      .text  $36, $38, $3a, $3c, $3d, $3f, $41, $43, $44, $46, $47, $49, $4a, $4c, $4d, $4f
                      .text  $50, $51, $53, $54, $55, $57, $58, $59, $5b, $5c, $5d, $5e, $60, $61, $62, $63
                      .text  $64, $65, $67, $68, $69, $6a, $6b, $6c, $6d, $6e, $70, $71, $72, $73, $74, $75
                      .text  $76, $77, $78, $79, $7a, $7b, $7c, $7d, $7e, $7f, $80, $81, $82, $83, $84, $84
                      .text  $85, $86, $87, $88, $89, $8a, $8b, $8c, $8d, $8e, $8e, $8f, $90, $91, $92, $93
                      .text  $94, $95, $95, $96, $97, $98, $99, $9a, $9a, $9b, $9c, $9d, $9e, $9f, $9f, $a0
                      .text  $a1, $a2, $a3, $a3, $a4, $a5, $a6, $a6, $a7, $a8, $a9, $aa, $aa, $ab, $ac, $ad
                      .text  $ad, $ae, $af, $b0, $b0, $b1, $b2, $b3, $b3, $b4, $b5, $b6, $b6, $b7, $b8, $b8
                      .text  $b9, $ba, $bb, $bb, $bc, $bd, $bd, $be, $bf, $bf, $c0, $c1, $c2, $c2, $c3, $c4
                      .text  $c4, $c5, $c6, $c6, $c7, $c8, $c8, $c9, $ca, $ca, $cb, $cc, $cc, $cd, $ce, $ce
                      .text  $cf, $d0, $d0, $d1, $d2, $d2, $d3, $d4, $d4, $d5, $d6, $d6, $d7, $d7, $d8, $d9
                      .text  $d9, $da, $db, $db, $dc, $dc, $dd, $de, $de, $df, $e0, $e0, $e1, $e1, $e2, $e3
                      .text  $e3, $e4, $e4, $e5, $e6, $e6, $e7, $e7, $e8, $e9, $e9, $ea, $ea, $eb, $ec, $ec
                      .text  $ed, $ed, $ee, $ef, $ef, $f0, $f0, $f1, $f1, $f2, $f3, $f3, $f4, $f4, $f5, $f5
                      .text  $f6, $f7, $f7, $f8, $f8, $f9, $f9, $fa, $fb, $fb, $fc, $fc, $fd, $fd, $fe, $ff
.align 256
RANDOM_LUT_Tbl		    .text  $1d, $c8, $a7, $ac, $10, $d6, $52, $7c, $83, $dd, $ce, $39, $cd, $c5, $3b, $15
				              .text  $22, $55, $3b, $94, $e0, $33, $1f, $38, $87, $12, $31, $65, $89, $27, $88, $42
				              .text  $b2, $32, $72, $84, $b2, $b2, $31, $52, $94, $ce, $56, $ec, $fe, $da, $58, $c9
				              .text  $c8, $5b, $53, $2a, $08, $3b, $19, $c1, $d0, $10, $2c, $b2, $4b, $ea, $32, $61
				              .text  $da, $34, $33, $8f, $2b, $da, $49, $89, $a1, $e6, $ca, $2d, $b3, $ce, $b0, $79
				              .text  $44, $aa, $32, $82, $91, $e9, $29, $16, $5f, $e3, $fb, $bd, $15, $2e, $be, $f5
				              .text  $e9, $4a, $e4, $2e, $60, $24, $94, $35, $8d, $8f, $2c, $80, $0a, $5e, $99, $36
				              .text  $ac, $ab, $21, $26, $42, $7c, $5e, $bc, $13, $52, $44, $2f, $e3, $ef, $44, $a2
				              .text  $86, $c1, $9c, $47, $5f, $36, $6d, $02, $be, $23, $02, $58, $0a, $52, $5e, $b4
				              .text  $9f, $06, $08, $c9, $97, $cb, $9e, $dd, $d5, $cf, $3e, $df, $c4, $9e, $da, $bb
				              .text  $9b, $5d, $c9, $f5, $d9, $c3, $7e, $87, $77, $7d, $b1, $3b, $4a, $68, $35, $6e
				              .text  $ee, $47, $ad, $8f, $fd, $73, $2e, $46, $b5, $8f, $44, $63, $55, $6f, $e1, $50
				              .text  $f4, $b6, $a3, $4f, $68, $c4, $a5, $a4, $57, $74, $b9, $bd, $05, $14, $50, $eb
				              .text  $a5, $5c, $57, $2f, $99, $dc, $2e, $8a, $44, $bc, $ec, $db, $22, $58, $fc, $be
				              .text  $5f, $3f, $50, $bd, $2a, $36, $ab, $ae, $24, $aa, $82, $11, $5c, $9f, $43, $4d
				              .text  $8f, $0c, $20, $00, $91, $b6, $45, $9e, $3e, $3d, $66, $7e, $0a, $1c, $6b, $74

TILE_TEST_CH00        .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
;
;
TILE_TEST_CH01        .text  $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $00, $33, $33, $00, $00, $00, $00, $00, $00, $00, $00, $33, $33, $00, $24
                      .text  $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
;
;
TILE_TEST_CH02        .text  $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24
                      .text  $24, $55, $55, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $55, $55, $24
                      .text  $24, $00, $55, $55, $00, $00, $00, $00, $00, $00, $00, $00, $55, $55, $00, $24
                      .text  $24, $00, $00, $55, $55, $00, $00, $00, $00, $00, $00, $55, $55, $00, $00, $24
                      .text  $24, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $24
                      .text  $24, $00, $00, $00, $00, $55, $55, $00, $00, $55, $55, $00, $00, $00, $00, $24
                      .text  $24, $00, $00, $00, $00, $00, $55, $55, $55, $55, $00, $00, $00, $00, $00, $24
                      .text  $24, $00, $00, $00, $00, $00, $00, $55, $55, $00, $00, $00, $00, $00, $00, $24
                      .text  $24, $00, $00, $00, $00, $00, $55, $55, $55, $55, $00, $00, $00, $00, $00, $24
                      .text  $24, $00, $00, $00, $00, $55, $55, $00, $00, $55, $55, $00, $00, $00, $00, $24
                      .text  $24, $00, $00, $00, $55, $55, $00, $00, $00, $00, $55, $55, $00, $00, $00, $24
                      .text  $24, $00, $00, $55, $55, $00, $00, $00, $00, $00, $00, $55, $55, $00, $00, $24
                      .text  $24, $00, $55, $55, $00, $00, $00, $00, $00, $00, $00, $00, $55, $55, $00, $24
                      .text  $24, $55, $55, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $55, $55, $24
                      .text  $24, $55, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $55, $24
                      .text  $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24, $24




* = $1A0000
TILE0_MAP
.binary "Graphics/TILE1.data.bin", 0, 65536
* = $1B0000
TILE0_PALETTE
.binary "Graphics/TILE1.data.pal.bin"        ;simple include, all bytes
JUMPMAN_SPRITES
JUMPMAN_SPRITES0
.binary "Graphics/All_Letter_Sprites1.data.bin"
JUMPMAN_SPRITES1
.binary "Graphics/All_Letter_Sprites2.data.bin"
JUMPMAN_SPRITES2
.binary "Graphics/All_Letter_Sprites3.data.bin"
JUMPMAN_SPRITES3
.binary "Graphics/All_Letter_Sprites4.data.bin"
JUMPMAN_SPRITES4
.binary "Graphics/All_Letter_Sprites5.data.bin"
JUMPMAN_SPRITES5
.binary "Graphics/All_Letter_Sprites6.data.bin"
JUMPMAN_SPRITES6
.binary "Graphics/All_Letter_Sprites7.data.bin"
JUMPMAN_SPRITES7
.binary "Graphics/All_Letter_Sprites8.data.bin"
JUMPMAN_SPRITES8
.binary "Graphics/All_Letter_Sprites9.data.bin"
JUMPMAN_SPRITES9
.binary "Graphics/All_Letter_Sprites10.data.bin"
JUMPMAN_SPRITES10
.binary "Graphics/All_Letter_Sprites11.data.bin"
JUMPMAN_SPRITES11
.binary "Graphics/All_Letter_Sprites12.data.bin"
JUMPMAN_SPRITES12
.binary "Graphics/All_Letter_Sprites13.data.bin"
JUMPMAN_SPRITES13
.binary "Graphics/All_Letter_Sprites14.data.bin"
JUMPMAN_SPRITES14
.binary "Graphics/All_Letter_Sprites15.data.bin"
JUMPMAN_SPRITES15
.binary "Graphics/All_Letter_Sprites16.data.bin"
JUMPMAN_SPRITES16
.binary "Graphics/All_Letter_Sprites17.data.bin"
.align 256
TILE_MAP_CONTENT0
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $63, $64, $65, $66, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $87, $88, $89,	$8A, $8B, $0E, $00, $73, $74, $75, $76, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $97, $98, $99, $9A, $9B, $1E, $1F, $00, $59, $5A, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $83, $84,	$85, $00, $00, $00, $00, $A7, $A8, $A9, $AA, $AB, $2E, $00, $5B, $5C, $5D, $5E, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $93, $94, $95, $00, $00, $00, $B6, $B7, $B8, $B9,	$BA, $BB, $3E, $00, $67, $68, $69, $6A, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $01, $01, $05, $06, $01, $01, $01, $01,	$01, $01, $01, $01, $01, $01, $A3, $A4, $A5, $01, $01, $01, $C6, $C7, $C8, $C9, $CA, $CB, $4E, $01, $6B, $6C, $6D, $6E,	$01, $01, $01, $01, $05, $06, $01, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $02, $00, $00, $02, $00, $00, $00, $02, $00, $02, $00,	$08, $00, $02, $02, $00, $00, $02, $02, $00, $00, $00, $00, $00, $00, $00, $02, $00, $02, $00, $00, $03, $04, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $02, $00, $00, $02, $00, $00, $00, $02, $00, $86, $00, $08, $00, $0A, $0B, $00, $00, $02, $02,	$00, $00, $00, $00, $00, $00, $00, $02, $00, $02, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $02, $00, $00,	$14, $00, $00, $00, $02, $00, $96, $00, $08, $00, $1A, $1B, $39, $00, $02, $02, $00, $00, $00, $00, $00, $00, $00, $02,	$00, $02, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $02, $00, $00, $24, $15, $43, $17, $27, $00, $A6, $00,	$08, $00, $2A, $2B, $49, $00, $02, $02, $00, $60, $61, $62, $00, $00, $00, $8C, $00, $02, $00, $00, $03, $04, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $10, $11, $12, $13, $34, $25, $53, $27, $28, $19, $00, $00, $08, $00, $0C, $0D, $00, $3A, $3B, $3C,	$3D, $70, $71, $72, $00, $EB, $EC, $9C, $00, $02, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $20, $21, $22, $23,	$44, $35, $00, $37, $38, $29, $00, $00, $08, $00, $1C, $1D, $00, $50, $00, $00, $51, $80, $81, $82, $00, $FB, $FC, $8D,	$8E, $8F, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $30, $31, $32, $33, $54, $45, $16, $47, $48, $00, $00, $00,	$08, $00, $2C, $2D, $00, $4A, $4B, $4C, $4D, $90, $91, $92, $00, $00, $AC, $9D, $9E, $9F, $00, $00, $03, $04, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $40, $41, $42, $00, $00, $55, $56, $57, $58, $00, $00, $00, $08, $00, $00, $00, $00, $00, $77, $78,	$00, $A0, $A1, $A2, $00, $00, $BC, $AD, $AE, $AF, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $08, $00, $00, $00, $00, $00, $79, $7A, $00, $00, $52, $00, $00, $00, $CC, $BD,	$BE, $BF, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$08, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $DC, $CD, $CE, $CF, $4F, $00, $03, $04, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $DD, $DE, $DF, $5F, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $ED,	$EE, $EF, $6F, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $B0, $B1, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FD, $FE, $FF, $00, $00, $03, $04, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $C0, $C1, $C2, $C3, $C4, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $D0, $D1, $D2, $D3, $D4, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $D5, $D6, $D7, $D8,	$D9, $DA, $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $03, $04, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E0, $E1, $E2,	$E3, $E4, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $E5, $E6, $E7, $E8, $E9, $EA, $00, $00, $03, $04, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $01, $01, $05, $06, $01, $01, $01, $01, $01, $01, $01, $01, $3F, $F0, $F1, $F2, $F3, $F4, $B2, $01, $01, $01, $01, $01,	$01, $01, $01, $01, $F5, $F6, $F7, $F8, $F9, $FA, $01, $01, $05, $06, $01, $01, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
                      .text  $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,	$00, $00, $00, $00, $00, $00, $00, $00
