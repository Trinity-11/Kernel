.cpu "65816"
.include "Floppy_def.asm"

* = $1A000

;-------------------------------------------------------------------------------
;
; setaxl
; LDA #`DESTINATION_BUFFER ; load the byte nb 3 (bank byte)
; PHA
; LDA #<>DESTINATION_BUFFER ; load the low world part of the buffer address
; PHA
; LDA $0 ; read sector 0
; JSL IFDD_READ ;
;
;
;-------------------------------------------------------------------------------

IFDD_INIT       JSL IFDD_RESET      ; Reset FDD : No DMA, Drive 0 selected, no motor activated
                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #1
                TSB FDD_DIGITAL_OUTPUT
                BRK
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_RESET_SOFT setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #FDD_nRESET             ; Load the reset bit to be clear
                TRB FDD_DIGITAL_OUTPUT      ; Clear the reset bit to go in reset mode
                NOP                         ; wait, the doc say 100ns min
                NOP
                NOP
                NOP
                TSB FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode
                BRK
;-------------------------------------------------------------------------------
IFDD_RESET      LDA #0                      ; Will set all the bit at 0 to reset everyting
                STA  FDD_DIGITAL_OUTPUT
                NOP                         ; wait, the doc say 100ns min
                NOP
                NOP
                NOP
                LDA #FDD_nRESET
                STA FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode
                BRK
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_READ       setaxl
                PHA ; save the sector to read
                LDA 8,S
                TAY
                LDA 6,S
                TAY
                PLA
                PHA ; save the sector read for the return value
                ASL A ; convert the sector number into byte count
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A

                ADC #<>data_floppy
                TAX

                ;LDX #<>data_floppy
                ; LDY #<>FAT12_ADDRESS_BUFFER_512
                setas
                LDA 8,S
                STA FFD_MVN_INSTRUCTION_ADDRESS + 2 ; rewrite the second parameter of the instruction in RAM
                setaxl
                LDA #511
FFD_MVN_INSTRUCTION_ADDRESS  MVN `FAT12_ADDRESS_BUFFER_512,`data_floppy
                PLA
                RTL

IFDD_READ_ORI   setaxl
                PHA
                LDA 8,S
                TAX
                LDA 6,S
                TAY
                PLA
                PHA ; save the sector read for the return value
                ASL A ; convert the sector number into byte count
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A
                ASL A

                ADC #<>data_floppy
                TAX
                LDA #511
                ;LDX #<>data_floppy
                LDY #<>FAT12_ADDRESS_BUFFER_512
                MVN `FAT12_ADDRESS_BUFFER_512,`data_floppy
                PLA
                RTL
IFDD_WRITE      BRK
IFDD_SETSECTOR  BRK
IFDD_SETTRACK  BRK
IFDD_SETSIDE    BRK
IFDD_RECALIGRATE BRK
IFDD_SEEK       BRK
IFDD_SEEKRELATIF BRK

.include "FDD_row_TEXT_HEX.asm"
