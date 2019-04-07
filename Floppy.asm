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

IFDD_INIT       BRK
IFDD_RESET      BRK

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
                STA FFD_MVN_INSTRUCTION_ADDRESS + 2 ; rewrite the second parameter in RAM
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
IFDD_DSETTRACK  BRK
IFDD_SETSIDE    BRK
IFDD_RECALIGRATE BRK
IFDD_SEEK       BRK
IFDD_SEEKRELATIF BRK

.include "FDD_row_TEXT_HEX.asm"
