.cpu "65816"
.include "Floppy_def.asm"

FLOPPY_CMD_BUFFER = $1A000 ; 10 Byte buffer for the command to be send to the FDC and the the data recieved as e result of the command
* = $1A00A
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

IFDD_INIT_AT    JSL IFDD_RESET_FULL         ; Reset FDD : No DMA, Drive 0 selected, no motor activated
                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #1
                TSB FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode  "Test and Reset Memory Bits Against Accumulator"
                BRK
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_RESET      setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #FDD_nRESET             ; reset the floppy disc controler, deactive all the motors and the DMA
                TRB FDD_DIGITAL_OUTPUT      ; Clear the reset bit to go in reset mode "Test and Reset Memory Bits Against Accumulator"
                NOP                         ; wait until the rest is done, the doc say 100ns min
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                ; reset the DATA_RATE_SELECT register : automatic low Power, Pres Comp => Default See tab 10 in FDC doc, Data Rate to 250 Kbps
                setdbr `FDD_DATA_RATE_SELECT
                LDA #2
                STA FDD_DATA_RATE_SELECT
                ; if in mode PC/AT or PS/2 the datarate is set in Config Control Register
                setdbr `FDD_CONFIG_CTRL
                LDA #2
                STA FDD_CONFIG_CTRL
                ; exit the reset mode
                setdbr `FDD_DIGITAL_OUTPUT
                LDA #FDD_nRESET             ; Load the reset bit to be set
                TSB FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode  "Test and Reset Memory Bits Against Accumulator"
                BRK
;-------------------------------------------------------------------------------
IFDD_RESET_FULL LDA #0                      ; Will set all the bit at 0 to reset everyting
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
;-------------------------------------------------------------------------------
; Reg A contain the Floppy driver to recalibrate, bring the hrad to the track 0
; by sensing the track0 pin of the FDD
;-------------------------------------------------------------------------------
IFDD_RECALIBRATE
                AND #3                    ; just get the 2 first bit
                STA FLOPPY_CMD_BUFFER+1
                LDA #7
                STA FLOPPY_CMD_BUFFER
                LDA #2
                JMP IFDD_SEND_CMD
                CMP #0
                BMI IFDD_RECALIBRATE_ERROR_SEND_CMD
                LDA #1
                BRA IFDD_RECALIBRATE_DONE
IFDD_RECALIBRATE_ERROR_SEND_CMD
                LDA #-1
                BRA IFDD_RECALIBRATE_ERROR
IFDD_RECALIBRATE_ERROR
IFDD_RECALIBRATE_DONE
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_SEEK       BRK
IFDD_SEEKRELATIF BRK

;-------------------------------------------------------------------------------
; Reg A contain the number of data to send to the Floppy Disc Controler
; The data are in FLOPPY_CMD_BUFFER at address $1A000
; The result is also places in FLOPPY_CMD_BUFFER, the command is over writen
;-------------------------------------------------------------------------------
IFDD_SEND_CMD   setdbr `FDD_MAIN_STATUE
IFDD_SEND_CMD_READ_MAIN_STATUS_REG
                PHA                       ; save the number of byt to be sent to the FDC
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                PHA                       ; Save the Maine Status value
                AND #$80                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_SEND_CMD_TRANSFERT_CAN_BE_DONE ;
                NOP
                NOP
                NOP
                BRA IFDD_SEND_CMD_READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------------ the FDC is now avaliable for transfert -----------
IFDD_SEND_CMD_TRANSFERT_CAN_BE_DONE
                LDA #1, S                 ; get the Main Status avlue
                AND #$40                  ; get DIO bit
                CMP #$40                  ; if == 0 we can write data into the FIFO, if == 1 we need to read data
                BNE IFDD_SEND_CMD_READDY_TO_SEND_DATA;
                LDA FDD_FIFO
                PLA                       ; remove the Main Status value saved
                BRA IFDD_SEND_CMD_READ_MAIN_STATUS_REG  ; retest if we can send data now#
                ;--------------- the FDC can now recuive command ---------------
IFDD_SEND_CMD_READDY_TO_SEND_DATA
                PLA                       ; remove the Main Status value saved
                LDX #0
SEND_NEXT_DATA  LDA X
                CMP #1, S                 ; Test if we sent all the data ot not
                BPL ALL_DATA_SENT
                LDA FLOPPY_CMD_BUFFER,X
                STA FDD_FIFO              ; Write the data in the FDC's FIFO
                INX
                setdbr `FDD_MAIN_STATUE   ; assume the FDC will never ask to read data while we are sendin the command
READ_MAIN_STATUS_REG_FOR_TRANSFERT
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #$80                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ SEND_NEXT_DATA
                NOP
                NOP
                NOP
                BRA READ_MAIN_STATUS_REG_FOR_TRANSFERT  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------ The command is sent now we need to read the result -----
                ;------ so west the dqta avaliable bit                     -----
ALL_DATA_SENT
IFDD_SEND_CMD_RETURN_ERROR
                RTL
;-------------------------------------------------------------------------------
; Reg A contain the number of data to read from the Floppy Disc Controler
; The data will be stored in FLOPPY_CMD_BUFFER at address $1A000
;-------------------------------------------------------------------------------
IFDD_READ_CMD_RESULT
                setdbr `FDD_MAIN_STATUE
IFDD_READ_CMD_RESULT_READ_MAIN_STATUS_REG
                PHA                       ; save the number of byt to be sent to the FDC
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                PHA                       ; Save the Maine Status value
                AND #$80                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_READ_CMD_TRANSFERT_CAN_BE_DONE ;
                NOP
                NOP
                NOP
                BRA IFDD_READ_CMD_RESULT_READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------------ the FDC is now avaliable for transfert -----------
IFDD_READ_CMD_TRANSFERT_CAN_BE_DONE
                LDA FDD_MAIN_STATUE
                AND #$40                  ; get DIO bit
                CMP #$40                  ; if == 0 we can write data into the FIFO, if == 1 we need to read data
                BNE READDY_TO_READ_DATA   ; We want to read the result of the command
                LDA #-1                   ; error, the FDC after reciving the commans is suppos to sent you data
                BRA IFDD_READ_CMD_RESULT_RETURN_ERROR
READDY_TO_READ_DATA
                LDX #0
READ_NEXT_DATA  LDA X
                CMP #1, S                 ; Test if we read all the data ot not
                BPL ALL_DATA_READ
                LDA FDD_FIFO              ; Read the data from the FDC's FIFO
                STA FLOPPY_CMD_BUFFER,X   ; Save it in the Buffer
                INX
                setdbr `FDD_MAIN_STATUE   ; assume the FDC will never ask to read data while we are sendin the command
READ_MAIN_STATUS_REG_FOR_TRANSFERT_2
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #$80                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ READ_NEXT_DATA
                NOP
                NOP
                NOP
                BRA READ_MAIN_STATUS_REG_FOR_TRANSFERT_2  ; Try to read the Main register again until it get the right value (will need e timout at some point)
ALL_DATA_READ
IFDD_READ_CMD_RESULT_RETURN_ERROR
                RTL
.include "FDD_row_TEXT_HEX.asm"
