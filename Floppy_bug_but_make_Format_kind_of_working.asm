.cpu "65816"
.include "Floppy_def.asm"

FLOPPY_CMD_BUFFER = $1A000 ; 10 Byte buffer for the command to be send to the FDC and the the data recieved as e result of the command
* = $1A00A
ILOOP           NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                NOP
                RTL

ILOOP_1         JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                JSL ILOOP
                RTL

ILOOP_1MS       JSL ILOOP_1
                RTL

ILOOP_MS        CPX #0
                BEQ LOOP_MS_END
                JSL ILOOP_1MS
                DEX
                BRA ILOOP_MS
LOOP_MS_END     RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_MOTOR_0_ON   setas
                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #FDD_ENABLE_MOTOR_0
                TSB FDD_DIGITAL_OUTPUT
                RTL

IFDD_MOTOR_0_OFF  setas
                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #FDD_ENABLE_MOTOR_0
                TRB FDD_DIGITAL_OUTPUT
                RTL
; Motor 1 wont work for now
;IFDD_MOTOR_1_ON   setas
;                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
;                LDA #FDD_ENABLE_MOTOR_1
;                TSB FDD_DIGITAL_OUTPUT
;                RTL
;
;IFDD_MOTOR_1_OFF  setas
;                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
;                LDA #FDD_ENABLE_MOTOR_1
;                TRB FDD_DIGITAL_OUTPUT
;                RTL

IFDD_MOTOR_ALL_OFF  setas
                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #FDD_ENABLE_MOTOR_0
                TRB FDD_DIGITAL_OUTPUT
                LDA #FDD_ENABLE_MOTOR_1
                TRB FDD_DIGITAL_OUTPUT
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------

IFDD_TRANSFERT_OK   .proc
                PHP
                PHD
                setaxl
                PHX
                PHA             ; save the value before converting the High part into ASCII
                setas


                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL
                .pend


;-------------------------------------------------------------------------------
SCREEN_PUTHEX   .proc
                PHP
                PHD
                setaxl
                PHX
                PHA             ; save the value before converting the High part into ASCII
                setas
                LDA #1, S       ; get the original value out of the stack
                LSR A             ; Extracting the high part of the byte
                LSR A
                LSR A
                LSR A
                setal
                AND #$F
                LDX A
                setas
                LDA hex_digits,x
                PHA
                setal
                LDA #4, S
                TAX
                setas
                PLA
                setdbr`$AFA000
                STA $AFA000,x
                LDA #1, S       ; get the original value out of the stack
                setal
                AND #$F         ; Extracting the low part of the byte
                LDX A
                setas
                LDA hex_digits,x
                PHA
                setal
                LDA #4, S
                TAX
                setas
                PLA
                INX
                setdbr`$AFA000
                STA $AFA000,x
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL
            .pend


IFDD_READ_FDD
                PHP
                PHD
                setaxl
                PHX
                PHA
                setas
                LDA #1, S
                LDX #$205
                JSL SCREEN_PUTHEX
                setdbr`$AFA200 ; print the sector number on the screen
                LDX #0
                STA $AFA200,x
                PLA
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER+4   ; R : Sector Address
                LDA #$46 ;;;;;;;;
                STA FLOPPY_CMD_BUFFER     ; command code 0 : MT MFM SK  0 0 1   1   0
                LDA X                     ; command code 1 : 0  0   0   0 0 HDS DS1 DS2
                AND #7
                STA FLOPPY_CMD_BUFFER+1
                LDA 0                     ; C : Cylinder Address
                STA FLOPPY_CMD_BUFFER+2
                LDA 0                     ; H : Head Address
                STA FLOPPY_CMD_BUFFER+3
                ;LDA 1                     ; R : Sector Address
                ;STA FLOPPY_CMD_BUFFER+4
                LDA 2                     ; N : Sector Size Code 0=>128 / 1=>256 / 2=>512
                STA FLOPPY_CMD_BUFFER+5
                LDA 1                     ; EOT : End of Track
                STA FLOPPY_CMD_BUFFER+6
                LDA 0                     ; GPL : Gap Length
                STA FLOPPY_CMD_BUFFER+7
                LDA 2                     ; DTL : Special Sector Size Determin the number of byte to read / 2=>512 ???
                STA FLOPPY_CMD_BUFFER+8
                setdbr `Text_READ
                LDX #<>Text_READ
                JSL UART_PUTS
                LDA #9                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                setas
                PHD
                PHP
                JSL IFDD_READ_DATA_FIFO
                PHP
                PHD
                setas
                CMP #1
                ;RTL
  ;LOOP_FOREVER_READ_FAIL  BEQ LOOP_FOREVER_READ_FAIL
                LDA #7
                PHA
                setdbr `Text_Stop_Rx_FIFO
                LDX #<>Text_Stop_Rx_FIFO
                JSL UART_PUTS
                PLA                    ; number of Bytes to read
                JSL IFDD_READ_CMD_RESULT
                JSL IFDD_SENS_INTERRUPT_STATUS
                JSL IFDD_MOTOR_0_OFF
DEBUG_INF_LOOP                BRA DEBUG_INF_LOOP
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL

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

IFDD_INIT_AT    setaxl
                setdbr `Text_INIT_AT
                LDX #<>Text_INIT_AT
                JSL UART_PUTS
                JSL IFDD_RESET_FULL         ; Reset FDD : No DMA, Drive 0 selected, no motor activated
                setdbr `FDD_CONFIG_CTRL  ; Set Data Bank Register
                LDA #$02
                TSB FDD_CONFIG_CTRL
                setdbr `FDD_DATA_RATE_SELECT  ; Set Data Bank Register
                LDA #$00
                TSB FDD_DATA_RATE_SELECT
                setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #$F0                    ; active all motor output and sellect drive 0 (bit 0-1)
                TSB FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode  "Test and Reset Memory Bits Against Accumulator"
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_RESET      setdbr `FDD_DIGITAL_OUTPUT  ; Set Data Bank Register
                LDA #FDD_nRESET             ; reset the floppy disc controler, deactive all the motors and the DMA
                TRB FDD_DIGITAL_OUTPUT      ; Clear the reset bit to go in reset mode "Test and Reset Memory Bits Against Accumulator"
                JSL ILOOP_1
                ; reset the DATA_RATE_SELECT register : automatic low Power, Pres Comp => Default See tab 10 in FDC doc, Data Rate to 250 Kbps
                setdbr `FDD_DATA_RATE_SELECT
                LDA #2
                STA FDD_DATA_RATE_SELECT    ; if in mode PC/AT or PS/2 the datarate is set in Config Control Register
                JSL ILOOP_1MS
                setdbr `FDD_CONFIG_CTRL
                LDA #2
                STA FDD_CONFIG_CTRL
                ; exit the reset mode
                setdbr `FDD_DIGITAL_OUTPUT
                LDA #FDD_nRESET             ; Load the reset bit to be set
                TSB FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode  "Test and Reset Memory Bits Against Accumulator"
                RTL
;-------------------------------------------------------------------------------
IFDD_RESET_FULL setas
                LDA #0                      ; Will set all the bit at 0 to reset everyting
                setdbr `FDD_DIGITAL_OUTPUT
                STA  FDD_DIGITAL_OUTPUT
                NOP                         ; wait, the doc say 100ns min
                NOP
                JSL ILOOP_1
                NOP
                setas
                LDA #FDD_nRESET
                setdbr `FDD_DIGITAL_OUTPUT
                STA FDD_DIGITAL_OUTPUT      ; Set the reset bit to exit the reset mode
                RTL
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
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
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
;------ Read the Main Status Register intil the MSB is set then returne --------
;-- 1 if the FCD need data or return 0 if the FDC have data avaliagle to read --
;-------------------------------------------------------------------------------

IFDD_WAIT_FOR_TRANSFERT_READY   .proc
                PHP
                PHD
                setaxl
                PHX
                PHA
                setas
                ;---------------------------------------------------------------
                PHA                       ; alocate space on the stack to save the main statur value
                setdbr `FDD_MAIN_STATUE
                IFDD_WAIT_FOR_TRANSFERT_READY____READ_MAIN_STATUS_REG
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we can send data to the FDD_CMD_BUSSY
                STA #1, S                 ; Save the Maine Status value
                ;---------- DEBUG ---------
                PHA
                JSL UART_PUTHEX_2
                PLA
                ;------- DEBUG END --------
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                      ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_WAIT_FOR_TRANSFERT_READY____TRANSFERT_CAN_BE_DONE ;
                NOP
                NOP
                NOP
                NOP
                BRA IFDD_WAIT_FOR_TRANSFERT_READY____READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------------ the FDC is now avaliable for transfert -----------
                ;------ Test if the FDC want to get data of if we need to ------
                ;----------------------- write data to it ----------------------
                IFDD_WAIT_FOR_TRANSFERT_READY____TRANSFERT_CAN_BE_DONE
                PLA
                AND #FDD_DIO                  ; get DIO bit
                CMP #$40                      ; if == 0 we can write data into the FIFO, if == 1 we need to read data
                BNE IFDD_WAIT_FOR_TRANSFERT_READY____READY_TO_SEND_DATA;
                LDA #0
                BRA IFDD_WAIT_FOR_TRANSFERT_READY____READY_TO_READ_DATA
IFDD_WAIT_FOR_TRANSFERT_READY____READY_TO_SEND_DATA
                LDA #1
IFDD_WAIT_FOR_TRANSFERT_READY____READY_TO_READ_DATA
                ;---------------------------------------------------------------
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL
                .pend


;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; FORMAT_TRACK : sets the initial values for each of the three internal times
; Stack 1-3: ret address
; Stack 4 : MFM
; Stack 5 : HDS/DS1-DS0 (Head DRIVE1-Drive0)
; Stack 6 : N (Sector size code 0:128 Bytes / 1:256 Bytes / 2 :512 Bytes / ... etc)
; Stack 7 : SC (Sector Per Cylender)
; Stack 8 : GPL (Gap3)
; Stack 9 : D (Byte filler)
; Stack 10 : C (Cylender Adress)
; Stack 11 : H (Head Address)
; Stack 12 : R (Sector Adress)
;
; Command part : Stack 4,5,6,7,8,9
;
; Execution part : Stack 10,11,12,6
; => 11 need to be upated every sector written on the disc the other parameter
; can stay the same as long as we are staying un the same cylinder and face

;-------------------------------------------------------------------------------
IFDD_FORMAT_TRACK
                PHP
                PHD
                setaxl
                PHX
                PHA ; stack ofset of 7 need to be addes to get the parametters
                setas
                LDA #4+7, S         ; Get the MFM Byte
                AND #1            ; Get only the MFM 1 bits info
                ASL
                ASL
                ASL
                ASL
                ASL
                ASL
                ORA #$D
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER
                LDA #5+7, S         ; Get the ID Info Byte
                AND #7            ; Get only the ID 3 bits info
                STA FLOPPY_CMD_BUFFER+1
                LDA #6+7, S         ; Get N (Byte per sector)
                STA FLOPPY_CMD_BUFFER+2
                LDA #7+7, S         ; Get SC (Sector Per Cylender)
                STA FLOPPY_CMD_BUFFER+3
                LDA #8+7, S         ; Get GPL (Gap3)
                STA FLOPPY_CMD_BUFFER+4
                LDA #9+7, S         ; Get D (Byte filler)
                STA FLOPPY_CMD_BUFFER+5
                setdbr `Text_FORMAT
                LDX #<>Text_FORMAT
                JSL UART_PUTS
                LDA #6                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                CMP #0
                BNE IFDD_FORMAT_TRACK_ERROR_SEND_CMD

                LDA #10+7, S         ; Get C (Cylender Adress)
                STA FLOPPY_CMD_BUFFER
                LDA #11+7, S         ; Get H (Head Address)
                STA FLOPPY_CMD_BUFFER+1
                LDA #12+7, S         ; Get N (Sector size code)
                STA FLOPPY_CMD_BUFFER+3
                LDA #13+7, S         ; Get R (Sector Adress)
Format_next_sector
                STA FLOPPY_CMD_BUFFER+2
                LDA #4
                JSL IFDD_SEND_EXECUTION_DATA
                CMP #0
                BEQ Format_Execution_parametter_sent_ok
                setas
                LDA #7                    ; number of Bytes to read
                JSL IFDD_READ_CMD_RESULT
                JMP IFDD_FORMAT_TRACK_ERROR_SEND_CMD
Format_Execution_parametter_sent_ok
                setas
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #20                  ; get NON DMA bit, will stay 1 until the Executuin phase
                CMP #$20
                BNE Format_sector_done
                LDA #13+7, S
                INC A
                STA FLOPPY_CMD_BUFFER+2
                STA #13+7, S
                BRA Format_next_sector
Format_sector_done
                LDA #1
                BRA IFDD_FORMAT_TRACK_DONE
IFDD_FORMAT_TRACK_ERROR_SEND_CMD
                setdbr `Text_ERROR
                LDX #<>Text_ERROR
                JSL UART_PUTS
                LDA #-1
IFDD_FORMAT_TRACK_DONE
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Send data to the FDD during the execution phase
; Stack 1-3: ret address
; A Reg : number of data to send
; FLOPPY_CMD_BUFFER : data to send
;
; Return
; A Reg : 1 ok -1 faill to send the data
;-------------------------------------------------------------------------------
IFDD_SEND_EXECUTION_DATA
                PHP
                PHD
                setaxl
                PHX
                PHA ; stack ofset of 7 need to be addes to get the parametters
                ;----------- Scan the Main Status Byte until data can be send
                ;----------- "FDD_RQM" bits AND data can be send "FDD_DIO"==0
                setas
                PHA                       ; save the number of byt to be sent to the FDC
                JSL IFDD_WAIT_FOR_TRANSFERT_READY ; Return 1 if the FDC is readdy to recieve data
                CMP #1
                BNE IFDD_SEND_EXECUTION_DATA____ERROR_SEND_EXECUTION_DATA_1
                setdbr `FLOPPY_CMD_BUFFER
                LDX #0
IFDD_SEND_EXECUTION_DATA____SEND_NEXT_ECUTION_DATA
                LDA X
                CMP #1, S                 ; Test if we sent all the data ot not
                BEQ IFDD_SEND_EXECUTION_DATA____ALL_DATA_ECUTION_SENT_1
                LDA FLOPPY_CMD_BUFFER,X
                STA FDD_FIFO              ; Write the data in the FDC's FIFO
                ;INX
                ;---------- Debug -------------
                PHX
                setdbr `Text_CMD_Parametter
                LDX #<>Text_CMD_Parametter
                JSL UART_PUTS
                PLX
                PHX
                TXA
                JSL UART_PUTHEX_2
                setdbr `Text_duble_dot
                LDX #<>Text_duble_dot
                JSL UART_PUTS
                PLX
                PHX
                LDA FLOPPY_CMD_BUFFER,X
                JSL UART_PUTHEX_2
                LDX #<>Text_EOL
                JSL UART_PUTS
                ;JSL ILOOP_1MS
                JSL IFDD_PRINT_FDD_MS_REG
                PLX
                ;------------------------------
                INX
                ;setas
                setdbr `FDD_MAIN_STATUE   ; assume the FDC will never ask to read data while we are sendin the command
IFDD_SEND_EXECUTION____READ_MAIN_STATUS_REG_FOR_TRANSFERT
                ;------------------DEBUG--------------
                BRA IFDD_SEND_EXECUTION____JUMP_BYPASS
IFDD_SEND_EXECUTION_DATA____ERROR_SEND_EXECUTION_DATA_1 BNE IFDD_SEND_EXECUTION_DATA____ERROR_SEND_EXECUTION_DATA
IFDD_SEND_EXECUTION_DATA____ALL_DATA_ECUTION_SENT_1 BRA IFDD_SEND_EXECUTION_DATA____ALL_DATA_ECUTION_SENT
IFDD_SEND_EXECUTION____JUMP_BYPASS
                LDA FDD_MAIN_STATUE
                PHA
                ;JSL UART_PUTHEX_2
                AND #$F
                CLC
                ADC #$30
                setdbr `$AFA207
                STA $AFA207
                LDA #1, S
                ;JSL UART_PUTHEX_2
                LSR A
                LSR A
                LSR A
                LSR A
                AND #$F
                CLC
                ADC #$30
                setdbr `$AFA206
                STA $AFA206
                PLA
                ;JSL UART_PUTHEX_2
                setdbr `FDD_MAIN_STATUE
                ;--------------- DEBUG END -----------
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BRL IFDD_SEND_EXECUTION_DATA____SEND_NEXT_ECUTION_DATA
                NOP
                NOP
                NOP
                BRA IFDD_SEND_EXECUTION____READ_MAIN_STATUS_REG_FOR_TRANSFERT  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------ The command is sent now we need to read the result -----
                ;------ so test the data avaliable bit                     -----
IFDD_SEND_EXECUTION_DATA____ALL_DATA_ECUTION_SENT
                PLA                       ; removing the number of commands byte to send
                LDA #0
                BRA IFDD_SEND_EXECUTION_DATA____EXIT
IFDD_SEND_EXECUTION_DATA____ERROR_SEND_EXECUTION_DATA
                PLA
                LDA #-1
                BRA IFDD_SEND_EXECUTION_DATA____EXIT
IFDD_SEND_EXECUTION_DATA____EXIT
                ;--------------------- Restore the register ------------------------
                setaxl
                PLX
                PLA
                PLD
                PLP
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Reg A contain the Floppy driver to recalibrate, bring the hrad to the track 0
; by sensing the track0 pin of the FDD
;-------------------------------------------------------------------------------
IFDD_RECALIBRATE
                setas
                AND #3                    ; just get the 2 first bit
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER+1
                LDA #7
                STA FLOPPY_CMD_BUFFER
                setdbr `Text_RECALIBRATE
                LDX #<>Text_RECALIBRATE
                JSL UART_PUTS
                LDA #2                    ; number of command Bytes
                JSL IFDD_SEND_CMD
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
;-------------------------------------------------------------------------------
; SENS_INTERRUPT_STATUS :
; Return :
; ST0
; PCN
;-------------------------------------------------------------------------------
IFDD_SENS_INTERRUPT_STATUS
                PHP
                PHD
                setaxl
                PHA
                PHX; stack ofset of 7 need to be addes to get the parametters
                setas
                LDA #$08
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER
                setdbr `Text_SENS_INTERRUPT_STATUS
                LDX #<>Text_SENS_INTERRUPT_STATUS
                JSL UART_PUTS
                LDA #1                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                CMP #0
                BMI IFDD_SENS_INTERRUPT_STATUS_ERROR_READ_CMD
                setas
                LDA #2                    ; number of Bytes to read
                JSL IFDD_READ_CMD_RESULT
                CMP #0
                BMI IFDD_SENS_INTERRUPT_STATUS_ERROR_READ_CMD
                LDA #1
                BRA IFDD_SENS_INTERRUPT_STATUS_DONE
IFDD_SENS_INTERRUPT_STATUS_ERROR_READ_CMD
                setdbr `Text_ERROR
                LDX #<>Text_ERROR
                JSL UART_PUTS
                LDA #-1
IFDD_SENS_INTERRUPT_STATUS_DONE
                setaxl
                PLX
                PLA
                PLD
                PLP
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Specify : sets the initial values for each of the three internal times
; Stack 1-3: ret address
; Stack 4 : SRT (Step Rate Time)
; Stack 5 : HUT (Head Unload Time)
; Stack 6 : HLT (Head Load Time)
; Stack 7 : ND ("1":non-DMA mode / "0":DMA mode)
;-------------------------------------------------------------------------------
IFDD_SPECIFY    PHP
                PHD
                setaxl
                PHX
                PHA ; stack ofset of 7 need to be addes to get the parametters
                setdbr `FLOPPY_CMD_BUFFER
                setas
                LDA #3                    ; "SPECIFY" command value
                STA FLOPPY_CMD_BUFFER
                LDA #4+7, S                 ; Get SRT (Step Rate Time)
                ASL
                ASL
                ASL
                ASL
                ORA #5+7, S                 ; Get HUT (Head Unload Time)
                STA FLOPPY_CMD_BUFFER+1
                LDA #6+7, S                 ; Get HLT (Head Load Time)
                ASL
                ORA #7+7, S                 ; Get ND (non-DMA)
                STA FLOPPY_CMD_BUFFER+2
                setdbr `Text_SPECIFY
                LDX #<>Text_SPECIFY
                JSL UART_PUTS
                LDA #3                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                CMP #0
                BMI IFDD_SPECIFY_ERROR_SEND_CMD
                LDA #1
                BRA IFDD_SPECIFY_DONE
IFDD_SPECIFY_ERROR_SEND_CMD
                LDA #-1
IFDD_SPECIFY_DONE
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Bring the head at the cylinder selected by X
; Reg X contain cylinder to reach
; Reg A contain the Floppy driver to work with
;-------------------------------------------------------------------------------
IFDD_SEEK       PHP
                PHD
                setaxl
                PHX
                PHA
                setas
                AND #7                    ; Get the 3 first bit side (2) and driver (1-0)
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER+1
                LDA #$F
                STA FLOPPY_CMD_BUFFER
                LDA X                     ; Get the cylinder index
                STA FLOPPY_CMD_BUFFER+2
                setdbr `Text_SEEK
                LDX #<>Text_SEEK
                JSL UART_PUTS
                LDA #3                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                CMP #0
                BMI IFDD_SEEK_ERROR_SEND_CMD
                LDA #1
                BRA IFDD_SEEK_DONE
IFDD_SEEK_ERROR_SEND_CMD
                LDA #-1
IFDD_SEEK_DONE
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
IFDD_SEEKRELATIF BRK
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------- READ ID --------------------------------------
;-------------------------------------------------------------------------------
; The Read ID command is used to find the present position of the recording heads.
; The FDC stores the values from the first ID field it is able to read into its
; registers. If the FDC does not find an ID address mark on the diskette after the
; second occurrence of a pulse on the nINDEX pin, it then sets the IC code in
; Status Register 0 to "01" (abnormal termination), sets the MA bit in Status
; Register 1 to "1", and terminates the command.
; The following commands will generate an interrupt upon completion. They do not
; return any result bytes. It is highly recommended that control commands be
; followed by the Sense Interrupt Status command. Otherwise, valuable interrupt
; status information will be lost.
;
; Reg X contain mode MFM/FM
; Reg A contain the Floppy driver to work with
;
; return:
; ST0 Status register
; ST1
; ST2
; C   Cylinder address (0 to 255)
; H   Head address, 0 disc side 0 / 1 disc side 1
; R   Sector number (on the cylinder probably)
; N   Sector size code 0:128 Bytes / 1:256 Bytes / 2 :512 Bytes / ... etc
;-------------------------------------------------------------------------------
IFDD_READ_ID
                PHP
                PHD
                setaxl
                PHA
                PHX
                setas
                AND #7                    ; Get the 3 first bit HDS (2) and driver (1-0)
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER+1
                LDA X                     ; Get the cylinder index
                AND #1
                ASL
                ASL
                ASL
                ASL
                ASL
                ASL
                PHA
                LDA #$0A
                ORA #1, S
                STA FLOPPY_CMD_BUFFER
                PLA
                ;---------- Debug -------------
                LDA FLOPPY_CMD_BUFFER
                JSL UART_PUTHEX_2
                LDA FLOPPY_CMD_BUFFER+1
                JSL UART_PUTHEX_2
                setdbr `Text_READ_ID
                LDX #<>Text_READ_ID
                JSL UART_PUTS
                LDA #2                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                CMP #0
                BMI IFDD_READ_ID_ERROR_READ_CMD
                setas
                LDA #7                    ; number of Bytes to read
                JSL IFDD_READ_CMD_RESULT
                CMP #0
                BMI IFDD_READ_ID_ERROR_READ_CMD
                LDA #1
                BRA IFDD_READ_ID_DONE
IFDD_READ_ID_ERROR_READ_CMD
                setdbr `Text_ERROR
                LDX #<>Text_ERROR
                JSL UART_PUTS
                LDA #-1
IFDD_READ_ID_DONE
                setaxl
                PLX
                PLA
                PLD
                PLP
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Bring the head at the cylinder selected by X
; Reg X contain cylinder to reach
; Reg A contain the Floppy driver to work with
;-------------------------------------------------------------------------------
IFDD_GET_DRIVE_STATUS
                setas
                AND #7                    ; Get the 3 first bit HDS (2) and driver (1-0)
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER+1
                LDA #$4
                STA FLOPPY_CMD_BUFFER
                LDA X                     ; Get the cylinder index
                STA FLOPPY_CMD_BUFFER+2
                LDA #2                    ; number of command Bytes
                JSL IFDD_SEND_CMD
                CMP #0
                BMI IFDD_DRIVE_STATUS_ERROR_READ_CMD
                setas
                LDA #1                    ; number of Bytes to read
                JSL IFDD_READ_CMD_RESULT
                CMP #0
                BMI IFDD_DRIVE_STATUS_ERROR_READ_CMD
                LDA #1
                BRA IFDD_DRIVE_STATUS_DONE
IFDD_DRIVE_STATUS_ERROR_READ_CMD
                LDA #-1
IFDD_DRIVE_STATUS_DONE
                RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Reg A contain the number of data to send to the Floppy Disc Controler
; The data are in FLOPPY_CMD_BUFFER at address $1A000
; The result is also places in FLOPPY_CMD_BUFFER, the command is over writen
;-------------------------------------------------------------------------------
IFDD_SEND_CMD   PHA                       ; save the number of byt to be sent to the FDC
                PHA                       ; alocate space on the stack to save the main statur value
                setdbr `FDD_MAIN_STATUE
IFDD_SEND_CMD_READ_MAIN_STATUS_REG
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                STA #1, S                 ; Save the Maine Status value
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_SEND_CMD_TRANSFERT_CAN_BE_DONE ;
                NOP
                NOP
                NOP
                BRA IFDD_SEND_CMD_READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------------ the FDC is now avaliable for transfert -----------
IFDD_SEND_CMD_TRANSFERT_CAN_BE_DONE
                LDA #1, S                 ; get the Main Status avlue
                AND #FDD_DIO                  ; get DIO bit
                CMP #$40                  ; if == 0 we can write data into the FIFO, if == 1 we need to read data
                BNE IFDD_SEND_CMD_READDY_TO_SEND_DATA;
                LDA FDD_FIFO                      ; remove the Main Status value saved
                JSL UART_PUTHEX_2
                BRA IFDD_SEND_CMD_READ_MAIN_STATUS_REG  ; retest if we can send data now#
                ;--------------- the FDC can now recuive command ---------------
IFDD_SEND_CMD_READDY_TO_SEND_DATA
                PLA                       ; remove the Main Status value saved
                ;------------------------------
                setdbr `Text_SEND_CMD
                LDX #<>Text_SEND_CMD
                JSL UART_PUTS
                LDX #<>Text_CMD_Parametter_Number
                JSL UART_PUTS
                LDA #1, S                 ; Get the number of parametter
                JSL UART_PUTHEX_2
                LDX #<>Text_EOL
                JSL UART_PUTS
                ;------------------------------
                setdbr `FLOPPY_CMD_BUFFER
                LDX #0
SEND_NEXT_DATA  LDA X
                CMP #1, S                 ; Test if we sent all the data ot not
                BEQ ALL_DATA_SENT_1
                LDA FLOPPY_CMD_BUFFER,X
                STA FDD_FIFO              ; Write the data in the FDC's FIFO
                ;INX
                ;---------- Debug -------------
                PHX
                setdbr `Text_CMD_Parametter
                LDX #<>Text_CMD_Parametter
                JSL UART_PUTS
                PLX
                PHX
                TXA
                JSL UART_PUTHEX_2
                setdbr `Text_duble_dot
                LDX #<>Text_duble_dot
                JSL UART_PUTS
                PLX
                PHX
                LDA FLOPPY_CMD_BUFFER,X
                JSL UART_PUTHEX_2
                LDX #<>Text_EOL
                JSL UART_PUTS
                ;JSL ILOOP_1MS
                JSL IFDD_PRINT_FDD_MS_REG
                PLX
                ;------------------------------
                INX
                ;setas
                setdbr `FDD_MAIN_STATUE   ; assume the FDC will never ask to read data while we are sendin the command
READ_MAIN_STATUS_REG_FOR_TRANSFERT
                ;------------------DEBUG--------------
                BRA ALL_DATA_SEND_JUMP_BYPASS
ALL_DATA_SENT_1 BRA ALL_DATA_SENT
ALL_DATA_SEND_JUMP_BYPASS
                LDA FDD_MAIN_STATUE
                PHA
                ;JSL UART_PUTHEX_2
                AND #$F
                CLC
                ADC #$30
                setdbr `$AFA207
                STA $AFA207
                LDA #1, S
                ;JSL UART_PUTHEX_2
                LSR A
                LSR A
                LSR A
                LSR A
                AND #$F
                CLC
                ADC #$30
                setdbr `$AFA206
                STA $AFA206
                PLA
                ;JSL UART_PUTHEX_2
                setdbr `FDD_MAIN_STATUE
                ;--------------- DEBUG END -----------
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BRL SEND_NEXT_DATA
                NOP
                NOP
                NOP
                BRA READ_MAIN_STATUS_REG_FOR_TRANSFERT  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------ The command is sent now we need to read the result -----
                ;------ so west the dqta avaliable bit                     -----
ALL_DATA_SENT
                PLA                       ; removing the number of commands byte to send
                ;setdbr `Text_Stop_Tx_CMD
                ;LDX #<>Text_Stop_Tx_CMD
                ;JSL UART_PUTS
                LDA #0
                RTL
;-------------------------------------------------------------------------------
; Reg A contain the number of data to read from the Floppy Disc Controler
; The data will be stored in FLOPPY_CMD_BUFFER at address $1A000
;-------------------------------------------------------------------------------
IFDD_READ_CMD_RESULT
                PHA                       ; save the number of byte to be read to the FDC
                PHA                       ; alocate space on the stack to save the main statur value
IFDD_READ_CMD_RESULT_READ_MAIN_STATUS_REG
                setdbr `FDD_MAIN_STATUE
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we can sent data to the FDD_CMD_BUSSY
                STA #1, S                 ; Save the Maine Status value
                JSL UART_PUTHEX_2
                LDA #1, S
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_READ_CMD_TRANSFERT_CAN_BE_DONE ;
                NOP
                LDX #2000
                JSL ILOOP_MS
                NOP
                BRA IFDD_READ_CMD_RESULT_READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------------ the FDC is now avaliable for transfert -----------
IFDD_READ_CMD_TRANSFERT_CAN_BE_DONE
                LDA FDD_MAIN_STATUE
                AND #FDD_DIO              ; get DIO bit
                CMP #$40                  ; if == 0 we can write data into the FIFO, if == 1 we need to read data
                BEQ READDY_TO_READ_DATA   ; We want to read the result of the command
                ;;PLA
                LDA #1                    ; error, the FDC after reciving the command, is supposed to sent you data
                BRA IFDD_READ_CMD_RESULT_READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;BRA IFDD_READ_CMD_RESULT_RETURN_ERROR
READDY_TO_READ_DATA
                PLA
                ;------------------------------
                setdbr `Text_EOL
                LDX #<>Text_EOL
                JSL UART_PUTS
                LDX #<>Text_Start_Rx_CMD
                JSL UART_PUTS
                LDX #<>Text_CMD_Result_Number
                JSL UART_PUTS
                LDA #1, S                 ; Get the number of parametter
                JSL UART_PUTHEX_2
                LDX #<>Text_EOL
                JSL UART_PUTS
                ;------------------------------
                LDX #0
READ_NEXT_DATA  LDA X
                CMP #1, S                 ; Test if we read all the data ot not
                BEQ ALL_DATA_READ
                setdbr `FDD_FIFO
                LDA FDD_FIFO              ; Read the data from the FDC's FIFO
                setdbr `FLOPPY_CMD_BUFFER
                STA FLOPPY_CMD_BUFFER,X   ; Save it in the Buffer
                ;INX
                ;---------- Debug -------------
                ;BRA ALL_DATA_READ_JUMP_BYPASS
                ;ALL_DATA_READ_1 BRA ALL_DATA_READ
                ;ALL_DATA_READ_JUMP_BYPASS
                PHX
                setdbr `Text_CMD_Parametter
                LDX #<>Text_CMD_Result
                JSL UART_PUTS
                PLX
                PHX
                TXA
                JSL UART_PUTHEX_2
                setdbr `Text_duble_dot
                LDX #<>Text_duble_dot
                JSL UART_PUTS
                PLX
                PHX
                LDA FLOPPY_CMD_BUFFER,X
                JSL UART_PUTHEX_2
                LDX #<>Text_EOL
                JSL UART_PUTS
                ;JSL ILOOP_1MS
                JSL IFDD_PRINT_FDD_MS_REG
                PLX
                ;------------------------------
                INX
                ;PHX
                ;JSL UART_PUTHEX
                ;LDA #$A
                ;JSL UART_PUTC
                ;LDA #$D
                ;JSL UART_PUTC
                ;JSL ILOOP_1MS
                ;JSL IFDD_PRINT_FDD_MS_REG
                ;PLX

                setdbr `FDD_MAIN_STATUE   ; assume the FDC will never ask to read data while we are sendin the command
READ_MAIN_STATUS_REG_FOR_TRANSFERT_2
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ READ_NEXT_DATA
                NOP
                NOP
                NOP
                BRA READ_MAIN_STATUS_REG_FOR_TRANSFERT_2  ; Try to read the Main register again until it get the right value (will need e timout at some point)
ALL_DATA_READ
                LDA #0
                BRA IFDD_READ_CMD_RESULT_RETURN
IFDD_READ_CMD_RESULT_RETURN_ERROR
                setdbr `Text_ERROR
                LDX #<>Text_ERROR
                JSL UART_PUTS
IFDD_READ_CMD_RESULT_RETURN
                PLA
                setdbr `Text_Stop_Rx_CMD
                LDX #<>Text_Stop_Rx_CMD
                JSL UART_PUTS
                LDX #0
                RTL

;-------------------------------------------------------------------------------
; read the data out from the FDD
; Stack 0-1-3-4 where to store the data
; Stack 5-6-7-8 number of byte to read
;-------------------------------------------------------------------------------
IFDD_READ_DATA_FIFO
                setxl
                PHA                       ; alocate space on the stack to save the main statur value
                setdbr `FDD_MAIN_STATUE
IFDD_READ_DATA_FIFO_READ_MAIN_STATUS_REG
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                STA #1, S                 ; Save the Maine Status value
                AND #FDD_RQM                  ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_READ_DATA_FIFO_TRANSFERT_CAN_BE_DONE ;
                NOP
                NOP
                NOP
                BRA IFDD_READ_DATA_FIFO_READ_MAIN_STATUS_REG  ; Try to read the Main register again until it get the right value (will need e timout at some point)
                ;------------ the FDC is now avaliable for transfert -----------
IFDD_READ_DATA_FIFO_TRANSFERT_CAN_BE_DONE
                LDA FDD_MAIN_STATUE
                AND #FDD_DIO              ; get DIO bit
                CMP #$40                  ; if == 0 we can write data into the FIFO, if == 1 we need to read data
                BEQ IFDD_READ_DATA_FIFO_READDY_TO_READ_DATA   ; We want to read the result of the command
                PLA
                LDA #1                   ; error, the FDC after reciving the commans is suppos to sent you data
                BRL IFDD_READ_DATA_FIFO_RETURN_ERROR
IFDD_READ_DATA_FIFO_READDY_TO_READ_DATA
                PLA
                ;setdbr `Text_Start_Rx_FIFO
                ;LDX #<>Text_Start_Rx_FIFO
                ;JSL UART_PUTS
                LDX #0
                LDY #$600
IFDD_READ_DATA_FIFO_READ_NEXT_DATA
                ;----------------------------------
                ;PHX
                ;JSL UART_PUTHEX
                ;LDA #$20
                ;JSL UART_PUTC
                ;JSL IFDD_PRINT_FDD_MS_REG
                ;LDA #$20
                ;JSL UART_PUTC
                ;PLX
                ;----------------------------------
                setdbr `FDD_FIFO
                LDA FDD_FIFO              ; Read the data from the FDC's FIFO
                setdbr`$19A000
                STA $19A000 ,X            ; Save it in the Buffer
                ;-----
                setdbr`$AFA000
                PHX
                PHA
                LDA #1, S
                ;;LDA #$42
                LSR A                   ; Extracting the high part of the byte
                LSR A
                LSR A
                LSR A
                setal
                AND #$F
                LDX A
                setas
                setdbr`hex_digits
                LDA hex_digits,X
                TYX
                STA $AFA000 ,X
                LDA #1, S
                ;PLA
                ;;LDA #$42
                setal
                AND #$F
                LDX A
                setas
                LDA hex_digits,X
                TYX
                STA $AFA000+1 ,X
                BRA BYPASS_IFDD_READ_DATA_FIFO_READ_NEXT_DATA
IFDD_READ_DATA_FIFO_READ_NEXT_DATA_step1 BRA IFDD_READ_DATA_FIFO_READ_NEXT_DATA
BYPASS_IFDD_READ_DATA_FIFO_READ_NEXT_DATA
                setal
                TYA
                CLC
                ADC #4
                AND #$40
                CMP #$40
                BNE NO_NEED_NEW_LINE
                TYA
                AND #$FF80
                CLC
                ADC #$80
                BRA NEW_LINE_ADDED
NO_NEED_NEW_LINE
                TYA
                CLC
                ADC #4
NEW_LINE_ADDED  TAY
                setas

                PLA
                PLX
                ;-----------------
                ;setdbr`$AFA000
                ;STA $AFA000 ,X            ; Save it in the Buffer
                INC X
                CPX #512
                BEQ ALL_DATA_READ_2
                ;PHX
                ;LDA X
                ;JSL UART_PUTHEX
                ;LDA #$20
                ;JSL UART_PUTC
                ;PLX
                setdbr `FDD_MAIN_STATUE   ; assume the FDC will never ask to read data while we are sendin the command
READ_MAIN_STATUS_REG_FOR_TRANSFERT_3
                LDA FDD_MAIN_STATUE       ; read bit 6 and 7 to see if we cal sent data to the FDD_CMD_BUSSY
                AND #FDD_RQM              ; get RQM bit
                CMP #$80                  ; if == 1 we can read or write data from the FIFO,depending on the DIO bit value
                BEQ IFDD_READ_DATA_FIFO_READ_NEXT_DATA_step1
                NOP
                NOP
                NOP
                BRA READ_MAIN_STATUS_REG_FOR_TRANSFERT_3  ; Try to read the Main register again until it get the right value (will need e timout at some point)
ALL_DATA_READ_2
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                JSL IFDD_PRINT_FDD_MS_REG
                LDA #0
IFDD_READ_DATA_FIFO_RETURN_ERROR
                PHA
                setdbr `Text_Stop_Rx_FIFO
                LDX #<>Text_Stop_Rx_FIFO
                JSL UART_PUTS
                PLA
                RTL

;-------------------------------------------------------------------------------
; Print on the terminal the value of the Main Status Register from the FDD
;-------------------------------------------------------------------------------
IFDD_PRINT_FDD_MS_REG

                PHP
                PHD
                setaxl
                PHX
                PHA
                setdbr `Text_FDD_MAIN_STATUE
                LDX #<>Text_FDD_MAIN_STATUE
                JSL UART_PUTS
                setas
                setdbr `FDD_MAIN_STATUE
                LDA FDD_MAIN_STATUE
                JSL UART_PUTHEX_2
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;LDA #$A
                ;;---------------
                ;JSL UART_PUTC
                ;LDA #$D
                ;JSL UART_PUTC
                setaxl
                PLA
                PLX
                PLD
                PLP
                RTL
;-------------------------------------------------------------------------------
; Print on the terminal the value of all the readeble register from the FDD
;-------------------------------------------------------------------------------
IFDD_PRINT_REG  setas
                setdbr `Text_FDD_STATUS_A
                LDX #<>Text_FDD_STATUS_A
                JSL UART_PUTS
                setdbr `FDD_STATUS_A
                LDA FDD_STATUS_A
                JSL UART_PUTHEX
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;---------------
                setdbr `Text_FDD_STATUS_B
                LDX #<>Text_FDD_STATUS_B
                JSL UART_PUTS
                setdbr `FDD_STATUS_B
                LDA FDD_STATUS_B
                JSL UART_PUTHEX
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;---------------
                setdbr `Text_FDD_DIGITAL_OUTPUT
                LDX #<>Text_FDD_DIGITAL_OUTPUT
                JSL UART_PUTS
                setdbr `FDD_DIGITAL_OUTPUT
                LDA FDD_DIGITAL_OUTPUT
                JSL UART_PUTHEX
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;---------------
                setdbr `Text_FDD_TAPE_DRIVER
                LDX #<>Text_FDD_TAPE_DRIVER
                JSL UART_PUTS
                setdbr `FDD_TAPE_DRIVER
                LDA FDD_TAPE_DRIVER
                JSL UART_PUTHEX
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;---------------
                setdbr `Text_FDD_MAIN_STATUE
                LDX #<>Text_FDD_MAIN_STATUE
                JSL UART_PUTS
                setdbr `FDD_MAIN_STATUE
                LDA FDD_MAIN_STATUE
                JSL UART_PUTHEX
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;---------------
                setdbr `Text_FDD_DIGITAL_INPUT
                LDX #<>Text_FDD_DIGITAL_INPUT
                JSL UART_PUTS
                setdbr `FDD_DIGITAL_INPUT
                LDA FDD_DIGITAL_INPUT
                JSL UART_PUTHEX
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                ;---------------
                LDA #$A
                JSL UART_PUTC
                LDA #$D
                JSL UART_PUTC
                RTL


Text_INIT_AT              .text "----------------- FDD INIT_AT -----------------",$A,$D,0
Text_RECALIBRATE          .text "--------------- FDD RECALIBRATE ---------------",$A,$D,0
Text_READ_ID              .text "------------------- READ_ID -------------------",$A,$D,0
Text_READ                 .text "--------------------- READ --------------------",$A,$D,0
Text_SENS_INTERRUPT_STATUS .text "------------ SENS_INTERRUPT_STATUS ------------",$A,$D,0
Text_FORMAT               .text "-------------------- FORMAT -------------------",$A,$D,0
Text_SEEK                 .text "--------------------- SEEK --------------------",$A,$D,0
Text_SPECIFY               .text "------------------- SPECIFY ------------------",$A,$D,0

Text_SEND_CMD         .text "- SEND CMD Start -",$A,$D,0
Text_Stop_Tx_CMD          .text "- TX CMD Stop -",$A,$D,0
Text_Start_Rx_CMD         .text "- RX RESULT Start -",$A,$D,0
Text_Stop_Rx_CMD          .text "- RX RESULT Stop -",$A,$D,0
Text_Start_Rx_FIFO        .text "- RX FIFO Start -",$A,$D,0
Text_Stop_Rx_FIFO         .text "- RX FIFO Stop -",$A,$D,0
Text_ERROR                .text "- FDD_ERROR : ",$A,$D,0
Text_EOL                  .text $A,$D,0
Text_CMD_Parametter_Number .text "Nb parametter : ",0
Text_CMD_Parametter       .text "Param ",0
Text_CMD_Result_Number    .text "Nb result : ",0
Text_CMD_Result           .text "Result ",0
Text_duble_dot             .text " : ",0

Text_FDD_STATUS_A         .text "FDD_STATUS_A       0x",0
Text_FDD_STATUS_B         .text "FDD_STATUS_B       0x",0
Text_FDD_DIGITAL_OUTPUT   .text "FDD_DIGITAL_OUTPUT 0x",0
Text_FDD_TAPE_DRIVER      .text "FDD_TAPE_DRIVER    0x",0
Text_FDD_MAIN_STATUE      .text "FDD_MAIN_STATUE    0x",0
Text_FDD_DIGITAL_INPUT    .text "FDD_DIGITAL_INPUT  0x",0

.include "FDD_row_TEXT_HEX.asm"
