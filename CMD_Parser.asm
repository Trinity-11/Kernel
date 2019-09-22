
* = $193000

;BCC is branch if less than; BCS is branch if greater than or equal.
KEY_BUFFER       = $000F00 ;64 Bytes keyboard buffer
KEY_BUFFER_SIZE  = $0080 ;128 Bytes (constant) keyboard buffer length
KEY_BUFFER_END   = $000F7F ;1 Byte  Last byte of keyboard buffer
;KEY_BUFFER_RPOS  = $000F80 ;2 Bytes keyboard buffer read position
;KEY_BUFFER_WPOS  = $000F82 ;2 Bytes keyboard buffer write position
KEY_BUFFER_CMD   = $000F83 ;1 Byte  Indicates the Command Process Status
COMMAND_SIZE_STR = $000F84 ; 1 Byte
COMMAND_COMP_TMP = $000F86 ; 2 Bytes

KEYBOARD_SC_FLG  = $000F87 ;1 Bytes that indicate the Status of Left Shift, Left CTRL, Left ALT, Right Shift
KEYBOARD_SC_TMP  = $000F88 ;1 Byte, Interrupt Save Scan Code while Processing

;##########################################
;## Command Parser
;## Interrupt Line Capture Code
;##########################################

; This is the Interrupt Code to Store the Keyboard Input and to trigger process is CR is keyed in
; We assume that X is Long and that A is Short
SAVECHAR2CMDLINE
                ; We don't accept Char less than 32
                PHD
                ;setdbr $00  ; Set the REco
                setas


NOT_CARRIAGE_RETURN
                LDX KEY_BUFFER_WPOS   ; So the Receive Character is saved in the Buffer
                CMP #$20
                BCC CHECK_LOWERTHANSPACE
                ; We Don't accept Char that are greater or Equal to 128
                CMP #$80
                BCS EXIT_SAVE2_CMDLINE

                ; Save the Character in the Buffer
                CPX #KEY_BUFFER_SIZE  ; Make sure we haven't been overboard.
                BCS EXIT_SAVE2_CMDLINE  ; Stop storing - An error should ensue here...
                CMP #$61              ; "a"
                BCC CAPS_NO_CHANGE ;
                CMP #$7B              ; '{'  Char after 'z'
                BCS CAPS_NO_CHANGE ;
                ; if we are here, it is because the ASCII is in small caps
                ; Transfer the small caps in big Caps
                AND #$DF    ; remove the $20 in $61
CAPS_NO_CHANGE
                STA @lKEY_BUFFER, X
                INX
                STX KEY_BUFFER_WPOS
                LDA #$00
                STA @lKEY_BUFFER, X   ; Store a EOL in the following location for good measure
                BRA EXIT_SAVE2_CMDLINE
CHECK_LOWERTHANSPACE
                CMP #$08    ; BackSpace
                BEQ GO_BACKTHEPOINTER;
                CMP #$0D    ; Check to see if the incomming Character is a Cariage Return
                BNE NOT_CARRIAGE_RETURN
                STA @lKEY_BUFFER, X
                ; Just Make sure the Read Point is Pointing at the beginning of the line
                LDX #$0000
                STX KEY_BUFFER_RPOS
                LDA @lKEY_BUFFER_CMD
                ORA #$01      ; Set Bit 0 - to indicate that there is a command to process
                STA @lKEY_BUFFER_CMD
EXIT_SAVE2_CMDLINE
                PLD
                RTL

GO_BACKTHEPOINTER
                LDA #$00
                STA @lKEY_BUFFER, X
                CPX #$0000
                BEQ EXIT_SAVE2_CMDLINE
                DEX
                BRA EXIT_SAVE2_CMDLINE




;##########################################
;## Command Parser
;## Being Executed from Kernel Mainloop
;##########################################
; This is run from the Main KERNEL loop
; We also asume that the Command line is all in CAPS even if a small caps appears on the screen
PROCESS_COMMAND_LINE
                PHP
                setxl   ; let's make sure X is long
                setas   ; and that A is short
                LDX #$0000
                STX KEY_BUFFER_WPOS
                LDX KEY_BUFFER_RPOS ; Load the Read Pointer

NOT_VALID_CHAR4CMD
                LDA @lKEY_BUFFER, X     ; Fetch the first char
                CMP #$0D              ; Check for Carriage Return
                BEQ NO_CMD_2_PROCESS  ; Exit, if the first char is a Carriage return
                ; Let's eliminate any Space before the command
                CMP #$41              ; Smaller than "A"
                BCC NOT_A_VALIDCHAR   ; check for space before the Command
                CMP #$5B              ; Smaller than "Z" We are going to accept the character
                BCC VALIDCHAR_GO_FIND_CMD;
NOT_A_VALIDCHAR
                INX
                CPX #KEY_BUFFER_SIZE
                BNE NOT_VALID_CHAR4CMD
                BEQ ERROR_BUFFER_OVERRUN  ; This means that we have reached the end of Buffer
VALIDCHAR_GO_FIND_CMD
                JSR HOWMANYCHARINCMD  ; Comming back from this Routine we know the size of the Command

                CPY #$0010            ; if the value of the size of the command is 16, then it is not a legit command
                BCS NOTRECOGNIZEDCOMMAND  ; This will output a Command Not Recognized
                JSR FINDCMDINLIST     ; This is where, it gets really cool
                BRA DONE_COMMANDPROCESS
ERROR_BUFFER_OVERRUN
                LDX #<>CMD_Error_Overrun
                JSL IPRINT       ; print the first line

DONE_COMMANDPROCESS
NO_CMD_2_PROCESS
                PLP
                RTL

                ; Error Handling Section
NOTRECOGNIZEDCOMMAND
                LDX #<>CMD_Error_Notfound
                JSL IPRINT       ; print the first line
                PLP
                RTS


; Let's count how many Characters there is in the Command
; Output: Y = How Many Character Does the Command has
HOWMANYCHARINCMD
                LDY #$0000
                PHX ; Push X to Stack for the time being
ENDOFCOMMANDNOTFOUND
                LDA @lKEY_BUFFER, X     ; Fetch the first char
                CMP #$20              ; Check for a Space
                BEQ FOUNDTHEFOLLOWINGSPACE
                CMP #$0D              ; Check to see end of Command (if there is no arguments)
                BEQ FOUNDTHEFOLLOWINGSPACE
                INX
                INY
                CPY #$0010              ; Set the Maximum number of Character to 16
                BCC ENDOFCOMMANDNOTFOUND
FOUNDTHEFOLLOWINGSPACE
                PLX ; Get the Pointer Location of the First Character of the Command
                RTS

; This is where we are going to go fetch the Command and its Attributes
; Y = Lenght of the Command in Characters
; X = Read Pointer in the Line Buffer
FINDCMDINLIST
                STX CMD_PARSER_TMPX   ; Save X for the Time Being
                STY CMD_PARSER_TMPY   ; Save Y for the Time Being
                ; First load the Pointer of the List of Pointer
                setal
                LDA #<>CMDListPtr
                STA CMD_LIST_PTR
                LDA #$0000  ; Just to make sure B is zero
                setas
                LDA #`CMDListPtr
                STA CMD_LIST_PTR+2

                LDY #$0000
                STY CMD_VARIABLE_TMP
NOTTHERIGHTSIZEMOVEON
                LDY CMD_VARIABLE_TMP
                ; Setup the Pointer to the Field Entry of the Command Structure
                LDA [CMD_LIST_PTR],Y
                STA CMD_PARSER_PTR
                INY
                LDA [CMD_LIST_PTR],Y
                STA CMD_PARSER_PTR+1
                INY
                LDA [CMD_LIST_PTR],Y
                STA CMD_PARSER_PTR+2
                INY
                CPY #size(CMDListPtr)
                BCS COMMANDNOTFOUND   ; If we reach that limit then the Count didn't match any command in place
                STY CMD_VARIABLE_TMP
                ; Load the Size of the Actual Command
                LDA [CMD_PARSER_PTR] ;
                CMP CMD_PARSER_TMPY ;
                BNE NOTTHERIGHTSIZEMOVEON
                JSR CHECKSYNTAX       ; Now we have found a Command in the list that matches the number of Char, let's see if this is one if we are looking for
                BCS NOTTHERIGHTSIZEMOVEON ; Failed to Find
                ; Now if we pass this branch and we are here, well, your parents didn't take things too bad, and despite that you are going to brake your own children than there should be no worry
                ; Let's move on
                ; at this point, the
                STX CMD_PARSER_TMPX   ; Just to make sure, this is where the Pointer in the line buffer is...
                INY   ; Point to after the $00, the next 2 bytes are the Attributes
                LDA [CMD_PARSER_PTR], Y ;
                STA CMD_ATTRIBUTE
                INY
                LDA [CMD_PARSER_PTR], Y
                STA CMD_ATTRIBUTE+1
                INY   ; This will point towards the Jumping Vector for the execution of the Command
                LDA [CMD_PARSER_PTR], Y
                STA CMD_EXEC_ADDY
                INY
                LDA [CMD_PARSER_PTR], Y
                STA CMD_EXEC_ADDY+1
                INY
                LDA [CMD_PARSER_PTR], Y
                STA CMD_EXEC_ADDY+2
                JML [CMD_EXEC_ADDY]


COMMANDNOTFOUND
                LDX #<>CMD_Error_Notfound
                JSL IPRINT       ; print the first line
                RTS


; If we are here, it is because we have match in size and we have a point to one of the Command Structure Entry, so let's see if it matches the LinebUffer

; Check the Structure Entry for the Name of the Command , if the calls exits with Zero than, the Command was Found
; Found the Command Return Carry Clear
; Not Found the Command Return Carry Set
CHECKSYNTAX
                LDY #$0001      ; Point towards the Next Byte after the Size
                LDX CMD_PARSER_TMPX ; This is the Pointer in the Line Buffer where the First Character ought to be...
CHECKSYNTAXNEXTCHAR
                LDA [CMD_PARSER_PTR], Y ;
                CMP #$00  ; End of Character Check, if we reach that point, then we are on our way to have something happening! Call mom and dad and tell them how they failed to be good parents! Like all parents
                BEQ SUCCESSFOUNDCOMMAND
                CMP @lKEY_BUFFER, X   ;
                BNE CHARDONTMATCH
                INX
                INY
                BRA CHECKSYNTAXNEXTCHAR

CHARDONTMATCH   SEC
                RTS

SUCCESSFOUNDCOMMAND ; If Success, it will return 00
                CLC
                RTS






; Will Setup the Pointer for the Name + its Size
; Input:
; Pointer To Argument
PROC_FILENAME_ARG
              RTS


; Will Setup the Variable to the Device to Target
PROC_DEVICE_ARG
              RTS

; Will Transform the ASCII into a Hex value
; This will specify an 24bits Address
PROC_ADDRESS_ARG
              RTS
; Will Transform the ASCII into a Hex value
; This will output a 8Bits Byte
PROC_DATA8_ARG
              RTS
; Will Transform the ASCII into a Hex value
; This will output a 16Bits Short
PROC_DATA16_ARG
              RTS

ENTRY_CMD_CLS
              LDX #$0000		; Only Use One Pointer
              LDA #$20		; Fill the Entire Screen with Space
CLEARSCREENL0	STA CS_TEXT_MEM_PTR, x	;
              inx
              cpx #$2000
              bne CLEARSCREENL0
; Now Set the Colors so we can see the text
              LDX	#$0000		; Only Use One Pointer
              LDA #$ED		; Fill the Color Memory with Foreground: 75% Purple, Background 12.5% White
CLEARSCREENL1	STA CS_COLOR_MEM_PTR, x	;
              inx
              cpx #$2000
              bne CLEARSCREENL1
              LDX #$0000
              STX KEY_BUFFER_WPOS
              STX KEY_BUFFER_RPOS
              LDY #$0000
              JSL ILOCATE
              RTS

ENTRY_CMD_DIR
LDX #<>DIR_COMMAND
JSL IPRINT       ; print the first line
RTS

ENTRY_CMD_EXEC
LDX #<>EXEC_COMMAND
JSL IPRINT       ; print the first line
RTS

ENTRY_CMD_LOAD
LDX #<>LOAD_COMMAND
JSL IPRINT       ; print the first line
RTS

ENTRY_CMD_SAVE RTS

ENTRY_CMD_PEEK8     RTS
ENTRY_CMD_POKE8     RTS
ENTRY_CMD_POKE16    RTS
ENTRY_CMD_PEEK16    RTS
ENTRY_CMD_RECWAV    RTS
ENTRY_CMD_EXECFNX   RTS
ENTRY_CMD_GETDATE   RTS
ENTRY_CMD_GETTIME   RTS
ENTRY_CMD_MONITOR   RTS
ENTRY_CMD_PLAYRAD   RTS
ENTRY_CMD_PLAYWAV   RTS
ENTRY_CMD_SETDATE   RTS
ENTRY_CMD_SETTIME   RTS
ENTRY_CMD_SYSINFO   RTS
ENTRY_CMD_DISKCOPY  RTS
ENTRY_CMD_SETTXTLUT RTS



; Command List
; Please Order the Commands by Size then Alpha
; Command Lenght, Command Text, EOS($00), ARGTYPE, Pointer To Code to Execute
CMD .block
CLS       .text $03, "CLS", $00, CMD_ARGTYPE_NO, ENTRY_CMD_CLS                                        ; Clear Screen
DIR       .text $03, "DIR", $00, CMD_ARGTYPE_DEV, ENTRY_CMD_DIR                                       ; @F, @S
EXEC      .text $04, "EXEC", $00, CMD_ARGTYPE_SA, ENTRY_CMD_EXEC                                        ; EXEC S:$00000
LOAD      .text $04, "LOAD", $00, (CMD_ARGTYPE_DEV | CMD_ARGTYPE_FN | CMD_ARGTYPE_EA), ENTRY_CMD_LOAD   ; "LOAD @F, "NAME.XXX", D:$000000
SAVE      .text $04, "SAVE", $00, (CMD_ARGTYPE_DEV | CMD_ARGTYPE_FN | CMD_ARGTYPE_SA | CMD_ARGTYPE_EA), ENTRY_CMD_SAVE           ; SAVE @F, "NAME.XXX", S:$000000, D:$000000
PEEK8     .text $05, "PEEK8", $00,  CMD_ARGTYPE_SA, ENTRY_CMD_PEEK8       ; PEEK8 $000000
POKE8     .text $05, "POKE8", $00, (CMD_ARGTYPE_SA | CMD_ARGTYPE_8D), ENTRY_CMD_POKE8          ; POKE8 $000000, $00
PEEK16    .text $06, "PEEK16", $00, CMD_ARGTYPE_SA, ENTRY_CMD_POKE16, ENTRY_CMD_PEEK16        ; PEEK16 $000000
POKE16    .text $06, "POKE16", $00, (CMD_ARGTYPE_SA | CMD_ARGTYPE_16D), ENTRY_CMD_POKE16           ; POKE16 $000000, $0000
RECWAV    .text $06, "RECWAV", $00, (CMD_ARGTYPE_DEV | CMD_ARGTYPE_FN) , ENTRY_CMD_RECWAV          ; RECWAV @S, "NAME.XXX" (Samples)
EXECFNX   .text $07, "EXECFNX", $00, CMD_ARGTYPE_FN, ENTRY_CMD_EXECFNX        ; "EXECFNX "NAME.XXX"
GETDATE   .text $07, "GETDATE", $00, CMD_ARGTYPE_NO, ENTRY_CMD_GETDATE       ; GETDATE
GETTIME   .text $07, "GETTIME", $00, CMD_ARGTYPE_NO, ENTRY_CMD_GETTIME        ; GETTIME
MONITOR   .text $07, "MONITOR", $00, CMD_ARGTYPE_NO, ENTRY_CMD_MONITOR       ; MONITOR TBD
PLAYRAD   .text $07, "PLAYRAD", $00, (CMD_ARGTYPE_DEV | CMD_ARGTYPE_FN), ENTRY_CMD_PLAYRAD        ; PLAYRAD @S, "NAME.XXX" (music File)
PLAYWAV   .text $07, "PLAYWAV", $00, (CMD_ARGTYPE_DEV | CMD_ARGTYPE_FN), ENTRY_CMD_PLAYWAV                ; PLAYWAV @S, "NAME.XXX" (samples)
SETDATE   .text $07, "SETDATE", $00, CMD_ARGTYPE_DAT, ENTRY_CMD_SETDATE      ; SETDATE YY:MM:DD
SETTIME   .text $07, "SETTIME", $00, CMD_ARGTYPE_TIM, ENTRY_CMD_SETTIME       ; SETTIME HH:MM:SS
SYSINFO   .text $04, "SYSINFO", $00, CMD_ARGTYPE_NO, ENTRY_CMD_SYSINFO
DISKCOPY  .text $08, "DISKCOPY", $00, CMD_ARGTYPE_DEV, CMD_ARGTYPE_DEV, ENTRY_CMD_DISKCOPY           ; DISKCOPY @F, @F
FILECOPY  .text $08, "FILECOPY", $00, (CMD_ARGTYPE_FN | CMD_ARGTYPE_FN2)
SETBGCLR  .text $08, "SETBGCLR", $00, CMD_ARGTYPE_DEC
SETFGCLR  .text $08, "SETFGCLR", $00, CMD_ARGTYPE_DEC
SETTXTLUT .text $09, "SETTXTLUT", $00, (CMD_ARGTYPE_DAT | CMD_ARGTYPE_RGB), ENTRY_CMD_SETTXTLUT        ; SETLUT $00, $000000
SETBRDCLR .text $09, "SETBRDCLR", $00, CMD_ARGTYPE_RGB
      .bend

CMDListPtr .long CMD.CLS, CMD.DIR, CMD.EXEC, CMD.LOAD, CMD.SAVE, CMD.PEEK8, CMD.POKE8, CMD.PEEK16, CMD.POKE16, CMD.RECWAV, CMD.EXECFNX, CMD.GETDATE, CMD.GETTIME, CMD.MONITOR, CMD.PLAYRAD, CMD.PLAYWAV, CMD.SETDATE, CMD.SETTIME, CMD.SYSINFO, CMD.DISKCOPY, CMD.SETTXTLUT

CMD_ARGTYPE_NO    = $0000 ; No Argument
CMD_ARGTYPE_DEV   = $0001 ; Device Type @S, @F
CMD_ARGTYPE_FN    = $0002 ; File Name
CMD_ARGTYPE_SA    = $0004 ; Starting Address (Source)
CMD_ARGTYPE_EA    = $0008 ; Ending Address (Destination)
CMD_ARGTYPE_8D    = $0010 ; 8bits Data
CMD_ARGTYPE_16D   = $0020 ; 16bits Data
CMD_ARGTYPE_TIM   = $0040 ; Time
CMD_ARGTYPE_DAT   = $0080 ; Date
CMD_ARGTYPE_RGB   = $0100 ; RGB Data (24Bit Data) for LUT mainly
CMD_ARGTYPE_FN2   = $0200 ; Second File name
CMD_ARGTYPE_DEC   = $0400 ; Decimal value

CLS_COMMAND .text "CLS", $00
DIR_COMMAND .text "DIR is happening...", $00
EXEC_COMMAND .text "EXEC Command Executing...", $00
LOAD_COMMAND .text "LOAD", $00
CMD_Error_Syntax  .text "SYNTAX  ERROR", $00
CMD_Error_Missing .text "Missing Parameters...", $00
CMD_Error_Wrong   .text "Wrong Parameters...", $00
CMD_Error_Overrun .text "Buffer Overrun...", $00
CMD_Error_Notfound .text "SYNTAX  ERROR", $00
