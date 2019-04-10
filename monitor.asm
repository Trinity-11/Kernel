.cpu "65816"

;Cmd   Command      Params
;A     ASSEMBLE     [Start] [Assembly code]
;C     COMPARE      Start1 Start2 [Len (1 if blank)]
;D     DISASSEMBLE  Start [End]
;F     FILL         Start End Byte
;G     GO           [Address]
;J                  [Address]
;H     HUNT (find)  Start End Byte [Byte]...
;L     LOAD         "File" [Device] [Start]
;M     MEMORY       [Start] [End]
;R     REGISTERS    Register [Value]  (A 1234, F 00100011)
;;                  PC A X Y SP DBR DP NVMXDIZC
;S     SAVE         "File" Device Start End
;T     TRANSFER     Start End Destination
;V     VERIFY       "File" [Device] [Start]
;X     EXIT
;>     MODIFY       Start Byte [Byte]...
;@     DOS          [Command] Returns drive status if no params.
;?     HELP         Display a short help screen

;Monitor.asm
;Jump Table
* = $195000
MONITOR         JML IMONITOR
MSTATUS         JML IMSTATUS
MREADY          JML IMREADY
MRETURN         JML IMRETURN
MPARSE          JML IMPARSE
MPARSE1         JML IMPARSE1
MEXECUTE        JML IMEXECUTE
MASSEMBLE       JML IMASSEMBLE
MASSEMBLEA      JML IMASSEMBLEA
MCOMPARE        JML IMCOMPARE
MDISASSEMBLE    JML IMDISASSEMBLE
MFILL           JML IMFILL

MJUMP           JML IMJUMP
MHUNT           JML IMHUNT
MLOAD           JML IMLOAD
MMEMORY         JML IMMEMORY
MREGISTERS      JML IMREGISTERS
MSAVE           JML IMSAVE
MTRANSFER       JML IMTRANSFER
MVERIFY         JML IMVERIFY
MEXIT           JML IMEXIT
MMODIFY         JML IMMODIFY
MDOS            JML IMDOS

;
; IMONITOR
; monitor entry point. This initializes the monitor
; and prints the prompt.
; Make sure 16 bit mode is turned on
;
IMONITOR        CLC           ; clear the carry flag
                XCE           ; move carry to emulation flag.
                LDA #STACK_END ; Reset the stack
                TAS
                JML IMREADY

;
; IMREADY
; Print the status prompt, then wait for input
;
IMREADY         ;set the READY handler to jump here instead of BASIC
                setaxl
                LDA #<>IMREADY
                STA JMP_READY+1
                setas
                LDA #`IMREADY
                STA JMP_READY+3

                ;set the RETURN vector and then wait for keyboard input
                setal
                LDA #<>IMRETURN
                STA RETURN+1
                setas
                LDA #`IMRETURN
                STA RETURN+3

                JML IMSTATUS

;
; IMSTATUS
; Prints the regsiter status
; Reads the saved register values at CPU_REGISTERS
;
; PC     A    X    Y    SP   DBR DP   NVMXDIZC
; 000000 0000 0000 0000 0000 00  0000 00000000
;
; Arguments: none
; Modifies: A,X,Y
IMSTATUS        ; Print the MONITOR prompt (registers header)
                setdbr `mregisters_msg
                LDX #<>mregisters_msg
                JSL IPRINT

                setas
                LDA #';'
                JSL IPUTC

                setaxl
                setdbr $0
                ; print Program Counter
                LDY #3
                LDX #CPUPC+2
                JSL IPRINTH

                ; print A register
                LDA ' '
                JSL IPUTC
                LDY #2
                LDX #CPUA+1
                JSL IPRINTH

                ; print X register
                LDA ' '
                JSL IPUTC
                LDY #2
                LDX #CPUX+1
                JSL IPRINTH

                ; print Y register
                LDA ' '
                JSL IPUTC
                LDY #2
                LDX #CPUY+1
                JSL IPRINTH

                ; print Stack Pointer
                LDA ' '
                JSL IPUTC
                LDY #2
                LDX #CPUSTACK+1
                JSL IPRINTH

                ; print DBR
                LDA ' '
                JSL IPUTC
                LDY #1
                LDX #CPUDBR
                JSL IPRINTH

                ; print Direct Page
                LDA ' '
                JSL IPUTC
                JSL IPUTC
                LDY #2
                LDX #CPUDP+1
                JSL IPRINTH

                ; print Flags
                LDA ' '
                JSL IPUTC
                PHP
                setas
                LDA CPUFLAGS
                JSL MPRINTB
                PLP

                JSL IPRINTCR

                JML IREADYWAIT

;
; Fill memory with specified value. Start and end must be in the same bank.
;
; Command: F
;
; Inputs:
;   MARG1 = Destination start address (3 bytes)
;   MARG2 = Destination end address (inclusive, 3 bytes)
;   MARG3 = Byte to store (3 bytes)
;
; Registers:
;   A: undefined
;
; Author:
;   PJW
;
IMFILL          .proc
                PHP                 ; Save the caller's context
                PHD

                ; Made the direct page coincide with our variables
                .setdp MARG1

do_copy         setas               ; Write the fill byte to the current destination
                LDA MARG3
                STA [MARG1]

                setas               ; Start address [23..16] == End address[23..16]?
                LDA MARG1+2
                CMP MARG2+2
                BNE go_next         ; No: we haven't reached end address yet
                setal               ; Yes: check [15..0]
                LDA MARG1
                CMP MARG2         ; Are they equal?
                BNE go_next         ; No: we haven't reached end address yet

                PLD                 ; Restore the caller's context
                PLP
                RTL 

go_next         setal               ; Point to the next destination address
                CLC
                LDA MARG1
                ADC #1
                STA MARG1
                setas
                LDA MARG1+1
                ADC #0
                STA MARG1+1
                BRA do_copy

                .dpage 0
                .pend

;
; Transfer (copy) data in memory
;
; Command: T
;
; Inputs
;   MARG1 = Source starting address (3 bytes)
;   MARG2 = Source ending address (inclusive, 3 bytes)
;   MARG3 = Destination start address (3 bytes)
;
; Registers:
;   A, X, Y: undefined
;
; Author:
;   PJW
;
IMTRANSFER      .proc
                PHP
                PHD

                ; Made the direct page coincide with our variables
                .setdp MARG1

                ; Check the direction of the copy...
                ; Is MARG1 < MARG3?
                setas               ; Check the bank byte first
                LDA MARG1+2
                CMP MARG3+2
                BLT copy_up         ; If MARG1 < MARG3, we are copying up
                setal               ; Check the lower word next
                LDA MARG1
                CMP MARG3
                BLT copy_up         ; If MARG1 < MARG3, we are copying up

                ; MARG1 > MARG3, so we are copying down
                ; Bytes should be copies from the start of the block, rather than the end
copy_byte_down  setas
                LDA [MARG1]         ; Copy the byte
                STA [MARG3]

                LDA MARG1+2         ; Are the source's current and end bank bytes equal?
                CMP MARG2+2
                BNE inc_pointers    ; No: we're not done yet
                setal
                LDA MARG1           ; Are the rest of the bits equal?
                CMP MARG2
                BNE inc_pointers    ; No: we're not done yet
                JMP done            ; Yes: we've copied the last byte, exit

inc_pointers    setal               ; Increment the current source pointer (MARG1)
                CLC
                LDA MARG1
                ADC #1
                STA MARG1
                setas
                LDA MARG1+1
                ADC #0
                STA MARG1+1

                setal               ; Increment the current destination pointer (MARG3)
                CLC
                LDA MARG3
                ADC #1
                STA MARG3
                setas
                LDA MARG3+1
                ADC #0
                STA MARG3+1
                BRA copy_byte_down  ; And copy that next byte over

copy_up         ; MARG1 < MARG3, so we are copying up
                ; Bytes should be copies from the end of the block, rather than the start

                ; Move the destination pointer up to where its end byte should be
                setal               ; MARG4 := MARG2 - MARG1
                SEC
                LDA MARG2
                SBC MARG1
                STA MARG4
                setas
                LDA MARG2+2
                SBC MARG1+2
                STA MARG4+2

                setal               ; MARG3 += MARG4
                CLC
                LDA MARG4
                ADC MARG3
                STA MARG3
                setas
                LDA MARG4+2
                ADC MARG3+2
                STA MARG3+2

copy_byte_up    setas               ; Copy the byte over
                LDA [MARG2]
                STA [MARG3]

                LDA MARG2+2         ; Are the source's current and start bank bytes equal?
                CMP MARG1+2
                BNE dec_pointers    ; No: we're not done yet
                setal
                LDA MARG2           ; Are the rest of the bits equal?
                CMP MARG1
                BNE dec_pointers    ; No: we're not done yet
                BRA done            ; Yes: we've copied the last byte, exit

dec_pointers    setal               ; Decrement the current source pointer (MARG2)
                SEC
                LDA MARG2
                SBC #1
                STA MARG2
                setas
                LDA MARG2+1
                SBC #0
                STA MARG2+1

                setal
                SEC                 ; Decrement the current destination pointer (MARG3)
                LDA MARG3
                SBC #1
                STA MARG3
                setas
                LDA MARG3+1
                SBC #0
                STA MARG3+1
                BRA copy_byte_up    ; And copy that next byte

done            PLD
                PLP
                RTL 
                .dpage 0
                .pend 

;
; Check to see if the character in A is a printable ASCII character
; Return with carry set if it is printable, clear otherwise.
;
; NOTE: this routine assumes a Latin1 character set.
;
; Author:
;   PJW
;
IS_PRINTABLE    .proc
                PHP

                setas
                CMP #33
                BLT not_printable   ; 0 .. 31 are not printable

                CMP #127
                BLT printable       ; 32 .. 126 are printable

                CMP #160
                BLT not_printable   ; 127 .. 159 are not printable (assuming Latin-1)

printable       PLP
                SEC
                RTL

not_printable   PLP
                CLC
                RTL
                .pend

MMEMDUMPSIZE = 256  ; Default number of bytes to dump
MMEMROWSIZE = 8    ; Number of bytes to dump per-row

;
; View memory
;
; Inputs:
;   MARG1 = Start address (optional: default is MCURSOR)
;   MARG2 = End address (optional: default is 256
;
; Command: M
;
; Registers:
;   A, X, Y: undefined
;
; Author:
;   PJW
;
IMMEMORY        .proc
                PHP
                PHB
                PHD

                ; Made the direct page coincide with our variables
                .setdp MARG1

                ; Check the number of arguments
                setas
                LDA MARG_LEN
                CMP #2
                BGE set_cursor      ; 2>= arguments? Use them as-is
                
                CMP #1
                BLT no_args         ; No arguments passed? Use defaults

                ; Has a start but no end address
                ; end address := start address + 256
                setal
                CLC
                LDA MARG1
                ADC #MMEMDUMPSIZE
                STA MARG2
                setas
                LDA MARG1+2
                ADC #0
                STA MARG2+2

                ; Set the cursor to the start address
set_cursor      setal
                LDA MARG1
                STA MCURSOR
                setas
                LDA MARG1+2
                STA MCURSOR+2
                BRA dump_line

                ; Has neither start nor end address
                ; end address := cursor + 256
no_args         setal
                CLC
                LDA MCURSOR
                ADC #MMEMDUMPSIZE
                STA MARG2
                setas
                LDA MCURSOR+2
                ADC #0
                STA MARG2+2
  
                ; Start the processing to dump the current line to the screen
dump_line       setas

                ; Copy an ASCII version of the line of memory into the line buffer
                LDY #0

                setal                   ; Set pointer to the same as CURSOR
                LDA MCURSOR             ; NOTE: the use of MARG4 here is a temporary
                STA MARG4               ; hack. the copy_loop instruction does not
                setas                   ; Seem to work with indexed-indirect-Y addressing
                LDA MCURSOR+2
                STA MARG4+2
                setas

copy_loop       LDA [MARG4]             ; TODO: Should be LDA [MCURSOR],Y, which doesn't seem to be working
                JSL IS_PRINTABLE        ; Is it printable?
                BCS buffer_char         ; Yes: go ahead and add it to the line buffer
                LDA #'?'                ; No: replace it with dot
buffer_char     STA #MLINEBUF,B,Y

                setal                   ; Increment pointer, part of the hack
                CLC
                LDA MARG4
                ADC #1
                STA MARG4
                setas
                LDA MARG4+2
                ADC #0
                STA MARG4+2

                INY
                CPY #MMEMROWSIZE
                BLT copy_loop
                LDA #0
                STA #MLINEBUF,B,Y

                ; Start printing out a line to the screen
                LDA #'>'
                JSL IPUTC
                LDA #' '
                JSL IPUTC

                ; Print the address
                setdbr `MCURSOR         ; Bank byte first
                setxl
                LDX #MCURSOR+2
                LDY #1
                JSL IPRINTH

                LDA #':'                ; then a colon
                JSL IPUTC

                setxl
                LDX #MCURSOR+1
                LDY #2
                JSL IPRINTH

                ; Set the counter for the number of bytes per row
                setal
                LDA #MMEMROWSIZE
                STA MCOUNT

prhex_loop      setas
                LDA #' '
                JSL IPUTC

                PHB
                LDA MCURSOR+2           ; Set the DBR to the bank of the byte to print
                PHA
                PLB
                LDX MCURSOR             ; Point to the byte to print
                LDY #1
                JSL IPRINTH             ; And print it
                PLB

                JSL M_INC_CURSOR        ; Point MCURSOR to the next byte

                ; Check to see if we need a new line
check_line      setas
                DEC MCOUNT              ; Count down the number of bytes on the row
                BNE prhex_loop          ; If we're not at zero, keep looping over the bytes

                ; We've reached the end of the line                
                LDA #' '
                JSL IPUTC
                LDA #' '
                JSL IPUTC

                ; Print the ASCII represnetation
                setdbr `MLINEBUF
                LDX #MLINEBUF
                JSL IPUTS

                JSL IPRINTCR            ; new line

                ; Check to see if we've printed the last byte
                LDA MCURSOR+2           ; Are the banks the same?
                CMP MARG2+2
                BLT continue            ; No: continue
                setal
                LDA MCURSOR             ; Are the lower bits the same?
                CMP MARG2
                BLT continue            ; Nope... keep going

done            PLD
                PLB
                PLP
                RTL

continue        JMP dump_line
                .dpage 0
                .pend

.include "assembler.asm"

;
; Utility subroutines
;

;
; Increment the monitor's memory cursor
;
M_INC_CURSOR    .proc
                PHP
                setal
                PHA

                CLC
                LDA MCURSOR
                ADC #1
                STA MCURSOR
                setas
                LDA MCURSOR+2
                ADC #0
                STA MCURSOR+2

                setal
                PLA
                PLP
                RTL
                .pend

;
; Given a NUL terminated ASCII string, convert all lower-case letters to upper case
;
; Inputs:
;   X = pointer to the NUL terminated ASCII string
;   DBR = bank containing the string
;
M_TO_UPPER      .proc
                PHP
                setxl
                PHX

                setas
loop            LDA #0,B,X
                BEQ done

                CMP #'z'+1
                BCS try_next
                CMP #'a'
                BCC try_next

                AND #%11011111
                STA #0,B,X

try_next        INX
                BRA loop

done            PLX
                PLP
                RTL
                .pend

;
; Print a byte as a string of binary digits
;
; Inputs:
;   A = the byte to print 
;
MPRINTB         .proc
                PHP
                setxl
                setas
                PHX

                ; M is clear, so accumulator is short and we shift 8 bits
                LDX #8          ; Set number of bits to print to 8

loop            ASL A           ; Shift MSB to C
                BCS is_one

                ; MSB was 0, to print a '0'
                PHA             ; Save value to print
                LDA #'0'        ; Print '0'
                JSL IPUTC
                BRA continue

is_one          PHA             ; Save value to print
                LDA #'1'        ; Print '1'
                JSL IPUTC
                
continue        PLA
                DEX             ; Count down the bits to shift
                BNE loop        ; And try the next one if there is one

                PLX             ; Otherwise, return
                PLP
                RTL
                .pend
                

;
; Unimplemented monitor routines
;

IMRETURN        RTL ; Handle RETURN key (ie: execute command)
IMPARSE         BRK ; Parse the current command line
IMPARSE1        BRK ; Parse one word on the current command line
IMEXECUTE       BRK ; Execute the current command line (requires MCMD and MARG1-MARG8 to be populated)
IMASSEMBLEA     BRK ; Assemble a line of text.
IMCOMPARE       BRK ; Compare memory. len=1
IMGO            BRK ; Execute from specified address
IMJUMP          BRK ; Execute from spefified address
IMHUNT          BRK ; Hunt (find) value in memory
IMLOAD          BRK ; Load data from disk. Device=1 (internal floppy) Start=Address in file
IMREGISTERS     BRK ; View/edit registers
IMSAVE          BRK ; Save memory to disk
IMVERIFY        BRK ; Verify memory and file on disk
IMEXIT          BRK ; Exit monitor and return to BASIC command prompt
IMMODIFY        BRK ; Modify memory
IMDOS           BRK ; Execute DOS command

;
; MMESSAGES
; MONITOR messages and responses.
MMESSAGES
MMERROR         .text

mregisters_msg  .null $0D," PC     A    X    Y    SP   DBR DP   NVMXDIZC"
