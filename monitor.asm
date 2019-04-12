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
MRMODIFY        JML IMRMODIFY
MCOMPARE        JML IMCOMPARE
MDISASSEMBLE    JML IMDISASSEMBLE
MFILL           JML IMFILL

MGO             JML IMGO
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
; IMREGISTERS
; Prints the regsiter status
; Reads the saved register values at CPU_REGISTERS
;
; PC     A    X    Y    SP   DBR DP   NVMXDIZC
; 000000 0000 0000 0000 0000 00  0000 00000000
;
; Arguments: none
; Modifies: A,X,Y
IMREGISTERS     ; Print the MONITOR prompt (registers header)
                setdbr `mregisters_msg
                LDX #<>mregisters_msg
                JSL IPRINT

                setas
                LDA #';'
                JSL IPUTC

                LDA #' '
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
                RTL

IMSTATUS        JSL IMREGISTERS
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

;
; Copy bytes from the arguments list to MLINEBUF
;
; Inputs:
;   A = number of arguments to copy
;   X = address of starting argument to copy
;   Y = address of destination
;
M_COPY_ARGB     .proc
                PHP

                STA MTEMP

                setas
loop            LDA #0,B,X
                STA #0,B,Y      ; Copy the byte
                
                LDA MTEMP       ; Check the count of characters remaining
                BEQ done        ; If it's 0, we're done

                INY             ; Point to the next destination byte

                INX             ; Point to the next source byte (skip three bytes)
                INX
                INX
                INX

                DEC MTEMP       ; Count down and see if we're done
                BRA loop

done            PLP
                RTL
                .pend

;
;>     MODIFY       Start Byte [Byte]...
;
; Inputs:
;   MARG1 = address to update
;   MARG2..MARG9 = bytes to write
;
; Author:
;   PJW
;
IMMODIFY        .proc
                PHP
                PHD
                PHB

                setdp <>MONITOR_VARS
                setdbr 0

                setaxl
                LDA MARG1           ; Set MCURSOR to MARG1
                STA MCURSOR
                LDA MARG1+2
                STA MCURSOR+2

                setas              
                LDA MARG_LEN        ; Set MCOUNT to the number of bytes in the pattern
                DEC A               ; (MARG_LEN - 1)
                STA MCOUNT

                LDX #<>MARG2        ; Copy MCOUNT bytes from MARG2..MARG9
                LDY #<>MLINEBUF     ; to MLINEBUF
                JSL M_COPY_ARGB

                LDY #0
loop            LDA MLINEBUF,Y      ; Copy the byte from the buffer
                STA [MCURSOR]       ; To the address indicated by MCURSOR

                JSL M_INC_CURSOR    ; Advance the cursor
                INY                 ; Go to the next buffered byte
                CPY MCOUNT          ; Did we just write the last one?
                BNE loop            ; No: continue writing

                PLB
                PLD
                PLP
                RTL
                .pend

;
; Find a value in memory
;
; Inputs:
;   MARG1 = starting address to scan
;   MARG2 = ending address to scan
;   MARG3 .. MARG9 = bytes to find (one byte per argument)
;   MARG_LEN = numbe of arguments passed (number of bytes to find + 2)
;
; Author:
;   PJW
;
IMHUNT          .proc
                PHP
                PHD
                PHB

                setdp <>MONITOR_VARS
                setdbr 0

                setas          
                setxl     
                LDA MARG_LEN        ; Set MCOUNT to the number of bytes in the pattern
                DEC A               ; (MARG_LEN - 2)
                DEC A
                STA MCOUNT

                LDX #<>MARG3
                LDY #<>MLINEBUF
                JSL M_COPY_ARGB

                setal
                LDA MARG1           ; Copy starting address to MCURSOR
                STA MCURSOR
                LDA MARG1+2
                STA MCURSOR+2

outer_loop      setal
                LDA MCURSOR+2      ; If MCURSOR < MARG2, we're not done yet
                CMP MARG2+2
                BNE not_done
                LDA MCURSOR
                CMP MARG2
                BEQ done            ; MCURSOR = MARG2: we're done

not_done        setas
                LDY #0

cmp_loop        LDA [MCURSOR],Y     ; Get the byte from the memory to check
                CMP MLINEBUF,Y      ; Compare it against our pattern
                BNE advance         ; If not equal, we need to move on
                INY                 ; Otherwise do we have more bytes to check?
                CPY MCOUNT
                BNE cmp_loop        ; No: check more

                setal               ; Yes: we have a match!
                LDA MCURSOR         ; Print the address
                STA MTEMP
                LDA MCURSOR+2
                STA MTEMP+2
                JSL M_PR_ADDR

                setas               ; Print a space
                LDA #' '
                JSL IPUTC

advance         JSL M_INC_CURSOR    ; Move MCURSOR forward by one
                BRA outer_loop      ; And try to compare that to the pattern                

done            JSL IPRINTCR
                PLB
                PLD
                PLP
                RTL
                .pend

;
; Execute from specified address
;
; Inputs:
;   MARG1 = address (optional)
;
; Author:
;   PJW
;
IMJUMP          setdp MONITOR_VARS

                setas
                LDA MARG_LEN        ; Check to see if an argument was provided
                BEQ MJUMPRESTORE    ; If not, just restore the registers

                setaxl
                LDA MARG1           ; Otherwise, replace PC and K
                STA @lCPUPC         ; With the value of the first argument
                LDA MARG1+2
                STA @lCPUPBR

MJUMPRESTORE    LDA @lCPUX          ; Restore X and Y
                TAX
                LDA @lCPUY
                TAY

                LDA @lCPUSTACK      ; Restore the stack pointer
                TCS

                LDA @lCPUDP         ; Restore the direct page register
                TCD

                setas               ; Push the return address to return to the monitor
                LDA #`MJUMPSTART
                PHA
                LDA #>MJUMPSTART
                PHA
                LDA #<MJUMPSTART
                PHA
                JMP MGOSTACK        ; And push remaining registers and restart execution

MJUMPSTART      NOP                 ; RTL increments PC pulled from stack, NOP leaves space for that
                JML MONITOR

;
; Execute from specified address
;
; Inputs:
;   MARG1 = address (optional)
;
; Author:
;   PJW
;
IMGO            setdp MONITOR_VARS

                setas
                LDA MARG_LEN        ; Check to see if an argument was provided
                BEQ MJUMPRESTORE    ; If not, just restore the registers

                setaxl
                LDA MARG1           ; Otherwise, replace PC and K
                STA @lCPUPC         ; With the value of the first argument
                LDA MARG1+2
                STA @lCPUPBR

MGORESTORE      LDA @lCPUX          ; Restore X and Y
                TAX
                LDA @lCPUY
                TAY

                LDA @lCPUSTACK      ; Restore the stack pointer
                TCS

                LDA @lCPUDP         ; Restore the direct page register
                TCD

MGOSTACK        setas
                LDA @lCPUDBR        ; Restore the data bank register
                PHA
                PLB

                LDA #$5C            ; Save the JSL opcode
                STA @lMJUMPINST
                LDA @lCPUPBR        ; Write PBR
                STA @lMJUMPADDR+2
                LDA @lCPUPC+1       ; Write PCH
                STA @lMJUMPADDR+1
                LDA @lCPUPC         ; Write PCL
                STA @lMJUMPADDR
                LDA @lCPUFLAGS      ; Push processor status
                PHA

                setal
                LDA @lCPUA          ; Restore A
                PLP                 ; And the status register

                JML MJUMPINST       ; And jump to the target address

;
;C     COMPARE      Start1 Start2 [Len (1 if blank)]
;
; Inputs:
;   MARG1 = Starting Address 1
;   MARG2 = Starting Address 2
;   MARG3 = Number of bytes to compare (default: 1)
;
; Author:
;   PJW
;
IMCOMPARE       .proc
                PHP
                PHD
                PHB

                setdbr `MERRARGS
                setdp MONITOR_VARS

                setxl
                setas
                LDA MARG_LEN                ; Check the number of arguments provided
                CMP #2
                BEQ default_len             ; If 2: set MCOUNT to default of 1
                CMP #3
                BNE bad_arguments           ; Otherwise, if not 3: print an error

                setal
                LDA MARG3                   ; If 3: set MCOUNT to MARG3
                STA MCOUNT
                BRA compare

default_len     setal
                LDA #1                      ; No length was provided, set MCOUNT to 1
                STA MCOUNT
                BRA compare

bad_arguments   LDX #<>MERRARGS             ; The wrong number of arguments was provided
                JSL IPRINTS                 ; Print an error
                BRA done

compare         LDA MARG1                   ; Set MTEMP to MARG1
                STA MTEMP
                LDA MARG1+2
                STA MTEMP+2

                LDY #0
loop            setas
                LDA [MTEMP]                 ; Compare the byte at MTEMP
                CMP [MARG2],Y               ; To the Yth byte from MARG2
                BEQ continue                ; If they're the same, keep going

mismatch        JSL M_PR_ADDR               ; If they're different, print MTEMP
                LDA #' '
                JSL IPUTC

continue        setal
                CLC                         ; Either way, increment MTEMP
                LDA MTEMP
                ADC #1
                STA MTEMP
                LDA MTEMP+2
                ADC #0
                STA MTEMP+2

                INY                         ; Increment Y
                CPY MCOUNT                  ; Try again unless we've checked MCOUNT bytes
                BNE loop

                JSL IPRINTCR           

done            PLB
                PLD
                PLP
                RTL
                .pend

;
; Registers -- modify registers (showing registers is done by MSTATUS)
; This should be called in response to the ';' command
;
;   PC     A    X    Y    SP   DBR DP   NVMXDIZC
; ; 000000 0000 0000 0000 0000 00  0000 00000000
;
; Inputs:
;   MARG1 = PC
;   MARG2 = A
;   MARG3 = X
;   MARG4 = Y
;   MARG5 = SP
;   MARG6 = DBR
;   MARG7 = DP
;   MARG8 = Flags (parsed from binary)
;
; Author:
;   PJW
;
IMRMODIFY       .proc
                PHP
                PHD
                PHB

                setdbr 0
                setdp MONITOR_VARS

                setas
                LDA MARG_LEN        ; Check the number of arguments
                BEQ done            ; 0? Just quit

                LDX MARG1           ; Set the PC and PBR
                STX #CPUPC,B
                LDX MARG1+2
                STX #CPUPBR,B

                CMP #1              ; Check the number of arguments
                BEQ done            ; 1? Just quit

                LDX MARG2           ; Set A
                STX #CPUA,B

                CMP #2              ; Check the number of arguments
                BEQ done            ; 2? Just quit

                LDX MARG3           ; Set X
                STX #CPUX,B

                CMP #3              ; Check the number of arguments
                BEQ done            ; 3? Just quit

                LDX MARG4           ; Set Y
                STX #CPUY,B

                CMP #4              ; Check the number of arguments
                BEQ done            ; 4? Just quit

                LDX MARG5           ; Set SP
                STX #CPUSTACK,B

                CMP #5              ; Check the number of arguments
                BEQ done            ; 5? Just quit

                setxs
                LDX MARG6           ; Set DBR
                STX #CPUDBR,B

                CMP #6              ; Check the number of arguments
                BEQ done            ; 6? Just quit

                setxl
                LDX MARG7           ; Set DP
                STX #CPUDP,B

                CMP #7              ; Check the number of arguments
                BEQ done            ; 7? Just quit

                setxs
                LDX MARG8           ; Set flags
                STX #CPUFLAGS,B   

done            PLB
                PLD
                PLP
                RTL
                .pend

;
; Execute the current command line (requires MCMD and MARG1-MARG8 to be populated)
;
; Inputs:
;   MCMD = the command (single letter) to execute
;   MARG1..MARG9 = the arguments provided to the command
;   MARG_LEN = the number of arguments passed
;
IMEXECUTE       .proc
                PHP
                PHD
                PHB

                setdbr `IMEXECUTE
                setdp MONITOR_VARS

                setas
                setxl

                LDX #0
loop            LDA MCOMMANDS,X         ; Get the Xth command key
                BEQ not_found           ; Is it NUL? we should just return
                CMP MCMD                ; Otherwise, is it the command?
                BEQ found_cmd           ; Yes: we found the command

                INX                     ; No: try the next one
                BRA loop

                ; Found the command...
found_cmd       TXA                     ; Take the index
                ASL A                   ; Multiply it by 2
                TAX

                PLB
                PLD
                PLP
                JMP (cmd_dispatch,X)    ; And jump via that position in the index table

cmd_dispatch    .word <>MASSEMBLE
                .word <>MCOMPARE
                .word <>MDISASSEMBLE
                .word <>MFILL
                .word <>MGO
                .word <>MJUMP
                .word <>MHUNT
                .word <>MLOAD
                .word <>MMEMORY
                .word <>MREGISTERS
                .word <>MRMODIFY
                .word <>MSAVE
                .word <>MTRANSFER
                .word <>MVERIFY
                .word <>MEXIT
                .word <>MMODIFY
                .word <>MDOS

not_found       PLB
                PLD
                PLP
                RTL
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
IMASSEMBLEA     BRK ; Assemble a line of text.
IMLOAD          BRK ; Load data from disk. Device=1 (internal floppy) Start=Address in file
IMSAVE          BRK ; Save memory to disk
IMVERIFY        BRK ; Verify memory and file on disk
IMEXIT          BRK ; Exit monitor and return to BASIC command prompt
IMDOS           BRK ; Execute DOS command

;
; MMESSAGES
; MONITOR messages and responses.
MMESSAGES
MMERROR         .text
MERRARGS        .null "Bad arguments"

mregisters_msg  .null $0D,"  PC     A    X    Y    SP   DBR DP   NVMXDIZC"
MCOMMANDS       .null "ACDFGJHLMR;STVX>@"
