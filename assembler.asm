;;;
;;; Disassembler portion of monitor
;;;

;
; Macros
;
MNEMONIC    .macro ; text, type, opcode
            .text \1
            .byte \2,\3
            .endm

;
; Definitions
;

; Addressing modes
ADDR_DP_IND_X = 0       ; (dd,X)
ADDR_DP = 1             ; dd
ADDR_IMM = 2            ; #dd
ADDR_ABS = 3            ; dddd
ADDR_DP_IND_Y = 4       ; (dd),Y
ADDR_DP_X = 5           ; dd,X
ADDR_ABS_Y = 6          ; dddd,Y
ADDR_ABS_X = 7          ; dddd,X
ADDR_ACC = 8            ; A
ADDR_SP_R = 9           ; #d,S
ADDR_DP_LONG = 10       ; [dd]
ADDR_ABS_LONG = 11      ; dddddd
ADDR_SP_R_Y = 12        ; #dd,S,Y
ADDR_DP_Y_LONG = 13     ; [dd],Y
ADDR_ABS_X_LONG = 14    ; dddddd,Y
ADDR_DP_IND = 15        ; (dd)
ADDR_ABS_X_ID = 16      ; (dddd,X)
ADDR_DP_Y = 17          ; dd,Y
ADDR_PC_REL = 18        ; PC relative
ADDR_IMPLIED = 19       ; Implied (no operand)
ADDR_XYC = 20           ; #dd, #dd
ADDR_ABS_IND = 21       ; (dddd)
ADDR_PC_REL_LONG = 22   ; PC relative ()

OP_M_EFFECT = $80       ; Flag to indicate instruction is modified by M
OP_X_EFFECT = $40       ; Flag to indicate instruction is modified by X

MN_CLASS_00 = 0
MN_CLASS_01 = 1
MN_CLASS_10 = 2
MN_CLASS_11 = 3
MN_CLASS_IRR = 4
MN_CLASS_SB = 5
MN_CLASS_BR = 6

;
; Disassembler code
;
                
SAMPLE          .al
                TAY
                NOP

;
; Disassemble a block of memeory
;
; Inputs:
;   MARG1 = start address of the block (optional)
;   MARG2 = end address of the block (optional)
;
DISASSEMBLE     .proc
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
                BRA dasm_loop

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

dasm_loop       JSL DS_PR_LINE

                ; Check to see if we've printed the last byte
                LDA MCURSOR+2           ; Are the banks the same?
                CMP MARG2+2
                BLT dasm_loop           ; No: continue
                setal
                LDA MCURSOR             ; Are the lower bits the same?
                CMP MARG2
                BLT dasm_loop           ; Nope... keep going

done            PLD
                PLB
                PLP
                RTL
                .pend

;
; Print a line of assembly assuming MCURSOR points to the opcode
;
; The disassemble command should call this repeatedly until MCURSOR >= the desired end address
;
DS_PR_LINE      .proc
                PHP
                PHD

                setas               ; Print "A "
                LDA #'A'
                JSL IPUTC
                LDA #' '
                JSL IPUTC

                setdp MCURSOR

                setal               ; Print the address of the line
                LDA MCURSOR
                STA MTEMP
                setas
                LDA MCURSOR+2
                STA MTEMP+2
                JSL M_PR_ADDR

                LDA #' '
                JSL IPUTC

                setas
                setxl
                LDA [MCURSOR]
                setal
                AND #$00FF
                ASL A
                TAX                     ; Get the index into the mnemonic lookup table

                LDA MNEMONIC_TAB,X      ; Get the mnemonic
                TAX
                JSL DS_PR_MNEMONIC      ; And print it

                setas
                LDA [MCURSOR]
                TAX

pr_operand      LDA ADDRESS_TAB,X       ; Get the addressing mode for the instruction
                JSL M_INC_CURSOR        ; Advance the cursor to the next byte
                JSL DS_PR_OPERAND       ; And print the correct operand

                JSL IPRINTCR
                PLD
                PLP
                RTL
                .pend

;
; Print an unknown opcode ???
;
DS_PR_UNKNOWN   .proc
                PHP
                setas
                .rept 3
                LDA #'?'
                JSL IPUTC
                .next
                PLP
                RTL
                .pend

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
; Print the operand for an instruction
;
; Inputs:
;   MCURSOR = pointer to the operand, assuming there is one
;   A = the addressing mode code (one of the ADDR_ values)
;   Y = mode bit to check for immediate operand length (OP_M_EFFECT for Accumulator, OP_X_EFFECT for X/Y, or none)
;
; On Return:
;   MCURSOR = pointer to the first byte of the next instruction
;
DS_PR_OPERAND   ;.proc
                PHP
                setas

                PHA         ; Save the address mode so we can get to the M and X flags

                ASL A       ; Compute the index to the table
                setxl
                TAX

                PLA         ; Restore A

                JMP (dispatch,X)
dispatch        .word <>is_dp_ind_x
                .word <>is_dp
                .word <>is_imm
                .word <>is_abs
                .word <>is_dp_ind_y
                .word <>is_dp_x
                .word <>is_abs_y
                .word <>is_abs_x
                .word <>is_accumulator
                .word <>is_stack_r
                .word <>is_dp_long
                .word <>is_abs_long
                .word <>is_stack_r_y
                .word <>is_dp_y_long
                .word <>is_abs_x_long
                .word <>is_dp_ind
                .word <>is_abs_x_id
                .word <>is_dp_y
                .word <>is_pc_rel
                .word <>is_implied
                .word <>is_xyc
                .word <>is_abs_ind
                .word <>is_pc_rel_long

                ; Addressing mode is zero-page-indirect-x
is_dp_ind_x     LDA #'('                ; Print (dd,X)
                JSL IPUTC
                JSL DS_PR_OPERAND1      ; Print dd
                LDA #','
                JSL IPUTC
                LDA #'X'
                JSL IPUTC
                LDA #')'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is zero-page
is_dp           JSL DS_PR_OPERAND1      ; Print dd
                JMP done_1

                ; Addressing mode is immediate
is_imm          setas                   
                LDA #'#'
                JSL IPUTC

                AND $%11000000          ; Filter so we just look at the mode bits
                CMP #$00                ; Are any set to check?
                BEQ is_imm_short        ; No: treat it as a short always
                AND MCPUSTAT            ; Otherwise, filter the mode bit we care about
                BNE is_imm_short        ; If it is set, immediate operation is short

                ; Otherwise, the immediate should be a long
                JSL DS_PR_OPERAND2      ; Print dddd
                JMP done_1

                ; the immediate should be a short
is_imm_short    JSL DS_PR_OPERAND1      ; Print dd
                JMP done_1

                ; Addressing mode is absolute
is_abs          JSL DS_PR_OPERAND2      ; Print dddd
                JMP done_1

                ; Addressing mode is zero-page-indirect-y
is_dp_ind_y     LDA #'('                ; Print (dd),Y
                JSL IPUTC
                JSL DS_PR_OPERAND1      ; Print dd
                LDA #')'
                JSL IPUTC
                LDA #','
                JSL IPUTC
                LDA #'Y'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is zero-page-x
is_dp_x         JSL DS_PR_OPERAND1      ; Print dd,X
                LDA #','
                JSL IPUTC
                LDA #'X'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is zero-page-y
is_dp_y         JSL DS_PR_OPERAND1      ; Print dd,Y
                LDA #','
                JSL IPUTC
                LDA #'Y'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is absolute-y
is_abs_y        JSL DS_PR_OPERAND2      ; Print dddd,Y
                LDA #','
                JSL IPUTC
                LDA #'Y'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is absolute-x
is_abs_x        JSL DS_PR_OPERAND2      ; Print dddd,X
                LDA #','
                JSL IPUTC
                LDA #'X'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is absolute x indirect
is_abs_x_id     LDA #'('
                JSL IPUTC
                JSL DS_PR_OPERAND2      ; Print (dddd,X)
                LDA #','
                JSL IPUTC
                LDA #'X'
                JSL IPUTC
                LDA #')'
                JSL IPUTC
                JMP done_1

is_dp_ind       LDA #'('
                JSL IPUTC
                JSL DS_PR_OPERAND1      ; Print (dd)
                LDA #')'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is accumulator
is_accumulator  LDA #'A'                ; Print A
                JSL IPUTC
                JMP done

                ; Addressing mode is stack relative
is_stack_r      LDA #'#'                ; #dd,S
                JSL IPUTC

                JSL DS_PR_OPERAND1      ; Print dd

                LDA #','
                JSL IPUTC
                LDA #'S'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is direct page long
is_dp_long      LDA #'['                ; [dd]
                JSL IPUTC

                JSL DS_PR_OPERAND1      ; Print dd

                LDA #']'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is absolute long
is_abs_long     JSL DS_PR_OPERAND3      ; Print dddddd
                JMP done_1

                ; Addressing mode is stack relative indirect indexed by Y
is_stack_r_y    LDA #'('                ; (dd,S),Y
                JSL IPUTC

                JSL DS_PR_OPERAND1      ; Print dd

                LDA #','
                JSL IPUTC
                LDA #'S'
                JSL IPUTC
                LDA #')'
                JSL IPUTC
                LDA #','
                JSL IPUTC
                LDA #'Y'
                JSL IPUTC
                JMP done_1

                ; Addressing mode is direct page indirected indexed by Y long
is_dp_y_long    LDA #'['                ; [dd],Y
                JSL IPUTC

                JSL DS_PR_OPERAND1      ; Print dd

                LDA #']'
                JSL IPUTC
                LDA #','
                JSL IPUTC
                LDA #'Y'
                JSL IPUTC
                JMP done_1

is_pc_rel_long  LDY #2
                BRA do_pcrel

is_pc_rel       LDY #1
do_pcrel        JSL DS_PR_PCREL
                JMP done

                ; Addressing mode is implied, there is not operand byte
is_implied      JMP done

                ; Addressing mode is absolute long indexed by X
is_abs_x_long   JSL DS_PR_OPERAND3      ; Print dddddd
                LDA #','
                JSL IPUTC
                LDA #'X'
                JSL IPUTC
                JMP done_1

                ; Addressing mode with two immediates (used by move instructions)
is_xyc          LDA #'#'
                JSL IPUTC

                PHB                     ; Make sure the databank is pointed to our number
                LDA MCURSOR+2
                PHA
                PLB

                LDX MCURSOR             ; Print dd
                INX
                LDY #1
                JSL IPRINTH

                LDA #','
                JSL IPUTC

                LDA #'#'
                JSL IPUTC
                LDX MCURSOR             ; Print dd
                LDY #1
                JSL IPRINTH

                PLB                     ; Get our old data bank back

                JSL M_INC_CURSOR
                JSL M_INC_CURSOR
                JMP done_1

is_abs_ind      LDA #'('
                JSL IPUTC
                JSL DS_PR_OPERAND2      ; Print (dddd)
                LDA #')'
                JSL IPUTC
                JMP done_1

done_1          JSL M_INC_CURSOR    ; Skip over a single byte operand
done            PLP
                RTL
                ;.pend

;
; Print a single byte operand value
;
; Inputs:
;   MCURSOR = pointer to the single byte operand to print
;
DS_PR_OPERAND1  .proc
                PHP
                PHB

                setas
                LDA MCURSOR+2
                PHA
                PLB

                setaxl
                LDX MCURSOR
                LDY #1
                JSL IPRINTH

                PLB
                PLP
                RTL
                .pend

;
; Print a two byte operand value
;
; Inputs:
;   MCURSOR = pointer to the two byte operand to print
;
DS_PR_OPERAND2  .proc
                PHP
                PHB

                JSL M_INC_CURSOR

                setas
                LDA MCURSOR+2
                PHA
                PLB

                setaxl
                LDX MCURSOR
                LDY #2
                JSL IPRINTH

                PLB
                PLP
                RTL
                .pend

;
; Print a three byte operand value
;
; Inputs:
;   MCURSOR = pointer to the three byte operand to print
;
DS_PR_OPERAND3  .proc
                PHP
                PHB
                PHD
                setaxl
                PHY

                setdp MCURSOR
                setdbr `MTEMP

                setas
                LDY #0
copy_loop       LDA [MCURSOR]       ; Copy the address pointed to by MCURSOR
                STA MTEMP,Y         ; to MTEMP
                JSL M_INC_CURSOR
                INY
                CPY #3
                BNE copy_loop

                JSL M_PR_ADDR       ; Print the address

                setaxl
                PLY
                PLD
                PLB
                PLP
                RTL
                .pend

;
; Print the mnemonic indicated by X
; All mnemonics are assumed to be 3 characters long
;
DS_PR_MNEMONIC  .proc
                PHP
                PHB
                setas
                setxl
                setdbr `MN_ORA

                .rept 3             ; Print the three characters
                LDA #0,B,X
                JSL IPUTC
                INX
                .next

                LDA #' '            ; Print a space
                JSL IPUTC

                PLB
                PLP
                RTL
                .pend

;
; Print a relative address where the offset is at MCURSOR
;
; Inputs:
;   MCURSOR points to the offset byte(s)
;   Y = number of bytes in the offset
;
DS_PR_PCREL     ;.proc
                PHP
                PHD

                ; Save sign-extended offste to MTEMP
                setdp MCURSOR
                setas

                CPY #2
                BEQ offset_2
                LDA [MCURSOR]
                STA MTEMP
                BMI is_negative
                STZ MTEMP+1
                STZ MTEMP+2
                BRA add_offset
is_negative     LDA #$FF
                STA MTEMP+1
                STA MTEMP+2
                BRA add_offset

offset_2        LDA [MCURSOR]
                STA MTEMP
                JSL M_INC_CURSOR
                LDA [MCURSOR]
                STA MTEMP+1
                BMI is_negative2
                STZ MTEMP+2
                BRA add_offset
is_negative2    LDA #$FF
                STA MTEMP+2

                ; Add the offset to the current address
add_offset      setal
                SEC             ; Add 1 to the offset
                LDA MCURSOR
                ADC MTEMP
                STA MTEMP
                setas
                LDA MCURSOR+2
                ADC MTEMP+2
                STA MTEMP+2

                ; And print the resulting address
                JSL M_PR_ADDR

                PLD
                PLP
                RTL
                ;.pend

;
; Print a 24-bit address in the format "BB:HHLL"
;
; Inputs:
;   MTEMP contains the address to print
; 
M_PR_ADDR       .proc
                PHP
                PHB
                setaxl
                PHA
                PHX
                PHY

                setdbr `MTEMP

                LDX #<>MTEMP+2     ; Print the bank byte of the address
                LDY #1
                JSL IPRINTH

                setas
                LDA #':'
                JSL IPUTC

                LDX #<>MTEMP+1     ; Print the lower 16-bits of the address
                LDY #2
                JSL IPRINTH                

                setaxl
                PLY
                PLX
                PLA
                PLB
                PLP
                RTL
                .pend

;
; Mnemonics
;
MN_ORA      #MNEMONIC "ORA",MN_CLASS_01 | OP_M_EFFECT,0
MN_AND      #MNEMONIC "AND",MN_CLASS_01 | OP_M_EFFECT,0
MN_EOR      #MNEMONIC "EOR",MN_CLASS_01 | OP_M_EFFECT,0
MN_ADC      #MNEMONIC "ADC",MN_CLASS_01 | OP_M_EFFECT,0
MN_STA      #MNEMONIC "STA",MN_CLASS_01 | OP_M_EFFECT,0
MN_LDA      #MNEMONIC "LDA",MN_CLASS_01 | OP_M_EFFECT,0
MN_CMP      #MNEMONIC "CMP",MN_CLASS_01 | OP_M_EFFECT,0
MN_SBC      #MNEMONIC "SBC",MN_CLASS_01 | OP_M_EFFECT,0
MN_ASL      #MNEMONIC "ASL",MN_CLASS_10 | OP_M_EFFECT,0
MN_ROL      #MNEMONIC "ROL",MN_CLASS_10 | OP_M_EFFECT,0
MN_LSR      #MNEMONIC "LSR",MN_CLASS_10 | OP_M_EFFECT,0
MN_ROR      #MNEMONIC "ROR",MN_CLASS_10 | OP_M_EFFECT,0
MN_STX      #MNEMONIC "STX",MN_CLASS_10 | OP_X_EFFECT,0
MN_LDX      #MNEMONIC "LDX",MN_CLASS_10 | OP_X_EFFECT,0
MN_DEC      #MNEMONIC "DEC",MN_CLASS_10 | OP_M_EFFECT,0
MN_INC      #MNEMONIC "INC",MN_CLASS_10 | OP_M_EFFECT,0
MN_BIT      #MNEMONIC "BIT",MN_CLASS_00 | OP_M_EFFECT,0
MN_JMP      #MNEMONIC "JMP",MN_CLASS_IRR,0
MN_STY      #MNEMONIC "STY",MN_CLASS_00 | OP_X_EFFECT,0
MN_LDY      #MNEMONIC "LDY",MN_CLASS_00 | OP_X_EFFECT,0
MN_CPY      #MNEMONIC "CPY",MN_CLASS_00 | OP_X_EFFECT,0
MN_CPX      #MNEMONIC "CPX",MN_CLASS_00 | OP_X_EFFECT,0

MN_BRK      #MNEMONIC "BRK",MN_CLASS_SB,$00
MN_JSR      #MNEMONIC "JSR",MN_CLASS_IRR,$00
MN_RTI      #MNEMONIC "RTI",MN_CLASS_SB,$40
MN_RTS      #MNEMONIC "RTS",MN_CLASS_SB,$60
MN_PHP      #MNEMONIC "PHP",MN_CLASS_SB,$08
MN_PLP      #MNEMONIC "PLP",MN_CLASS_SB,$28
MN_PHA      #MNEMONIC "PHA",MN_CLASS_SB,$48
MN_PLA      #MNEMONIC "PLA",MN_CLASS_SB,$68
MN_DEY      #MNEMONIC "DEY",MN_CLASS_SB,$88
MN_TAY      #MNEMONIC "TAY",MN_CLASS_SB,$A8
MN_INY      #MNEMONIC "INY",MN_CLASS_SB,$C8
MN_INX      #MNEMONIC "INX",MN_CLASS_SB,$E8
MN_CLC      #MNEMONIC "CLC",MN_CLASS_SB,$19
MN_SEC      #MNEMONIC "SEC",MN_CLASS_SB,$38
MN_CLI      #MNEMONIC "CLI",MN_CLASS_SB,$58
MN_SEI      #MNEMONIC "SEI",MN_CLASS_SB,$78
MN_TYA      #MNEMONIC "TYA",MN_CLASS_SB,$98
MN_CLV      #MNEMONIC "CLV",MN_CLASS_SB,$B8
MN_CLD      #MNEMONIC "CLD",MN_CLASS_SB,$D8
MN_SED      #MNEMONIC "SED",MN_CLASS_SB,$F8
MN_TXA      #MNEMONIC "TXA",MN_CLASS_SB,$8A
MN_TXS      #MNEMONIC "TXS",MN_CLASS_SB,$9A
MN_TAX      #MNEMONIC "TAX",MN_CLASS_SB,$AA
MN_TSX      #MNEMONIC "TSX",MN_CLASS_SB,$BA
MN_DEX      #MNEMONIC "DEX",MN_CLASS_SB,$CA
MN_NOP      #MNEMONIC "NOP",MN_CLASS_SB,$EA

; Branches (follow format xxy10000)
MN_BPL      #MNEMONIC "BPL",MN_CLASS_BR,$10
MN_BMI      #MNEMONIC "BMI",MN_CLASS_BR,$30
MN_BVC      #MNEMONIC "BVC",MN_CLASS_BR,$50
MN_BVS      #MNEMONIC "BVS",MN_CLASS_BR,$70
MN_BCC      #MNEMONIC "BCC",MN_CLASS_BR,$90
MN_BCS      #MNEMONIC "BCS",MN_CLASS_BR,$B0
MN_BNE      #MNEMONIC "BNE",MN_CLASS_BR,$D0
MN_BEQ      #MNEMONIC "BEQ",MN_CLASS_BR,$F0

; 65C02
MN_TSB      #MNEMONIC "TSB",MN_CLASS_IRR,$00
MN_TRB      #MNEMONIC "TRB",MN_CLASS_IRR,$00
MN_STZ      #MNEMONIC "STZ",MN_CLASS_IRR,$00
MN_BRA      #MNEMONIC "BRA",MN_CLASS_IRR,$00
MN_PHY      #MNEMONIC "PHY",MN_CLASS_SB,$5A
MN_PLY      #MNEMONIC "PLY",MN_CLASS_SB,$7A
MN_PHX      #MNEMONIC "PHX",MN_CLASS_SB,$DA
MN_PLX      #MNEMONIC "PLX",MN_CLASS_SB,$FA

; 65816
MN_PHD      #MNEMONIC "PHD",MN_CLASS_SB,$0B
MN_PLD      #MNEMONIC "PLD",MN_CLASS_SB,$2B
MN_PHK      #MNEMONIC "PHK",MN_CLASS_SB,$4B
MN_RTL      #MNEMONIC "RTL",MN_CLASS_SB,$6B
MN_PHB      #MNEMONIC "PHB",MN_CLASS_SB,$8B
MN_PLB      #MNEMONIC "PLB",MN_CLASS_SB,$AB
MN_WAI      #MNEMONIC "WAI",MN_CLASS_SB,$CB
MN_XBA      #MNEMONIC "XBA",MN_CLASS_SB,$EB
MN_TCS      #MNEMONIC "TCS",MN_CLASS_SB,$1B
MN_TSC      #MNEMONIC "TSC",MN_CLASS_SB,$3B
MN_TCD      #MNEMONIC "TCD",MN_CLASS_SB,$5B
MN_TDC      #MNEMONIC "TDC",MN_CLASS_SB,$7B
MN_TXY      #MNEMONIC "TXY",MN_CLASS_SB,$9B
MN_TYX      #MNEMONIC "TYX",MN_CLASS_SB,$BB
MN_STP      #MNEMONIC "STP",MN_CLASS_SB,$DB
MN_XCE      #MNEMONIC "XCE",MN_CLASS_SB,$FB
MN_COP      #MNEMONIC "COP",MN_CLASS_IRR,$02
MN_JSL      #MNEMONIC "JSL",MN_CLASS_IRR,$22
MN_WDM      #MNEMONIC "WDM",MN_CLASS_IRR,$42
MN_PER      #MNEMONIC "PER",MN_CLASS_IRR,$62
MN_BRL      #MNEMONIC "BRL",MN_CLASS_IRR,$82
MN_REP      #MNEMONIC "REP",MN_CLASS_IRR,$C2
MN_SEP      #MNEMONIC "SEP",MN_CLASS_IRR,$E2
MN_MVP      #MNEMONIC "MVP",MN_CLASS_IRR,$44
MN_MVN      #MNEMONIC "MVN",MN_CLASS_IRR,$54
MN_PEI      #MNEMONIC "PEI",MN_CLASS_IRR,$D4
MN_PEA      #MNEMONIC "PEA",MN_CLASS_IRR,$F4
MN_JML      #MNEMONIC "JML",MN_CLASS_IRR,$DC

                ; A table of all 256 possible instruction slots, listing their mnemonics
MNEMONIC_TAB    .word <>MN_BRK, <>MN_ORA, <>MN_COP, <>MN_ORA, <>MN_TSB, <>MN_ORA, <>MN_ASL, <>MN_ORA    ; 0x
                .word <>MN_PHP, <>MN_ORA, <>MN_ASL, <>MN_PHD, <>MN_TSB, <>MN_ORA, <>MN_ASL, <>MN_ORA

                .word <>MN_BPL, <>MN_ORA, <>MN_ORA, <>MN_ORA, <>MN_TRB, <>MN_ORA, <>MN_ASL, <>MN_ORA    ; 1x
                .word <>MN_CLC, <>MN_ORA, <>MN_INC, <>MN_TCS, <>MN_TRB, <>MN_ORA, <>MN_ASL, <>MN_ORA

                .word <>MN_JSR, <>MN_AND, <>MN_JSL, <>MN_AND, <>MN_BIT, <>MN_AND, <>MN_ROL, <>MN_AND    ; 2x
                .word <>MN_PLP, <>MN_AND, <>MN_ROL, <>MN_PLD, <>MN_BIT, <>MN_AND, <>MN_ROL, <>MN_AND

                .word <>MN_BMI, <>MN_AND, <>MN_AND, <>MN_AND, <>MN_BIT, <>MN_AND, <>MN_ROL, <>MN_AND    ; 3x
                .word <>MN_SEC, <>MN_AND, <>MN_DEC, <>MN_TSC, <>MN_BIT, <>MN_AND, <>MN_ROL, <>MN_AND

                .word <>MN_RTI, <>MN_EOR, <>MN_WDM, <>MN_EOR, <>MN_MVP, <>MN_EOR, <>MN_LSR, <>MN_EOR    ; 4x
                .word <>MN_PHA, <>MN_EOR, <>MN_LSR, <>MN_PHK, <>MN_JMP, <>MN_EOR, <>MN_LSR, <>MN_EOR

                .word <>MN_BVC, <>MN_EOR, <>MN_EOR, <>MN_EOR, <>MN_MVN, <>MN_EOR, <>MN_LSR, <>MN_EOR    ; 5x
                .word <>MN_CLI, <>MN_EOR, <>MN_PHY, <>MN_TCD, <>MN_JMP, <>MN_EOR, <>MN_LSR, <>MN_EOR

                .word <>MN_RTS, <>MN_ADC, <>MN_PER, <>MN_ADC, <>MN_STZ, <>MN_ADC, <>MN_ROR, <>MN_ADC    ; 6x
                .word <>MN_PLA, <>MN_ADC, <>MN_ROR, <>MN_RTL, <>MN_JMP, <>MN_ADC, <>MN_ROR, <>MN_ADC

                .word <>MN_BVS, <>MN_ADC, <>MN_ADC, <>MN_ADC, <>MN_STZ, <>MN_ADC, <>MN_ROR, <>MN_ADC    ; 7x
                .word <>MN_SEI, <>MN_ADC, <>MN_PLY, <>MN_TDC, <>MN_JMP, <>MN_ADC, <>MN_ROR, <>MN_ADC

                .word <>MN_BRA, <>MN_STA, <>MN_BRL, <>MN_STA, <>MN_STY, <>MN_STA, <>MN_STX, <>MN_STA    ; 8x
                .word <>MN_DEY, <>MN_BIT, <>MN_TXA, <>MN_PHB, <>MN_STY, <>MN_STA, <>MN_STX, <>MN_STA

                .word <>MN_BCC, <>MN_STA, <>MN_STA, <>MN_STA, <>MN_STY, <>MN_STA, <>MN_STX, <>MN_STA    ; 9x
                .word <>MN_TYA, <>MN_STA, <>MN_TXS, <>MN_TXY, <>MN_STZ, <>MN_STA, <>MN_STZ, <>MN_STA

                .word <>MN_LDY, <>MN_LDA, <>MN_LDX, <>MN_LDA, <>MN_LDY, <>MN_LDA, <>MN_LDX, <>MN_LDA    ; Ax
                .word <>MN_TAY, <>MN_LDA, <>MN_TAX, <>MN_PLB, <>MN_LDY, <>MN_LDA, <>MN_LDX, <>MN_LDA

                .word <>MN_BCS, <>MN_LDA, <>MN_LDA, <>MN_LDY, <>MN_LDA, <>MN_LDY, <>MN_LDX, <>MN_LDA    ; Bx
                .word <>MN_CLV, <>MN_LDA, <>MN_TSX, <>MN_TYX, <>MN_LDY, <>MN_LDA, <>MN_LDX, <>MN_LDA

                .word <>MN_CPY, <>MN_CMP, <>MN_REP, <>MN_CMP, <>MN_CPY, <>MN_CMP, <>MN_DEC, <>MN_CMP    ; Cx
                .word <>MN_INY, <>MN_CMP, <>MN_DEX, <>MN_WAI, <>MN_CPY, <>MN_CMP, <>MN_DEC, <>MN_CMP

                .word <>MN_BNE, <>MN_CMP, <>MN_CMP, <>MN_CMP, <>MN_PEI, <>MN_CMP, <>MN_DEC, <>MN_CMP    ; Dx
                .word <>MN_CLD, <>MN_CMP, <>MN_PHX, <>MN_STP, <>MN_JML, <>MN_CMP, <>MN_DEC, <>MN_CMP

                .word <>MN_CPX, <>MN_SBC, <>MN_SEP, <>MN_SBC, <>MN_CPX, <>MN_SBC, <>MN_INC, <>MN_SBC    ; Ex
                .word <>MN_INX, <>MN_SBC, <>MN_NOP, <>MN_XBA, <>MN_CPX, <>MN_SBC, <>MN_INC, <>MN_SBC

                .word <>MN_BEQ, <>MN_SBC, <>MN_SBC, <>MN_SBC, <>MN_PEA, <>MN_SBC, <>MN_INC, <>MN_SBC    ; Fx
                .word <>MN_SED, <>MN_SBC, <>MN_PLX, <>MN_XCE, <>MN_JSR, <>MN_SBC, <>MN_INC, <>MN_SBC
                
                ; A table of all 256 possible instruction slots, listing their addressing modes
ADDRESS_TAB     .byte ADDR_IMPLIED, ADDR_DP_IND_X, ADDR_IMM, ADDR_SP_R                  ; 0x
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_IND
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_ACC, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; 1x
                .byte ADDR_DP, ADDR_DP_X, ADDR_DP_X, ADDR_DP_IND_Y
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_ACC, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG

                .byte ADDR_ABS, ADDR_DP_IND_X, ADDR_ABS_LONG, ADDR_SP_R                 ; 2x
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_IND
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_ACC, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; 3x
                .byte ADDR_DP_X, ADDR_DP_X, ADDR_DP_X, ADDR_DP_IND_Y
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_ACC, ADDR_IMPLIED
                .byte ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG

                .byte ADDR_IMPLIED, ADDR_DP_IND_X, ADDR_IMPLIED, ADDR_SP_R              ; 4x
                .byte ADDR_XYC, ADDR_DP, ADDR_DP, ADDR_DP_LONG
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_ACC, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; 5x
                .byte ADDR_XYC, ADDR_DP_X, ADDR_DP_X, ADDR_DP_IND_Y
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED, ADDR_ABS_LONG

                .byte ADDR_IMPLIED, ADDR_DP_IND_X, ADDR_PC_REL_LONG, ADDR_SP_R          ; 6x
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_IND
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_ACC, ADDR_IMPLIED
                .byte ADDR_ABS_IND, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_IMPLIED, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y             ; 7x
                .byte ADDR_DP_X, ADDR_DP_X, ADDR_DP_X, ADDR_DP_IND_Y
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS_X_ID, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_X, ADDR_PC_REL_LONG, ADDR_SP_R           ; 8x
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_IND
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; 9x
                .byte ADDR_DP_X, ADDR_DP_X, ADDR_DP_Y, ADDR_DP_Y_LONG
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG

                .byte ADDR_IMM | OP_X_EFFECT, ADDR_DP_IND_X, ADDR_IMM | OP_X_EFFECT, ADDR_SP_R  ; Ax
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_IND
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; Bx
                .byte ADDR_DP_X, ADDR_DP_X, ADDR_DP_Y, ADDR_DP_IND_Y
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_Y, ADDR_ABS_X_LONG

                .byte ADDR_IMM | OP_X_EFFECT, ADDR_DP_IND_X, ADDR_IMM, ADDR_SP_R        ; Cx
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_LONG
                .byte ADDR_IMPLIED, ADDR_IMM | OP_X_EFFECT, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; Dx
                .byte ADDR_DP, ADDR_DP_X, ADDR_DP_X, ADDR_DP_IND_Y
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS_IND, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG

                .byte ADDR_IMM | OP_X_EFFECT, ADDR_DP_IND_X, ADDR_IMM, ADDR_SP_R        ; Ex
                .byte ADDR_DP, ADDR_DP, ADDR_DP, ADDR_DP_IND
                .byte ADDR_IMPLIED, ADDR_IMM | OP_M_EFFECT, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS, ADDR_ABS, ADDR_ABS, ADDR_ABS_LONG

                .byte ADDR_PC_REL, ADDR_DP_IND_Y, ADDR_DP_IND, ADDR_SP_R_Y              ; Fx
                .byte ADDR_ABS, ADDR_DP_X, ADDR_DP_X, ADDR_DP_IND_Y                     ; TODO: different address mode for PEA?
                .byte ADDR_IMPLIED, ADDR_ABS_Y, ADDR_IMPLIED, ADDR_IMPLIED
                .byte ADDR_ABS_X_ID, ADDR_ABS_X, ADDR_ABS_X, ADDR_ABS_X_LONG
.dpage 0