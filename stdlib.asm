;
; IPRINT
; Print a string, followed by a carriage return
; DBR: bank containing string
; X: address of the string in data bank
; Modifies: X
;
IPRINT_ABS      JSL IPUTS_ABS
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
IPUTS_ABS       PHA
                PHP
                setas
                setxl
                STA LDA_instruction+3
LDA_instruction LDA $FF0000,X      ; read from the string
                CMP #0
                BEQ iputs_done_ABS
iputs2_ABS      JSL IPUTC
iputs3_ABS      INX
                JMP LDA_instruction
iputs_done_ABS  INX
                PLP
                PLA
                RTL

;
; Print a byte converted in two hex char to the UART
;
; Inputs:
;   A = the character to print
;
IPRINT_HEX   .proc
            PHP
            setaxl
            PHX
            PHA             ; save the value before converting the High part into ASCII
            LDA #$0
            setas
            LDA #1, S       ; get the original value out of the stack
            LSR A             ; Extracting the high part of the byte
            LSR A
            LSR A
            LSR A
            AND #$F
            LDX A
            LDA hex_digits_stdlib,x
            JSL IPUTC
            LDA #1, S       ; get the original value out of the stack
            AND #$F         ; Extracting the low part of the byte
            LDX A
            LDA hex_digits_stdlib,x
            JSL IPUTC
            setaxl
            PLA
            PLX
            PLP
            RTL
            .pend
hex_digits_stdlib      .text "0123456789ABCDEF",0
