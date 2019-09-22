.cpu "65816"
.include "LPT_def.asm"

LPT_DATA
LPT_Temp_data       .text $0
; thit code is handelling the LPT hardware for normal LPT comunication mode
; (unidirectional), EPP and the ECP mode

;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Set the HostClk(nStrob) signal to "0" ot "1"
;-------------------------------------------------------------------------------

IECP_SET_HostClk_LINE_LOW
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$1
                  TSB ECP_DCR

                  PLD
                  PLP
                  RTL

IECP_SET_HostClk_LINE_HIGH
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$1
                  TRB ECP_DCR

                  PLD
                  PLP
                  RTL

;-------------------------------------------------------------------------------
; Set the HostAck(nAutoFd) signal to "0" ot "1"
;-------------------------------------------------------------------------------

IECP_SET_HostAck_LINE_LOW
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$2
                  TSB ECP_DCR

                  PLD
                  PLP
                  RTL

IECP_SET_HostAck_LINE_HIGH
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$2
                  TRB ECP_DCR

                  PLD
                  PLP
                  RTL

;-------------------------------------------------------------------------------
; Set the nReverseRequest(nInit) signal to "0" ot "1"
;-------------------------------------------------------------------------------

IECP_SET_nReverseRequest_LINE_HIGH
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$4
                  TSB ECP_DCR

                  PLD
                  PLP
                  RTL

IECP_SET_nReverseRequest_LINE_LOW
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$4
                  TRB ECP_DCR

                  PLD
                  PLP
                  RTL
;
;-------------------------------------------------------------------------------
; Set the ECPMode(nSelectln) signal to "0" ot "1"
;-------------------------------------------------------------------------------

IECP_SET_ECPMode_LINE_LOW
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$8
                  TSB ECP_DCR

                  PLD
                  PLP
                  RTL

IECP_SET_ECPMode_LINE_HIGH
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$8
                  TRB ECP_DCR

                  PLD
                  PLP
                  RTL

;-------------------------------------------------------------------------------
; Active or deactive the interrupt generation
;-------------------------------------------------------------------------------

IECP_ACTIVE_INTERRUPT
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$10
                  TSB ECP_DCR

                  PLD
                  PLP
                  RTL

IECP_DEACTIVE_INTERRUPT
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$10
                  TRB ECP_DCR

                  PLD
                  PLP
                  RTL

;-------------------------------------------------------------------------------
; Set the LPT dirrection
;-------------------------------------------------------------------------------

IECP_SET_DATA_IN
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$20
                  TSB ECP_DCR

                  PLD
                  PLP
                  RTL

IECP_SET_DATA_OUT
                  PHP
                  PHD
                  setas

                  setdbr `ECP_DCR
                  LDA #$20
                  TRB ECP_DCR

                  PLD
                  PLP
                  RTL
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Set the LPT module in ECP mode
;-------------------------------------------------------------------------------

IECP_SET_LPT_TO_ECP_MODE
                  PHP
                  PHD
                  setaxl
                  PHA
                  setas

                  LDA LPT_SET_ECP_MODE_CMD;
                  setdbr `LPT_SET_ECP_MODE_ADRESS

                  LDA #0 ; mode 0 used to start the negociation
                  JSL IECP_SET_ECP_MODE
                  setaxl
                  PLA
                  PLD
                  PLP
                  RTL

;
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
;-------------------------------------------------------------------------------
; Set the LPT module in ECP mode
;-------------------------------------------------------------------------------

IECP_NEGOCIATE    PHP
                  PHD
                  setaxl
                  PHX
                  PHA

                  setas
                  LDA #$10          ; Request ECP Mode
                  setdbr `ECP_DATA
                  STA ECP_DATA

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
; Sellect the ECP mode :
;
; A Reg : mode to write in cr
; 000 SPP mode
; 001 PS/2 Parallel Port mode
; 010 Parallel Port Data FIFO mode
; 011 ECP Parallel Port mode
; 100 EPP mode (If this option is enabled in the configuration registers)
; 101 Reserved
; 110 Test mode
; 111 Configuration mode
;-------------------------------------------------------------------------------

IECP_SET_ECP_MODE PHP
                  PHD
                  setaxl
                  PHX

                  ; prepare the mode value
                  setaxs
                  AND #$7           ; keep only the forst 3 bits of the mode value
                  ASL               ; move the msb on bit position 5
                  ASL
                  ASL
                  ASL
                  ASL
                  setdbr `LPT_Temp_data
                  STA LPT_Temp_data     ; save ther mode value

                  setdbr `ECP_ECR   ; Get the curent ECR value
                  LDA ECP_ECR
                  AND #$1F
                  setdbr `LPT_Temp_data
                  ORA LPT_Temp_data
                  STA ECP_ECR       ; write the new value back

                  setaxl
                  PLX
                  PLD
                  PLP
                  RTL
