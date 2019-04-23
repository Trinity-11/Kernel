; Register Address
FDD_STATUS_A         = $AF13F0 ; Read
FDD_STATUS_B         = $AF13F1 ; Read
FDD_DIGITAL_OUTPUT   = $AF13F2 ; Read/Write
FDD_TAPE_DRIVER      = $AF13F3 ; Read/Write
FDD_MAIN_STATUE      = $AF13F4 ; Read
FDD_DATA_RATE_SELECT = $AF13F4 ; Write
FDD_FIFO             = $AF13F5 ; Read/Write
FDD_DIGITAL_INPUT    = $AF13F7 ; Read
FDD_CONFIG_CTRL      = $AF13F7 ; Write

;-------------------------------------------------------------------------------
; Status A : PS/2 Mode  (0xAF13F0)
FFD_DIRECTION         = $01 ; "1" Head is moving inward direction, "0" it moving outward direction
FDD_nWRITE_PROTECT    = $02 ; "0" protected / "1" unprotected
FDD_nINDEX            = $04 ;
FDD_HEAD_SELLECT      = $08 ; "1" side 1 selected / "0" side 0 selected
FDD_nTRACK0           = $10 ; "0" => head at track 0
FDD_STEP              = $20 ;
FDD_INTERRUPT_PENDING = $80 ; "1" interrupt output is active

; Status A : PS/2 Modele 30 Mode (0xAF13F0)
FDD_nDIRECTION        = $01 ; "0" Head is moving inward direction, "1" it moving outward direction
FDD_WRITE_PROTECT     = $02 ; "1" protected / "0" unprotected
FDD_INDEX             = $04 ;
FDD_nHEAD_SELLECT     = $08 ; "0" side 1 selected / "1" side 0 selected
FDD_TRACK0            = $10 ; "1" => head at track 0
;FDD_STEP              = $20 ;
FDD_DMA_REQUEST       = $40 ; "1" => DMA requeste pending
;FDD_INTERRUPT_PENDING = $80 ; "1" interrupt output is active
;-------------------------------------------------------------------------------
; Status B PS/2 Mode (0xAF13F1)
FDD_MOTOR_ENABLE_0    = $01 ; "1" => motor enabled
FDD_MOTOR_ENABLE_1    = $02 ; "1" => motor enabled
FDD_WRITE_GATE        = $04 ;
FDD_READ_DATA_TOOGLE  = $08 ;
FDD_WRITE_DATA_TOOGLE = $10 ;
FDD_DRIVER_SELLECT_0  = $20 ; bit reflet the bite 0 in "Data Outpu Register"

; Status B PS/2 Model 30 Mode (0xAF13F1)
;FDD_WRITE_GATE        = $04 ;
FDD_READ_DATA         = $08 ;
FDD_WRITE_DATA        = $01 ;
;FDD_DRIVER_SELLECT_0  = $20 ;
FDD_nDRIVE_SELLECT_1  = $40 ;
;-------------------------------------------------------------------------------
; Digital Output Register (0xAF13F2)
FDD_DRIVE_SEL         = $03
FDD_nRESET            = $04 ; Set at 1 will reset the FDD write back 0 to reactivate it
FDD_DMAEN             = $08 ; Set at 1 will active the DMA and interupt IF in PC/AT and Model 30 mode otherwise alreaddy active
FDD_ENABLE_MOTOR_0    = $10
FDD_ENABLE_MOTOR_1    = $20
;FDD_ENABLE_MOTOR_2    = $40 ; not suported in the LPC47M10X
;FDD_ENABLE_MOTOR_3    = $80 ; not suported in the LPC47M10X
;-------------------------------------------------------------------------------
; Tape Drive Register  (0xAF13F3)
FDD_TAPE_SEL          = $03
FDD_FLOPPY_BOOT_DRIVE = $0C
FDD_DRIVE_ID          = $30
;-------------------------------------------------------------------------------
; Main Status Register (0xAF13F4 READ)
FDD_DRIVER_BUSY       = $03
FDD_CMD_BUSSY         = $10 ; set to "1" when a command is in progress
FDD_NO_DMA            = $20
FDD_DIO               = $40
FDD_RQM               = $80

; Data Rate Select Register (0xAF13F4 WRITE)
FDD_DATA_RATE         = $03
FDD_PRE_COMP          = $1C
FDD_LOW_POWER         = $40 ; write 1 to activeate
FDD_SOFTWARE_RESET    = $80 ; write 1 to reset the controler , this bit will reset himself
;-------------------------------------------------------------------------------
; Data Register (0xAF13F5 READ)
;-------------------------------------------------------------------------------
; Digital Input register (0xAF13F6 READ ONLY)
FDD_nHIGH_ENSITY      = $01 ; "0" if 500Kbps or 1Mbps / "1" if 250 or 300Kpbs
FDD_DRATE_SEL_PS2     = $06
FDD_DRATE_SEL_MODE_30 = $03
FDD_NOPREC            = $04
FDD_DMAEN_MODE_30     = $08
FDD_DSKCHG            = $80 ; Disk Change state
;-------------------------------------------------------------------------------
; Configuration Control Register (0xAF13F7 WRITE)
FDD_DRATE_SEL         = $03
;FDD_NOPREC            = $04

;-------------------------------------------------------------------------------
;
; Value send back by the controler afer executing command
;
;-------------------------------------------------------------------------------
; Status register 0
FDD_ST0_DRIVE_SELLECT       = $03 ; give the curent sellected drive
FDD_ST0_HEAD_ADDRESS        = $04
FDD_ST0_EQUIPMENT_CHeCK     = $10 ; "1" if fail
FDD_ST0_SEEK_END            = $20
FDD_ST0_INTERRUPT_COE       = $C0 ; "00" Normal termination
                                  ; "01" Faill executing CMD
                                  ; "10" Invalide CMD
                                  ; "11" canceled CMD due to pooling

; Status register 1
FDD_ST1_MISSIG_ADDRESS_MARK = $01
FDD_ST1_NOT_WRITABLE        = $02
FDD_ST1_NO_DATA             = $04
FDD_ST1_OVERRUN_UDNERRUN    = $10
FDD_ST1_DATTA_ERROR         = $20
FDD_ST1_END_OF_CYLINDER     = $80

; Status register 2
FDD_ST2_MISSIG_DATA_ADDRESS_MARK = $01
FDD_ST2_BAD_CYLINDER        = $02
FDD_ST2_WRONG_CYLINDER      = $10
FDD_ST2_DATA_ERROR_IN_DATA_FIELD = $20
FDD_ST2_CONTROL_MASK        = $40

; Status register 3
FDD_ST3_DRIVE_SELLECT       = $03
FDD_ST3_HEAD_ADDRESS        = $04
FDD_ST3_TRACK_0             = $10
FDD_ST3_WRITE_PROTECT       = $40
