.cpu "65816"
.include "FAT12_def.asm"

* = $19000

Byte_Per_Sector = $19000 ; 512 for a Floppy disk
Sector_Per_Cluster = $19002 ; 1
Nb_Of_reserved_Cluster = $19004 ; 1 , this number include the boot sector
Nb_Of_FAT = $19006 ; 2
Max_Root_Entry = $19008 ; 224  => 14 sector for the root directory and 32Byte foe en entry (14 * 512 / 32)
Total_Sector_Count = $1900A ; 2880 => 80 track of 18 sector on each 2 dide (80*18*2)
Sector_per_Fat = $1900C
Sector_per_Track = $1900E
Nb_of_Head = $19010
Total_Sector_Count_FAT32 = $19012
Boot_Signature = $19016
Volume_ID = $19018
Volume_Label =  $1901C
File_System_Type =  $19027

Sector_loaded_in_ram = $1902F ; updated by any function readding Sector from FDD like : IFAT12_READ_BOOT_SECTOR / IFAT12_GET_ROOT_DIR_POS
Root_entry_Sector_index = $19031 ; hold the sector index position of the fists Root directory sectoe
Root_entry_value = $19033 ; store the 32 byte of root entry

Fat12_Sector_index = $19053 ; store the sector index of the first fat sector
Fat12_next_entry = $19055  ; store the 12 bit fat entry
Fat12_Sector_loaded_in_ram = $19057 ; store the actual fat sector loades in ram
;Temp_data_FAT12 = $19059
;19061
* = $19060
;-------------------------------------------------------------------------------
;
;
;
;-------------------------------------------------------------------------------
IFAT12_READ_BOOT_SECTOR
                  setaxl
                  LDA #`FAT12_ADDRESS_BUFFER_512 ; load the byte nb 3 (bank byte)
                  PHA
                  LDA #<>FAT12_ADDRESS_BUFFER_512 ; load the low world part of the buffer address
                  PHA
                  LDA $0 ; read sector 0 (where the boot sector is stored)
                  JSL IFDD_READ
                  PLX
                  PLX;
                  STA Sector_loaded_in_ram
                   ; Byte per sector offset ; 2 byte data
                  LDX #11
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Byte_Per_Sector

                  LDX #13 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  AND #$FF
                  STA Sector_Per_Cluster

                  LDX #14 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Nb_Of_reserved_Cluster

                  LDX #16 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  AND #$FF
                  STA Nb_Of_FAT

                  LDX #17 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Max_Root_Entry

                  LDX #19 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Total_Sector_Count

                  LDX #22 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Sector_per_Fat

                  LDX #24 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Sector_per_Track

                  LDX #26 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Nb_of_Head

                  LDX #32 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Total_Sector_Count_FAT32
                  LDX #34 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Total_Sector_Count_FAT32+2

                  LDX #38 ;
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  AND #$FF
                  STA Boot_Signature

                  LDX #39
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Volume_ID
                  LDX #41
                  LDA FAT12_ADDRESS_BUFFER_512,X
                  STA Volume_ID+2

                  LDA #<>FAT12_ADDRESS_BUFFER_512
                  ADC #43
                  TAX
                  LDY #<>Volume_Label
                  LDA #11-1
                  MVN `Volume_Label , `FAT12_ADDRESS_BUFFER_512

                  LDA #<>FAT12_ADDRESS_BUFFER_512
                  ADC #54
                  TAX
                  LDY #<>File_System_Type
                  LDA #8-1
                  MVN `File_System_Type, `FAT12_ADDRESS_BUFFER_512
                  ; at this point all the important FAT infornation are stored between 19000 and 19030
                  LDA Byte_Per_Sector
                  CMP #512
                  BNE ERROR_BLOCK_SEIZE

                  LDA Sector_Per_Cluster
                  CMP #1
                  BNE ERROR_SECTOR_PER_CLUSTER

                  LDA Nb_Of_reserved_Cluster
                  CMP #1
                  BCC ERROR_RESERVED_SECTOR

                  LDA Nb_Of_FAT
                  CMP #1
                  BCC ERROR_NB_FAT

                  LDA Max_Root_Entry
                  CMP #224
                  BNE ERROR_NB_ROOT_ENTRY

                  LDA Total_Sector_Count
                  CMP #2880
                  BNE ERROR_NB_TOTAL_SECTOR_COUNT

                  LDA Sector_per_Fat
                  CMP #9
                  BNE ERROR_SECTOR_PER_FAT

                  LDA Sector_per_Track
                  CMP #18
                  BNE ERROR_SECTOR_PER_TRACK

                  LDA Nb_of_Head
                  CMP #1
                  BCC ERROR_NB_HEAD_NULL

                  LDA Boot_Signature
                  CMP #$29
                  BNE ERROR_BOOT_SIGNATURE
                  LDA #1
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_BLOCK_SEIZE LDA #-1
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_SECTOR_PER_CLUSTER
                  LDA #-2
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_RESERVED_SECTOR
                  LDA #-3
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_NB_FAT      LDA #-4
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_NB_ROOT_ENTRY
                  LDA #-5
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_NB_TOTAL_SECTOR_COUNT
                  LDA #-6
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_SECTOR_PER_FAT
                  LDA #-7
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_SECTOR_PER_TRACK
                  LDA #-8
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_NB_HEAD_NULL
                  LDA #-9
                  BRA RETURN_IFAT12_READ_BOOT_SECTOR
ERROR_BOOT_SIGNATURE
                  LDA #-10
RETURN_IFAT12_READ_BOOT_SECTOR
                  RTL
;-------------------------------------------------------------------------------
;
;
;
;-------------------------------------------------------------------------------
IFAT12_GET_ROOT_DIR_POS
                  setaxl
                  LDA Nb_Of_FAT;
                  TAX
                  LDA Sector_per_Fat
ADD_ONE_FAT       DEC X
                  CPX #0
                  BEQ FDD_END_LOOP_FAT_SECTOR_USAGE
                  CLC
                  ADC Sector_per_Fat
                  BRA ADD_ONE_FAT
FDD_END_LOOP_FAT_SECTOR_USAGE
                  CLC
                  ADC Nb_Of_reserved_Cluster ; at this point we have the sector where the Root directory is starting
                                             ; and because for the floppy disc Cluster = sector we just need to read the sector number
                                             ; stored in A
                  STA Root_entry_Sector_index
                  RTL

IFAT12_GET_ROOT_FIRST_ENTRY
                  setaxl
                  RTL

IFAT12_GET_ROOT_NEXT_ENTRY
                  setaxl
                  RTL
;-------------------------------------------------------------------------------
;
; REG A (16 bit) contain the root directory entry to read from 0 to Max_Root_Entry
; for a FDD the maw is 224
; if wrong it return -1 else return the secrot read
;-------------------------------------------------------------------------------
IFAT12_GET_ROOT_ENTRY
                  setaxl
                  CMP Max_Root_Entry
                  BPL FAT23_ERROR_NB_ROOT_ENTRY_INDEX
                  PHA ; Save the root entry index we want to read
                  LDX #0 ; compute in witch sector the desired root entry is, 16 entry per sector so we just need to divit the sector size by 16
KEEP_SHIFT_ROOT_ENTRY_INDEX
                  LSR
                  INC X
                  CPX #4 ; divide by 16
                  BNE KEEP_SHIFT_ROOT_ENTRY_INDEX
                  CLC ; reset the carry flag potencialy set by CPX
                  ADC Root_entry_Sector_index ; add the relative sector position of the rrot entry to the start root entry position shoud be 19 (0 based index)
                  ; test if the sector is alreaddy loaddes in RAM
                  CMP Sector_loaded_in_ram
                  BEQ FDD_SECTOR_ALREADDY_LOADDES_IN_RAM
                  STA Sector_loaded_in_ram ; save the new sector loaded
                  LDA #`FAT12_ADDRESS_BUFFER_512 ; load the byte nb 3 (bank byte)
                  PHA
                  LDA #<>FAT12_ADDRESS_BUFFER_512 ; load the low world part of the buffer address
                  PHA
                  LDA Sector_loaded_in_ram ; read the ROOT directory sector saved at the begining of the function
                  JSL IFDD_READ
                  PLX
                  PLX
FDD_SECTOR_ALREADDY_LOADDES_IN_RAM
                  ; get the root entry now we have the right sector loaded in RAM
                  PLA ; GET the root entry FDD_INDEX
                  PHA
                  AND #$0F ; get only the 4 first byte to get the 16 value ofset in the root entry sector loades in ram (16 entry per sector)
                  ASL
                  ASL
                  ASL
                  ASL ; now A contain the ofset to read the root entry from
                  CLC
                  ADC #<>FAT12_ADDRESS_BUFFER_512
                  TAX
                  LDA #<>Root_entry_value
                  TAY
                  LDA #31
                  MVN `Root_entry_value, `FAT12_ADDRESS_BUFFER_512
                  PLA

                  BRA RETURN_IFAT12_IFAT12_GET_ROOT_ENTRY
FAT23_ERROR_NB_ROOT_ENTRY_INDEX
                  LDA #-1
RETURN_IFAT12_IFAT12_GET_ROOT_ENTRY
                  RTL
;-------------------------------------------------------------------------------
;
;
;
;
;-------------------------------------------------------------------------------
IFAT12_GET_FAT_POS
                  LDA Nb_Of_reserved_Cluster
                  STA Fat12_Sector_index
                  RTL

;-------------------------------------------------------------------------------
;
; Get  in A the FAT entry where to get the next One
; return the next fat entry to read  in Fat12_next_entry
;if even fat indes , we get the address ofset by 1+(3*n)/2
;if odd (3*n)/2 and 1+(3*n)/2 for the last part
;
;-------------------------------------------------------------------------------
IFAT_GET_FAT_ENTRY
                  PHA
                  ; find in witch sector the fat entry is suposed to be
                  LDX #0
                  CMP #340
                  BCC FAT12_ENTRY_SECTOR_LOCATION_FIND
FAT12_FAT_ENTRY_NEXT_SECTOR
                  INC X
                  SBC #340
                  BPL FAT12_FAT_ENTRY_NEXT_SECTOR
FAT12_ENTRY_SECTOR_LOCATION_FIND
                  TXA
                  ADC Nb_Of_reserved_Cluster
                  CMP Fat12_Sector_loaded_in_ram
                  BEQ FAT12_COMPUT_OFSET_IN_CLUSTER ; dont need to load the fat sector because it's alreaddy loaded
                  STA Fat12_Sector_loaded_in_ram
                  LDA #`FAT12_FAT_ADDRESS_BUFFER_512 ; load the byte nb 3 (bank byte)
                  PHA
                  LDA #<>FAT12_FAT_ADDRESS_BUFFER_512 ; load the low world part of the buffer address
                  PHA
                  LDA Fat12_Sector_loaded_in_ram ; read the ROOT directory sector saved at the begining of the function
                  JSL IFDD_READ
                  PLX
                  PLX
                  ; from here the right fat sector is loades in ram
FAT12_COMPUT_OFSET_IN_CLUSTER
                  ; need to test for an even or even fat entry number
                  ; PLA ; get the fat entry
                  ; PHA ; save it back again
                  LDA 1,S
                  AND #1
                  CMP #0
                  PLA ; get the fat entry
                  PHA ; save it back again
                  ; LDA 1,S note used because it affect the zero flag
                  BNE FAT12_ODD_FAT_ENTRY
                  ;----------------------------------------
                  ; Odd fat entry
                  ; Compute A*3 to get the low part of the FAT12 => A*2+A
                  ; then just add 1 to get the last 4 missing bite
                  STA Fat12_next_entry
                  ASL ; A*2
                  CLC ; clear the carry flag
                  ADC Fat12_next_entry ; A*3
                  ; compute now (A*3)/2
                  LSR
                  STA Fat12_next_entry
                  ;PHA
                  ;A now get the ofset of the first part of the even fat entry
                  LDA #<>FAT12_FAT_ADDRESS_BUFFER_512
                  CLC
                  ADC Fat12_next_entry
                  TAX
                  LDA #<>Fat12_next_entry
                  TAY
                  LDA #0 ; read one byte only
                  MVN `Fat12_next_entry,`FAT12_FAT_ADDRESS_BUFFER_512
                  LDA Fat12_next_entry
                  PHA
                  ; dont need to reload the source address the instrction MVN already incrementer it to the right value
                  LDA #<>Fat12_next_entry
                  TAY
                  LDA #0 ; read one byte only
                  MVN `Fat12_next_entry,`FAT12_FAT_ADDRESS_BUFFER_512
                  LDA Fat12_next_entry
                  AND #$0F
                  ASL
                  ASL
                  ASL
                  ASL
                  ASL
                  ASL
                  ASL
                  ASL
                  STA Fat12_next_entry
                  PLA
                  CLC
                  ADC Fat12_next_entry
                  STA Fat12_next_entry
                  PLA ; get the initial fat entry, Fat12_next_entry contain the next fat entry and sector to read
                  BRA RETURN_IFAT12_GET_FAT_ENTRY

FAT12_ODD_FAT_ENTRY
                  STA Fat12_next_entry
                  ASL ; A*2
                  CLC ; clear the carry flag
                  ADC Fat12_next_entry ; A*3
                  ; compute now (A*3)/2
                  LSR
                  STA Fat12_next_entry
                  ;PHA
                  ;A now get the ofset of the first part of the odd fat entry
                  LDA #<>FAT12_FAT_ADDRESS_BUFFER_512
                  CLC
                  ADC Fat12_next_entry
                  TAX
                  LDA #<>Fat12_next_entry
                  TAY
                  LDA #0 ; read one byte only
                  MVN `Fat12_next_entry,`FAT12_FAT_ADDRESS_BUFFER_512
                  LDA Fat12_next_entry
                  AND #$F0
                  LSR
                  LSR
                  LSR
                  LSR
                  PHA ; save the low part of the fat entry
                  ; dont need to reload the source address the instrction MVN already incrementer it to the right value
                  LDA #<>Fat12_next_entry
                  TAY
                  LDA #0 ; read one byte only
                  MVN `Fat12_next_entry,`FAT12_FAT_ADDRESS_BUFFER_512
                  LDA Fat12_next_entry
                  ASL
                  ASL
                  ASL
                  ASL
                  STA Fat12_next_entry ; save the low fat entry
                  PLA ; get the low paart
                  CLC
                  ADC Fat12_next_entry ; add the low part to the hi part
                  STA Fat12_next_entry
                  PLA ; get the initial fat entry, Fat12_next_entry contain the next fat entry and sector to read
RETURN_IFAT12_GET_FAT_ENTRY
                  RTL

;-------------------------------------------------------------------------------
; Search for the file name in the root directory
; Stack 0-1-3-4 pointer to the file name strings to load
; Stack 5-6-7-8 buffer where to load the file
;-------------------------------------------------------------------------------
ILOAD_FILE
                  JSL IFAT12_READ_BOOT_SECTOR
                  CMP #$0001
                  BEQ ILOAD_FILE_FAT_12_BOOT_SECTOR_PARSING_OK
                  LDA #-1
                  BRA ILOAD_FILE_RETURN_ERROR_temp
ILOAD_FILE_FAT_12_BOOT_SECTOR_PARSING_OK
                  JSL IFAT12_GET_ROOT_DIR_POS
                  setaxl
                  LDA #$00 ; sellect the first entry
                  PHA
ILOAD_FILE_READ_NEXT_ROOT_ENTRY
                  JSL IFAT12_GET_ROOT_ENTRY
                  LDA Root_entry_value + 11 ; get the flag Byte to test if it a file or a directory
                  AND #$10
                  CMP #$10
                  BNE ILOAD_FILE_ENTRY ; if equal we read a directory so just read the next one
ILOAD_FILE_STRING_NOT_MATCHED
                  PLA   ; get the actual root entry
                  CMP Max_Root_Entry ; prevent to loop forever so exit
                  BEQ ILOAD_FILE_NO_FILE_MATCHED
                  INC A ; sellect the next root entry
                  PHA   ; save the next root entry to read
                  BRA ILOAD_FILE_READ_NEXT_ROOT_ENTRY
ILOAD_FILE_ENTRY
                  setaxl
                  PLA   ; get the actual root entry
                  CMP Max_Root_Entry ; prevent to loop forever so exit
                  BEQ ILOAD_FILE_NO_FILE_MATCHED
                  INC A ; sellect the next root entry
                  PHA   ; save the next root entry to read

                  ; copare the file name we want to load and the root entry file name
                  LDX #-1
                  LDY #-1
ILOAD_FILE_CHAR_MATCHING
                  INC X
                  INC Y
                  CPX #11 ; FAT12 file or folder size
                  BEQ ILOAD_FILE_STRING_MATCHED
                  LDA (6,S),Y ; load the "y" char file name we want to read
                  CMP Root_entry_value,X
                  BEQ ILOAD_FILE_CHAR_MATCHING
                  BRA ILOAD_FILE_STRING_NOT_MATCHED
ILOAD_FILE_NO_FILE_MATCHED
                  PLA
                  LDA #-2
                  BRA ILOAD_FILE_RETURN_ERROR
ILOAD_FILE_RETURN_ERROR_temp
                  BRA ILOAD_FILE_RETURN_ERROR
ILOAD_FILE_STRING_MATCHED
                  PLA
                  LDA Root_entry_value + 26 ; get the first fat entry for the fil from the root directory entry we matched
                  STA Fat12_next_entry

ILOAD_FILE_Read_next_sector; read sector function to call there
                  ;LDA #`FAT12_ADDRESS_BUFFER_512 ; load the byte nb 3 (bank byte)
                  ;PHA
                  ;LDA #<>FAT12_ADDRESS_BUFFER_512 ; load the low world part of the buffer address
                  ;PHA
                  LDA 10,S ; load the byte nb 3 (bank byte)
                  PHA
                  LDA 12,S ; load the low world part of the buffer address
                  PHA
                  LDA Fat12_next_entry ; sector to read
                  ADC #1+9+9+14 ; skip the reserved sector ,  the 2 fat and the root sector
                  JSL IFDD_READ
                  PLX
                  PLX
                  ; point to the next 512 byte in the buffer
                  LDA 12,S ; load the low world part of the buffer address
                  CLC
                  LDA #0
                  PHA ; save the conter vaue
                  PHA ; save the
ILOAD_FILE_Add_More_Sector_Per_Cluster
                  LDA 4,S ; load the Byte per cluster value
                  ADC Byte_Per_Sector
                  STA 4,S
                  LDA Sector_Per_Cluster
                  CMP 2,S
                  BEQ ILOAD_FILE_Read_Next_Data
                  LDA 2,S
                  INC A
                  STA 2,S
                  BRA ILOAD_FILE_Add_More_Sector_Per_Cluster
ILOAD_FILE_Read_Next_Data
                  PLX ; removing the compter valuer from the stack
                  ;-------------------------------------------------------------
                  ; compute the next buffer address to use to copy file
                  LDA 14,S ; load the low world part of the buffer address
                  CLC
                  ADC 2,S
                  BCC ILOAD_FILE_New_Buiffer_Address_computed
                  STA 14,S ; Save the low world part of the buffer address
                  LDA 12,S ; load the byte nb 3 (bank byte)
                  ADC #1
                  STA 12,S ; Save the byte nb 3 (bank byte)
ILOAD_FILE_New_Buiffer_Address_computed
                  PLX ; get the Byte per cluster count out of the stack
                  ;-------------------------------------------------------------
                  LDA Fat12_next_entry ; sector to read
                  JSL IFAT_GET_FAT_ENTRY
                  LDA Fat12_next_entry
                  CMP #$FFE
                  BCS ILOAD_FILE_END_OF_FILE
                  BRA ILOAD_FILE_Read_next_sector
ILOAD_FILE_END_OF_FILE
ILOAD_FILE_RETURN_ERROR
                  RTL
