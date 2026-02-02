; fat32.nasm (20260201), ported to nasm by deomsh from fat32.S
; checks with disassembly from: nasm-webui by pi (assembly of older version)
;
;/*
; * COPYRIGHT:       See COPYING in the top level directory
; * PROJECT:         ReactOS Bootsector
; * FILE:            boot/freeldr/bootsect/fat32.S
; * PURPOSE:
; * PROGRAMMERS:     Brian Palmer
; */
;
;/* INCLUDES ******************************************************************/
;
;#include <asm.inc>
;#include <freeldr/include/arch/pc/x86common.h>
;
;#define BP_REL(x) [bp+x-offset start]
;
;.code16
;
;//ORG HEX(7c00)
org 0x7C00
bits 16
;
FREELDR_BASE					equ		0xf800
;
%define	BytesPerSector			bp + 0xb
%define	SectsPerCluster			bp + 0xd
%define	ReservedSectors			bp + 0xe
%define	NumberOfFats			bp + 0x10
%define	MaxRootEntries			bp + 0x11
%define	TotalSectors			bp + 0x13
%define	MediaDescriptor			bp + 0x15
%define	SectorsPerFat			bp + 0x16
%define	SectorsPerTrack			bp + 0x18
%define	NumberOfHeads			bp + 0x1a
;NONEED;%define	HiddenSectorsLow		bp + 0x1c
;NONEED;%define	HiddenSectorsHigh		bp + 0x1e
%define	HiddenSectors			bp + 0x1c
%define	TotalSectorsBig			bp + 0x20
;
%define	SectorsPerFatBig		bp + 0x24
%define	ExtendedFlags			bp + 0x28
%define	FSVersion				bp + 0x2a
%define	RootDirStartCluster		bp + 0x2c
%define	FSInfoSector			bp + 0x30
%define	BackupBootSector		bp + 0x32
%define	Reserved1				bp + 0x34
;
%define	BootDrive				bp + 0x40
;%define	
;
start:
    jmp short main
    nop
;
;OEMName:
	db	'FrLdr1.0'
;    .ASCII "FrLdr1.0"
;BytesPerSector:
	dw	512
;    .word 512
;SectsPerCluster:
	db	1
;    .byte 0
;ReservedSectors:
	dw	32
;    .word 32
;NumberOfFats:
	db	2
;    .byte 2
;MaxRootEntries:
	dw	0
;    .word 0             ;// Always zero for FAT32 volumes
;TotalSectors:
	dw	0
;    .word 0             ;// Always zero for FAT32 volumes
;MediaDescriptor:
	db	0xf8
;    .byte HEX(0f8)
;SectorsPerFat:
	dw	0
;    .word 0             ;// Always zero for FAT32 volumes
;SectorsPerTrack:
	dw	0
;    .word 0
;NumberOfHeads:
	dw	0
;    .word 0
;HiddenSectors:
	dd	0
;    .long 0
;TotalSectorsBig:
	dd	0
;    .long 0
;
;// FAT32 Inserted Info
;SectorsPerFatBig:
	dd	0
;    .long    0
;ExtendedFlags:
	dw	0
;    .word    0
;FSVersion:
	dw	0
;    .word    0
;RootDirStartCluster:
	dd	0
;    .long    0
;FSInfoSector:
	dw	0
;    .word    0
;BackupBootSector:
	dw	0
;    .word    6
;Reserved1:
	db	0,0,0,0,0,0,0,0,0,0,0,0
;    .space 12, 0
;// End FAT32 Inserted Info
;
;BootDrive:
	db	0x80
;    .byte 0
;Reserved:
	db	0
;    .byte 0
;ExtendSig:
	db	0x29
;    .byte HEX(29)
;SerialNumber:
	dd	0x10171113
;    .long 0
;VolumeLabel:
	db	'NO NAME    '
;    .ascii "NO NAME    "
;FileSystem:
	db	'FAT12   '
;    .ascii "FAT32   "
;
main:
    xor ax,ax               ;// Setup segment registers
    mov ds,ax               ;// Make DS correct
    mov es,ax               ;// Make ES correct
    mov ss,ax               ;// Make SS correct
    mov bp, 0x7c00
;    mov bp, HEX(7c00)
    mov sp, 0x7c00         ;// Setup a stack
;    mov sp, HEX(7c00)       ;// Setup a stack
;
;00000068  807E40FF          cmp byte [bp+0x40],0xff
    cmp byte [BootDrive], 0xff                   ;// If they have specified a boot drive then use it
;    cmp byte ptr BP_REL(BootDrive), HEX(0ff)    ;// If they have specified a boot drive then use it
    jne CheckSectorsPerFat
;
;0000006E  885640            mov [bp+0x40],dl
    mov byte [BootDrive], dl                    ;// Save the boot drive
;    mov byte ptr BP_REL(BootDrive), dl          ;// Save the boot drive
;
CheckSectorsPerFat:
;
;00000071  837E1600          cmp word [bp+0x16],byte +0x0
    cmp word [SectorsPerFat], 0                 ;// Check the old 16-bit value of SectorsPerFat
;    cmp word ptr BP_REL(SectorsPerFat), 0       ;// Check the old 16-bit value of SectorsPerFat
    jnz CheckFailed                             ;// If it is non-zero then exit with an error
CheckTotalSectors:                              ;// Check the old 16-bit value of TotalSectors & MaxRootEntries
;00000077  66837E1100        cmp dword [bp+0x11],byte +0x0
    cmp dword [MaxRootEntries], 0               ;// by comparing the DWORD at offset MaxRootEntries to zero
;    cmp dword ptr BP_REL(MaxRootEntries), 0     ;// by comparing the DWORD at offset MaxRootEntries to zero
    jnz CheckFailed                             ;// If it is non-zero then exit with an error
CheckFileSystemVersion:
;0000007E  837E2A00          cmp word [bp+0x2a],byte +0x0
    cmp word [FSVersion], 0           ;// Check the file system version word
;    cmp word ptr BP_REL(FSVersion), 0           ;// Check the file system version word
    jna GetDriveParameters                      ;// It is zero, so continue
CheckFailed:
    jmp PrintFileSystemError                    ;// If it is not zero then exit with an error
;
GetDriveParameters:
;00000087  B80008        
    mov ax,0x800
;    mov  ax, HEX(0800)
;0000008A  8A5640            mov dl,[bp+0x40]
    mov  dl, byte [BootDrive]                   ;// Get boot drive in dl
;    mov  dl, byte ptr BP_REL(BootDrive)         ;// Get boot drive in dl
;0000008D  CD13          
    int 0x13
;    int  HEX(13)                                ;// Request drive parameters from the bios
    jnc  CalcDriveSize                          ;// If the call succeeded then calculate the drive size
;
    ;// If we get here then the call to the BIOS failed
    ;// so just set CHS equal to the maximum addressable
    ;// size
;00000091  B9FFFF        
    mov cx,0xffff
;    mov  cx, HEX(0ffff)
    mov  dh, cl
;
CalcDriveSize:
    ;// Now that we have the drive geometry
    ;// lets calculate the drive size
    mov  bl, ch         ;// Put the low 8-bits of the cylinder count into BL
    mov  bh, cl         ;// Put the high 2-bits in BH
    shr  bh, 6          ;// Shift them into position, now BX contains the cylinder count
;0000009D  80E13F        
    and cl,0x3f
;    and  cl, HEX(3f)    ;// Mask off cylinder bits from sector count
    ;// CL now contains sectors per track and DH contains head count
    movzx eax, dh       ;// Move the heads into EAX
    movzx ebx, bx       ;// Move the cylinders into EBX
    movzx ecx, cl       ;// Move the sectors per track into ECX
    inc   eax           ;// Make it one based because the bios returns it zero based
    inc   ebx           ;// Make the cylinder count one based also
    mul   ecx           ;// Multiply heads with the sectors per track, result in edx:eax
    mul   ebx           ;// Multiply the cylinders with (heads * sectors) [stored in edx:eax already]
;
    ;// We now have the total number of sectors as reported
    ;// by the bios in eax, so store it in our variable
;000000B6  66A3A77D          mov [0x7da7],eax
    mov dword ds:[BiosCHSDriveSize], eax
;    mov dword ptr ds:[BiosCHSDriveSize], eax
;
LoadExtraBootCode:
    ;// First we have to load our extra boot code at
    ;// sector 14 into memory at [0000:7e00h]
;000000BA  66B80E000000  
    mov eax,0xe
;    mov  eax, HEX(0e)
;000000C0  6603461C          add eax,[bp+0x1c]
    add  eax, dword [HiddenSectors]             ;// Add the number of hidden sectors
;    add  eax, dword ptr BP_REL(HiddenSectors)   ;// Add the number of hidden sectors
    mov  cx, 1
    xor  bx, bx
    mov  es, bx                                 ;// Read sector to [0000:7e00h]
    mov  bx, 0x7e00
;    mov  bx, HEX(7e00)
    call ReadSectors
    jmp  StartSearch
;
;
;// Reads logical sectors into [ES:BX]
;// EAX has logical sector number to read
;// CX has number of sectors to read
ReadSectors:
    push es
    cmp  eax, dword ds:[BiosCHSDriveSize]       ;// Check if they are reading a sector outside CHS range
;    cmp  eax, dword ptr ds:[BiosCHSDriveSize]   ;// Check if they are reading a sector outside CHS range
    jae  ReadSectorsLBA                         ;// Yes - go to the LBA routine
                                                ;// If at all possible we want to use LBA routines because
                                                ;// They are optimized to read more than 1 sector per read
;
    pushad                                      ;// Save logical sector number & sector count
;
CheckInt13hExtensions:                          ;// Now check if this computer supports extended reads
;000000DD  B441          
    mov ah,0x41
;    mov  ah, HEX(41)                            ;// AH = 41h
;000000DF  BBAA55        
    mov bx,0x55aa
;    mov  bx, HEX(55aa)                          ;// BX = 55AAh
;000000E2  8A5640            mov dl,[bp+0x40]
    mov  dl, byte [BootDrive]                   ;// DL = drive (80h-FFh)
;    mov  dl, byte ptr BP_REL(BootDrive)         ;// DL = drive (80h-FFh)
;000000E5  CD13          
    int 0x13
;    int  HEX(13)                                ;// IBM/MS INT 13 Extensions - INSTALLATION CHECK
    jc   ReadSectorsCHS                         ;// CF set on error (extensions not supported)
;000000E9  81FB55AA      
    cmp bx,0xaa55
;    cmp  bx, HEX(0aa55)                         ;// BX = AA55h if installed
    jne  ReadSectorsCHS
    test cl,1                                   ;// CX = API subset support bitmap
    jz   ReadSectorsCHS                         ;// Bit 0, extended disk access functions (AH=42h-44h,47h,48h) supported
;
    popad                                       ;// Restore sector count & logical sector number
;
ReadSectorsLBA:
    pushad                                      ;// Save logical sector number & sector count
;
    cmp  cx, 64                                 ;// Since the LBA calls only support 0x7F sectors at a time we will limit ourselves to 64
    jbe  ReadSectorsSetupDiskAddressPacket      ;// If we are reading less than 65 sectors then just do the read
    mov  cx, 64                                 ;// Otherwise read only 64 sectors on this loop iteration
;
ReadSectorsSetupDiskAddressPacket:
    mov word ds:[LBASectorsRead],cx
;    mov word ptr ds:[LBASectorsRead],cx
    push 0
    push 0
    push eax                                ;// Put 64-bit logical block address on stack
    push es                                 ;// Put transfer segment on stack
    push bx                                 ;// Put transfer offset on stack
    push cx                                 ;// Set transfer count
    push 16                                 ;// Set size of packet to 10h
    mov  si, sp                             ;// Setup disk address packet on stack
;
;00000111  8A5640            mov dl,[bp+0x40]
    mov  dl, byte [BootDrive]               ;// Drive number
;    mov  dl, byte ptr BP_REL(BootDrive)     ;// Drive number
;00000114  B442          
    mov ah,0x42
;    mov  ah, HEX(42)                        ;// Int 13h, AH = 42h - Extended Read
;00000116  CD13          
    int 0x13
;    int  HEX(13)                            ;// Call BIOS
    jc   PrintDiskError                     ;// If the read failed then abort
;
    add  sp, 16                             ;// Remove disk address packet from stack
;
    popad                                   ;// Restore sector count & logical sector number
;
    push bx
    mov  ebx, dword ds:[LBASectorsRead]
;    mov  ebx, dword ptr ds:[LBASectorsRead]
    add  eax, ebx                           ;// Increment sector to read
    shl  ebx, 5
    mov  dx, es
    add  dx, bx                             ;// Setup read buffer for next sector
    mov  es, dx
    pop  bx
;
    sub  cx, word ds:[LBASectorsRead]
;    sub  cx, word ptr ds:[LBASectorsRead]
    jnz  ReadSectorsLBA                     ;// Read next sector
;
    pop es
    ret
;
LBASectorsRead:
	dd	0
;    .long    0
;
;
;// Reads logical sectors into [ES:BX]
;// EAX has logical sector number to read
;// CX has number of sectors to read
ReadSectorsCHS:
    popad                                        ;// Get logical sector number & sector count off stack
;
ReadSectorsCHSLoop:
    pushad
    xor  edx, edx
;00000145  660FB74E18        movzx ecx,word [bp+0x18]
    movzx ecx, word [SectorsPerTrack]
;    movzx ecx, word ptr BP_REL(SectorsPerTrack)
    div  ecx                                    ;// Divide logical by SectorsPerTrack
    inc  dl                                     ;// Sectors numbering starts at 1 not 0
    mov  cl, dl                                 ;// Sector in CL
    mov  edx, eax
    shr  edx, 16
;00000158  F7761A            div word [bp+0x1a]
    div  word [NumberOfHeads]                    ;// Divide logical by number of heads
;    div  word ptr BP_REL(NumberOfHeads)         ;// Divide logical by number of heads
    mov  dh, dl                                 ;// Head in DH
;0000015D  8A5640            mov dl,[bp+0x40]
    mov  dl, byte [BootDrive]                    ;// Drive number in DL
;    mov  dl, byte ptr BP_REL(BootDrive)         ;// Drive number in DL
    mov  ch, al                                 ;// Cylinder in CX
    ror  ah, 1                                  ;// Low 8 bits of cylinder in CH, high 2 bits
    ror  ah, 1                                  ;//  in CL shifted to bits 6 & 7
    or   cl, ah                                 ;// Or with sector number
;00000168  B80102        
    mov ax,0x201
;    mov  ax, HEX(0201)
;0000016B  CD13          
    int 0x13
;    int  HEX(13)    ;// DISK - READ SECTORS INTO MEMORY
                     ;// AL = number of sectors to read, CH = track, CL = sector
                     ;// DH = head, DL = drive, ES:BX -> buffer to fill
                     ;// Return: CF set on error, AH = status (see AH=01h), AL = number of sectors read
;
    jc   PrintDiskError                         ;// If the read failed then abort
;
    popad
;
    inc  eax                                    ;// Increment Sector to Read
;
    mov  dx, es
    add  dx, 32                                 ;// Increment read buffer for next sector
    mov  es, dx
;
    loop ReadSectorsCHSLoop                     ;// Read next sector
;
    pop es
    ret
;
;// Displays a disk error message
;// And reboots
PrintDiskError:
    mov  si, msgDiskError               ;// Bad boot disk message
;    mov  si, offset msgDiskError        ;// Bad boot disk message
    call PutChars                       ;// Display it
;
    jmp  Reboot
;
;// Displays a file system error message
;// And reboots
PrintFileSystemError:
    mov  si, msgAnyKey                  ;// Press any key message
;    mov  si, offset msgAnyKey           ;// Press any key message
    call PutChars                       ;// Display it
;
Reboot:
    mov  si, msgAnyKey                  ;// Press any key message
;    mov  si, offset msgAnyKey           ;// Press any key message
    call PutChars                       ;// Display it
    xor  ax, ax
;00000194  CD16          
    int 0x16
;    int  HEX(16)                        ;// Wait for a keypress
;00000196  CD19          
    int 0x19
;    int  HEX(19)                        ;// Reboot
;
PutChars:
    lodsb
    or   al, al
    jz   short Done
;0000019D  B40E          
    mov ah,0xe
;
;    mov  ah, HEX(0e)
;0000019F  BB0700            mov bx,0x7
    mov  bx, 7
;000001A2  CD10          
    int 0x10
;    int  HEX(10)
    jmp  short PutChars
Done:
    ret
;
;
BiosCHSDriveSize:
	dd	0
;    .long 0
;
msgDiskError:
	db	'Disk error',0xd,0xa,0
;    .ascii "Disk error", CR, LF, NUL
msgFileSystemError:
	db	'File system error',0xd,0xa,0
;    .ascii "File system error", CR, LF, NUL
msgAnyKey:
	db	'Press any key to restart',0xd,0xa,0
;    .ascii "Press any key to restart", CR, LF, NUL
;
times 509 - ($ - $$) db 0
;.org 509 ;// Pad to 509 bytes
;
;000001FD  0055AA            
BootPartition:
	db	0
;    .byte 0
;
BootSignature:
	db	0x55,0xaa
;    .word HEX(0aa55)    ;// BootSector signature
;
;// End of bootsector
;//
;// Now starts the extra boot code that we will store
;// at sector 14 on a FAT32 volume
;//
;// To remain multi-boot compatible with other operating
;// systems we must not overwrite anything other than
;// the bootsector which means we will have to use
;// a different sector like 14 to store our extra boot code
;
;
;
StartSearch:
;
    ;// Now we must get the first cluster of the root directory
;00000200  668B462C          mov eax,[bp+0x2c]
    mov  eax, dword [RootDirStartCluster]
;    mov  eax, dword ptr BP_REL(RootDirStartCluster)
;00000204  663DF8FFFF0F  
    cmp eax,0xffffff8
;    cmp  eax, HEX(0ffffff8)     ;// Check to see if this is the last cluster in the chain
    jb   ContinueSearch         ;// If not continue, if so then we didn't find freeldr.sys
    jmp  PrintFileNotFound
;
ContinueSearch:
;0000020F  BB0020        
    mov bx,0x2000
;    mov  bx, HEX(2000)
    mov  es, bx             ;// Read cluster to [2000:0000h]
    call ReadCluster        ;// Read the cluster
;
    ;// Now we have to find our way through the root directory to
    ;// The FREELDR.SYS file
    xor  bx,bx
;00000219  8A5E0D            mov bl,[bp+0xd]
    mov  bl, byte [SectsPerCluster]
;    mov  bl, byte ptr BP_REL(SectsPerCluster)
    shl  bx, 4              ;// BX = BX * 512 / 32
;0000021F  B80020        
    mov ax,0x2000
;    mov  ax, HEX(2000)      ;// We loaded at 2000:0000
    mov  es, ax
    xor  di, di
    mov  si, filename
;    mov  si, offset filename
    mov  cx, 11
    repe cmpsb              ;// Compare filenames
    jz   FoundFile          ;// If same we found it
    dec  bx
    jnz  FindFile
    jmp  PrintFileNotFound
;
FindFile:
    mov  ax, es             ;// We didn't find it in the previous dir entry
    add  ax, 2              ;// So lets move to the next one
    mov  es, ax             ;// And search again
    xor  di, di
    mov  si, filename
;    mov  si, offset filename
    mov  cx, 11
    repe cmpsb              ;// Compare filenames
    jz   FoundFile          ;// If same we found it
    dec  bx                 ;// Keep searching till we run out of dir entries
    jnz  FindFile           ;// Last entry?
;
    ;// Get the next root dir cluster and try again until we run out of clusters
;0000024C  668B462C          mov eax,[bp+0x2c]
    mov  eax, dword [RootDirStartCluster]
;    mov  eax, dword ptr BP_REL(RootDirStartCluster)
    call GetFatEntry
;00000253  6689462C          mov [bp+0x2c],eax
    mov dword [RootDirStartCluster], eax
;    mov dword ptr BP_REL(RootDirStartCluster), eax
    jmp  StartSearch
;
FoundFile:
                                    ;// Display "Loading FreeLoader..." message
    mov  si, msgLoading             ;// Loading message
;    mov  si, offset msgLoading      ;// Loading message
    call PutChars                   ;// Display it
;
    xor  di, di                     ;// ES:DI has dir entry
    xor  dx, dx
    mov  ax, word es:[di+20]    ;// Get start cluster high word
;    mov  ax, word ptr es:[di+20]    ;// Get start cluster high word
    shl  eax, 16
    mov  ax, word es:[di+26]    ;// Get start cluster low word
;    mov  ax, word ptr es:[di+26]    ;// Get start cluster low word
;
CheckStartCluster:
    cmp  eax, 2                     ;// Check and see if the start cluster starts at cluster 2 or above
    jnb  CheckEndCluster            ;// If so then continue
    jmp  PrintFileSystemError       ;// If not exit with error
CheckEndCluster:
;0000027B  663DF8FFFF0F 
    cmp eax,0xffffff8
;    cmp  eax, HEX(0ffffff8)         ;// Check and see if the start cluster is and end of cluster chain indicator
    jb   InitializeLoadSegment      ;// If not then continue
    jmp  PrintFileSystemError       ;// If so exit with error
;
InitializeLoadSegment:
;00000286  BB0008            mov bx,0x800
    mov  bx, FREELDR_BASE / 16
    mov  es, bx
;
LoadFile:
;0000028B  663DF8FFFF0F  
    cmp eax,0xffffff8
;    cmp  eax, HEX(0ffffff8)     ;// Check to see if this is the last cluster in the chain
    jae  LoadFileDone           ;// If so continue, if not then read the next one
    push eax
    xor  bx, bx                 ;// Load ROSLDR starting at 0000:F800h
    push es
    call ReadCluster
    pop  es
;
    xor  bx, bx
;0000029E  8A5E0D            mov bl,[bp+0xd]
    mov  bl, byte [SectsPerCluster]
;    mov  bl, byte ptr BP_REL(SectsPerCluster)
    shl  bx, 5                  ;// BX = BX * 512 / 16
    mov  ax, es                 ;// Increment the load address by
    add  ax, bx                 ;// The size of a cluster
    mov  es, ax
;
    pop  eax
    push es
    call GetFatEntry            ;// Get the next entry
    pop  es
;
    jmp  LoadFile               ;// Load the next cluster (if any)
;
LoadFileDone:
;000002B4  8A5640            mov dl,[bp+0x40]
    mov  dl, byte [BootDrive]     ;// Load boot drive into DL
;    mov  dl, byte ptr BP_REL(BootDrive)     ;// Load boot drive into DL
;000002B7  8A36FD7D          mov dh,[0x7dfd]
    mov  dh, byte ds:[BootPartition]         ;// Load boot partition into DH
;    mov  dh, byte ptr ds:[BootPartition]    ;// Load boot partition into DH
;
    ;/* Transfer execution to the bootloader */
; ***************** OLDER ********************
;000002BB  31C0              xor ax,ax
;000002BD  50                push ax
;000002BE  680080            push word 0x8000
;000002C1  CB                retf
; ***************** END of OLDER *************
    jmp FREELDR_BASE / 16:0                 ; TEST [I]
;    ljmp16 FREELDR_BASE / 16, 0
;
;// Returns the FAT entry for a given cluster number
;// On entry EAX has cluster number
;// On return EAX has FAT entry for that cluster
GetFatEntry:
;
    shl   eax, 2                                ;// EAX = EAX * 4 (since FAT32 entries are 4 bytes)
    mov   ecx, eax                              ;// Save this for later in ECX
    xor   edx, edx
;000002CC  660FB75E0B        movzx ebx,word [bp+0xb]
    movzx ebx, word [BytesPerSector]
;    movzx ebx, word ptr BP_REL(BytesPerSector)
    push  ebx
    div   ebx                                   ;// FAT Sector Number = EAX / BytesPerSector
;000002D6  660FB75E0E        movzx ebx,word [bp+0xe]
    movzx ebx, word [ReservedSectors]
;    movzx ebx, word ptr BP_REL(ReservedSectors)
    add   eax, ebx                              ;// FAT Sector Number += ReservedSectors
;000002DE  668B5E1C          mov ebx,[bp+0x1c]
    mov   ebx, dword [HiddenSectors]
;    mov   ebx, dword ptr BP_REL(HiddenSectors)
    add   eax, ebx                              ;// FAT Sector Number += HiddenSectors
    pop   ebx
    dec   ebx
    and   ecx,ebx                               ;// FAT Offset Within Sector = ECX % BytesPerSector
                                                ;// EAX holds logical FAT sector number
                                                ;// ECX holds FAT entry offset
;
                                                ;// Now we have to check the extended flags
                                                ;// to see which FAT is the active one
                                                ;// and use it, or if they are mirrored then
                                                ;// no worries
;000002EC  660FB75E28        movzx ebx,word [bp+0x28]
    movzx ebx, word [ExtendedFlags]             ;// Get extended flags and put into ebx
;    movzx ebx, word ptr BP_REL(ExtendedFlags)   ;// Get extended flags and put into ebx
;000002F1  81E30F00     
     and bx,0xf
;    and   bx, HEX(0f)                           ;// Mask off upper 8 bits, now we have active fat in bl
    jz    LoadFatSector                         ;// If fat is mirrored then skip fat calcs
;000002F7  3A5E10            cmp bl,[bp+0x10]
    cmp   bl, byte [NumberOfFats]               ;// Compare bl to number of fats
;    cmp   bl, byte ptr BP_REL(NumberOfFats)     ;// Compare bl to number of fats
    jb    GetActiveFatOffset
    jmp   PrintFileSystemError                  ;// If bl is bigger than numfats exit with error
GetActiveFatOffset:
    push  eax                                   ;// Save logical FAT sector number
;00000301  668B4624          mov eax,[bp+0x24]
    mov   eax, dword [SectorsPerFatBig]         ;// Get the number of sectors occupied by one fat in eax
;    mov   eax, dword ptr BP_REL(SectorsPerFatBig)   ;// Get the number of sectors occupied by one fat in eax
    mul   ebx                                   ;// Multiplied by the active FAT index we have in ebx
    pop   edx                                   ;// Get logical FAT sector number
    add   eax, edx                              ;// Add the current FAT sector offset
;
LoadFatSector:
;0000030D  6651              push ecx
    push  ecx
; ****************** NEW ****************
    mov   bx, 0x9000                            ;// We will load it to [9000:0000h]
;    mov   bx, HEX(9000)                         ;// We will load it to [9000:0000h]
    mov   es, bx
; ******************END of  NEW *********
    ;// EAX holds logical FAT sector number
    ;// Check if we have already loaded it
;0000030F  663B063A7F        cmp eax,[0x7f3a]
    cmp   eax, dword ds:[FatSectorInCache]
;    cmp   eax, dword ptr ds:[FatSectorInCache]
    je    LoadFatSectorAlreadyLoaded
;
    mov   dword ds:[FatSectorInCache], eax
;    mov   dword ptr ds:[FatSectorInCache], eax
; ***************** UNUSED NOW **********
;0000031A  BB0070            mov bx,0x7000
;0000031D  8EC3              mov es,bx
; ***************** END of UNUSED NOW ***
    xor   bx, bx
    mov   cx, 1
    call  ReadSectors
;
LoadFatSectorAlreadyLoaded:
; ***************** UNUSED NOW **********
00000327  BB0070            mov bx,0x7000
0000032A  8EC3              mov es,bx
; ***************** END of UNUSED NOW ***
    pop   ecx
    mov   eax, dword es:[ecx]                    ;// Get FAT entry
;    mov   eax, dword ptr es:[ecx]               ;// Get FAT entry
;00000333  6625FFFFFF0F 
     and eax,0xfffffff
;    and   eax, HEX(0fffffff)                    ;// Mask off reserved bits
;
    ret
;
FatSectorInCache:                               ;// This variable tells us which sector we currently have in memory
;0000033A  FF            
     db 0xff
;0000033B  FF            
     db 0xff
;0000033C  FF           
     db 0xff
;0000033D  FF           
     db 0xff
;    .long    HEX(0ffffffff)                     ;// There is no need to re-read the same sector if we don't have to
;
;
;// Reads cluster number in EAX into [ES:0000]
ReadCluster:
    ;// StartSector = ((Cluster - 2) * SectorsPerCluster) + ReservedSectors + HiddenSectors;
;
    dec   eax
    dec   eax
    xor   edx, edx
;00000345  660FB65E0D        movzx ebx,byte [bp+0xd]
    movzx ebx, byte [SectsPerCluster]
;    movzx ebx, byte ptr BP_REL(SectsPerCluster)
    mul   ebx
    push  eax
    xor   edx, edx
;00000352  660FB64610        movzx eax,byte [bp+0x10]
    movzx eax, byte [NumberOfFats]
;    movzx eax, byte ptr BP_REL(NumberOfFats)
;00000357  66F76624          mul dword [bp+0x24]
    mul   dword [SectorsPerFatBig]
;    mul   dword ptr BP_REL(SectorsPerFatBig)
;0000035B  660FB75E0E        movzx ebx,word [bp+0xe]
    movzx ebx, word [ReservedSectors]
;    movzx ebx, word ptr BP_REL(ReservedSectors)
    add   eax, ebx
;00000363  6603461C          add eax,[bp+0x1c]
    add   eax, dword [HiddenSectors]
;    add   eax, dword ptr BP_REL(HiddenSectors)
    pop   ebx
    add   eax, ebx              ;// EAX now contains the logical sector number of the cluster
    xor   bx, bx                ;// We will load it to [ES:0000], ES loaded before function call
;0000036E  0FB64E0D          movzx cx,[bp+0xd]
    movzx cx, byte [SectsPerCluster]
;    movzx cx, byte ptr BP_REL(SectsPerCluster)
    call  ReadSectors
    ret
;
;// Displays a file not found error message
;// And reboots
; *************** UNUSED NOW **************
;0000037C  BECC7D            mov si,0x7dcc
;0000037F  E816FE            call 0x198
; *************** END of UNUSED NOW *******
PrintFileNotFound:
    mov  si, msgFreeLdr         ;// FreeLdr not found message
;    mov  si, offset msgFreeLdr  ;// FreeLdr not found message
    call PutChars               ;// Display it
;
    jmp  Reboot
;
msgFreeLdr:
	db	'freeldr.sys not found',0xd,0xa,0
;    .ascii "freeldr.sys not found", CR, LF, NUL
filename:
	db	'FREELDR SYS'
;    .ascii "FREELDR SYS"
msgLoading:
	db	'Loading FreeLoader...',0xd,0xa,0
;    .ascii "Loading FreeLoader...", CR, LF, NUL
;
times 1022 - ($ - $$) db 0
;.org 1022   ;// Pad to 1022 bytes
;
	db	0x55,0xaa
;    .word HEX(0aa55)       ;// BootSector signature
;
;.endcode16
;
;END
