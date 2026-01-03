;    Copyright (C) 2025, deomsh 
;   	deomshorg@gmail.com
;
;    This program is free software; you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation; either version 2 of the License, or
;    any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program; if not, write to the Free Software
;    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;
; MSBOOT71.nasm, derived from grub4dos' grldrstart.S (grldr.pbr)
; v.26 (20251229), by deomsh with MUCH help of Copilot
;
bits            16                      ; real mode, 16-bit instructions
org             0x7C00   		    	; boot sector load address
ORIGIN  	    equ  0x7C00             ; bad, not taken by compiler
;BIOSEG 		equ	 0x2000 	; BIOSEG	    EQU 70H			; destingation segment of BIOS
; NEW segment for MSDOS71
BIOSEG 		    equ 0x70            	; BIOSEG	    EQU 70H			; destingation segment of BIOS
;IBMLOADSIZE	equ	 630		; Size of current GRLDR module in sectors
; NEW constant for MSDOS71 + 1 because before ResdDisk ne less ?1?
; IBMLOADSIZE	equ		58		; IBMLOADSIZE 	equ 3			;J.K. Size of IBMLOAD module in sectors
;OKE; IBMLOADSIZE	equ		4		; IBMLOADSIZE 	equ 3			;J.K. Size of IBMLOAD module in sectors
IBMLOADEND	    equ 0xF0        	    ; Segment is used, if value is 0xF0 four sectors are loaded
DIRSEG	        equ	0x50 	            ; The first [!root-] directory sector is loaded into 00500h
;
BioClusLOffset	equ	Start - $$          ; first via stack: reserved first 4 bytes
BioClusHOffset	equ	Start - $$ + 2      ; first via stack: reserved first 4 bytes
Label44Offset 	equ	Start - $$ + 4      ; was offset [0x7C44]
Label48Offset	equ	Start - $$ + 8      ; was offset [0x7C48]
Label4COffset	equ	Start - $$ + 12     ; was offset [0x7C4C]
;
Int13AhOffset   equ     Int13Ax - $$ + 2    ; At label Int13Ax: mov ax, 0x201 = B8 01 02 => third byte overwritten if EBIOS present
DiskErrOffset   equ     DiskErr - $$        ; written at 0x1EE for BIO v7 Msg
;
GLOBAL EntryPoint:		  ; Declares EntryPoint as a global symbol, making it visible to the linker or other modules.
; EntryPoint:
    jmp Start         	  ; jump to boot code Start:
    nop                   ; filler byte
;
; -------------------------
; BIOS Parameter Block (BPB)
; -------------------------
%define OEM			bp+3
    db "MSWIN4.1" ; MS-DOS7.1 OEM-String
;org; OEM         db "IBM  2.0" ; OEM string
%define ByteSec		bp+0xb
    dw 512        ; bytes per sector
%define SecClus		bp+0xd
    db 1          ; sectors per cluster
%define ReservSec	bp+0xe
    dw 32         ; reserved sectors
%define NumFATs		bp+0x10
    db 2          ; number of FATs
%define RootEntries	bp+0x11
    dw 0          ; root entries (FAT32=0)
%define TotSect16	bp+0x13
    dw 0          ; total sectors (16-bit)
%define MediaByte	bp+0x15
    db 0xF8       ; media descriptor
%define FatSize16	bp+0x16
    dw 0          ; FAT size (16-bit)
%define SPT_Heads	bp+0x18
	dd 0x003F00FF ; sectors per track combined with number of heads
;SecTrack    dw 0x3F       ; sectors per track
;NumHeads    dw 0xFF       ; number of heads
%define HiddSec		bp+0x1c
    dd 0x3F       ; hidden sectors
%define TotSect		bp+0x20
    dd 0x3FFBC1   ; total sectors (32-bit)
%define FatSize32	bp+0x24
    dd 0x00000FFB ; FAT size (32-bit)
%define Mirror		bp+0x28
    dw 0          ; mirror flags
%define FSVersion	bp+0x2a
    dw 0          ; filesystem version
%define RootClus	bp+0x2c
    dd 2          ; root cluster
%define FSInfo		bp+0x30
    dw 1          ; FSInfo sector
%define BackupBoot	bp+0x32
    dw 6          ; backup boot sector
;
times 12 db 0             ; reserved
;
%define DriveNum	bp+0x40
    db 0x80       ; drive number
%define ReservedNT	bp+0x41
    db 0          ; reserved NT
%define BootSig		bp+0x42
    db 0x29       ; boot signature
%define UUID		bp+0x43
    dd 0          ; volume ID
%define VolLabel	bp+0x47
    db "NO NAME    " ; volume label
%define FileSys		bp+0x52
    db "FAT32   " ; filesystem type
;
; -------------------------
; Boot code starts at 5Ah
; -------------------------
;v22; NEW storage bytes for MSDOS71: overwriting original code!
;v22;UDATA:
;v22; --- Local variable definitions ---
;v22;BIOCLUSL	equ	UDATA+0     ; first via stack: reserved first 4 bytes
;v22;BIOCLUSH	equ	UDATA+2     ; first via stack: reserved first 4 bytes
;v22;LABEL44 	equ	UDATA+4		; was offset [0x7C44]
;v22;LABEL48		equ	UDATA+8	  	; was offset [0x7C48]
;v22;LABEL4C		equ	UDATA+12  	; was offset [0x7C4C]
;v22;NEXT4		equ	UDATA+16
;v22;NEXT5		equ	UDATA+20
;v22; Max 44 bytes up to LABEL1 = DiskInit , but cannot write to higher UDATA before
;    add [0x7C48], eax     	; store var = LBA of data-area, only written once (after use in diskinit:)!
Start:
    cli                   	            ; disable interrupts
    cld                   	            ; clear direction flag
    xor ax, ax            	            ; ax=0
    mov bp, ORIGIN        	            ; set BP to base
    mov sp, bp            	            ; set SP
    mov ss, ax            	            ; set SS=0
    sti                   	            ; enable interrupts
;v22; Option to change in 0x0C afterwards for LBA access only (BIO v7 specific)
;v22;    mov [bp+2], byte 0x90   ; Default 90 = CHS, if changed to 0C LBA (on FAT12/16 0E!)
; NEW code for MSDOS71
	push eax			  	            ; Reserve 4 bytes on Stack, to later load BIOSEC
;OKE;	push IBMLOADSIZE	  	; should be NOW at [bp-6] (first [bp-10])
;
; --- Check BIOS INT13h Extensions ---
    push ax               	            ; save ax
;
    mov [DriveNum], dl	  	            ; store BIOS drive
;BAD?;    mov word [DriveNum], dx       ; store BIOS drive
;
    mov ah, 0x41          	            ; check extensions
    mov bx, 0x55AA                  	; signature
    int 0x13              	            ; BIOS disk service
;
    pop ax                	            ; restore ax
    mov ds, ax            	            ; set DS
;
CheckExtensions:
    jb DiskInit           	            ; jump if error
    cmp bx, 0xAA55        	            ; check signature
    jne DiskInit          	            ; jump if not match
    ror cl, 1             	            ; rotate cl
    jae DiskInit          	            ; jump if carry clear
; Compiler placed next address at offset 194h !
    mov byte [bp+Int13AhOffset], 0x42   ; patch AH=42h ; More bytes behind now: mov byte [0x7DB4], 0x42 ; patch byte at offset 0x1B4
;v23;    mov byte [0x7DB4], 0x42	            ; patch AH=42h ; More bytes behind now: mov byte [0x7D86], 0x42 ; patch byte at offset 0x186
;v22;    mov byte [0x7DBA], 0x42	            ; patch AH=42h ; More bytes behind now: mov byte [0x7D86], 0x42 ; patch byte at offset 0x186
; NEW code for MSDOS71
	mov		byte [bp+2], 0xc			; Set FAT32 LBA type instead of CHS if EBIOS is present. CHS is '90'
; END OF NEW code for MSDOS71
;
DiskInit:
    xor bx, bx            	            ; bx=0
    xor eax, eax          	            ; eax=0
    mov dword [bp+Label44Offset], eax   ; clear var
;v22;    mov dword [LABEL44], eax       ; clear var
    mov ax, word [ReservSec]            ; load reserved
    add eax, dword [HiddSec]            ; add hidden
    mov [bp+Label48Offset], eax         ; store var
;v22;    mov [LABEL48], eax     	    ; store var
    mov [bp+Label4COffset], eax    	    ; store var
;v22;    mov [LABEL4C], eax     	         ; store var
; New here
;BAD;	mov dword [bp-4], eax	; Save starting LBA of DataArea to stack
;
;mismatch!;    mov eax, byte [NumFATs] 	  ; load FAT count
;BAD;
    mov eax, [NumFATs] 	  	            ; load FAT count
    mul dword [FatSize32] 	            ; multiply
    add [bp+Label4COffset], eax     	; add to var
;v22;    add [LABEL4C], eax     	; add to var
; Try here
	mov eax, dword [bp+Label4COffset]   ; NOW: LBA of dataarea - var is reused ??
;v22;	mov eax, dword [LABEL4C] ; NOW: LBA of dataarea - var is reused ??
	mov dword [bp-4], eax               ;
;BAD;	mov dword [bp-4], [LABEL4C]	    ; Save starting LBA of DataArea to stack
;BAD;	mov dword [bp-4], eax	        ; Save starting LBA of DataArea to stack
;
    mov eax, dword [RootClus]           ; load root cluster
;
LoadRootCluster:
    push eax              	            ; save eax
    call ClusterToLBA     	            ; convert cluster
    mov si, NOBIO         	            ; point to "No"
    jb PrintErrorLoop     	            ; jump if error
;
SearchDirLoop:
    push BIOSEG           	            ; push seg
    pop es                	            ; set ES
    push es               	            ; save ES
    call DiskRead         	            ; read sector
    pop es                	            ; restore ES
    xor di, di            	            ; di=0
;
SearchFileLoop:
    mov cx, 0xB                       	; length=11
    mov si, BIO                    	    ; point to "GRLDR"
    repe cmpsb            	            ; compare strings
    je FileFound                      	; found
    add di, 0x20          	            ; next entry
    and di, 0xFFE0                  	; align
    cmp di, word [ByteSec]	            ; check limit
    jne SearchFileLoop    	            ; loop
    dec dx                	            ; decrement
    jne SearchDirLoop     	            ; loop
    pop eax               	            ; restore eax
    call GetFATEntry      	            ; get FAT entry
    jmp LoadRootCluster             	; loop
;
FileFound:
    push word es:[di+9]   	            ; push cluster hi
    push word es:[di+0xF] 	            ; push cluster lo
    pop eax               	            ; eax=cluster
;
	mov dword [bp+BioClusLOffset], eax  ; Save first cluster number of BIO to reserved stack fixed loacation, maxbe UDATA+0 or =44 ?
;v22;	mov dword [BIOCLUSL], eax ; Save first cluster number of BIO to reserved stack fixed loacation, maxbe UDATA+0 or =44 ?
;
LoadFileLoop:
    push eax              	            ; save eax
    call ClusterToLBA     	            ; convert cluster
    jae ReadClusterLoop   	            ; jump if valid
;
MSDOS71:	; Prepare for BIO32 handshake
; MediaByte  seems not to be used in MSDOS7 handshake protocol => soon 'mov cx, 0x800'
;BAD;	mov dh, byte [MediaByte] ; Load DH with the media descriptor byte from memory.
;BADTOO?;    mov dl, byte [DriveNum]	; load drive, high byte is zero = [ReservedNT]
;	mov ch, byte [MediaByte] ; Load CH with the media descriptor byte from memory.
;org;	mov dx, word [DriveNum]	; load drive, high byte is zero = [ReservedNT]
;ORG;    push dx            ; push drive
;
	mov si, word [bp+BioClusHOffset]    ; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
;v22;	mov si, word [BIOCLUSH]	; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
	mov di, word [bp+BioClusLOffset]    ; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
;v22;	mov di, word [BIOCLUSL]	; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
; DTP at 0x78 & 0x7A => try to read 0x78 - 4 = 0x74, because of four pops in BIO v7 
	push word [0x7A]
	push word [0x78]
	push ax
	push ax
;ORG;	push word [0x78]
;ORG; 	push word [0x7A]
;badtoo;	mov sp, word 0x74		; Set SP to start of DTP, vector INT 1E should be at sp+4, popped to by BIO v7
;nodiff;	mov eax, ss:[0x78]		; vector for INT12/13
;nodiff;	mov [bp-12], eax		; expected on the stack by BIO?
	jmp BIOSEG:0x0200		            ; far jump
;
ReadClusterLoop:
; NEW to copy first cluster number from EAX
;NONEED;	cmp byte [bp-6], IBMLOADSIZE ; check if still first sector
;NONEED;	jne NoBioSec			; only saving LBA if first sector going to be read (flag from 'cmp ax, LOADSIZE')
;NONEED;	mov dword [bp-4], eax	; Save starting LBA of BIO to stack
;NONEED;NoBioSec:
;OKE;	push ax					; save current value of ax
;
; NEW code for MSDOS71: check if IBMLOADSIZE sectors are loaded
;OKE;	mov ax, [bp-6]			; get sector counter from stack
;now 3;	cmp ax, 1				; Check if 4 sectors are copied
;OKE;	cmp ax, 0				; Check if 4 sectors are copied <=> actually 5 sectors copied !!!
;OKE;	je  MSDOS71 	        ; jump if equal
;OKE;	dec ax					; decrease with one
;OKE;	mov [bp-6], ax			; Save number of sectors to copy AFTER next DiskRead to the stack
;OKE;	pop ax					; Restore AX from the stack: ASSUME original eax is sector LBA (see: fourth line in DiskRead:)
; Original code
    call DiskRead         	            ; read cluster
; Better new code: check if BX or ES is 0xF0 after last call to DiskRead, then 4 secors are loaded at 0x70, 0x90, 0xB0 and 0xD0
    push ax                 
    mov ax, es              
	cmp al, byte IBMLOADEND 
; BAD; cmp es, byte IBMLOADEND          ; Not possible in NASM (compiler error)
    pop ax
	jae MSDOS71				
; Original code
    dec dx                	            ; decrement
    jne ReadClusterLoop   	            ; loop
    pop eax               	            ; restore eax
    call GetFATEntry      	            ; get FAT entry
    jmp LoadFileLoop                  	; loop
;
GetFATEntry:
    push es               	            ; save ES
    shl eax, 2            	            ; shift
    movzx ebx, word [ByteSec]           ; load bytes/sector
    div ebx               	            ; divide
    add eax, [bp+Label48Offset]         ; add base
;v22;    add eax, [LABEL48]     	; add base
; Maybe conflict with BIOSEG ?? Eventually switch to 0x2000 or copy 2048 bytes from 0x2000 to 0x700 ??
    mov bx, DIRSEG         	            ; set ES
;ORG;    mov bx, 0x60          	; set ES
    mov es, bx            	            ; ES=0x50 (was: ES=0x60)
    xor bx, bx            	            ; bx=0
    cmp eax, [bp+Label44Offset]     	; compare
;v22;    cmp eax, [LABEL44]     	; compare
    je FATEntryLoaded     	            ; already loaded
    mov [bp+Label44Offset], eax     	; store
;v22;    mov [LABEL44], eax     	; store
    push es               	            ; save ES
    call DiskRead         	            ; read sector
    pop es                	            ; restore ES
FATEntryLoaded:
    and byte es:[edx+3], 0xF            ; mask
    mov eax, dword es:[edx]             ; load entry
    pop es                      	    ; restore ES
    ret                           	    ; return
;
ClusterToLBA:
    cmp eax, 0x0FFFFFF8           	    ; check end
    cmc                   	            ; complement carry
    jb EndCluster         	            ; jump if end
    dec eax               	            ; decrement
    dec eax               	            ; decrement
    movzx edx, byte [SecClus]           ; load sectors/clus
    push dx               	            ; save dx
    mul edx               	            ; multiply
    pop dx                	            ; restore dx
    add eax, [bp+Label4COffset]         ; add base
;v22;    add eax, [LABEL4C]             ; add base
EndCluster:
    ret 	       			            ; return
;
DiskRead:
    pushad                	            ; save regs
    xor edx, edx          	            ; edx=0
    push edx              	            ; push 0
    push eax              	            ; push LBA
    push es               	            ; push ES
    push dx               	            ; push offset
    push 1                	            ; sector count
    push 0x10             	            ; heads const
    xor ecx, ecx          	            ; ecx=0
;
;Malforms Stack;    push word [SecTrack] ; push sectors/track
;GOOD
    push dword [SPT_Heads] 	            ; push sectors/track & NumHeads in High word
;BAD;    push dword [SecTrack] 	; push sectors/track & NumHeads in High word
;    
	pop cx                	            ; load CX
    div ecx               	            ; divide, EAX is the sector LBA
    inc dx                	            ; inc sector to base 1, DX is the remainder
    pop cx                	            ; load CX
    push dx               	            ; push sector
    xor dx, dx            	            ; dx=0
    div ecx                            	; divide
    xchg dh, dl                       	; swap head
    pop cx                	            ; load CX
    xchg ch, al           	            ; swap cylinder
    shl ah, 6                        	; shift
    or cl, ah                         	; combine
Int13Ax:                                ; Label needed to calculate Int13AxOffset to copy 42h if EBIOS present
    mov ax, 0x201         	            ; read sectors
    mov si, sp            	            ; SI=SP
    mov dl, [0x7C40]      	            ; load drive
    push es               	            ; save ES
    push ds               	            ; save DS
    int 0x13              	            ; BIOS disk read
    pop ax                	            ; restore ax
    mov ds, ax            	            ; restore DS
    pop bx                	            ; restore bx
    jb DiskError          	            ; jump if error
; NEW Code for printing Dot
   mov ax, 0x0e2e			    ; Combine 'mov ah, 0xE' and 'mov al, "."'
   int 0x10									
; END of new Code for printing Dot
    lea bx, [bx+0x20]     	            ; next seg
    mov es, bx            	            ; set ES
    popa                  	            ; restore regs
    popad                 	            ; restore regs
    inc eax               	            ; next LBA
    ret                                	    ; return
;
DiskError:
    mov si, DiskErr                   	    ; point to error msg
PrintErrorLoop:
    lodsb                 	            ; load char
;;    test al, al      			    ; 64 C0 (2)
;;    jz	Hang    			    ; 74 02 (2) ???
    mov ah, 0x0E          	            ; set INT10 mode
    int 0x10			  	    ;
    cmp al, 0			  	            ;	compare 
;;    jmp PrintErrorLoop    	            ; jump if not last char
    jne PrintErrorLoop    	            ; jump if not last char
Hang:
    jmp Hang              	            ; infinite loop
;
; -------------------------
; Strings
; -------------------------
;
;times 0x1E3 - ($ - $$) db 0
;v24;;times 0x1E5 - ($ - $$) db 0
;0ke;times 0x1EE - ($ - $$) db 0
;
;DiskErr db "Disk Error", 0			    ; Last char 'r' is Error message at 0x1EE because of BIO protocol
;oke;DiskErr db "E", 0			; Error message at 0x1EE because of BIO protocol
;
NOBIO   db 0x0d, 0x0a, 0x0a, "No "
BIO 	db "IO      SYS"
;
times 0x1EE - ($ - $$) db 0
        dw DiskErrOffset
;
;NOBIO   db "No "
;v24;NOBIO   db "No"
;BIO 	db "IO      SYS"
;v24;BIO 	db "IO      SYS", 0
;ORG;BIO 	db "GRLDR      ", 0
;NOGRLDR db "No"
;GRLDR   db "GRLDR      ", 0
;org; DiskErr db "DiskErr", 0
DiskErr db 0x0d, 0x0a, "Disk Error"
;
times 508 - ($ - $$) db 0
db 0
db 0
;ORG;
times 510 - ($ - $$) db 0
db 0x55
db 0xAA
