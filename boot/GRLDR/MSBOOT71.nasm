;    Copyright (C) 2026, deomsh 
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
; v.27 (20260404), by deomsh with help of Copilot and DeepSeek V3
;
bits            16                      ; real mode, 16-bit instructions
org             0x7C00   		    	; boot sector load address
ORIGIN  	    equ 0x7C00      	    ; bad, not taken by compiler
BIOSEG 		    equ 0x70            	; BIOSEG	  EQU 70H			; destingation segment of BIOS
IBMLOADSIZE		equ	4					; IBMLOADSIZE equ 3			;J.K. Size of IBMLOAD module in sectors
;IBMLOADEND	    equ 0xF0        	    ; Segment is used, if value is 0xF0 four sectors are loaded
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
    jmp Start         	  ; jump to boot code Start:
    nop                   ; filler byte
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
times 12 db 0     ; reserved in FAT32
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
; -------------------------
; Moved Data Area: Boot code starts now at 78h
; -------------------------
NOBIO   db "No "
BIO 	db "IO      SYS", 0x20			; terminating zero not needed, if not found All three messages loaded in SI, starting with NOBIO
DiskErr db "Error Key2reboot", 0
Start:
    cli                   	            ; disable interrupts
    cld                   	            ; clear direction flag
    xor ax, ax            	            ; ax=0
    mov bp, ORIGIN        	            ; set BP to base
    mov sp, bp            	            ; set SP
    mov ss, ax            	            ; set SS=0
    sti                   	            ; enable interrupts
	push eax			  	            ; Reserve 4 bytes on Stack, to later load BIOSEC
; --- Check BIOS INT13h Extensions ---
    push ax               	            ; save ax
    mov [DriveNum], dl	  	            ; store BIOS drive
    mov ah, 0x41          	            ; check extensions
    mov bx, 0x55AA                  	; signature
    int 0x13              	            ; BIOS disk service
    pop ax                	            ; restore ax
    mov ds, ax            	            ; set DS
CheckExtensions:
    jb DiskInit           	            ; jump if error
    cmp bx, 0xAA55        	            ; check signature
    jne DiskInit          	            ; jump if not match
    ror cl, 1             	            ; rotate cl
    jae DiskInit          	            ; jump if carry clear
; Compiler placed next address currently at offset 1C9h !
    mov byte [bp+Int13AhOffset], 0x42   ; patch AH=42h ; More bytes behind now: mov byte [0x7DC9], 0x42 ; patch byte at offset 0x1C0
	mov		byte [bp+2], 0xc			; Set FAT32 LBA type instead of CHS if EBIOS is present. CHS is '90' (New code for MS-DOS 7.1)
DiskInit:
    xor bx, bx            	            ; bx=0
    xor eax, eax          	            ; eax=0
    mov dword [bp+Label44Offset], eax   ; clear var
    mov ax, word [ReservSec]            ; load reserved
    add eax, dword [HiddSec]            ; add hidden
    mov [bp+Label48Offset], eax         ; store var
    mov [bp+Label4COffset], eax    	    ; store var
    mov eax, [NumFATs] 	  	            ; load FAT count
    mul dword [FatSize32] 	            ; multiply
    add [bp+Label4COffset], eax     	; add to var
	mov eax, dword [bp+Label4COffset]   ; NOW: LBA of dataarea - var is reused ??
	mov dword [bp-4], eax               ; save starting LBA of BIO to stack [Really needed ??]
    mov eax, dword [RootClus]           ; load root cluster
;
LoadRootCluster:
    push eax              	            ; save eax
    call ClusterToLBA     	            ; convert cluster
    mov si, NOBIO         	            ; point to "No"
    jb PrintError       	            ; jump if error
SearchDirLoop:
    push BIOSEG           	            ; push seg
    pop es                	            ; set ES
    push es               	            ; save ES
    call DiskRead         	            ; read sector
    pop es                	            ; restore ES
    xor di, di            	            ; di=0
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
FileFound:
    push word es:[di+9]   	            ; push cluster hi
    push word es:[di+0xF] 	            ; push cluster lo
    pop eax               	            ; eax=cluster
	mov dword [bp+BioClusLOffset], eax  ; Save first cluster number of BIO to reserved stack fixed loacation, maxbe UDATA+0 or =44 ?
LoadFileLoop:
    push eax              	            ; save eax
    call ClusterToLBA     	            ; convert cluster
    jnae PrintErrorStart                    ; jump if not valid
    mov dl, IBMLOADSIZE
;v26;    jae ReadClusterLoop   	            ; jump if valid
;NEW in v27: Rely in LDOS MS-DOSv7 load protocol: first fours sectors of BIO must be contiguous. Always the case if SecClus>=4
ReadClusterLoop:
    call DiskRead         	            ; read cluster
    dec dl                	            ; decrement
    jne ReadClusterLoop   	            ; loop
MSDOS71:								; Prepare for BIO32 handshake
; MediaByte  seems not to be used in MSDOS7 handshake protocol => soon 'mov cx, 0x800'
;	mov ch, byte [MediaByte] ; Load CH with the media descriptor byte from memory.
;org;	mov dx, word [DriveNum]	; load drive, high byte is zero = [ReservedNT]
	mov si, word [bp+BioClusHOffset]    ; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
	mov di, word [bp+BioClusLOffset]    ; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
; DTP at 0x78 & 0x7A => try to read 0x78 - 4 = 0x74, because of four pops in BIO v7 
	push word [0x7A]
	push word [0x78]
	push ax
	push ax
	jmp BIOSEG:0x0200		            ; far jump
;
;v26;ReadClusterLoop:
;v26;    call DiskRead         	            ; read cluster
;v26;    push ax                 
;v26;    mov ax, es              
;v26;    cmp al, byte IBMLOADEND 
;v26;    pop ax
;v26;	 jae MSDOS71				
; Original code
;v26;    dec dx                	            ; decrement
;v26;    jne ReadClusterLoop   	            ; loop
;v26;    pop eax               	            ; restore eax
;v26;    call GetFATEntry      	            ; get FAT entry
;v26;    jmp LoadFileLoop                  	; loop
;
; --- Sub-routines ---
GetFATEntry:
    push es               	            ; save ES
    shl eax, 2            	            ; shift
    movzx ebx, word [ByteSec]           ; load bytes/sector
    div ebx               	            ; divide
    add eax, [bp+Label48Offset]         ; add base
    mov bx, DIRSEG         	            ; set ES
    mov es, bx            	            ; ES=0x50 (was: ES=0x60)
    xor bx, bx            	            ; bx=0
    cmp eax, [bp+Label44Offset]     	; compare
    je FATEntryLoaded     	            ; already loaded
    mov [bp+Label44Offset], eax     	; store
    push es               	            ; save ES
    call DiskRead         	            ; read sector
    pop es                	            ; restore ES
FATEntryLoaded:
    and byte es:[edx+3], 0xF            ; mask
    mov eax, dword es:[edx]             ; load entry
    pop es                      	    ; restore ES
    ret                           	    ; return
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
EndCluster:
    ret 	       			            ; return
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
    push dword [SPT_Heads] 	            ; push sectors/track & NumHeads in High word
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
    mov dl, [DriveNum]      	        ; load drive
;v26;    mov dl, [0x7C40]      	            ; load drive
    push es               	            ; save ES
    push ds               	            ; save DS
    int 0x13              	            ; BIOS disk read
    pop ax                	            ; restore ax
    mov ds, ax            	            ; restore DS
    pop bx                	            ; restore bx
    jb PrintErrorStart     	            ; jump if error
;v26;    jb DiskError          	            ; jump if error
; Code for printing Dot
    mov ax, 0x0e2e					    ; Combine 'mov ah, 0xE' and 'mov al, "."'
    int 0x10							
; END of Code for printing Dot
    lea bx, [bx+0x20]     	            ; next seg
    mov es, bx            	            ; set ES
    popa                  	            ; restore regs
    popad                 	            ; remove DAP from stack
    inc eax               	            ; next LBA
    ret                            	    ; return
; --- END of Sub-routines ---
;v26;DiskError:
;v26;    mov si, DiskErr                    ; point to error msg
;v26;PrintErrorLoop:
;v26;    lodsb                 	            ; load char
;v26;    mov ah, 0x0E          	            ; set INT10 mode
;v26;    int 0x10			  	            ;
;v26;    cmp al, 0			  	            ;	compare 
;v26;    jne PrintErrorLoop    	            ; jump if not last char
;v26;Hang:
;v26;    jmp Hang              	            ; infinite loop
;v26; -------------------------
;v26; Strings
;v26; -------------------------
;
;v26;NOBIO   db 0x0d, 0x0a, 0x0a, "No "
;v26;BIO 	db "IO      SYS"
;V27:
PrintErrorStart:
    mov si, DiskErr                   	; point to error msg
    jmp PrintError						; needed to jump over field 1EEh (LDOS MS-DOSv7 message address rules)
times 0x1EE - ($ - $$) db 0				; LDOS MS-DOS v7 message rules: IO.SYS can reuse message in Boot code [not observed so far]
        dw DiskErrOffset
;v26;DiskErr db 0x0d, 0x0a, "Disk Error"
;v26;times 508 - ($ - $$) db 0
;V27: moved to the end; no empty bytes anymore at 1FCh and 1FDh !
PrintError:								; Modified by DeepSeek V3 to gain one byte
    mov     ah, 0x0E        			; 2 bytes: Setup for INT 10h once
PrintErrorLoop:
    lodsb                   			; 1 byte: Load char from SI
    int     0x10            			; 2 bytes: Print char
    or      al, al          			; 2 bytes: Check for null terminator
    jnz     PrintErrorLoop  			; 2 bytes: Jump if not null
WaitForKey:								; Label not needed now
    cbw                     			; 1 byte: FIX! Clear AH using the fact AL=0
    int     0x16            			; 2 bytes: Wait for keypress
    int     0x19            			; 2 bytes: Reboot
    ; Total: 14 bytes
;v26;times 508 - ($ - $$) db 0
;V26;    db 0
;V26;    db 0
times 510 - ($ - $$) db 0
; Magic Bytes
    db 0x55
    db 0xAA
