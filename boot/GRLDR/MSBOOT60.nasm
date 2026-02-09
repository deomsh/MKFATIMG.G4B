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
; MSBOOT70.nasm, based on GRLDRFAT.nasm, Originally: GRLDRSTART.S (Part FAT12/16)
; Disassembly from: https://shell-storm.org/online/Online-Assembler-and-Disassembler/
; v.0.3.0 (20251227), prepared for NASM by deomsh
;
bits		16										; Compiles okay with 'bits 16'
org			0x7C00
ORIGIN					equ		0x7C00				; BP stays at 0x7C00 - no relocation here
; NEW for MS-DOS 6.22
LOADSEG					equ		0x0050				; Memory 0x0500; used only for temporary loading Root Directory!
LOADOFF					equ		0x0000				; Offset
;ORG;LOADSEG					equ		0x2000				; Memory 0x20000 ?? Seems to be used for temporary loading Root Directory too
;ORG;LOADOFF					equ		0x0000				; Offset
BIOSEG					equ		0x70				; Memory 0x700 for BIO-load
; NEW for MS-DOS 6.22
BIOSIZE					equ		3					; Three contiguous sectors of BIO to load (2048 bytes), according to IDOS v6 boot protocol
;ORG;BIOSIZE					equ		4					; Four contiguous sectors of BIO to load (2048 bytes), according to IDOS v7 boot protocol
;ORG;BIOMEM					equ		0x700				; Start first sector of BIO v7 with 'MZ'
;ORG;BIOMEM2					equ		0x900				; Start second sector of BIO v7 with 'BJ'
;NONEED; NEW for MS-DOS 6.22
;NONEED;ROOTSEG					equ		0x50				; Memory at 0x500
;NONEED;ROOTOFF					equ		0x0000				; offset
;
%define ByteSec 		bp+0xb	
%define SecClus 		bp+0xd	
%define ReservSec		bp+0xe	
%define NumFATs			bp+0x10	
%define RootEntries		bp+0x11	
%define TotSec16		bp+0x13	
%define MediaByte		bp+0x15	
%define FATsz16			bp+0x16	
%define SecTrack		bp+0x18	
%define NumHeads		bp+0x1a	
%define HiddSec			bp+0x1c	
%define TotSec32		bp+0x20	
%define DriveNum		bp+0x24	
%define ReservNT		bp+0x25	
%define BootSig			bp+0x26	
;
RootStartOffset			equ	Start - $$ + 4			; Start - 0x7C00 => now at 0x71 
DataStartOffset			equ	Start - $$ + 8			; Start - 0x7C00 => now at 0x75 
BioClusLOffset			equ	Start - $$ + 12			; Start - 0x7C00 => now at 0x79 
BioClusHOffset			equ	Start - $$ + 14			; Start - 0x7C00 => now at 0x7b 
;NONEED; NEW for MS-DOS 6.22
;NONEED;RootAddressOffset		equ RootOff - $$    		; RootOff - 0x7C00 => now at 0x3f
;
LoadAddressOffset		equ LoadOff - $$    		; LoadOff - 0x7C00 => now at 0x3f
Int13AlPtrOffset		equ Int13Al - $$			; = Int13Al - 0x7C00
Int13AhPtrOffset		equ Int13Ah - $$			; = Int13Ah - 0x7C00
FileNamePtrOffset		equ FileNamePtr - $$		; = FileNamePtr - 0x7C00
FileErrorMsgOffset		equ FileErr  - $$       	; = FileErr - 0x7C00
File1Offset				equ Bio - $$				; = Bio - 0x7C00
; NEW for MS-DOS 6.22
File2Offset				equ Dos - $$				; = Dos - 0x7C00
;ORG;File2Offset				equ Bio2 - $$				; = Bio2 - 0x7C00
;
DiskErrorMsgOffset		equ DiskErr  - $$			; = DiskErr - 0x7C00
CommonErrorMsgOffset	equ CommonErr  - $$			; = DiskErr - 0x7C00
;
Entrypoint:
	jmp		Start									; original 0x3e, now variable if offsets change (MAX: 0x7F for short jump)
	nop
	OEM		db	'MSWIN4.1'							; Test v0.1.0
; BPB
; Values in BPB for Standard 1440KB Floppy-disk
			dw	512									; ByteSec Offset 0Bh =	512, Required
			db	1									; SecClus Offset 0Dh
			dw	1									; ReservSec Offset 0Eh
			db	2									; NumFATs Offset 10h
			dw	224									; RootEntries Offset 11h
			dw	2880								; TotSec16 Offset 13h
			db	0xf0								; MediaByte Offset 15h
			dw	9									; FATsz16 Offset 16h
			dw	18									; SecTrack Offset 18h
			dw	2									; NumHeads Offset 1Ah
			dd	0									; HiddSec Offset 1Ch
			dd	0									; TotSec32 Offset 20h ; needs dd
			db	0									; DriveNum Offset 24h
			db 0									; ReservNT Offset 25h - 0x25 maybe Partition Number, default = 0 (=ReservedNT)
			db	0x29								; BootSig Offset 26h
	Uuid	dd	0x190c1c01							; Boot_Serial Offset 27h - unused here
	Label	db	'NO NAME    '						; Boot_Vol_Label Offset 0x2B - unused here
	FileSys	db	'FAT12/16'							; Boot_System_id Offset 36h
; --- Data 1 ---
;NONEED;RootOff		dw	ROOTOFF								; Offset 0000  Offset ...h => [bp + ....] = RootAddress as dword
;NONEED;RootSeg		dw	ROOTSEG								; Segment 0050
;
LoadOff		dw	LOADOFF								; Offset 0000  Offset ...h => [bp + 0x3e] = LoadAddress as dword
LoadSeg		dw	LOADSEG								; Segment 0050
;ORG;LoadSeg		dw	LOADSEG								; Segment 2000
Int13Al	    db	1									; 
Int13Ah	    db	2									; 
FileNamePtr	dw	Bio									; Initial: first filename
FileErr		db 0x0d, 0x0a, 0x0a, "No "				; 
Bio			db "IO      SYS"						; 
;ORG;
NoBioDos		db "/ "								; Label is still unused!
;ORG;NoBio2		db "/ "									; 
;ORG;
Dos			db "MSDOS   SYS", 0						; 
;ORG;Bio2		db "WINBOOT SYS", 0						; 
DiskErr		db	0x0a, 0x0d, 0x0a, "Disk error", 0	; 
; --- Start of Boot Code ---
Start:
	cli												; Disable interrupt's
	cld												; Set direction to default (low to high)
	xor		eax, eax								; Make high word of EAX = 0 too
	mov		bp, ORIGIN								; Set BP to ORIGIN = 0x7C00
	mov		sp, bp									; Set SP =BP
	mov		ss, ax									; Set SS = 0
	mov		ds, ax									; Set DS = 0
	mov		es, ax									; Set ES = 0
	sti												; Enable interrupt's
; --- cs=ss=ds=es=0, bp=0x7C00 ---					; CS is not explicit set to 0 - Assumed CS = 0 ?
	mov		word [DriveNum], dx						; Write DX to DriveNum (DH will be '00' => ReservNT = 0) = [bp + 0x24]
; NEW code for MSDOS70
;ORG;	push 	eax									  	; Reserve 4 bytes on Stack, to later load DataStart
; END OF NEW code for MSDOS70
; --- Check BIOS INT13h Extensions ---
	mov		ah, 0x41								;
	mov		bx, 0x55aa								;
	push	ds										;
	push	es										;
	int		0x13									;
	pop		es										;
	pop		ds										;
	jb		FindRoot								; No EBIOS
	cmp		bx, 0xaa55								;
	jne		FindRoot								; No EBIOS
	ror		cl, byte 1								;
	jae		FindRoot								; No EBIOS
; --- EBIOS supported ---
	mov		byte [bp+Int13AhPtrOffset], 0x42		; Check if EBIOS is active: [Data] => Double Check if not changed with new code
; NEW code for MSDOS70
;ORG;	mov		byte [bp+2], 0xe						; Set FAT12/16 LBA type instead of CHS if EBIOS is present. CHS is '90'
; END OF NEW code for MSDOS70
; --- GET DRIVE PARMS: Calculate start of some disk areas ---
FindRoot:											; 
	movzx	ebx, word [ReservSec]					; ReservSec = [bp + 0xe]
	add		ebx, dword [HiddSec]					; HiddSec = [bp + 0x1c]
	movzx	eax, byte [NumFATs]						; NumFATs = [bp + 0x10]
	movzx	ecx, word [FATsz16]						; FATsz16 = [bp + 0x16]
	mul		ecx										; EDX=0, EAX=total sectors for FAT. Multiply EAX by src (unsigned). Implicit Registers: EAX (destination), EDX (high dword of result)??
	add		ebx, eax								; 
	mov		dword [bp+RootStartOffset], ebx			; Write LBA of Root Directory to VAR2= RootStart => Write Root Directory LBA from ebx = [bp + 0x75]
	mov		ax, word [RootEntries]	    			; RootEntries = [bp + 0x11]
; --- Calculate how many sectors the root directory occupies ---
	add		ax, 0xf									; Used to round up
	mov		cx, 0x10								; 16 Directory Entries per Sector
	div		cx										; Get number of RootDirSectors: AX = sectors per root directory
	cwde											; 
; NEW code for MS-DOS 6.22
	mov		cx, 1									; Now only one sector needed
;ORG;	mov		cx, ax									; 
	add		ebx, eax								; LBA of DataStart
	mov		dword [bp+DataStartOffset], ebx			; Write LBA of DataStart is VAR3 = [bp + 0x79]
; NEW Code for MSDOS70
;ORG;	mov dword [bp-4], ebx							; Write Start of DataArea to bp-4 for BIO v7 handshake
; End of NEW Code for MSDOS70
; --- First, read the whole root directory into the temporary buffer ---
	mov		eax, dword [bp+RootStartOffset]			; VAR2 = RootStart = [bp + 0x75]
;NONEED; NEW for MS-DOS 6.22
;NONEED;	les		bx, [bp+RootAddressOffset]				; Target in memory: Values are '00 00 50 00' = [bp + 0x3e] = RootAddress - ES:BX = 0x0050:0000
;NONEED; --- cs=ss=ds=0 es=0x0050 ---
	les		bx, [bp+LoadAddressOffset]				; Target in memory: Values are '00 00 50 00' = [bp + 0x3e] = LoadAddress - ES:BX = 0x0050:0000
;ORG;	les		bx, [bp+LoadAddressOffset]				; Target in memory: Values are '00 00 00 20' = [bp + 0x3f] = LoadAddress - ES:BX = 0x2000:0000
; --- cs=ss=ds=0 es=0x2000 ---
	call	DiskRead								; ?? = 0, eax, ES is changed [NOT 0x2000 anymore]
SearchFile:
;NONEED; NEW for MS-DOS 6.22
;NONEED;	les		di, [bp+RootAddressOffset]				; Target in memory: Values are '00 00 50 00' = [bp + 0x3e] = RootAddress - ES:BX = 0x0050:0000
	les		di, [bp+LoadAddressOffset]				; Target in memory: Values are '00 00 50 00' = [bp + 0x3e] = LoadAddress - ES:BX = 0x0050:0000
;ORG;	les		di, [bp+LoadAddressOffset]				; Target in memory: Values are '00 00 00 20' = [bp + 0x3e] = LoadAddress - ES:BX = 0x2000:0000
; --- Search for kernel file name, and find start cluster ---
;ORG;SearchFileLoop:										; BX=0, CX=0
	mov		si, [bp+FileNamePtrOffset]				; Load FileName in SI from location pointing to BIO
	mov		cl, 0xb									; Length of kernel filename
;ORG;	push	di										;
	repe											; Repeat string operation until CX = 0 or ZF = 1. Implicit Registers: Uses CX (counter) and ZF (Zero Flag)
	cmpsb											; Compare byte from DS:SI with byte from ES:DI. Implicit Registers: Uses DS:SI (source) and ES:DI (destination)
;ORG;	pop		di										;
;
; NEW for MS-DOS 6.22
	jne		NotFound								; Better Label-name
	push	word es:[di+0xf]						; Save first cluster number of BIO. Offset Fh is now 1Ah -/- 0xB. 1Ah is FAT12/16 cluster number in Directory entry
	xor		eax, eax								; Clean EAX
;ORG;	je		FileFound								; Better Label-name
;ORG;
	cmp		byte [bp+FileNamePtrOffset], File1Offset ;		 (6)
	jne 	NotFound								 ; 75 07 (2)
	mov		byte [bp+FileNamePtrOffset], File2Offset ;	     (6)
	add		si, 0x15								; Next Directory Entry
	add		di, 0x15								; Next Directory Entry
	mov		si, [bp+FileNamePtrOffset]				; Load FileName in SI from location pointing to BIO
	mov		cl, 0xb									; Length of kernel filename
	repe											; Repeat string operation until CX = 0 or ZF = 1. Implicit Registers: Uses CX (counter) and ZF (Zero Flag)
	cmpsb											; Compare byte from DS:SI with byte from ES:DI. Implicit Registers: Uses DS:SI (source) and ES:DI (destination)
	je		FileFound								; Better Label-name
; END of NEW for MS-DOS 6.22
;
;ORG;	add		di, 0x20								; Next Directory Entry
;ORG;
;ORG;	je		NoBio									; OR jc 2f, exceeding 64K
;ORG;	cmp		byte es:[di], ch						; CH=0
;ORG;	jne		SearchFileLoop							; 
;ORG;
NoBio:
;ORG;
;ORG;	cmp		byte [bp+FileNamePtrOffset], File1Offset ;		 (6)
;ORG;	jne 	NotFound								 ; 75 07 (2)
;ORG;	mov		byte [bp+FileNamePtrOffset], File2Offset ;	     (6)
;ORG;	jmp		SearchFile								;		 (2)
NotFound:											; 
	mov		si, FileErr								; Load msg in SI: ( = "NO IO      SYS or WINBOOT SYS")
	call	Print									; 
	jmp 	CommonError								; 
FileFound:
;ORG;	push	word es:[di+0x1a]						; Save first cluster number of BIO. Offset 1Ah is FAT12/16 cluster number in Directory entry
;ORG;	xor		eax, eax								; Clean EAX
; NEW Code for MSDOS70
	pop		ax										; Restore first cluster number of file
	mov		[bp+BioClusLOffset], eax				; Write First Cluster number of BIO to VAR and zero's to high word => bp+0x7d
; End of NEW code for MSDOS70
	sub		ax, 0x2									; Decrease Cluster Number by two. Implicit Registers: Updates flags (ZF, CF, etc.)
	jb		EndSector								; Probably CF=1 set by sub: if AX=0 (EOC) gives -2 ??
	movzx	eax, ax									; Clean high word ??
	mov		cl, byte [SecClus]						; SecClus = [bp + 0xd]
	mul		ecx										; Get Sector offset of Cluster. Multiply EAX by src (unsigned). Implicit Registers: EAX (destination), EDX (high dword of result)??
	add		eax, dword [bp+DataStartOffset]			; VAR3 = DataStart => add to get Sector LBA = [bp + 0x79]
; NEW Code for MSDOS70
	mov		cl,	byte BIOSIZE						; Set number of sectors to read
	push	BIOSEG									; Change segment to 0x70
	pop		es										; set ES = 0x70
	push	ss										; Assumed: SS = 0
	pop		ds										; DS=SS
; END of NEW Code for MSDOS70
	call	DiskRead					;
EndSector:											; EOC encountered - done
; --- IDOS7 BIO load protocol ---
; NEW Code for MSDOS70
;ORG;	mov di, word [bp+BioClusLOffset]				; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
;ORG;	push 	word [0x7A]								; SP = 0x7B..
;ORG;	push 	word [0x78]								; SP = 0x7B..
;ORG;	push 	ax										; SP = 0x7B..
;ORG;	push 	ax										; SP = 0x7B..
;ORG;	cmp		word [cs:BIOMEM], 0x5a4d				; word must be specified
;ORG;	jne		NotFound								; 
;ORG;	cmp		word [cs:BIOMEM2], 0x4a42				; word must be specified
;ORG;	jne		NotFound								; 
    mov     si, NewLines							; 
    call    Print									; 
;
; NEW Code for MSDOS60
;ORG;	mov di, word [bp+BioClusLOffset]				; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
;ORG;	mov si, word [bp+BioClusHOffset]				; set Cluster number to first cluster of BIO = MSDOS7 handshake protocol
	xor		cx, cx
	mov     ch, byte [MediaByte]					; Load CH with the media descriptor byte from memory.
	xor		dx, dx
	mov     dl, byte [DriveNum]						; Load DL with the physical drive number from memory.
	mov     bx, word [bp+DataStartOffset]			; Load lower 16 bits into BX. AN000; J.K.I1.Get bios sector in bx
	mov     ax, word [bp+DataStartOffset+2]			; Overwrite AX with higher 16 bits (AX now contains only BIOS$_R). AN000; J.K.I1.
	jmp		BIOSEG:0x0000 							; 
; END of NEW Code for MSDOS60
;ORG;	jmp 	BIOSEG:0x0200						; 
; END of NEW Code for MSDOS70
DiskError:											; 
	mov		si, DiskErr								; Load msg
	call	Print									; 
CommonError:										; 
	mov		si, CommonErr							; Load msg
	call	Print									; 
WaitForKey:											; Label not needed now
	xor 	ah, ah									;
	int 	0x16									; Wait for key
	int 	0x19									; BIOS bootstrap (retry boot)
;*** SUB-ROUTINE *********************************************************************
DiskRead:											; Read a number of sectors into memory
; Call with:	DS=SS=0
;		eax = 32-bit DOS sector number
;		CX = number of sectors to read
;		ES:BX = destination buffer
; Returns:	CX=0
;		ES increased, BX untouched
;		ES:BX points one byte after the last byte read
;		eax = next sector number after read
;		All other registers preserved
; --- Total pushes before int 0x13 = 52 bytes, same count of total pop's afterwards ---
	pushad											; Save regs: 32 bytes pushed [ORG: 'pushal']
; --- Build the Disk Address Packet (DAP) on the stack ---
; --- 16 bytes: 10h = Packet size, 0 = Reserved, Sector count, Buffer offset, Buffer segment, LBA low word, LBA high word) ---
; --- AH = 0x4201 and Vector DI:SI = 0000:SI => SI is set to SP after building DAP on the stack (if jumped to Extensions)---
	xor		edx, edx								; 
	push	edx										; Hi 32bit of sector number. 4 bytes pushed
	push	eax										; Lo 32bit of sector number. 4 bytes pushed
	push	es										; Buffer segment. 2 bytes pushed
	push	bx										; Buffer offset. 2 bytes pushed
	push	0x1										; 1 sector to read. 2 bytes pushed
	push	0x10									; Size of this parameter block. 2 bytes pushed
	cmp		byte [bp+Int13AhPtrOffset], 0x42		; Check if EBIOS is active: [Data] => Double Check if not changed with new code
	je		Extensions								; Jump if EBIOS is active (INT13 AH=42)
; --- Old BIOS, AH=02 only ---
	xor		ecx, ecx								;
	push	dword [SecTrack]						; Lo:sectors per track, Hi:number of heads. SecTrack HERE SPTHeads (both words in one dword) = [bp + 0x18]
	pop		cx										; ECX = sectors per track
	div		ecx										; Residue is in EDX
	inc		dx										; Sector number in DL
	pop		cx										; ECX = number of heads
	push	dx										; push sector number into stack
	xor		dx, dx									; EDX:EAX = cylinder * TotalHeads + head
	div		ecx										; Residue is in EDX, head number
;		quotient is in EAX, cylinder number
	xchg	dh, dl									; Head number should be in DH - NASM: becomes 86 F2 which is identical and for 'xchg dl, dh', no differnce in operation
;		DL = 0
	pop		cx										; pop sector number from stack
	xchg	ch, al									; Lo 8bit cylinder should be in CH - NASM: becomes 86 E8 which is identical and for 'xchg ah, cl', no differnce in operation
;		AL = 0
	shl		ah, 6									; Hi 2 bits = cylinder ...
	or		cl, ah									; ... should be in CL
Extensions:											;
    mov     ax, [bp+Int13AlPtrOffset]				;
	mov		si, sp									; DS:SI points to disk address packet
	mov		dl, byte [DriveNum]						; Hard disk drive number. DriveNum = [bp+0x24]
	push	ds										; 2 bytes pushed
	push	es										; 2 bytes pushed
	int		0x13									;
	pop		bx										; 2 bytes popped
	pop		ds										; 2 bytes popped
	jb		DiskError								; Disk read error, jc 1f if caller handles
; NEW Code for printing Dot
	mov		ax, 0x0e2e								; Combine 'mov ah, 0xE' and 'mov al, "."'
	int		0x10									
; END of new Code for printing Dot
	lea		bx, [bx + 0x20]							;
	mov		es, bx									;
	popa											; Remove parameter block from stack. 16 bytes popped [ORG: popaw]
	popad											; 32 bytes popped [ORG: popal]
	inc		eax										; Next sector
	loop	DiskRead								;
	ret												;
; -------------------------
; Data 2
; -------------------------
CommonErr	db	0x0d, 0x0a, "Replace disk, press key.."	;
NewLines    db  0x0d, 0x0a, 0xa, 0					;
times 494 - ($ - $$) db 0							;
			dw	CommonErrorMsgOffset				;
;*** SUB-ROUTINE *********************************************************************
Print:												; Should be 12 bytes now: 11 bytes better code + 1 extra byte for 'ret' ( = C3 )
; --- prints string DS:SI (modifies AX BX SI) ---
	lodsb											; AC    (1) Get token. Load byte from DS:SI into AL. Implicit Registers: DS:SI (source), AL (destination)
	test	al, al									; 64 C0 (2)
	jz		EndPrint								; 74 02 (2) ???
	mov		ah, 0xe									; B4 OE (2) Print it
	int		0x10									; Cd 10 (2) Via TTY mode 
	jmp		Print									; EB F4 (2) Until done
EndPrint:
	ret												; C3    (1)
times 508 - ($ - $$) db 0							;
; Magic Bytes: Win9x uses all 4 bytes as magic value here [Says: Tinybit ??]
		db	0
		db	0
		db	0x55
		db	0xAA
