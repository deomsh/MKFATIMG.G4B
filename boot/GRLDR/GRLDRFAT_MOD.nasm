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
; GRLDRFAT.nasm, Originally: GRLDRSTART.S (Part FAT12/16)
; Disassembly from: https://shell-storm.org/online/Online-Assembler-and-Disassembler/
; v.9.14 (20260401), prepared for NASM by deomsh
; CHANGES: FAT12/ FAT16 identified with Total Number of Clusters, writing Dot after each sector Read, extra INT 0x10 for stability and AH-error written after ' Error  ' as ASCII converted to Hex, and Key2reboot added
;
bits		16							; Compiles okay with 'bits 16'
org			0x7C00
ORIGIN		equ		0x7C00				; BP stays at 0x7C00 - no relocation here
LOADSEG		equ		0x2000				; Memory 0x20000 ?? Seems to be used for temporary loading Root Directory too
LOADOFF		equ		0x0000				; Offset
ROOTSEG		equ		0x2000				; Memory 0x20000 ?? Seems to be used for temporary loading Root Directory too
ROOTOFF		equ		0x0000				; Offset
FATBUF		equ		0x2000				; Offset of temporary buffer for FAT chain
;
%define ByteSec 	bp+0xb
%define SecClus 	bp+0xd
%define ReservSec	bp+0xe
%define NumFATs		bp+0x10
%define RootEntries	bp+0x11
%define TotSec16	bp+0x13
%define MediaByte	bp+0x15
%define FATsz16		bp+0x16
%define SecTrack	bp+0x18
%define NumHeads	bp+0x1a
%define HiddSec		bp+0x1c
%define TotSec32	bp+0x20
%define DriveNum	bp+0x24
%define ReservNT	bp+0x25
%define BootSig		bp+0x26
;
FatStartOffset			equ	Start - $$ + 0			; Start - 0x7C00 => now at 0x
RootStartOffset			equ	Start - $$ + 4			; Start - 0x7C00 => now at 0x
DataStartOffset			equ	Start - $$ + 8			; Start - 0x7C00 => now at 0x
;
;MOD2;%define FatStart	bp+0x3e
;ORG;%define FatStart	bp+0x28
;MOD2;%define RootStart	bp+0x42
;ORG;%define RootStart	bp+0x2c
;MOD2;%define DataStart	bp+0x46
;ORG;%define DataStart	bp+0x30
;Mod1;%define LoadAddress	bp+0x1cd			; Fixed just before Strings and with 4 bytes lower 'times ...'
;%define LoadAddress	bp+0x1d2			; Fixed just before Strings and with 4 bytes lower 'times ...'
;ORG;%define LoadAddress	bp+0x18d
;%define 	bp+
;
LoadAddressOffset		equ LoadOff - $$    		; LoaddOff - 0x7C00 => now at 0x
Int13AlPtrOffset		equ Int13Al - $$			; = Int13Al - 0x7C00
Int13AhPtrOffset		equ Int13Ah - $$			; = Int13Ah - 0x7C00
;v9.12;FileNameByteOffset		equ FileName + 11 - $$			; = FileName + 11 - 0x7C00
;FileNameOffset			equ FileName - $$			; = FileName - 0x7C00
;BADTOO;FileNamePtrOffset		equ Bio - $$		; = FileNamePtr - 0x7C00
;OKE;
;FileNamePtrOffset		equ FileNamePtr - $$		; = FileNamePtr - 0x7C00
;FileErrorMsgOffset		equ FileErr  - $$       	; = FileErr - 0x7C00
;
DiskErrMsgOffset		equ DiskErr - $$       	; = DiskErr + 6 - 0x7C00
NoFileByteOffset		equ NoFile + 10 - $$			; = NoFile + 10 - 0x7C00
;
Entrypoint:
	jmp		DataArea					; 0x3e
;v9.13;	jmp		Start						; 0x3e
	nop
; OEM: 
	OEM		db	'IBM  2.0'				; Or: ???
; BPB
; Values in BPB for Standard 1440KB Floppy-disk
			dw	512						; ByteSec Offset 0Bh =	512, Required
;			dw	0						; ByteSec Offset 0Bh =	512, Required
			db	1						; SecClus Offset 0Dh
;			db	0						; SecClus Offset 0Dh
			dw	1						; ReservSec Offset 0Eh
;			dw	0						; ReservSec Offset 0Eh
			db	2						; NumFATs Offset 10h
;			db	0						; NumFATs Offset 10h
			dw	224						; RootEntries Offset 11h
;			dw	0						; RootEntries Offset 11h
			dw	2880					; TotSec16 Offset 13h
;			dw	0						; TotSec16 Offset 13h
			db	0xf0						; MediaByte Offset 15h
;			db	0						; MediaByte Offset 15h
			dw	9						; FATsz16 Offset 16h
;			dw	0						; FATsz16 Offset 16h
			dw	18						; SecTrack Offset 18h
;			dw	0						; SecTrack Offset 18h
			dw	2						; NumHeads Offset 1Ah
;			dw	0						; NumHeads Offset 1Ah
			dd	0						; HiddSec Offset 1Ch
;			dd	0						; HiddSec Offset 1Ch
			dd	0						; TotSec32 Offset 20h ; needs dd
;			dd	0						; TotSec32 Offset 20h ; needs dd
			db	0						; DriveNum Offset 24h
;			db	0						; DriveNum Offset 24h
			db 0						; ReservNT Offset 25h
;			db 0						; ReservNT Offset 25h
			db	0x29					; BootSig Offset 26h
;			db	0						; BootSig Offset 26h
; UNUSED !!
;ORG;%define RootDirSecs     bp+0x26		; # of sectors root dir uses
	Uuid	dd	0x190c1c01				; Boot_Serial Offset 27h - unused here
	Label	db	'NO NAME    '			; Boot_Vol_Label Offset 0x2B - unused here
;			db	0						; Padding Zero - needed here
;			dd	0						; FatStart Offset 28h = VAR1 - overwrites EBPB
;			dd	0						; RootStart Offset 2Ch = VAR2 - overwrites EBPB
;			dd	0						; DataStart Offset 30h = VAR3 - overwrites EBPB
;			dw	0						; Padding Zero's
FileSys		db	'FAT12/16'				; Boot_System_id Offset 36h
; --- Start of Moved Data-Area ---
DataArea:
			jmp Start					; second jump to maintain initial jump 'EB 3C 90' to ' trick' some USB-drivers: test with XUSBSUPP on Windows 95 OSR2x
; Fixed space of overwriting 'mov ax, 0x201' if AH=42 exists. Thanks to bp-ptr code same number of bytes as Tyokok's
Int13Al	    db	1						; 
Int13Ah	    db	2						; 
;
; -------------------------
; Strings
; -------------------------
;v9.13:
DiskErr db "AH=   "						;
NoFile db "Key2reboot "				;
;
		db	"No "					;
;v9.12;NoFile		db	"No "					;
;FileNamePtr	dw	Bio									; initial: first filename
;BAD!;FileNamePtr	db	"GRLDR      ", 0		;
FileName	db	"GRLDR      "		; zero ending not needed because of zero's on LOADOFF = next!
;v9.12;FileName	db	"GRLDR      ", 0		;
;v9.12;DiskErr		db	"Error    Key2reboot"	; +19 is zero. Using space of LoadOff
; -------------------------
; End of Strings
; -------------------------
; DO NOT MOVE NEXT TWO LINES !!! First space of LoadOff is used as terminator for DiskErr message
LoadOff		dw	LOADOFF					; Offset 0000  Offset h => [bp + 0x] = LoadAddress as dword
LoadSeg		dw	LOADSEG					; Segment 2000
; -------------------------
;
; Start of Boot Code
Start:
	cli									;
	cld									;
	xor		ax, ax						;
	mov		bp, ORIGIN					; ORIGIN = 0x7C00
	mov		sp, bp						;
	mov		ss, ax						;
	mov		ds, ax						;
	mov		es, ax						;
	sti									;
; --- cs=ss=ds=es=0, bp=0x7C00 ---
	mov		word [DriveNum], dx			; Write DX to DriveNum (DH will be '00' => ReservNT = 0) = [bp + 0x24]
; --- Check BIOS INT13h Extensions ---
	mov		ah, 0x41					;
	mov		bx, 0x55aa					;
	push	ds							;
	push	es							;
	int		0x13						;
	pop		es							;
	pop		ds							;
	jb		FindRoot					;  No EBIOS
	cmp		bx, 0xaa55
	jne		FindRoot					;  No EBIOS
	ror		cl, byte 1
	jae		FindRoot					; No EBIOS
; --- EBIOS supported ---
	mov		byte [bp+Int13AhPtrOffset], 0x42	; Overwrites '20' in 0x201='01 02' = [bp+Int13AhPtrOffset] [Watch to shift if boot code changes]
;Mod1;	mov		byte [0x7d71], 0x42			; Overwrites '20' in 0x201='01 02' = [0x7d71] [Watch to shift if boot code changes]
;ORG;	mov		byte [0x7d72], 0x42			; Overwrites '20' in 0x201='01 02' = [0x7d72] [Watch to shift if boot code changes]
;
; --- GET DRIVE PARMS: Calculate start of some disk areas ---
FindRoot:								; 0x6e
	movzx	ebx, word [ReservSec]		; ReservSec = [bp + 0xe]
	add		ebx, dword [HiddSec]		; HiddSec = [bp + 0x1c]
	mov		dword [bp+FatStartOffset], ebx		; VAR1 = was FatStart = [bp+FatStartOffset]
	movzx	eax, byte [NumFATs]			; NumFATs = [bp + 0x10]
	movzx	ecx, word [FATsz16]			; FATsz16 = [bp + 0x16]
	mul		ecx							; EDX=0, EAX=total sectors for FAT. Multiply EAX by src (unsigned). Implicit Registers: EAX (destination), EDX (high dword of result)??
	add		ebx, eax					; 
	mov		dword [bp+RootStartOffset], ebx		; Write LBA of Root Directory to VAR2 = was RootStart => Write Root Directory LBA from ebx = [bp+RootStartOffset]
	mov		ax, word [RootEntries]	    	; RootEntries = [bp + 0x11]
; --- Calculate how many sectors the root directory occupies ---
	add		ax, 0xf						; Used to round up
	mov		cx, 0x10					; 16 Directory Entries per Sector
	div		cx							; Get number of RootDirSectors: AX = sectors per root directory
	cwde								; 
	mov		cx, ax						; 
	add		ebx, eax					; LBA of DataStart
	mov		dword [bp+DataStartOffset], ebx		; Write LBA of DataStart is VAR3 = [bp+DataStartOffset]
; --- First, read the whole root directory into the temporary buffer ---
	mov		eax, dword [bp+RootStartOffset]		; VAR2 = was RootStart = [bp+RootStartOffset]
	les		bx, [bp+LoadAddressOffset]	; Target in memory: Values are '00 00 00 20' = [bp + 0x] = LoadAddress
; --- cs=ss=ds=0 es=0x2000 ---
	call	DiskRead					; =0, eax,ES changed *
	les		di, [bp+LoadAddressOffset]	; Target in memory: Values are '00 00 00 20' = [bp + 0x] = LoadAddress
;
; --- Search for kernel file name, and find start cluster ---
SearchFileLoop:							; BX=0, CX=0
	mov		si, FileName	; Load FileName in SI
;ORG;	mov		si, GRLDR					; Load FileName in SI
	mov		cl, 0xb						; Length of kernel filename
	push	di							;
	repe								; Repeat string operation until CX = 0 or ZF = 1. Implicit Registers: Uses CX (counter) and ZF (Zero Flag)
	cmpsb								; Compare byte from DS:SI with byte from ES:DI. Implicit Registers: Uses DS:SI (source) and ES:DI (destination)
	pop		di							;
	je		CopyFat						;
	add		di, 0x20					; Next Directory Entry
	je		NotFound					;  OR jc 2f, exceeding 64K
	cmp		byte es:[di], ch			; CH=0
	jne		SearchFileLoop				; 
;
NotFound:								; 0xc9
; TEST: Same binary !! as with +11
;v9.12;	mov 	[bp+FileNameByteOffset], byte 0x20
;	mov 	[bp+FileNameOffset+11], byte 0x20
;OKE;	mov 	[FileName+11], byte 0x20
;ORG;        mov [GRLDR+11], byte 0x20
;        mov [GRLDR+22], byte 0x00
	mov		si, NoFile					; Load msg in SI
;ORG;	mov		si, NOGRLDR					; Load msg in SI
	jmp		PrintErrorLoop				;
;
CopyFat:								; BX=0, CX=0
;	#####################################################################
;	# Reads the FAT chain and stores it in a temporary buffer in the
;	# first 64KB.  The FAT chain is stored an array of 16-bit cluster
;	# numbers, ending with 0
;	#
;	# The file must fit in conventional memory, so it can't be larger
;	# than 640KB. The sector size must be at least 512 bytes, so the
;	# FAT chain can't be larger than around 3KB
;	#####################################################################
;
;	*********************************************************************
;	* First, load the complete FAT into memory. The FAT can't be       
;	* larger than 128KB, so it should fit in the temporary buffer     
;	*********************************************************************
;
	push	word es:[di + 0x1a]			; Save first cluster number of file. Offset 1Ah is FAT12/16 cluster number in Directory entry
	mov		cx, word [FATsz16]			; FATsz16 = [bp + 0x16]
	mov		eax, dword [bp+FatStartOffset]		; VAR1 = was FatStart = [bp+FatStartOffset]
	push	es							; ES=0x2000
	call	DiskRead					; CX=0, eax,ES changed
	pop		ds							; DS=0x2000 */
; --- cs=ss=0 ds=0x2000 es=x ---
	pop		ax							; Restore first cluster number of file
;	********************************************************************
;	* Then, extract the clusters of the file from the FAT              *
;	********************************************************************
;
; --- Set ES:DI to the temporary storage for the FAT chain ---
;
	push	ds							; ES=0x2000
	push	ss							;
	pop		es							;
; --- cs=es=ss=0 ds=0x2000 ---
; TEST: COMPILING is not cheaper
;	mov		di, [bp+LoadAddressOffset+2]	; 0x2000
;ORG;
	mov		di, FATBUF					; 0x2000
;
ClusterLoop:
	stosw      							; Store cluster number: Purpose: Store word from AX into ES:DI. Implicit Registers: ES:DI (destination), AX (source).
	mov		si, ax						;
; --- FAT16 can occupy 128KB, so the segment must be adjusted ---
	pop		dx							;
	push	dx
	add		si, si						; Double Cluster Number => Multiply by two
	jae		FatRead						;
	add		dh, 0x10					; Overflow: add 0x1000 to Segment value (or 0x10 to high bits)
;
FatRead:
; NEW: routine to caculate Number of Clusters to choose FAT12/ FAT16 [total 24 Bytes]
        pusha                                                   ; Save 16-bits registers already in use
        mov             ax, [TotSec16]                          ; TotSec16 is a word and is zero if TotSec32 is not
        add             ax, [TotSec32]                          ; Totsec32 is zero if TotSec16 is not, so sum is always 16-bits
        mov             dx, [TotSec32+2]                        ; If zero, dx will be just zero'd
        sub             ax, [bp+DataStartOffset]                ; Substract Low word of DataStart form Low word of Total Sectors
        sbb             dx, [bp+DataStartOffset+2]              ; Substract with Carry High word of DataStart form High word Total Sectors
        movzx           cx, byte [SecClus]                      ; Copy SecClus to CL and zero CH [4 Bytes]: 1 Byte cheaper
;        xor             cx, cx                                  ; Zero cx - 16 bits diviser needed [2 Bytes]
;        mov             cl, [SecClus]                           ; SecClus is one byte only [3 Bytes]
        div             cx                                      ; divide DX:AX by CX to get Number of Clusters in AX, reminders vanishes in Total Number of Clusters
        cmp             ax, 4084                                ; result AX is always 16 bit on FAT16! MS FATSPEC: < 4085 Clusters is FAT12
        popa
; END of NEW
;MOD1;	cmp		word [FATsz16], 12			; FATsz16 = [bp + 0x16] ; No FAT12 assumed above 12 SecPerFAT
;ORG;	cmp		word [TotSec16], 0			; TotSec16 = [bp + 0x13] ; No FAT16 assumed below 32MB !!!
	jnbe	        IsFat16						; Not Below Or Equal 8084 Clusters is fAT16
;	je		IsFat16						;
; --- FAT12 ---	; Assumed only if below 32MB!
; NEW: load 16-bits registers saved earlier
;EARLIER;        popa
; END of NEW
	add		si, ax						; Now Cluster Number added again: multiplied by three
	shr		si, 1						; One byte shorter in NASM, '1' is implicit: 'D1 EE' instead of 'C1 EE 01' If used offsets of AH=42 must be changed
;ORG;	shr		si, byte 1					; Shift bits one right: Cluster Number divided by two. Check if cluster number is even (bits 0-11 of AX)
	lodsw								; Load word from DS:SI into AX. Implicit Registers: DS:SI (source), AX (destination).
;	If the cluster number was even, the cluster value is now in
;	bits 0-11 of AX. If the cluster number was odd, the cluster
;	value is in bits 4-15, and must be shifted right 4 bits. If
;	the number was odd, CF was set in the last shift instruction
;
	jae		IsEven						;
	shr		ax, 4						; If odd in bits 4-15, must be shifter 4 bits ti the right
IsEven:									;
	and		ah, 0xf						; Mask highest 4 bits
	cmp		ax, 0xff7					; Check for EOF
	jmp		IfEof						;
;
; --- FAT16 ---
IsFat16:
; NEW: load 16-bits registers saved earlier
;EARLIER;        popa
; END of NEW
	mov		ds, dx						; DS:SI points to next cluster
	lodsw      							; AX = next cluster. Load word from DS:SI into AX. Implicit Registers: DS:SI (source), AX (destination)
	cmp		ax, 0xfff7					; Check for EOF
IfEof:
	jbe		ClusterLoop					; Continue if not EOF
;	Mark end of FAT chain with 0, so we have a single EOF marker for both FAT12 and FAT16 systems
;
	xor		ax, ax						;
	stosw								; Store byte from AX into ES:DI. Implicit Registers: ES:DI (destination), AX (source)
;
;****************************************************************************
;
;	Load the file into memory, one cluster at a time
;	 cs=es=ss=0 ds=0x2000
;
	pop		es							; ES=0x2000
	push	ss							;
	pop		ds							; DS=SS
; TEST: compiling is not cheaper
;	mov		si, [bp+LoadAddressOffset+2]; Target in Memory. Set DS:SI to the FAT chain
	mov		si, FATBUF					; Target in Memory. Set DS:SI to the FAT chain
;
ReadClusterLoop:						; 0x11c
;	* CH=0 *
;
	lodsw								; AX = next cluster to read. Load word from DS:SI into AX. Implicit Registers: DS:SI (source), AX (destination)
	sub		ax, 0x2						; Decrease Cluster Number by two. Implicit Registers: Updates flags (ZF, CF, etc.)
	jb		EndCluster					; Probably CF=1 set by sub: if AX=0 (EOC) gives -2 ??
	movzx	eax, ax						;
	mov		cl, byte [SecClus]			; SecClus = [bp + 0xd]
	mul		ecx							; Get Sector offset of Cluster. Multiply EAX by src (unsigned). Implicit Registers: EAX (destination), EDX (high dword of result)??
	add		eax, dword [bp+DataStartOffset]	; VAR3 = was DataStart => add to get Sector LBA = [bp+DataStartOffset]
	call	DiskRead					;
	jmp		ReadClusterLoop				;
EndCluster:								; EOC encountered - done
	mov		dx, word [DriveNum]			; DriveNum, possibbly Partition number from 0x25, but DX is written from BIOS earlier to [bp+0x25]!
	push	dx							; Needed on Stack: simple handshake with GRLDR
;NEW?;	jmp		LOADSEG:LOADOFF				; Hack from Copilot. LoadAddress = [bp+] - consisting of LOADOFF 0000 and LOADSEG 2000 (Little Endian: 00 00 00 20]
	jmp		far [bp+LoadAddressOffset]			; Hack from Copilot. LoadAddress = [bp+] - consisting of LOADOFF 0000 and LOADSEG 2000 (Little Endian: 00 00 00 20]
;DiskError;	jmp		far dword [bp+0x18d] ; ljmp not valid in NASM ?? BECOMES FF A6 8D 01 => So jmp far [bp+018d] will load: IP: 0x0000 with CS: 0x2000
;	ljmp		[bp+0x18d]				; ljmp not valid in NASM !! BECOMES FF A6 8D 01
;	jmp		dword [bp+0x18d]
;NoDiff;    jmp near word [bp+0x18d]                 ; Trick from Copilot
;NoDiff;    jmp word [bp+0x18d]
;BADforBoot;	jmp		dword [bp+0x18d]					; ljmp not valid in NASM ?? BECOMES FF A6 8D 01 => So jmp far [bp+018d] will load: IP: 0x0000 with CS: 0x2000
;
;****************************************************************************
DiskRead:								;
;
; Read a number of sectors into memory
;
; Call with:	DS=SS=0
;		eax = 32-bit DOS sector number
;		CX = number of sectors to read
;		ES:BX = destination buffer
;
; Returns:	CX=0
;		ES increased, BX untouched
;		ES:BX points one byte after the last byte read
;		eax = next sector number after read
;		All other registers preserved
;
; --- Total pushes before int 0x13 = 52 bytes, same count of total pop's afterwards ---
	pushad								; Save regs: 32 bytes pushed
; --- Build the Disk Address Packet (DAP) on the stack ---
; --- 16 bytes: 10h = Packet size, 0 = Reserved, Sector count, Buffer offset, Buffer segment, LBA low word, LBA high word) ---
; --- AH = 0x4201 and Vector DI:SI = 0000:SI => SI is set to SP after building DAP on the stack (if jumped to Extensions)---
	xor		edx, edx					;
	push	edx							; Hi 32bit of sector number. 4 bytes pushed
	push	eax							; Lo 32bit of sector number. 4 bytes pushed
	push	es							; Buffer segment. 2 bytes pushed
	push	bx							; Buffer offset. 2 bytes pushed
	push	0x1							; 1 sector to read. 2 bytes pushed
	push	0x10						; Size of this parameter block. 2 bytes pushed
	cmp		byte [bp+Int13AhPtrOffset], 0x42	; Check if EBIOS is active: [bp+Int13AhPtrOffset] => Double Check if not changed with new code
;Mod1;	cmp		byte [0x7d71], 0x42			; Check if EBIOS is active: [0x7d71] => Double Check if not changed with new code
;ORG;	cmp		byte [0x7d72], 0x42			; Check if EBIOS is active: [0x7d72] => Double Check if not changed with new code
	je		Extensions					; Jump if EBIOS is active (INT13 AH=42)
; --- Old BIOS, AH=02 only ---
	xor		ecx, ecx					;
	push	dword [SecTrack]			; Lo:sectors per track, Hi:number of heads. SecTrack HERE SPTHeads (both words in one dword) = [bp + 0x18]
	pop		cx							; ECX = sectors per track
	div		ecx							; Residue is in EDX
	inc		dx							; Sector number in DL
	pop		cx							; ECX = number of heads
	push	dx							; push sector number into stack
	xor		dx, dx						; EDX:EAX = cylinder * TotalHeads + head
	div		ecx							; Residue is in EDX, head number
;				quotient is in EAX, cylinder number
	xchg	dh, dl						; Head number should be in DH - NASM: becomes 86 F2 which is identical and for 'xchg dl, dh', no differnce in operation
;				DL = 0
	pop		cx							; pop sector number from stack
	xchg	ch, al						; Lo 8bit cylinder should be in CH - NASM: becomes 86 E8 which is identical and for 'xchg ah, cl', no differnce in operation
;				AL = 0
	shl		ah, 6						; Hi 2bit cylinder ...
	or		cl, ah						; ... should be in CL
Extensions:								;
	mov		ax, [bp+Int13AlPtrOffset]	; Read 1 sector, AH=02h OR set to AH=42h if available
	mov		si, sp						; DS:SI points to disk address packet
	mov		dl, byte [DriveNum]			; Hard disk drive number. DriveNum = [bp+0x24]
	push	ds							; 2 bytes pushed
	push	es							;2 bytes pushed
; NEW extra cleaning CF (idea of MS-Copilot)
;v9.13;
	clc
; END of extra cleaning CF (idea of MS-Copilot)
	int		0x13						;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Extended Read Function
    ;;
    ;; Call with AH=42h
    ;; DL = drive number
    ;; DS:SI = Disk Address Packet (DAP)
    ;;       Offset Size Description
    ;;       00h    db   Size of packet (10h or 18h; we're using 10h here, see below)
    ;;       01h    db   Reserved (0)
    ;;       02h    dw   Number of sectors to read (max 007Fh for Phoenix EDD)
    ;;       04h    dd   Initial segment:offset where to load the read in sector(S)
    ;;       08h    dq   starting absolute sector number (for non-LBA devices, compute as (Cylinder*NumHeads + SelectedHead) * SectorPerTrack + SelectedSector - 1
    ;;       10h    dq   (EDD-3.0, optional) 64-bit flat address of transfer buffer; used if dd at 04h is FFFFh:FFFFh
    ;;
    ;; Return: If Function Successful,
    ;; Carry Flag = Clear
    ;; AH = 00h
    ;;
    ;; If Function Unsuccessful,
    ;; Carry Flag = Set
    ;; AH = Error code
    ;;
    ;; Error code:
    ;;   00h    successful completion
    ;;   01h    invalid function in AH or invalid parameter
    ;;   02h    address mark not found
    ;;   03h    disk write-protected
    ;;   04h    sector not found/read error
    ;;   05h    reset failed (hard disk)
    ;;   05h    data did not verify correctly (TI Professional PC)
    ;;   06h    disk changed (floppy)
    ;;   07h    drive parameter activity failed (hard disk)
    ;;   08h    DMA overrun
    ;;   09h    data boundary error (attempted DMA across 64K boundary or >80h sectors)
    ;;   0Ah    bad sector detected (hard disk)
    ;;   0Bh    bad track detected (hard disk)
    ;;   0Ch    unsupported track or invalid media
    ;;   0Dh    invalid number of sectors on format (PS/2 hard disk)
    ;;   0Eh    control data address mark detected (hard disk)
    ;;   0Fh    DMA arbitration level out of range (hard disk)
    ;;   10h    uncorrectable CRC or ECC error on read
    ;;   11h    data ECC corrected (hard disk)
    ;;   20h    controller failure
    ;;   31h    no media in drive (IBM/MS INT 13 extensions)
    ;;   32h    incorrect drive type stored in CMOS (Compaq)
    ;;   40h    seek failed
    ;;   80h    timeout (not ready)
    ;;   AAh    drive not ready (hard disk)
    ;;   B0h    volume not locked in drive (INT 13 extensions)
    ;;   B1h    volume locked in drive (INT 13 extensions)
    ;;   B2h    volume not removable (INT 13 extensions)
    ;;   B3h    volume in use (INT 13 extensions)
    ;;   B4h    lock count exceeded (INT 13 extensions)
    ;;   B5h    valid eject request failed (INT 13 extensions)
    ;;   B6h    volume present but read protected (INT 13 extensions)
    ;;   BBh    undefined error (hard disk)
    ;;   CCh    write fault (hard disk)
    ;;   E0h    status register error (hard disk)
    ;;   FFh    sense operation failed (hard disk)
    ;;
	pop		bx							; 2 bytes popped
	pop		ds							; 2 bytes popped
	jb		DiskError					; Disk read error, jc 1f if caller handles
	lea		bx, [bx + 0x20]				;
	mov		es, bx						;
; NEW Code for 'printing' find cursor [no video output]
	mov ah, 0x03
	xor	bh, bh							; Page 0 (to be sure <= Copilot)
	int 0x10
; END of Code for 'printing' find cursor [no video output]
; NEW Code for printing Dot
	mov ax, 0x0e2e						; Combine 'mov ah, 0xE' and 'mov al, "."'
	int 0x10
; END of new Code for printing Dot
	popa								; Remove parameter block from stack. 16 bytes popped
	popad								; 32 bytes popped
	inc		eax							; Next sector
	loop	DiskRead					;
	ret									;
;
;ORG;LoadOff		dw	LOADOFF					; Offset 0000  Offset 18Dh => [bp + 0x18d] = LoadAddress as dword
;ORG;LoadSeg		dw	LOADSEG					; Segment 2000
; Address of target in memory
;	Target		dd	0x20000000			; Target in Memory
;
;******************************************************************************
; Moving PrintError-routine not better, one byte lost if directly after FileErr
DiskError:								;
;v9.12:
; Optimized solution that matches working register order:
    mov     al, ah          ; 2 bytes: Move input to AL
    and     al, 0Fh         ; 2 bytes: Mask Low Nibble
    add     al, 90h         ; 2 bytes: Convert Low Nibble
    daa                     ; 1 byte
    adc     al, 40h         ; 2 bytes
    daa                     ; 1 byte
    xchg    ah, al          ; 2 bytes: AH=Low ASCII, AL=Input (Restored)
    shr     al, 4           ; 3 bytes: Shift High Nibble down
    add     al, 90h         ; 2 bytes: Convert High Nibble
    daa                     ; 1 byte
    adc     al, 40h         ; 2 bytes
    daa                     ; 1 byte
    ; Total: 21 bytes
    ; Result: AH contains Low ASCII ('0'), AL contains High ASCII ('8')
; BAD First convert HEX in AH return code to ASCII
;    mov     al, ah          ; 2 bytes: Copy error code to AL
;    shr     al, 4           ; 3 bytes: Shift high nibble to low position
;    add     al, 90h         ; 2 bytes: Hex->ASCII conversion start
;    daa                     ; 1 byte: Decimal adjust
;    adc     al, 40h         ; 2 bytes: Add correction for A-F
;    daa                     ; 1 byte: Decimal adjust
;    xchg    ah, al          ; 2 bytes: Save High ASCII in AH, restore Code to AL
;    and     al, 0Fh         ; 2 bytes: Mask low nibble
;    add     al, 90h         ; 2 bytes: Hex->ASCII conversion start
;    daa                     ; 1 byte: Decimal adjust
;    adc     al, 40h         ; 2 bytes: Add correction for A-F
;    daa                     ; 1 byte: Decimal adjust
;v9.13:
	mov 	[bp+DiskErrMsgOffset+3], ax	; Overwrite offset 6 DiskErr with value of AX
	mov 	[bp+NoFileByteOffset], byte 0x0
;v9.12;	mov 	[bp+DiskErrMsgOffset+6], ax	; Overwrite offset 6 DiskErr with value of AX
; Better Error info: 80h=Timeout (Not ready), 01h=Invalid function (bad LBA regognition), 0Ch=Media type not found
;OKE;	mov 	[bp+DiskErrMsgOffset], ah	; Overwrite offset 6 DiskErr with value of AH
;AH1;	mov 	[DiskErr+16], ah			; Overwrite first space in DISKERR with value of AH
	mov		si, DiskErr					; Load msg
PrintErrorLoop:							;
; --- prints string DS:SI (modifies AX BX SI) ---
;
	lodsb								; Get token. Load byte from DS:SI into AL. Implicit Registers: DS:SI (source), AL (destination)
	mov		ah, 0xe						; Print it
	int		0x10						; Via TTY mode 
	cmp		al, 0						; End of string? */
	jne		PrintErrorLoop				; Until done
;Hang:									;
;	jmp		Hang						;
xor ah, ah
int 0x16
int 0x19
;******************************************************************************
;Mod1;times 0x1cd - ($ - $$) db 0
;Mod;times 457 - ($ - $$) db 0
;ORG;times 480 - ($ - $$) db 0
;
;Mod1;LoadOff		dw	LOADOFF					; Offset 0000  Offset 18Dh => [bp + 0x18d] = LoadAddress as dword
;Mod1;LoadSeg		dw	LOADSEG					; Segment 2000
; -------------------------
; Strings
; -------------------------
;
;ORG;	NOGRLDR		db	"No "					;
;ORG;	GRLDR		db	"GRLDR      ", 0		;
; Room for writing AH
;Mod2-AH;	DISKERR		 db	"Key to reboot.. ", 0, " Disk Error"	; +11 is last space, tested!
;Mod-AH;	DISKERR		db	"Disk Error  "	; +11 is last space, tested!
;ORG;	DISKERR		db	"Disk error", 0	;
; 2 zeros extra in Original
;
times 510 - ($ - $$) db 0
;ORG;times 508 - ($ - $$) db 0
; Magic Bytes: Win9x uses all 4 bytes as magic value here [Says: Tinybit ??]
;ORG;				db	0
;ORG;				db	0
				db	0x55
				db	0xAA
