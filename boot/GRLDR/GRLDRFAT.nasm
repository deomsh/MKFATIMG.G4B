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
; GRLDRFAT.nasm, Originally: GRLDRSTART.S (fat12_16 dbr)
; v.8 (20251214), prepared for NASM by deomsh (some changes regarding placement and OTHER choice of detectection FAT12/FAT16)
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
%define FatStart	bp+0x3e
;ORG;%define FatStart	bp+0x28
%define RootStart	bp+0x42
;ORG;%define RootStart	bp+0x2c
%define DataStart	bp+0x46
;ORG;%define DataStart	bp+0x30
%define LoadAddress	bp+0x1dc			; Fixed just before Strings and with 4 bytes lower 'times ...'
;ORG;%define LoadAddress	bp+0x18d
;%define 	bp+
;
Entrypoint:
	jmp		Start						; 0x3e
	nop
; OEM
	OEM		db	'IBM  2.0'				; Or: ???
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
			db	0xf0					; MediaByte Offset 15h
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
	Uuid	dd	0x190c1c01				; Boot_Serial Offset 27h - unused here
	Label	db	'NO NAME    '			; Boot_Vol_Label Offset 0x2B - unused here
;			db	0						; Padding Zero - needed here
;			dd	0						; FatStart Offset 28h = VAR1 - overwrites EBPB
;			dd	0						; RootStart Offset 2Ch = VAR2 - overwrites EBPB
;			dd	0						; DataStart Offset 30h = VAR3 - overwrites EBPB
;			dw	0						; Padding Zero's
	FileSys	db	'FAT12/16'				; Boot_System_id Offset 36h
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
	mov		byte [0x7d71], 0x42			; Overwrites '20' in 0x201='00 20' = [0x7d71] [Watch to shift if boot code changes]
;ORG;	mov		byte [0x7d72], 0x42			; Overwrites '20' in 0x201='00 20' = [0x7d72] [Watch to shift if boot code changes]
;
; --- GET DRIVE PARMS: Calculate start of some disk areas ---
FindRoot:								; 0x6e
	movzx	ebx, word [ReservSec]		; ReservSec = [bp + 0xe]
	add		ebx, dword [HiddSec]		; HiddSec = [bp + 0x1c]
	mov		dword [FatStart], ebx		; VAR1 = FatStart = [bp + 0x28]
	movzx	eax, byte [NumFATs]			; NumFATs = [bp + 0x10]
	movzx	ecx, word [FATsz16]			; FATsz16 = [bp + 0x16]
	mul		ecx							; EDX=0, EAX=total sectors for FAT. Multiply EAX by src (unsigned). Implicit Registers: EAX (destination), EDX (high dword of result)??
	add		ebx, eax					; 
	mov		dword [RootStart], ebx		; Write LBA of Root Directory to VAR2= RootStart => Write Root Directory LBA from ebx = [bp + 0x2c]
	mov		ax, word [RootEntries]	    	; RootEntries = [bp + 0x11]
; --- Calculate how many sectors the root directory occupies ---
	add		ax, 0xf						; Used to round up
	mov		cx, 0x10					; 16 Directory Entries per Sector
	div		cx							; Get number of RootDirSectors: AX = sectors per root directory
	cwde								; 
	mov		cx, ax						; 
	add		ebx, eax					; LBA of DataStart
	mov		dword [DataStart], ebx		; Write LBA of DataStart is VAR3 = [bp + 0x30]
; --- First, read the whole root directory into the temporary buffer ---
	mov		eax, dword [RootStart]		; VAR2 = RootStart = [bp + 0x2c]
	les		bx, [LoadAddress]			; ES:BX = 0x2000:0000 Target in memory: Values are '00 00 00 20' = [bp + 0x18d] = LoadAddress
; --- cs=ss=ds=0 es=0x2000 ---
	call	DiskRead					; =0, eax,ES changed *
	les		di, [LoadAddress]			; ES:DI = 0x2000:0000 Target in memory: Values are '00 00 00 20' = [bp + 0x18d] = LoadAddress
;
; --- Search for kernel file name, and find start cluster ---
SearchFileLoop:							; BX=0, CX=0
	mov		si, GRLDR					; Load FileName in SI
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
	mov		si, NOGRLDR					; Load msg in SI
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
	mov		eax, dword [FatStart]		; VAR1 = FatStart = [bp + 0x28]
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
	mov		di, FATBUF					; 0x2000
;
ClusterLoop:
	stosw      							; Store cluster number: Purpose: Store word from AX into ES:DI. Implicit Registers: ES:DI (destination), AX (source).
	mov		si, ax						; SI = Cluster Number
; --- FAT16 can occupy 128KB, so the segment must be adjusted ---
	pop		dx							; Segment for FAT16 = 0x2000
	push	dx
	add		si, si						; Double Cluster Number => Multiply by two
	jae		FatRead						;
	add		dh, 0x10					; Overflow: add 0x1000 to Segment value (or 0x10 to high bits)
;
FatRead:
; --- CHANGED by deomsh ! ---
	cmp		word [FATsz16], 12			; FATsz16 = [bp + 0x16] ; No FAT12 assumed above 12 SecPerFAT
;ORG;	cmp		word [TotSec16], 0			; TotSec16 = [bp + 0x13] ; No FAT16 assumed below 32MB !!!
	jnbe	IsFat16						; Working!
;ORG;	je		IsFat16						;
; --- FAT12 ---	; Assumed only if below 32MB!
	add		si, ax						; Now Cluster Number added again: multiplied by three
	shr		si, 1						; One byte shorter in NASM, '1' is implicit: 'D1 EE' instead of 'C1 EE 01' If used offsets of AH=42 must be changed
;ORG;	shr		si, byte 1					; Shift bits one right: Cluster Number divided by two. Check if cluster number is even (bits 0-11 of AX)
	lodsw								; Load word from DS:SI into AX. Implicit Registers: DS:SI (source), AX (destination). Check if cluster number is even (bits 0-11 of AX)
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
IsFat16:								;
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
	add		eax, dword [DataStart]		; VAR3 = DataStart => add to get Sector LBA = [bp + 0x30]
	call	DiskRead					;
	jmp		ReadClusterLoop				;
EndCluster:								; EOC encountered - done
	mov		dx, word [DriveNum]			; DriveNum, possibbly Partition number from 0x25, but DX is written from BIOS earlier to [bp+0x25]!
	push	dx							; Needed on Stack: simple handshake with GRLDR
;NEW?;	jmp		LOADSEG:LOADOFF				; Hack from Copilot. LoadAddress = [bp+018d] - consisting of LOADOFF 0000 and LOADSEG 2000 (Little Endian: 00 00 00 20]
	jmp		far [LoadAddress]			; Hack from Copilot. LoadAddress = [bp+018d] - consisting of LOADOFF 0000 and LOADSEG 2000 (Little Endian: 00 00 00 20]
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
;0x000000000000013d:  66 60             pushal     
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
	cmp		byte [0x7d71], 0x42			; Check if EBIOS is active: [0x7d71] => Double Check if not changed with new code
;ORG;	cmp		byte [0x7d72], 0x42			; Check if EBIOS is active: [0x7d72] => Double Check if not changed with new code
	je		Extensions					; Jump if EBIOS is active (INT13 AH=42)
;
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
	mov		ax, 0x201					; Read 1 sector
	mov		si, sp						; DS:SI points to disk address packet
	mov		dl, byte [DriveNum]			; Hard disk drive number. DriveNum = [bp+0x24]
	push	ds							; 2 bytes pushed
	push	es							;2 bytes pushed
	int		0x13						;
	pop		bx							; 2 bytes popped
	pop		ds							; 2 bytes popped
	jb		DiskError					; Disk read error, jc 1f if caller handles
	lea		bx, [bx + 0x20]				;
	mov		es, bx						;
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
DiskError:								;
	mov		si, DISKERR					; Load msg
;
PrintErrorLoop:							;
; --- prints string DS:SI (modifies AX BX SI) ---
;
	lodsb								; Get token. Load byte from DS:SI into AL. Implicit Registers: DS:SI (source), AL (destination)
	mov		ah, 0xe						; Print it
	int		0x10						; Via TTY mode 
	cmp		al, 0						; End of string? */
	jne		PrintErrorLoop				; Until done
Hang:									;
	jmp		Hang						;
;
; Padding Zero's
times 476 - ($ - $$) db 0
;ORG;times 480 - ($ - $$) db 0
;
LoadOff		dw	LOADOFF					; Offset 0000  Offset 18Dh => [bp + 0x18d] = LoadAddress as dword
LoadSeg		dw	LOADSEG					; Segment 2000
; -------------------------
; Strings
; -------------------------
;
	NOGRLDR		db	"No "					;
	GRLDR		db	"GRLDR      ", 0		;
	DISKERR		db	"Disk error", 0			;
;
; 2 zeros extra in Original
;
times 508 - ($ - $$) db 0
;
;                db  0
;                db  0
; Magic Bytes: Win9x uses all 4 bytes as magic value here [Says: Tinybit ??]
				db	0
				db	0
				db	0x55
				db	0xAA
