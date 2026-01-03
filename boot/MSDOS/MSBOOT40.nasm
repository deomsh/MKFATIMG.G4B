; Modified by deomsh in 2025
    ; Original code from MS-DOS 4.0 (c) Microsoft/IBM
    ; Licensed under the MIT License
;
; MSBOOT40.nasm = original MASM: MSBOOT.ASM converted to NASM
; (20251124), by deomsh, with help of Gemini 2.5 PRO, DeepSeek v3 and Copilot (GPT-5)
;	Page 60,132 ;	    SCCSID = @(#)msboot.asm	    1.1 85/05/13
; TITLE BOOT	SECTOR 1 OF TRACK 0 - BOOT LOADER
;
;   Rev 1.0 ChrisP, AaronR and others.	2.0 format boot
;
;   Rev 3.0 MarkZ   PC/AT enhancements
;		    2.50 in label
;   Rev 3.1 MarkZ   3.1 in label due to vagaries of SYSing to IBM drive D's
;		    This resulted in the BPB being off by 1.  So we now trust
;		    2.0 and 3.1 boot sectors and disbelieve 3.0.
;
;   Rev 3.2 LeeAc   Modify layout of extended BPB for >32M support
;		    Move PHYDRV to 3rd byte from end of sector
;		    so that it won't have to be moved again
;		    FORMAT and SYS count on PHYDRV being in a known location
;
;   Rev. 3.3 D.C. L. Changed Sec 9 EOT field from 15 to 18. May 29, 1986.
;
;   Rev 3.31 MarkT  The COUNT value has a bogus check (JBE????) to determine
;		    if we've loaded in all the sectors of IBMBIO. This will
;		    cause too big of a load if the sectors per track is high
;		    enough, causing either a stack overflow or the boot code
;		    to be overwritten.
;
;   Rev 4.00 J. K.  For DOS 4.00 Modified to handle the extended BPB, and
;		    32 bit sector number calculation to enable the primary
;		    partition be started beyond 32 MB boundary.
;
;
; The ROM in the IBM PC starts the boot process by performing a hardware
; initialization and a verification of all external devices.  If all goes
; well, it will then load from the boot drive the sector from track 0, head 0,
; sector 1.  This sector is placed at physical address 07C00h.	The initial
; registers are set up as follows:  CS=DS=ES=SS=0.  IP=7C00h, SP=0400H.
;
; The code in this sector is responsible for locating the MSDOS device drivers
; (IBMBIO) and for placing the directory sector with this information at
; physical address 00500h.  After loading in this sector, it reads in the
; entirety of the BIOS at BIOSEG:0 and does a long jump to that point.
;
; If no BIOS/DOS pair is found an error message is displayed and the user is
; prompted to reinsert another disk.  If there is a disk error during the
; process, a message is displayed and things are halted.
;
; At the beginning of the boot sector, there is a table which describes the
; MSDOS structure of the media.  This is equivalent to the BPB with some
; additional information describing the physical layout of the driver (heads,
; tracks, sectors)
;
;==============================================================================
; REVISION HISTORY:
; AN000 - New for DOS Version 4.00 - J.K.
; AC000 - Changed for DOS Version 4.00 - J.K.
; AN00x - PTM number for DOS Version 4.00 - J.K.
;==============================================================================
; AN001; d52 Make the fixed positioned variable "CURHD" to be local.  7/6/87 J.K.
; AN002; d48 Change head settle at boot time.			     7/7/87 J.K.
; AN003; P1820 New message SKL file				   10/20/87 J.K.
; AN004; D304 New structrue of Boot record for OS2.		   11/09/87 J.K.
;==============================================================================
; The segment:offset notation is a way to specify a memory address in real mode (16-bit x86 architecture). It consists of two parts:
; Segment: A 16-bit value representing a memory segment; Offset: A 16-bit value representing the location within that segment.
; The actual physical address is calculated as: Physical Address = (Segment * 16) + Offset
; Example: Segment:Offset Notation: Segment: 0x70 : Offset: 0x1234
; Physical Address = (0x70 * 16) + 0x1234 = 0x700 + 0x1234 = 0x1934
; 4 segment registers in x86 (CS, DS, ES, SS): used to specify the segment part of a memory address in segment:offset notation.
; Segment Registers Overview: 
;	CS (Code Segment): Defines the segment for code execution; The instruction pointer (IP) works with CS to form the address of the next instruction to execute (CS:IP).
;   DS (Data Segment): Defines the segment for most data access operations (e.g., mov, add, etc.). Default segment for memory operands unless overridden.
;   ES (Extra Segment): Used as an additional data segment, often for string operations or BIOS interrupts. Commonly paired with DI (Destination Index) for destination memory access.
;	SS (Stack Segment): Defines the segment for the stack. The stack pointer (SP) works with SS to manage the stack (SS:SP).
;==============================================================================
; This is the corrected NASM conversion of the provided MSBOOT.ASM source code [1].
bits 16
org 0x7C00
; --- Configuration Constants ---
ORIGIN 		equ 	0x7C00	; ORIGIN	    EQU 7C00H		; Origin of bootstrap LOADER
BIOSEG 		equ		0x70 	; BIOSEG	    EQU 70H			; destingation segment of BIOS
BioOff 		equ		0x700 	; BioOff	    EQU 700H		; offset of bios
cbSec 		equ		512		; cbSec	    	EQU 512
cbDirEnt 	equ		32 		; cbDirEnt    	EQU 32
DirOff	    equ		0x500 	; DirOff	    EQU 500h
IBMLOADSIZE	equ		3		; IBMLOADSIZE 	equ 3			;J.K. Size of IBMLOAD module in sectors
ROM_DISKRD	equ		2		; ROM_DISKRD  	equ 2
;
%include "version.inc"		; include file "version.inc"
;
; Define the destination segment of the BIOS, including the initialization label
BIOS equ BIOSEG ; !for nasm !NOT sure if really needed - because of BIOS$_L/H ???
	;	Use equ if: You want BIOS to act as a label pointing to the address 0x70.
	;	You need to access memory at BIOS (e.g., mov al, [BIOS]).
; SEGBIOS SEGMENT AT BIOSEG
; BIOS	LABEL	BYTE
; SEGBIOS ENDS
; CODE	SEGMENT
;	ASSUME CS:CODE,DS:NOTHING,ES:NOTHING,SS:NOTHING
; org ORIGIN		;	ORG	ORIGIN
; !from msdos027.nasm: DSKADR      equ 0x0078 ; POINTER TO DRIVE PARAMETERS (INT 1Eh vector is at 0000:0078)
DSKADR      equ 0x1E * 4   ;DSKADR	=	1EH*4			;POINTER TO DRIVE PARAMETERS
;
GLOBAL EntryPoint:			; Declares EntryPoint as a global symbol, making it visible to the linker or other modules.  
							; In NASM, GLOBAL is equivalent to global (case-insensitive).
    jmp short START			; 
; A short jump (2-byte instruction) to the label START.  
; The short keyword ensures the jump is encoded as a relative 8-bit displacement (saving space).
	nop						; NOP for alignment, common practice. A no-operation instruction (1 byte). Often used for alignment or as a placeholder for patching.
; Purpose: This pattern is typically used to separate the Entry Point from Code. The EntryPoint label marks where execution begins, but the actual code starts at START.  
; This allows for header data (e.g., BIOS Parameter Block in bootloaders) to be placed between EntryPoint and START.
; Compatibility with COM Files: In MS-DOS COM files, the first instruction is often a jmp to skip over data or headers.
; Bootloader Usage: In boot sectors, this pattern skips over the BIOS Parameter Block (BPB) or other metadata. J.K. Extened_BPB
%if IBMCOPYRIGHT	         ; Check if ibmcopyright is defined
    db 'IBM  '      	     ; If true, define "IBM  "
%else
    db 'MSDOS'               ; If false, define "MSDOS"
%endif
    db '4.0'                 ; Always define "4.0"
;
	ByteSec			dw	cbSec      	; Bytes per Sector ; ByteSec   DW	  cbSec 	; SIZE OF A PHYSICAL SECTOR
					db	8			; DB	  8			; SECTORS PER ALLOCATION UNIT
	cSecRes			dw	1			; cSecRes   DW	  1			; NUMBER OF RESERVED SECTORS
	cFat			db	2			; cFat	  DB	  2			; NUMBER OF FATS
	DirNum			dw 	512			; DirNum	  DW	  512			; NUMBER OF DIREC ENTRIES
	cTotSec			dw	4*17*305-1	; cTotSec   DW	  4*17*305-1		; NUMBER OF SECTORS - NUMBER OF HIDDEN SECTORS
									;  (0 when 32 bit sector number)
	MEDIA			db 	0xF8		; MEDIA	  DB	  0F8H			; MEDIA BYTE
	cSecFat			dw	8			; cSecFat   DW	  8			; NUMBER OF FAT SECTORS
	SECLIM			dw	17			; SECLIM	  DW	  17			; SECTORS PER TRACK
	HDLIM			dw	4			; HDLIM	  DW	  4			; NUMBER OF SURFACES !HEADS
	cSecHid_L		dw	63			; cSecHid_L DW	  1 	; Ext_cSecHid label dword AN000; NUMBER OF HIDDEN SECTORS
	cSecHid_H		dw	0			; cSecHid_H dw	  0			;AN000; high order word of Hiden Sectors
	cTotSec_L 		dw	0			; cTotSec_L dw	  0	; Ext_cTotSec label dword AN000; 32 bit version of NUMBER OF SECTORS
	cTotSec_H 		dw	0			; AN000; (when 16 bit version is zero)
	PhyDrv	  		db	0x80		; PhyDrv	  db	 80h			;AN004;
	CURHD			db  0			; Current Head Number
	Ext_Boot_Sig	db    41		; Ext_Boot_Sig	db    41		;AN000;
	Boot_Serial		dd    0			; Boot_Serial	dd    0 		;AN000;
	Boot_Vol_Label	db    'NO NAME    ' ; AN000; 
	Boot_System_id	db    'FAT12   '	; AN000; 
;
; J.K. Danger!!! If not 32 bit sector number calculation, FORMAT should set the value of cSecHid_h and Ext_cTotSec to 0 !!!
; Start of Bootcode 0x3e
;----------------------------------------------------------
; Bootloader Code
;----------------------------------------------------------
UDATA:
; --- Local variable definitions ---
Sec9        	equ UDATA+0       	; A temporary storage area for the disk parameter table.
BIOS$_L      	equ UDATA+11      	; Stores the low word of the data area's starting sector LBA.
BIOS$_H      	equ UDATA+13      	; Stores the high word of the data area's starting sector LBA.
CURTRK      	equ UDATA+15      	; Stores the current track/cylinder for disk reads.
CURSEC      	equ UDATA+17      	; Stores the current sector for disk reads.
DIR$_L       	equ UDATA+18      	; Stores the low word of the root directory's starting sector LBA.
DIR$_H       	equ UDATA+20      	; Stores the high word of the root directory's starting sector LBA.
;
START:
; First thing is to reset the stack to a better and more known place. The ROM may change, but we'd like to get the stack in the correct place.
	cli								; Stop interrupts till stack ok, (Clear Interrupts) instruction, which disables hardware interrupts.
; Prevent interrupts during critical sections (e.g., setting up the stack). Avoid race conditions during hardware initialization.
; NEW code, safe according to DeepSeek XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX;
    mov 		ax, cs              ; Copy CS to AX (8C C8) instead of MASM:   ASSUME	SS:CODE
    mov 		ss, ax              ; Set SS = CS (8E D0)
; IF SS is not explicitly set to match CS, this can be risky because the BIOS might load the bootloader with CS = 0x07C0 instead of CS = 0x0000.
; 	ASSUME	SS:CODE					; NOT used in NASM
	mov 		sp, ORIGIN 			; Sets the stack pointer (SP) to ORIGIN (i.e. 0x7C00). 
	xor     	ax, ax				; Clear AX. Prepares AX for use in setting up segment registers. Clears all arithmetic flags (ZF=1, CF=0, etc.). Smaller and faster than mov ax, 0 (2 bytes vs 3 bytes).
    mov 		ds, ax              ; Set DS = 0x0000
;    mov 		es, ax              ; Set ES = 0x0000
;    sti         		            ; Re-enable interrupts
; END of new code XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX;
;	xor        	ax, ax
;	mov        	ss, ax				; Sets the stack segment (SS) to 0x0000 (since AX=0). Prepares the stack for use in real mode. SS defines the segment for stack operations (push, pop, etc.).
; On x86, mov ss, ax automatically disables interrupts for the next instruction to ensure atomic stack setup.
;   ASSUME	SS:CODE					; NOT used in NASM
;	mov 		sp, ORIGIN 			; Stack pointer starts at 0x7C00. Sets the stack pointer (SP) to ORIGIN (i.e. 0x7C00).
; The stack grows downward from 0x7C00 (e.g., push decrements SP), ensuring it doesn’t overwrite the bootloader code.
	push       	ss					; Push SS onto the stack
	pop        	es					; Pop SS into ES	
; 	ASSUME	ES:CODE					; NOT used in NASM
; We copy the disk parameter table into a local area.  We scan the table above for non-zero parameters.  Any we see get changed to their non-zero values.
; J.K. We copy the disk parameter table into a local area (overlayed into the code), and set the head settle time to 1, and End of Track to SECLIM given by FORMAT.
	mov        	bx, DSKADR			; Set BX = 0x78
	lds        	si, ss:[bx]			; Load DS:SI from SS:BX
	push       	ds					; Push DS onto the stack
	push       	si					; Push SI onto the stack
	push       	ss					; Push SS onto the stack
	push       	bx					; Push BX onto the stack
; DeepSeek V3: In NASM 0.98, the offset keyword is not used. Instead, you directly reference the label (Sec9 or UDATA) to calculate offsets.
	mov        	di, Sec9			; DeepSeek: DI now points to Sec9 (UDATA+0) as destination. offset in 'MOV	DI,offset Sec9' is implicit.
	mov        	cx, 0xB				; DeepSeek: Sets CX to the number of bytes to copy (11)
	cld								; DeepSeek: cld Ensures MOVSB processes data in the forward direction
; clears the Direction Flag (DF), ensuring string operations process data in the forward direction.
; It’s typically used before string operations like MOVSB, LODSB, or STOSB.
; In your example, CLD prepares the processor for a string operation involving DI and CX.
;
; if	$ le BIOS$_L
;	%OUT Don't destroy unexcuted code yet!!!
; endif
;
; DeepSeek: In NASM, the full syntax rep movsb byte es:[di], byte [si] is not needed and would actually result in an error.
; DeepSeek: NASM simplifies string instructions like MOVSB, assuming the source and destination implicitly.
	rep movsb						; AN000; NASM assumes DS for the source
	; MOVSB: Moves a byte from DS:[SI] (source) to ES:[DI] (destination). Automatically increments SI and DI (or decrements if DF = 1).
	; REP: Repeats MOVSB CX times.
; Following two lines: copies ES into DS (DS = ES)
	push       	es					; Push ES onto the stack
	pop        	ds					; AN000; Pop SS into DS	
; 	assume	ds:code 				; Unused in NASM!
; AN000; Head settle time. J.K. Change the head settle to 15 ms will slow the boot time quite a bit!!!
	mov         byte [di - 2], 0xF	; Updates the segment part of the disk parameter table vector at SS:BX+2
	mov    	    cx, word [SECLIM]	; AN004; Set CX = SECLIM (SECTORS PER TRACK). Loads the sectors per track value into CX
	mov        	byte [di - 7], cl	; Update disk parameter table with SECLIM
; Place in new disk parameter table vector.
	mov	        word [bx + 2], ax	; Updates the segment part of the disk parameter table vector at SS:BX+2
	mov         word [bx], Sec9		; Updates the offset part of the disk parameter table vector at SS:BX.
; DeepSeek: writes a 16-bit value (either the offset or value of Sec9) to the memory location pointed to by BX.
;   MOV	[BX],offset SEC9			; offset is implicit in NASM ??
; We may now turn interrupts back on.  Before this, there is a small window; when a reboot command may come in when the disk parameter table is garbage
	sti								; Re-enable interrupts
; Reset the disk system just in case any thing funny has happened.
	int        	0x13				; Reset the system using BIOS INT 0x13. Jumps to CKErr on failure.
	jb         	CKErr				; AN000; Points to Non-System Message
; The system is now prepared for us to begin reading.  First, determine logical sector numbers of the start of the directory and the start of the data area. 
	xor     	ax, ax				; Clear AX. 
	cmp        	word [cTotSec], ax	; AN000; 32 bit calculation? Compare cTotSec with 0. 
	je         	Dir_Cont			; AN000; Jump to Dir_Cont if cTotSec = 0.
	mov        	cx, word [cTotSec]	; AN000; Load CX with cTotSec. 
	mov        	word [cTotSec_L], cx ; AN000; cTotSec_L,cTotSec_H will be used for calculation. Save CX in cTotSec_L. 
Dir_Cont:
	mov        	al, byte [cFat]		; Determine sector dir starts on (DX:AX). Load AL with the number of FAT copies. 
	mul        	word [cSecFat]		; Multiply AL by the number of sectors per FAT (DX:AX = AL * cSecFat). 
	add        	ax, word [cSecHid_L] ; Add the lower word of hidden sectors to AX. 
	adc        	dx, word [cSecHid_H] ; Add the higher word of hidden sectors to DX with carry. 
	add        	ax, word [cSecRes]	; Add the reserved sectors to AX. 
	adc        	dx, 0				; Add carry to DX (if AX overflowed). 
	mov        	word [DIR$_L], ax	; DX;AX = cFat*cSecFat + cSecRes + cSecHid. Save AX (lower word) in DIR$_L. 
	mov        	word [DIR$_H], dx	; AN000; Save DX (higher word) in DIR$_H. 
	mov        	word [BIOS$_L], ax	; DeepSeek: writes the (!lower) 16-bit value in AX to the memory location specified by BIOS$_L
; 	mov [BIOS$_L], ax       ; DeepSeek Implicit word size - in nasm 'word' is only needed if not both are 16 bits, c.q. one is 'byte' for example
; A 32-bit value is typically stored in a pair of registers (e.g., DX:AX in 16-bit mode, where DX holds the upper 16 bits and AX holds the lower 16 bits)
	mov        	word [BIOS$_H], dx	; AN000; DeepSeek: Upper 16 bits stored in BIOS$_H
; Take into account size of directory (only know number of directory entries)
	mov			ax, cbDirEnt		; bytes per directory entry. Load AX with the size of a directory entry (32 bytes). 
	mul        	word [DirNum]		; convert to bytes in directory. Multiply AX by the number of directory entries (DX:AX = AX * DirNum). 
	mov        	bx, word [ByteSec]	; add in sector size. Load BX with the number of bytes per sector. 
	add        	ax, bx				; Add BX to AX (round up to the nearest sector). 
	dec        	ax					; decrement so that we round up. Decrement AX to ensure correct rounding up.
	div        	bx					; convert to sector number. Divide AX by BX (AX = sectors in directory).
	add        	word [BIOS$_L], ax	; Start sector # of Data area. DeepSeek: Adds the value in AX to the 16-bit value stored at the memory location [BIOS$_L]
; The instruction adc word [BIOS$_H], 0 performs an add with carry operation. Here’s a detailed breakdown of its meaning and purpose:
; adc (Add with Carry): Adds the source operand (0 in this case) and the Carry Flag (CF) to the destination operand ([BIOS$_H]).
; word [BIOS$_H]: 		Specifies that the destination is a 16-bit memory location at the address given by BIOS$_H.
; 0:					The source operand is 0, so this instruction essentially adds the Carry Flag (CF) to [BIOS$_H].
; 	If CF = 1, this increments the value at [BIOS$_H]. If CF = 1, the destination is incremented by 1.
;	If CF = 0, the value at [BIOS$_H] remains unchanged.
;	The result is stored in the memory location [BIOS$_H]
	adc        	word [BIOS$_H], 0	; AN000; adds the Carry Flag (CF) (!from previous calculation) to [BIOS$_H].
; adc word [BIOS$_H], 1: Adds 1 + CF to [BIOS$_H]; can be used but is uncommon because:
;	It combines a constant (1) with the carry (CF), which is rarely needed.
;	It’s less readable than separate add and adc instructions.
;	Multi-Precision Arithmetic with a Constant: 
;	If you’re performing a multi-word addition and need to add a constant (1) while also incorporating a carry from a lower-precision operation.
; We load in the first directory sector and examine it to make sure the the BIOS and DOS are the first two directory entries. If they are not found, the user is prompted to insert a new disk. The directory sector is loaded into 00500h
	mov        	bx, DirOff			; sector to go in at 00500h
	mov        	dx, word [DIR$_H]	; AN000;
	mov        	ax, word [DIR$_L]	; logical sector of directory
	call       	DODIV				; convert to sector, track, head
	jb         	CKErr				; AN000; Overflow? BPB must be wrong!! Points to Non-System Message
	mov        	al, 1				; AN000; disk read 1 sector
	call        DOCALL				; do the disk read
	jb	   		CKErr				; Points to Non-System Message
; Now we scan for the presence of IBMBIO  COM and IBMDOS  COM.	Check the first directory entry.
	mov        	di, bx				; 
	mov        	cx, 0xB				; 0xb = 11
	mov        	si, BIO				; ORG: 'MOV	SI,OFFSET BIO' [OFFSET is impicit in NASM?] - point to "ibmbio  com"
	repe cmpsb ; Compare CX bytes from DS:SI with ES:DI
;	BAD: repe cmpsb 	byte [si], byte es:[di]	;
	jb	       	CKErr				; Points to Non-System Message - if not there advise the user
; Found the BIOS.  Check the second directory entry.
; Copilot: "Load the Effective Address" of the source operand into the destination register di
	lea        	di, [bx + 0x20]		; [bx + 0x20]: This is the source operand, specified using an x86 addressing mode. The value inside the brackets is the effective address that is calculated: the current value in the bx register plus the hexadecimal offset 0x20 (which is 32 in decimal).
; The result of this addition is placed into the di register. The lea instruction is often used for fast pointer arithmetic and general arithmetic calculations (like addition and limited multiplication by 2, 4, or 8) because it can combine several additions into a single instruction and does not affect any CPU flags, unlike add. 
	mov        	si, DOS				; ORG: 'MOV	SI,OFFSET DOS' [OFFSET is impicit in NASM?] - point to "ibmdos  com"
	mov        	cx, 0xB				; 0xb = 11
	repe 		cmpsb 				; Compare CX bytes from DS:SI with ES:DI
;	BAD: repe cmpsb 	byte [si], byte es:[di]	;
	je         	DoLoad
; There has been some recoverable error. Display a message and wait for a keystroke.
	jb         	CKErr				; Points to Non-System Message
CKErr:
	MOV			si, SYSMSG			; point to no system message ; now at 0xf0 - same as 1 line before
ErrOut:
	CALL		WRITE				; and write on the screen ; WRITE is called at 0x147
; The next code up to 'jb CKerr' clears AH, waits for keyboard input, restores the disk parameter table, reboots the system, and handles errors. 
	xor     	ah, ah				; wait for response. Clear AH (high byte of AX). Clears all arithmetic flags (ZF=1, CF=0, etc.). Any value XORed with itself yields 0. Thus, AH is set to 0x00. 
	int        	0x16				; BIOS interrupt 0x16: Keyboard input. Waits for a keypress. 
	pop 		si					; reset disk parameter table back to rom. Pop the saved SI value from the stack. SI points to the disk parameter table. 
	pop 		ds					; Pop the saved DS value from the stack. DS is the segment for the disk parameter table. 
	pop        	word [si]			; Pop the saved offset of the disk parameter table into [SI]. 
	pop        	word [si + 2]		; Pop the saved segment of the disk parameter table into [SI + 2]. 
; BIOS-interrupts like int 0x13 use CF for errors: 
;    CF = 1: Operatie failed (forinstance read/write error on disk). 
;    CF = 0: Operation succeeded
	int        	0x19				; BIOS interrupt 0x19: Reboot the system. Reloads the bootloader. Continue in loop till good disk
Load_Failure:
	pop        	ax					; Pop a value into AX (adjusts the stack).
	pop        	ax					; Pop a value into AX (adjusts the stack).
	pop        	ax					; Pop a value into AX (adjusts the stack).
	pop        	ax					; Pop a value into AX (adjusts the stack).
	pop        	ax					; Pop a value into AX (adjusts the stack).
	pop        	ax					; Pop a value into AX (adjusts the stack).
	jb         	CKErr				; Jump to CKErr if the carry flag (CF) is set (conditional jump). Points to Non-System Message and reboot
; We now begin to load the BIOS in. Compute the number of sectors needed.
; J.K. All we have to do is just read in sectors contiguously IBMLOADSIZE times. We here assume that IBMLOAD module is contiguous. Currently we estimate that IBMLOAD module will not be more than 3 sectors.
DoLoad:
	mov 		bx, BioOff			; Set BX = Offset of IBMBIO (IBMLOAD) to be loaded.
	mov        	cx, IBMLOADSIZE 	; # of sectors to read. Offset of ibmbio(IBMLOAD) to be loaded.
	mov        	ax, word [BIOS$_L]	; Sector number to read. DeepSeek: Read from memory, the reverse operation of: mov word [BIOS$_L], ax
	mov        	dx, word [BIOS$_H]	; AN000. DeepSeek: reads a 16-bit value from [BIOS$_H] and stores it in DX (!Here the high bytes).
;  Segment Assumption: NASM assumes DS as the segment register for [BIOS$_H] unless overridden.
Do_While:							; AN000
	push       	ax					; AN000. Push AX (sector number, lower word) onto the stack.
	push       	dx					; AN000. Push DX (sector number, higher word) onto the stack.
	push       	cx					; AN000. Push CX (number of sectors) onto the stack.
	call		DODIV				; AN000; DX;AX = sector number. Call subroutine to calculate cylinder, head, and sector.
	jb         	Load_Failure		; AN000; Adjust stack. Show error message. Jump to Load_Failure if carry flag (CF) is set (error).
	mov        	al, 1				; AN000; Read 1 sector at a time. Set AL = 1 (read 1 sector).
;This is to handle a case of media when the first sector of IBMLOAD is the the last sector in a track.
	call       	DOCALL				; AN000; Read the sector. Call subroutine to read the sector from disk.
	pop        	cx					; AN000. Restore CX (number of sectors) from the stack.
	pop        	dx					; AN000. Restore DX (sector number, higher word) from the stack.
	pop        	ax					; AN000. Restore AX (sector number, lower word) from the stack.
	jb         	CKErr				; AN000; Read error? Jump to CKErr if carry flag (CF) is set (read error). Points to Non-System Message
	add        	ax, 1				; AN000; Next sector number. Increment AX (sector number, lower word) by 1.
	adc        	dx, 0				; AN000. Add carry to DX (sector number, higher word) if AX overflowed.
	add        	bx, word [ByteSec]	; AN000; adjust buffer address. Add ByteSec (bytes per sector) to BX (buffer address).
	loop       	Do_While			; AN000. Decrement CX and loop if CX ≠ 0 (read next sector).
;
; IBMINIT requires the following input conditions:
;   DL = INT 13 drive number we booted from
;   CH = media byte
; J.K.I1. BX was the First data sector on disk (0-based)
; J.K.I1. IBMBIO init routine should check if the boot record is the
; J.K.I1. extended one by looking at the extended_boot_signature.
; J.K.I1. If it is, then should us AX;BX for the starting data sector number.
DISKOK:
	mov        	ch, byte [MEDIA]	; Load CH with the media descriptor byte from memory.
	mov        	dl, byte [PhyDrv]	; Load DL with the physical drive number from memory.
	mov        	bx, word [BIOS$_L]	; Load lower 16 bits into BX. AN000; J.K.I1.Get bios sector in bx
	mov        	ax, word [BIOS$_H]	; Overwrite AX with higher 16 bits (AX now contains only BIOS$_R). AN000; J.K.I1.
; Result: Now AX = high word, BX = low word - Now AX:BX = 32-bit value (non-standard pairing): Call BIOS disk service (AX/BX used for parameters).
; 	AX is often used for arithmetic operations or system calls (e.g., BIOS interrupts).
; 	Why Start with BX? (Possible Reasons): 1. Preserving AX for a Specific Purpose; 2. Preparing for a 32-bit Operation; 3. Avoiding Register Clobbering; 4. Data Structure Alignment.
; Final jump to handle execution to IO.SYS
	jmp 		BIOSEG:0x0000 		; Far jump to BIOS entry point (BIOSEG = 0x70). !BioOff is 0x700 - can not be used here ;0x0000000000000142:  EA 00 00 70 00    ljmp       0x70:0							; JMP	FAR PTR BIOS		;CRANK UP THE DOS
;
WRITE:	; The code is a loop that prints characters from memory (DS:SI) using BIOS INT 0x10 teletype function. It stops when a null terminator (\0) is encountered. 
	lodsb							; Load next character from DS:SI into AL. Increment SI. GET NEXT CHARACTER ; called now at 0x147
	or         	al, al				; Check if AL (character) is zero (end of string), clear the high bit
	je         	ENDWR				; Jump to ENDWR if AL = 0 (end of string), ERROR MESSAGE UP, JUMP TO BASIC
	mov        	ah, 0xE				; Set AH = 0x0E (BIOS teletype function), WILL WRITE CHARACTER & ATTRIBUTE
	mov        	bx, 7				; Set BX = 7 (attribute: white on black). Sets the video attribute byte for BIOS INT 0x10 teletype output.
	int        	0x10				; Call BIOS teletype function to print the character in AL.
	jmp        	WRITE				; Jump back to WRITE to process the next character.
DODIV:	; convert a logical sector into Track/sector/head.  AX has the logical sector number. J.K. DX;AX has the sector number. Because of not enough space, we are going to use Simple 32 bit division here. Carry set if DX;AX is too big to handle.
	cmp        	dx, word [SECLIM]	; Compare DX (high word of sector number) with SECLIM (sectors per track). 
	jae        	DivOverFlow			; Jump to DivOverFlow if DX ≥ SECLIM (overflow prevention). 
	div        	word [SECLIM]		; Divide DX:AX by SECLIM (sectors per track), AX = Total tracks, DX = sector number
	inc        	dl					; Increment DL (sector number). 
	mov        	byte [CURSEC], dl	; Save DL (sector number) in CURSEC. Cursec is 1-based. DeepSeek: Many BIOS routines and disk formats (e.g., floppy disks) use 1-based sector numbering (!instead starting with 0).
	xor     	dx, dx				; Clear DX. Clears all arithmetic flags (ZF=1, CF=0, etc.).
	div        	word [HDLIM]		; Divide AX (total tracks) by HDLIM (heads per cylinder). 
	mov        	byte [CURHD], dl	; Save DL (head number) in CURHD. 
	mov        	word [CURTRK], ax	; Save AX (track number) in CURTRK. 
	clc								; Clear carry flag (CF = 0). 
	ret								; Return from subroutine. 
DivOverFlow:						; AN000;
	stc								; Set carry flag (CF = 1). 
ENDWR:
	ret								; Return from subroutine. 
; Issue one read request.  ES:BX have the transfer address, AL is the number of sectors.
DOCALL:	; The DOCALL subroutine prepares and executes a BIOS disk read operation. It combines track, sector, head, and drive information for the BIOS call.  
	mov			ah, ROM_DISKRD		; Set AH = 0x02 (BIOS disk read function), see: AC000:=2
	mov        	dx, word [CURTRK]	; Load DX with the current track number. 
	mov        	cl, 6				; Set CL = 6 (shift count). 
	shl        	dh, cl				; Shift DH (high byte of DX) left by 6 bits. 
; or: Bitwise OR operation (sets bits to 1 if either operand has a 1 in that position); destination operand (8-bit high half of the DX register); The source operand, an 8-bit value read from memory at the address CURSEC.
; The source operand, an 8-bit value read from memory at the address CURSEC; byte specifies an 8-bit operation (required because [CURSEC] is a memory operand); Without byte, NASM would throw an error due to ambiguity.
; SF (Sign Flag): Set if the result’s MSB is 1; ZF (Zero Flag): Set if the result is 0; PF (Parity Flag): Set if the result has an even number of 1 bits; OF/CF (Overflow/Carry Flags): Cleared (always for or).
; Common Use Cases: Setting Specific Bits; Merging Data; Fast Zero Check.
	or         	dh, byte [CURSEC]	; Combine DH (track) with CURSEC (sector number). 
; DeepSeek: bitwise OR operation between the 8-bit value in DH (!high bytes of dx) and the 8-bit value stored at the memory location [CURSEC], storing the result back in DH.
; 	or dh, [CURSEC]: Invalid because DH alone doesn’t imply the size of the memory operand.
; The 8-bit registers (AH, AL, BH, BL, CH, CL, DH, DL) are part of the general-purpose registers (AX, BX, CX, DX).
	mov        	cx, dx				; Move DX (cylinder/sector) into CX. 
	xchg       	cl, ch				; Swap CH (cylinder) and CL (sector). 
	mov        	dl, byte [PhyDrv]	; Load DL with the physical drive number. 
	mov        	dh, byte [CURHD]	; Load DH with the current head number. 
	int        	0x13				; Call BIOS interrupt 0x13 (disk read).
	ret								; Return from subroutine. 
;
%include "boot.cl1" 				; Data area for messages and filenames in file "boot.cl1", see AN003
									; Original:
%if IBMCOPYRIGHT 					;   IF IBMCOPYRIGHT
BIO db "IBMBIO  COM" 				; BIO	DB	"IBMBIO  COM"
DOS db "IBMDOS  COM" 				; DOS	DB	"IBMDOS  COM"
%else 								; 	ELSE
BIO db "IO      SYS" 				; BIO	DB	"IO      SYS"
DOS db "MSDOS   SYS" 				; DOS	DB	"MSDOS   SYS"
%endif 								;	ENDIF
;
    times 510 - ($ - $$) db 0   	; Pad remainder of boot sector with 0 ; From v027
;
    db 0x55, 0xAA					; db	55h,0aah = Boot sector signature
;