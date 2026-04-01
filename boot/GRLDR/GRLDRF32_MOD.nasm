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
; GRLDRF32.nasm, derived from grub4dos' grldr.pbr
; v.11.11 (20260330), by deomsh with MUCH help of Copilot
; CHANGES for writing Dot after each sector Read, extra INT 0x10 for stability and AH-error written after 'Disk Error  ' as ASCII
;
; bits 16                   ; real mode, 16-bit instructions
org 0x7C00       ; boot sector load address
; ORG  equ  0x7C00        ; bad, not taken by compiler
ORIGIN              equ 0x7C00        ; bad, not taken by compiler
ROOTSEG             equ 0x2000
LOADOFF             equ 0
LOADSEG             equ 0x2000
;
%define VAR44       bp+0x44
%define VAR48       bp+0x48
%define VAR4C       bp+0x4C
;
;v11.3;Int13AlPtrOffset    equ Int13Al - $$ ; = Int13Al - 0x7C00
;v11.3;Int13AhPtrOffset    equ Int13Ah - $$ ; = Int13Ah - 0x7C00
DiskErrMsgOffset    equ DiskErr - $$ ; = DiskErr - 0x7C00
NOGRLDRByteOffset   equ NOGRLDR + 10 - $$  ; = NOGRLDR + 10 - 0x7C00
;DiskErrMsgOffset    equ DiskErr - $$ ; = DiskErr + 6 - 0x7C00
;
GLOBAL EntryPoint:         ; Declares EntryPoint as a global symbol, making it visible to the linker or other modules.
; global EntryPoint        ; export entry symbol
; EntryPoint:
    jmp Start              ; jump to boot code Start:
    nop                    ; filler byte
;
; -------------------------
; BIOS Parameter Block (BPB)
; -------------------------
%define OEM         bp+3
;OEM
    db "IBM  2.0" ; OEM string
%define ByteSec     bp+0xb
;ByteSec
    dw 512        ; bytes per sector
%define SecClus     bp+0xd
;SecClus 
    db 1          ; sectors per cluster
%define ReservSec   bp+0xe
;ReservSec
    dw 32         ; reserved sectors
%define NumFATs     bp+0x10
;NumFATs 
    db 2          ; number of FATs
%define RootEntries bp+0x11
;RootEntries
    dw 0          ; root entries (FAT32=0)
%define TotSect16  bp+0x13
;TotSect16
    dw 0          ; total sectors (16-bit)
%define MediaByte   bp+0x15
;MediaByte
    db 0xF8       ; media descriptor
%define FatSize16   bp+0x16
;FatSize16
    dw 0          ; FAT size (16-bit)
%define SPT_Heads   bp+0x18
;SPT_Heads
    dd 0x003F00FF ; sectors per track combined with number of heads
;SecTrack    dw 0x3F       ; sectors per track
;NumHeads    dw 0xFF       ; number of heads
%define HiddSec     bp+0x1c
;HiddSec 
    dd 0x3F       ; hidden sectors
%define TotSect     bp+0x20
;TotSect 
    dd 0x3FFBC1   ; total sectors (32-bit)
%define FatSize32   bp+0x24
;FatSize32
    dd 0x00000FFB ; FAT size (32-bit)
%define Mirror      bp+0x28
;Mirror  
    dw 0          ; mirror flags
%define FSVersion   bp+0x2a
;FSVersion
    dw 0          ; filesystem version
%define RootClus    bp+0x2c
;RootClus
    dd 2          ; root cluster
%define FSInfo      bp+0x30
;FSInfo  
    dw 1          ; FSInfo sector
%define BackupBoot  bp+0x32
;BackupBoot
    dw 6          ; backup boot sector
times 12 db 0             ; reserved
%define DriveNum    bp+0x40
;DriveNum
    db 0x80       ; drive number
%define ReservedNT  bp+0x41
;ReservedNT
    db 0          ; reserved NT
%define BootSig     bp+0x42
;BootSig 
    db 0x29       ; boot signature
%define UUID        bp+0x43
;UUID    
    dd 0          ; volume ID
%define VolLabel    bp+0x47
;VolLabel
    db "NO NAME    " ; volume label
%define FileSys     bp+0x52
;FileSys 
    db "FAT32   " ; filesystem type
;
; -------------------------
; Extended Data Area (moved)
; -------------------------
;Int13Al db 1              ; 
;Int13Ah db 2              ; 
;v11.9; DiskErr db "Error    "     ;
DiskErr db "AH=   "     ;
NOGRLDR db "Key2reboot "    ;
;v11.4;NOGRLDR db "Key2Reboot "    ;
;notnow db "No"
;v11.10;
 db "No "
;v11.9; db "No"
;v11.5; db "No"
;v11.9;
GRLDR   db "GRLDR      ", 0
; Last space needed to write AH-error as ASCII char
;v11.3;DiskErr db "Disk error   ", 0
;v11.2;DiskErr db "Disk error  ", 0
; -------------------------
; Boot code starts at 5Ah
; -------------------------
Start:
    cli                   ; disable interrupts
    cld                   ; clear direction flag
    xor ax, ax            ; ax=0
    mov bp, ORIGIN        ; set BP to base
    mov sp, bp            ; set SP
    mov ss, ax            ; set SS=0
    sti                   ; enable interrupts
;
; --- Check BIOS INT13h Extensions ---
    push ax               ; save ax
;
    mov [DriveNum], dl	  ; store BIOS drive
;BAD?;    mov word [DriveNum], dx ; store BIOS drive
;
    mov ah, 0x41          ; check extensions
    mov bx, 0x55AA        ; signature
    int 0x13              ; BIOS disk service
;
    pop ax                ; restore ax
    mov ds, ax            ; set DS
;
CheckExtensions:
    jb DiskInit           ; jump if error
    cmp bx, 0xAA55        ; check signature
    jne DiskInit          ; jump if not match
    ror cl, 1             ; rotate cl
    jae DiskInit          ; jump if carry clear
; Compiler placed next address at offset 188h !
;notnow;    mov byte [0x7DA7], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7DA7], 0x42   ; patch byte at offset 0x1A7
;v11.10;
    mov byte [0x7DA8], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7DA8], 0x42   ; patch byte at offset 0x1A8
;v11.9;    mov byte [0x7DAB], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7DAB], 0x42   ; patch byte at offset 0x1AB
;v11.4;    mov byte [0x7DAA], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7DAA], 0x42   ; patch byte at offset 0x1AA
;v11.3;    mov byte [bp+Int13AhPtrOffset], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7D88], 0x42   ; patch byte at offset 0x188
;    mov byte [0x7D88], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7D88], 0x42   ; patch byte at offset 0x188
; Compiler placed next address at offset 194h !
;LAST;    mov byte [0x7D94], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7D94], 0x42   ; patch byte at offset 0x194
;BAD;    mov byte [0x7D88], 0x42 ; patch AH=42h ; Two bytes behind now: mov byte [0x7D86], 0x42   ; patch byte at offset 0x186
;
DiskInit:
    xor bx, bx            ; bx=0
    xor eax, eax          ; eax=0
    mov dword [VAR44], eax ; clear var at 0x7C44
    mov ax, word [ReservSec]   ; load reserved
;    mov ax, [ReservSec]   ; load reserved
    add eax, dword [HiddSec]    ; add hidden
;    add eax, [HiddSec]    ; add hidden
    mov [VAR48], eax     ; store var at 0x7C48
    mov [VAR4C], eax     ; store var at 0x7C4C
;
;mismatch!;    mov eax, byte [NumFATs] 	  ; load FAT count
;BAD;
    mov eax, [NumFATs] 	  ; load FAT count
    mul dword [FatSize32] ; multiply
    add [VAR4C], eax     ; add to var at 0x7C4C
    mov eax, dword [RootClus]   ; load root cluster
;    mov eax, [RootClus]   ; load root cluster
;
LoadRootCluster:
    push eax              ; save eax
    call ClusterToLBA     ; convert cluster
    mov si, NOGRLDR       ; point to "No"
    jb PrintErrorLoop     ; jump if error
;
SearchDirLoop:
    push ROOTSEG           ; push seg
    pop es                ; set ES
    push es               ; save ES
    call DiskRead         ; read sector
    pop es                ; restore ES
    xor di, di            ; di=0
;
SearchFileLoop:
    mov cx, 0xB           ; length=11
    mov si, GRLDR         ; point to "GRLDR"
    repe cmpsb            ; compare strings
    je FileFound          ; found
    add di, 0x20          ; next entry
    and di, 0xFFE0        ; align
    cmp di, word [ByteSec]     ; check limit
;    cmp di, [ByteSec]     ; check limit
    jne SearchFileLoop    ; loop
    dec dx                ; decrement
    jne SearchDirLoop     ; loop
    pop eax               ; restore eax
    call GetFATEntry      ; get FAT entry
    jmp LoadRootCluster   ; loop
;
FileFound:
    push word es:[di+9]   ; push cluster hi
    push word es:[di+0xF] ; push cluster lo
    pop eax               ; eax=cluster
;
LoadFileLoop:
    push eax              ; save eax
    call ClusterToLBA     ; convert cluster
    jae ReadClusterLoop   ; jump if valid
;NOTSOGOOD;    mov dl, byte [DriveNum]    ; load drive
;BAD;
    mov dx, word [DriveNum] ; load drive, high byte is zero = [ReservedNT]
    push dx               ; push drive
    jmp LOADSEG:LOADOFF   ; far jump
;
ReadClusterLoop:
    call DiskRead         ; read cluster
    dec dx                ; decrement
    jne ReadClusterLoop   ; loop
    pop eax               ; restore eax
    call GetFATEntry      ; get FAT entry
    jmp LoadFileLoop      ; loop
;
GetFATEntry:
    push es               ; save ES
    shl eax, 2            ; shift
    movzx ebx, word [ByteSec] ; load bytes/sector
    div ebx               ; divide
    add eax, [VAR48]     ; add base - var at 0x7C48
    mov bx, 0x60          ; set ES
    mov es, bx            ; ES=0x60
    xor bx, bx            ; bx=0
    cmp eax, [VAR44]     ; compare - var at 0x7C44
    je FATEntryLoaded     ; already loaded
    mov [VAR44], eax     ; store - var at 0x7C44
    push es               ; save ES
    call DiskRead         ; read sector
    pop es                ; restore ES
FATEntryLoaded:
    and byte es:[edx+3], 0xF ; mask
    mov eax, dword es:[edx] ; load entry
    pop es                ; restore ES
    ret                   ; return
;
ClusterToLBA:
    cmp eax, 0x0FFFFFF8   ; check end
    cmc                   ; complement carry
    jb EndCluster         ; jump if end
    dec eax               ; decrement
    dec eax               ; decrement
    movzx edx, byte [SecClus] ; load sectors/clus
    push dx               ; save dx
    mul edx               ; multiply
    pop dx                ; restore dx
    add eax, [VAR4C]     ; add base - 0x7C4C
EndCluster:
    ret                   ; return
;
DiskRead:
    pushad                ; save regs
    xor edx, edx          ; edx=0
    push edx              ; push 0
    push eax              ; push LBA
    push es               ; push ES
    push dx               ; push offset
    push 1                ; sector count
    push 0x10             ; heads const
    xor ecx, ecx          ; ecx=0
;
;Malforms Stack;    push word [SecTrack] ; push sectors/track
;BAD; OR GOOD?
    push dword [SPT_Heads] ; push sectors/track & NumHeads in High word
;    push dword [SecTrack] ; push sectors/track & NumHeads in High word
;    
    pop cx                ; load CX
    div ecx               ; divide, EAX is the sector LBA
    inc dx                ; inc sector to base 1, DX is the remainder
    pop cx                ; load CX
    push dx               ; push sector
    xor dx, dx            ; dx=0
    div ecx               ; divide
    xchg dh, dl           ; swap head
    pop cx                ; load CX
    xchg ch, al           ; swap cylinder
    shl ah, 6             ; shift
    or cl, ah             ; combine
;v11.3;    mov ax, [bp+Int13AlPtrOffset] ; Read 1 sector, AH=02h OR set to AH=42h if available
;ORG; Back to spare ONE byte
    mov ax, 0x201         ; read sectors
    mov si, sp            ; SI=SP
    mov dl, [0x7C40]      ; load drive
    push es               ; save ES
    push ds               ; save DS
; NEW extra cleaning CF (idea of MS-Copilot)
    clc
; END of NEW
    int 0x13              ; BIOS disk read
    pop ax                ; restore ax
    mov ds, ax            ; restore DS
    pop bx                ; restore bx
    jb DiskError          ; jump if error
    lea bx, [bx+0x20]     ; next seg
    mov es, bx            ; set ES
; NEW Code for 'printing' find cursor [no video output]
    mov ah, 0x03
    xor bh, bh            ; Page 0
    int 0x10              ;
; END of Code for 'printing' find cursor [no video output]
; NEW Code for printing Dot
    mov ax, 0x0e2e        ; Combine 'mov ah, 0xE' and 'mov al, "."'
    int 0x10              ;
; END of new Code for printing Dot
    popa                  ; restore regs
    popad                 ; restore regs
    inc eax               ; next LBA
    ret                   ; return
;
DiskError:
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
; BAD NEW Code from GLM 4.7 (286+ CPU): 20 bytes
;    xor     al, al          ; 2 bytes: Clear AL (prepare for aam)
;    aam     0x10            ; 2 bytes: Split AH=High, AL=Low
;    xchg    al, ah          ; 2 bytes: Move High Nibble to AL (safe in AH)
;    add     al, 90h         ; 2 bytes: Hex->ASCII start
;    daa                     ; 1 byte
;    add     al, 40h         ; 2 bytes: FIX! Use ADD instead of ADC (ignores old CF)
;    daa                     ; 1 byte
;    xchg    al, ah          ; 2 bytes: Save result to AH, get Low Nibble in AL
;    add     al, 90h         ; 2 bytes: Hex->ASCII start
;    daa                     ; 1 byte
;    add     al, 40h         ; 2 bytes: Use ADD instead of ADC
;    daa                     ; 1 byte
; FOURTH Optimized Code from GLM 4.7 (286+ CPU): 19 bytes
;    mov     al, ah          ; 2 bytes: FIX! Move Error Code to AL (was: xor al, al)
;    aam     0x10            ; 2 bytes: AH=High Nibble, AL=Low Nibble
;    add     al, 90h         ; 2 bytes: Convert Low Nibble
;    daa                     ; 1 byte
;    adc     al, 40h         ; 2 bytes
;    daa                     ; 1 byte
;    clc                     ; 1 byte: Clear Carry Flag for next step
;    add     ah, 90h         ; 3 bytes: Convert High Nibble
;    daa                     ; 1 byte
;    adc     ah, 40h         ; 3 bytes
;    daa                     ; 1 byte
; THIRD Optimized Code from GLM 4.7 (286+ CPU): 19 bytes
;    xor     al, al          ; 2 bytes: Clear AL (remove junk sector count)
;    aam     0x10            ; 2 bytes: AH=High Nibble, AL=Low Nibble
;                            ;
;    ; --- Convert Low Nibble (AL) ---
;    add     al, 90h         ; 2 bytes
;    daa                     ; 1 byte
;    adc     al, 40h         ; 2 bytes
;    daa                     ; 1 byte
;                            ;
;    clc                     ; 1 byte: FIX! Clear Carry Flag from previous step
;                            ;
;    ; --- Convert High Nibble (AH) ---
;    add     ah, 90h         ; 3 bytes
;    daa                     ; 1 byte
;    adc     ah, 40h         ; 3 bytes
;    daa                     ; 1 byte
;                            ;
    ; Result: AH = High ASCII, AL = Low ASCII
    ; Total Size: 19 bytes
; BAD SECOND Optimized Code from GLM 4.7 (286+ CPU): 18 bytes
;    xor     al, al          ; 2 bytes: Clear AL (remove sector count junk)
;    aam     0x10            ; 2 bytes: AH=High Nibble, AL=Low Nibble
;                            ;
;    ; Convert Low Nibble (AL) - Uses short 2-byte opcodes
;    add     al, 90h         ; 2 bytes
;    daa                     ; 1 byte
;    adc     al, 40h         ; 2 bytes
;    daa                     ; 1 byte
;                            ;
;    ; Convert High Nibble (AH) - Uses long 3-byte opcodes
;    add     ah, 90h         ; 3 bytes
;    daa                     ; 1 byte
;    adc     ah, 40h         ; 3 bytes
;    daa                     ; 1 byte
;                            ;
;    ; Result: AH = High ASCII, AL = Low ASCII
;    ; Total Size: 18 bytes
; BAD Optimized Code from GLM 4.7 (286+ CPU): diveide by zero possible
;    xchg    al, ah          ; 1 byte:  AL = Error Code, AH = Trash (don't care)
;    aam     16              ; 2 bytes:  AH = High Nibble, AL = Low Nibble (Div by 16)
;                            ; 6 bytes:  Convert Low Nibble (AL)
;    add     al, 90h         
;    daa                     
;    adc     al, 40h         
;    daa                     
;    xchg    al, ah          ; 1 byte:  Swap to convert High Nibble
;                            ; 6 bytes:  Convert High Nibble (now in AL)
;    add     al, 90h         
;    daa                     
;    adc     al, 40h         
;    daa                     
;    xchg    al, ah          ; 1 byte:  Swap back to correct order (AH=High, AL=Low)
;    ; Total: 17 bytes
    mov [bp+DiskErrMsgOffset+3], ax ; Overwrite offset 3 in DiskErr with value of AX
;v11.9;    mov [bp+DiskErrMsgOffset+6], ax ; Overwrite offset 6 in DiskErr with value of AX
    mov [bp+NOGRLDRByteOffset], byte 0x0
;V11.3;    mov [bp+DiskErrMsgOffset+11], ax ; Overwrite offset 11 DiskErr with value of AX
;v11.2;    mov [bp+DiskErrMsgOffset+11], ah ; Overwrite offset 11 DiskErr with value of AH
;Mod1;    mov [DiskErr+11], ah  ; Overwrite last space in DISKERR with value of AH
    mov si, DiskErr       ; point to error msg
PrintErrorLoop:
    lodsb                 ; load char
    mov ah, 0x0E          ; set INT10 mode
    int 0x10			  ;
    cmp al, 0			  ;	compare 
    jne PrintErrorLoop    ; jump if not last char
xor ah, ah
int 0x16
int 0x19
;ORG;Hang:
;ORG;    jmp Hang              ; infinite loop
; -------------------------
; Strings
; -------------------------
;NOGRLDR db "No"
;GRLDR   db "GRLDR      ", 0
; Last space needed to write AH-error as ASCII char
;DiskErr db "Disk error  ", 0
;ORG;DiskErr db "Disk error", 0
;
times 510 - ($ - $$) db 0
db 0x55
db 0xAA
;
