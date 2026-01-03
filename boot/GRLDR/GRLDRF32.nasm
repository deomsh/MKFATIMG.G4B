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
; GRLDRF32.nasm, derived from grub4dos' grldrstart.S (fat32_dbr)
; v.9 (20251201), by deomsh with MUCH help of Copilot
;
; bits 16                   ; real mode, 16-bit instructions
org  0x7C00       ; boot sector load address
; ORG  equ  0x7C00        ; bad, not taken by compiler
ORIGIN  equ  0x7C00        ; bad, not taken by compiler
;
GLOBAL EntryPoint:		  ; Declares EntryPoint as a global symbol, making it visible to the linker or other modules.
; global EntryPoint        ; export entry symbol
; EntryPoint:
    jmp Start         	  ; jump to boot code Start:
    nop                   ; filler byte
;
; -------------------------
; BIOS Parameter Block (BPB)
; -------------------------
OEM         db "IBM  2.0" ; OEM string
ByteSec     dw 512        ; bytes per sector
SecClus     db 1          ; sectors per cluster
ReservSec   dw 32         ; reserved sectors
NumFATs     db 2          ; number of FATs
RootEntries dw 0          ; root entries (FAT32=0)
TotSect16   dw 0          ; total sectors (16-bit)
MediaByte   db 0xF8       ; media descriptor
FatSize16   dw 0          ; FAT size (16-bit)
SPT_Heads	dd 0x003F00FF ; sectors per track combined with number of heads
;SecTrack    dw 0x3F       ; sectors per track
;NumHeads    dw 0xFF       ; number of heads
HiddSec     dd 0x3F       ; hidden sectors
TotSect     dd 0x3FFBC1   ; total sectors (32-bit)
FatSize32   dd 0x00000FFB ; FAT size (32-bit)
Mirror      dw 0          ; mirror flags
FSVersion   dw 0          ; filesystem version
RootClus    dd 2          ; root cluster
FSInfo      dw 1          ; FSInfo sector
BackupBoot  dw 6          ; backup boot sector
times 12 db 0             ; reserved
DriveNum    db 0x80       ; drive number
ReservedNT  db 0          ; reserved NT
BootSig     db 0x29       ; boot signature
UUID        dd 0          ; volume ID
VolLabel    db "NO NAME    " ; volume label
FileSys     db "FAT32   " ; filesystem type
;
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
; Compiler placed next address at offset 194h !
    mov byte [0x7D94], 0x42 ; patch AH=42h ; More bytes behind now: mov byte [0x7D86], 0x42   ; patch byte at offset 0x186
;BAD;    mov byte [0x7D88], 0x42 ; patch AH=42h ; Two bytes behind now: mov byte [0x7D86], 0x42   ; patch byte at offset 0x186
;
DiskInit:
    xor bx, bx            ; bx=0
    xor eax, eax          ; eax=0
    mov dword [0x7C44], eax ; clear var
    mov ax, word [ReservSec]   ; load reserved
;    mov ax, [ReservSec]   ; load reserved
    add eax, dword [HiddSec]    ; add hidden
;    add eax, [HiddSec]    ; add hidden
    mov [0x7C48], eax     ; store var
    mov [0x7C4C], eax     ; store var
;
;mismatch!;    mov eax, byte [NumFATs] 	  ; load FAT count
;BAD;
    mov eax, [NumFATs] 	  ; load FAT count
    mul dword [FatSize32] ; multiply
    add [0x7C4C], eax     ; add to var
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
    push 0x2000           ; push seg
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
    mov dx, word [DriveNum] ; load drive, high byte is zero = [ReservedNT]
    push dx               ; push drive
    jmp 0x2000:0          ; far jump
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
    add eax, [0x7C48]     ; add base
    mov bx, 0x60          ; set ES
    mov es, bx            ; ES=0x60
    xor bx, bx            ; bx=0
    cmp eax, [0x7C44]     ; compare
    je FATEntryLoaded     ; already loaded
    mov [0x7C44], eax     ; store
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
    add eax, [0x7C4C]     ; add base
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
    push dword [SPT_Heads] ; push sectors/track & NumHeads in High word
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
    mov ax, 0x201         ; read sectors
    mov si, sp            ; SI=SP
    mov dl, [0x7C40]      ; load drive
    push es               ; save ES
    push ds               ; save DS
    int 0x13              ; BIOS disk read
    pop ax                ; restore ax
    mov ds, ax            ; restore DS
    pop bx                ; restore bx
    jb DiskError          ; jump if error
    lea bx, [bx+0x20]     ; next seg
    mov es, bx            ; set ES
    popa                  ; restore regs
    popad                 ; restore regs
    inc eax               ; next LBA
    ret                   ; return
;
DiskError:
    mov si, DiskErr       ; point to error msg
PrintErrorLoop:
    lodsb                 ; load char
    mov ah, 0x0E          ; set INT10 mode
    int 0x10			  ;
    cmp al, 0			  ;	compare 
    jne PrintErrorLoop    ; jump if not last char
Hang:
    jmp Hang              ; infinite loop
;
; -------------------------
; Strings
; -------------------------
NOGRLDR db "No"
GRLDR   db "GRLDR      ", 0
DiskErr db "Disk error", 0
;
times 510 - ($ - $$) db 0
db 0x55
db 0xAA
;
