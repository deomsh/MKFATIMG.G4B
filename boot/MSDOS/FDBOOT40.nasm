; Modified by deomsh in 2025
    ; Original code from MS-DOS 4.0 (c) Microsoft/IBM
    ; Licensed under the MIT License
;
; FDBOOT40.nasm, ported to NASM from FDBOOT.ASM with messages from FDISK5.SKL and repnz changed to rep.
; v0.1 (20251231), by deomsh
;
;       BOOT - IBM hard disk boot record             6/8/82
;
;
; This is the standard boot record that will be shipped on all hard disks. It contains:
;
; 1. Code to load (and give control to) the boot record for 1 of 4 possible
;    operating systems.
;
; 2. A partition table at the end of the boot record, followed by the required signature.
;
;
;ORG;_data   segment public
;ORG;        assume  cs:_data,ds:_data
;
		bits	16
;
        org		600h
;
Entrypoint:
;
        cli             		;no interrupts for now
;
;        db 33h, 0C0h                    ; xor ax, ax (MASM style)
;ORG;
        xor ax, ax
        mov ss, ax
        mov sp, 7c00h    		;new stack at 0:7c00
;
;        db 8Bh, 0F4h                    ; mov bp, si (MASM style)
;ORG;
        mov si, sp       		        ;where this boot record starts - 0:7c00
        push ax
        pop es          		;seg regs the same
        push ax
        pop ds
        sti             		;interrupts ok now
        cld
        mov di, 0600h    		;where to relocate this boot record to
        mov cx, 100h
;NEW;
		rep movsw				; correct according to pi: opcode 'F3 A5'. Repeats while C<>0
;ORG;        repne movsw     		;relocate to 0:0600 = incorrect/ correct by accident according to pi: opcode 'F2 A5'. Repeats while C<>0 and ZF=0
;ORG;        repnz movsw     		;relocate to 0:0600
;Commented out in ORG;       jmp entry2
;
		jmp 0:(0600h+entry2-$$)	; OKE: gives 'EA 1D 06 00 00'
;BAD;		jmp 0:(0600h+entry2)		; Gives 'EA 1D 1C 00 00'
;ORG;        db   0eah				; EA = ljump
;ORG;        dw   $+4,0				; 1D 06 00 00 = 0:0x61d
;0x0000000000000018:  EA 1D 06 00 00       ljmp        0:0x61d
;
entry2:
        mov si, tab          	;partition table
;OKE;        mov si, 0x7be                   ;partition table
;ORG;        mov si,offset tab      	;partition table
;0x000000000000001d:  BE BE 07             mov         si, 0x7be
        mov bl, 4        		;number of table entries
next:
        cmp byte [si], 80h  	;is this a bootable entry?
;ORG;        cmp byte ptr[si],80h  	;is this a bootable entry?
        je boot         		;yes
        cmp byte [si], 0    	;no, is boot indicator zero?
;ORG;        cmp byte ptr[si],0    	;no, is boot indicator zero?
        jne bad         		;no, it must be x"00" or x"80" to be valid
        add si, 16       		;yes, go to next entry
        dec bl
        jnz next
        int 18h         		;no bootable entries - go to rom basic
boot:
        mov dx, [si]     		;head and drive to boot from
        mov cx, [si+2]   		;cyl, sector to boot from
;
;        db 8Bh, 0EEh                    ; mov bp, si (MASM style)
;ORG;
        mov bp, si       		        ;save table entry address to pass to partition boot record
next1:
        add si, 16       		;next table entry
        dec bl          		;# entries left
        je tabok        		;all entries look ok
;ORG;        jz tabok        		;all entries look ok
        cmp byte [si], 0    	;all remaining entries should begin with zero
;ORG;        cmp byte ptr[si],0    	;all remaining entries should begin with zero
        je next1        		;this one is ok
bad:
        mov si, m1 				;oops - found a non-zero entry - the table is bad
;ORG;        mov si,offset m1 		;oops - found a non-zero entry - the table is bad
msg:
        lodsb           		;get a message character
        cmp al, 0
        je  hold
        push si
        mov bx, 7
        mov ah, 14
        int 10h         		;and display it
        pop si
        jmp msg         		;do the entire message
;
hold:   jmp hold        		;spin here - nothing more to do
tabok:
        mov di, 5        		;retry count
rdboot:
        mov bx, 7c00h    		;where to read system boot record
        mov ax, 0201h    		;read 1 sector
        push di
        int 13h         		;get the boot record
        pop di
        jae goboot      		;successful - now give it control
;ORG;        jnc goboot      		;successful - now give it control
;        db 33h, 0C0h                    ; xor ax, ax (MASM style)
;ORG;
        xor ax, ax       		        ;had an error, so
        int 13h         		;recalibrate
        dec di          		;reduce retry count
        jne rdboot      		;if retry count above zero, go retry
;ORG;        jnz rdboot      		;if retry count above zero, go retry
        mov si, m2 				;all retries done - permanent error - point to message,
;ORG;        mov si,offset m2 		;all retries done - permanent error - point to message,
        jmp msg          		;go display message and loop
goboot:
        mov si, m3 				;prepare for invalid boot record
;ORG;        mov si,offset m3 		;prepare for invalid boot record
;BAD;        mov di, tab
;ORG;
        mov di, 07dfeh
;BAD:        mov di, signa
        cmp word [di], 0aa55h 	;does the boot record have the
;ORG;        cmp word ptr [di],0aa55h ;does the boot record have the
                                ;    required signature?
        jne msg         		;no, display invalid system boot record message
;
;        db 8Bh, 0F5h                    ; mov si, bp (MASM style)
;ORG;
        mov si, bp       		        ;yes, pass partition table entry address
        db 0eah					; EA = ljump
        dw 7c00h,0				; 00 7C 00 00 = 0:0x7c00
;
m1		db "Invalid partition table", 0
;0x000000000000008b:  49                   
m2		db "Error loading operating system", 0
;0x00000000000000a3:  45 72                
m3		db "Missing operating system", 0
;0x00000000000000c2:  4D 69                
;
;ORG;include fdisk5.cl1
;
    times 1beh - ($ - $$) db 0 	; Pad remainder of boot sector with 0
;ORG;        org 7beh
tab:                    ;partition table
        dw 0,0          ;partition 1 begin
        dw 0,0          ;partition 1 end
        dw 0,0          ;partition 1 relative sector (low, high parts)
        dw 0,0          ;partition 1 # of sectors (low, high parts)
        dw 0,0          ;partition 2 begin
        dw 0,0          ;partition 2 end
        dw 0,0          ;partition 2 relative sector
        dw 0,0          ;partition 2 # of sectors
        dw 0,0          ;partition 3 begin
        dw 0,0          ;partition 3 end
        dw 0,0          ;partition 3 relative sector
        dw 0,0          ;partition 3 # of sectors
        dw 0,0          ;partition 4 begin
        dw 0,0          ;partition 4 end
        dw 0,0          ;partition 4 relative sector
        dw 0,0          ;partition 4 # of sectors
;
signa   db 55h,0aah     ;signature
;
;ORG;_data   ends
;ORG;        end
