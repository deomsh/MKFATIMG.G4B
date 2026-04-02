# History of MBR Bootcodes  

## FDBOOT  
v0.1: First published version  
Features: original MASM-version ported to NASM and made binary identical  

## FDBOOT40  
v0.1: First published version from FDBOOT  
Features: full NASM code (not binary identcal), F2 => F3 change  

# History of FAT12/ FAT16 PBR Bootcodes

## MSBOOT
First published version
Features: MSBOOT.ASM converted to NASM and made binary identical  

## MSBOOT40  
First published version, based on MSBOOT  
Features: full NASM code (not binary identcal)  

## MSBOOT50  
v0.2: First published version, based on MSBOOT  
Features: free placement of IO.SYS' clusters with MS-DOS5xx-MS-DOS6xx (NOT of root-directory entries AND first three sectors must still be contiguous); use with MS_DOS 4xx is possible under the usual assumptions    

## MSBOOT5070  
v0.13: First published version, based on MSBOOT  
Features: loads MS-DOS v7 if first four sectors of IO.SYS are contiguous (order of System File root-directory entries must same as with MS-DOS v4-v6); address bp+0x1EE carries address of start of Message Table (MS-DOS v7 LDOS load protocol); further same as MSBOOT50  

## MSBOOT30  
v30.5.7: first published version, based on MSBOOT  
Features: Working version, loading all sectors of IO.SYS (load protocol unknown); default Disk Signature at bp+0x1FD = 80h; compatible with Grubutil 'fat'  

## MSBOOT20  
First published version, based on MSBOOT30  
Features: Working version, loading all sectors of IBMBIO.COM; default Disk Signature at bp+0x1FD = 80h; compatible with Grubutil 'fat'  
