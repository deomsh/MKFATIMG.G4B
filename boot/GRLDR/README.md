# History of PBR Bootcodes  

## GRLDRFAT  
v8.0: first published version, port of original to nasm (FAT12/ FAT16)  
CHANGES: FatRead based FATsz16 (number of Sectors Per FAT) instead on TotSec16.  

## GRLDRFAT_MOD  
v9.14: first published version, based on GRLDRFAT (FAT12/ FAT16)  
NEW: FAT12/ FAT16 recognition based on number of Total Clusters; dot's during loading (visual and stability issues with USB-legacy boot on N68-chipset); Press Key2reboot added on error; Print of INT13 AH return-code on disk error; moved Data Area.  

## GRLDRF32  
v9.0: first published version, port of original to nasm (FAT32)  

## GRLDRF32_MOD  
v11.11: first published version, based on GRLDRF32 (FAT32)  
NEW: dot's during loading (visual and stability issues with USB-legacy boot on N68-chipset); Press Key2reboot added on error; Print of INT13 AH return-code on disk error; moved Data Area.  

## MSBOOT71  
v0.26: first published version, based on GRLDRF32 (FAT32)  
Features: writes after every sector read a dot on screen; error messages on new lines; address bp+0x1EE carries address of start of Message Table (not needed in MS-DOS v6 LDOS load protocol)  

## MSBOOT70  
v0.3.1: reverted to jump EB3C90 because of USB-access problems on several Windows 9x USB stack's  
v0.3.0: first published version, based on GRLDRFAT (FAT12/ FAT16)  
Features: uses INT13h AH=42h extensions if available, will search for WINBOOT.SYS too if IO.SYS is not found; writes after every sector read a dot on screen; error messages on new lines; address bp+0x1EE carries address of start of Message Table (MS-DOS v7 LDOS load protocol)  

## MSBOOT60  
v0.9.1: reverted to jump EB3C90 because of USB-access problems on several Windows 9x USB stack's  
v0.9.0: first published version, modification of MSBOOT70 (FAT12/ FAT16)  
Features: can de used on MS-DOS4xx-MS-DOS6xx under the usual conditions; uses INT13h AH=42h extensions if available (usefull with certain disk geometry in case not good recognized by legacy (USB) BIOS - MS-DOS6xx and lower will not use these extension as such); writes after every sector read a dot on screen; error messages on new lines; address bp+0x1EE carries address of start of Message Table (not needed in MS-DOS v6 LDOS load protocol)  
