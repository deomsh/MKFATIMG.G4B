# History  

## GRLDRFAT  
v8.0 First published version, port of original to nasm  
CHANGES: FatRead based FATsz16 (number of Sectors Per FAT) instead on TotSec16.  

## GRLDRFAT_MOD  
v9.14 First published version, based on GRLDRFAT  
NEW: FAT12/ FAT16 recognition based on number of Total Clusters; dot's during loading (visual and stability issues with USB-legacy boot on N68-chipset); Press Key2reboot added on error; Print of INT13 AH return-code on disk error; moved Data Area.  

## GRLDRF32  
v9.0 First published version, port of original to nasm  

## GRLDRF32_MOD  
v11.11 First published version, based on GRLDRF32  
NEW: dot's during loading (visual and stability issues with USB-legacy boot on N68-chipset); Press Key2reboot added on error; Print of INT13 AH return-code on disk error; moved Data Area.  

## MSDOOT60  
v0.9.1: reverted to jump EB3C90 because of USB-access problems on Windows 95 OSR2  
v0.9.0: First published version, modification of MSBOOT70  

## MSBOOT70  
v0.3.1: reverted to jump EB3C90 because of USB-access problems on Windows 95 OSR2  
v0.3.0 First published version, based on GRLDRFAT  

## MSBOOT71  
v0.26 First published version, based on GRLDRF32  
