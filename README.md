## MKFATIMG.G4B v0.7 (20250910), by deomsh
<pre><code>Use 1:    MKFATIMG.G4B --size=n|--sectors=N|--CHS=C/H/S FILE switches
Switches: [/HDD|/FDD] [/FAT12[:nk]|/FAT16[:nk]|/FAT32[:nk]] [/V:VOLUMENAME]
          [/BOOT[:OS]] [/NOTACTIVE] [/LOG] [/CON] [/R] [/SPT:S] [/HEADS:H]
          [/ROOTENTR:n] [/RSRV:n] [/SPC:n] [/NOTRM] [/FHD] [/HIDDSEC:n]
          [/RDBASE:n] [/RDSIZE:n] [/Q|/T] [/Y]

Use 2:    MKFATIMG.G4B FILE /F:floppysize switches
Switches: [/V:VOLUMENAME] [/BOOT[:OS]] [/FAT16|/FAT32] [/CON] [/RSRV:n]
          [/ROOTENTR:n] [/NOTRM] [/FHD] [/HIDDSEC:n] [/RDBASE:n] [/Q|/T] [/Y]
          
Use 3:    MKFATIMG.G4B FILE /D:disktype switches
Switches: [/SPT:S] [/HEADS:H] [/LOG] [/BOOT[:OS]] [/NOTACTIVE] [/R] [/CON]
          [/FAT12|/FAT16|/FAT32] [/V:VOLUMENAME] [/ROOTENTR:n] [/RSRV:n]
          [/RDBASE:n] [/Q|/T] [/Y]
          
Use 4:    MKFATIMG.G4B FILE /COPY [/CON] [/Q|/T] [/Y]
          
Help:     MKFATIMG.G4B [/?|--?|?]

Features
Make Floppy Disk or Hard Drive Images with FAT filesystem (image-creation on FAT device only)
Make full FAT filesystem on Grub4dos Ram-Disk: (rd)
Always preview with fat info on (rd)
Size can be set in (k/m/g) bytes, in sectors or with C/H/S
Geometry auto, switches to set Number of Heads/ Sectors per Track
IMG-type auto (below 4MB floppy, above MBR-harddrive) or forced with switch
FAT-type auto (<16MB: FAT12, <512MB: FAT16, above FAT32) or forced with switch
Always MS-DOS uuid written
FAT-Volumename written with switch (with valid time/ date)
Boot Code with switch (many Boot Codes onboard, default MS-DOS 7)
Switch to force Logical Partition (Boot Code can be added)
Files can be copied to grub4dos Ram-Disk before making image-file
Quiet operation possible with switch
For maximum compatibily on Hard Drive always one empty test cylinder is set
Switch to make predefined floppies (more than 60 in range 4KB-240MB)
Switch to make IBM PC AT/ PS/2 Harddisk Types 1-14/ 16-44 
Switch to set Sectors per Cluster (max 128)
Switch to set Reserved Sectors (>=32 on FAT32)
Switch to set Root Directory Entries (FAT12/ FAT16)
Switch to set reserved Root Directory Entries (FAT32)
Experimental switch to force contiguous Image
Experimental switch to make partial Image in Ram-Disk (max 2048TB)
More switches available for special purposes
Long File Names: Libraries FATLFN.LLL and ATTRIBFT.LLL needed in
 same locations as Grubutil FAT (if not found 8+3 Short File Names only)

Compatibility
Grub4dos version 2017-08-30 or higher
Compatible with Grub4dos for UEFI
Grubutil FAT needed in (bd)/%~d0: /, /grub, /boot/grub, /g4dll or current root
 Use FAT version from 2023, april or later for writing images >2GB

Loosely Linked Libraries: 
FATLFN.LLL (>=v0.3): https://github.com/deomsh/FATLFN
ATTRIBFT.LLL (>=v0.9): https://github.com/deomsh/ATTRIBFT.LLL

To copy MS-DOS system files to (mkfatimg.g4b-) Ram-Disk first:
Use COPYSYS.G4B: https://github.com/deomsh/COPYSYS.G4B
Copy Ram-Disk afterwards to Image file (see 'Use 4' above)</code></pre> 

### HISTORY
V0.7:  
NEW: with switch /BOOT on floppy always (newly added) MSWIN4.0 Boot Code, includes WINBOOT.SYS (FAT12/ FAT16 only)  
NEW: switch /HIDDSEC:n to set (fake) number of hidden sectors on floppy  
NEW: max size 2048TB, on (partial) ram-disk only (rounded down)  
NEW: partition/ volume size above 1024 cylinders allowed (partition type auto 0E/0C)  
NEW: max 1025 cylinders allowed on hard disk (one for mandatory test cylinder)  
NEW: cleaning memory with dd and check with cmp (max 3GB)  
NEW: switch /RDSIZE:n make partial ram-disk in memory (max 4GB)  
NEW: switch /SPC:n max 128 sectors per cluster  
NEW: switch /FAT32:nk or /FAT16:nk or /FAT12:nk to align FAT (FAT32/16 n=1k-16k, FAT12 n=1-2k)  
NEW: space(s) after Y/ N in dialogs auto-removed  
NEW: max about 7400 Directory Root Entries for grub4dos' uuid to mount partition => safe(r) max set to 4096  
CHANGE: Default rdbase changed to 290m (seems better with switch /LOG and with smaller sizes)  
CHANGE: fat info screen gives uuid/ vol with switch /NOTRM  
BETTER: alignment of Help (in grub4dos textmode)  
BUGFIX: no exit with (rd)/ - / was ignored (harmless)  
BUGFIX: fat driver echo with target (rd) using switch /T (harmless)  
BUGFIX: on FAT16 max 65624 clusters (instead of 65524 => Microsoft White Paper)  

V0.6:  
NEW: FAT (and FATLFN.LLL + ATTRIBFT.LLL if used) not unloaded afterwards if already loaded with insmod  

V0.5:  
Switch '/Y' to suppress Make/ Overwrite dialogs (use with /Q: silent overwrite file too)  
BUGFIX: if ATTRIBFT.lll/ FATLFN.LLL is missing, working again for 8.3 Short File names  

V0.4:  
Long File Names supported if Libraries are present  
Switch '/ROOTENTR:n' with predefined floppies too (switch '/F:floppysize')  
Rootentries set to 240 on all predefined floppies >1440k <=3840k for compatibility with MS-DOS <=6.22  
Lowest base memory of Ram-Disk: 64m, Default now 256m  
OEM-name added to fat-info  
New Boot Codes: MSDOS33 and MSDOS40 (in PBR)  

V0.3: first published version  

### SCREENSHOTS
![MKFATIMG G4B v0 7 VERSION and TEXTSTAT](https://github.com/user-attachments/assets/b3609a96-110d-4c2d-9420-011837b23d93)

#### Small Help:
![MKFATIMG G4B v0 7 SmallHelp](https://github.com/user-attachments/assets/f4ead1fd-a6ec-4ef6-95f7-389fe45ddfcd)

#### Example of making Image with Argument --size=n and Auto-settings (here Geometry, HDD and FAT16)
![MKFATIMG G4B v0 7 --size=512m (hd0,0)-MYIMAGE IMG (HDD FAT16)](https://github.com/user-attachments/assets/694817ab-2392-44d7-bc83-dfd746bf2ecd)

#### Example of making preset Floppy Disk Image with switch Volume-name and Contiguous with switch /CON 
![MKFATIMG G4B v0 7 (hd0,0)-MyImage IMG -F=2880 -V=F2880KIMAGE (FDD FAT12, contiguous)](https://github.com/user-attachments/assets/45a8c0ec-b7d6-453b-82fb-3d7f831c75e6)

#### Example of making Bootable Image with Argument --size=n and force FAT16 with switch (Auto-settings Geometry, HDD and MS-DOS 7 Boot Code here)
![MKFATIMG G4B --size=2g -H2GFAT16 IMG -V=HDD2GBFAT16 -FAT16 -BOOT](https://github.com/user-attachments/assets/755e7241-b7e4-4ac1-bbb7-500bc16356e5)

#### Example of making Image with Argument --sectors and Long File Name on Root and Auto-settings (here Geometry, HDD and FAT16)
![MKFATIMG G4B --sectors=0x100000 ''-Image with max 1m Sectors IMG''](https://github.com/user-attachments/assets/62178710-1cf2-4853-b161-c51b45356768)

#### Example of making preset 'IBM PC AT/ PS/2' (Hard) Disk-Type iImage with changed Geometry and set Bootable (default MS-DOS 7) and forced Not-active
![MKFATIMG G4B -HDDTYPE IMG -D=25 -HEADS=16 -SPT=63 -BOOT -NOTACTIVE](https://github.com/user-attachments/assets/8b99e763-ef3c-49c8-8a21-62666b8c3ba6)

#### Example of making El Torito Boot Floppy with GRUB Boot Code on Ram-Disk and image-production afterwards with switch /COPY - more verbose with switch /T
![MKFATIMG G4B --CHS=24-2-15 (rd) -BOOT=GRUB -T with (hd1,0)-BOOTCD IMG -COPY](https://github.com/user-attachments/assets/9a2bd88b-b16b-4148-a27e-58630fd4c3b8)

#### Example of making partial max CHS-Partition on Ram-Disk with switch /RDSIZE:n (Auto-setting HDD with one unused test cylinder after partition, still FAT32 Partition type 0B)
![MKFATIMG G4B --CHS=1025-255-63 (rd) -V=MYMAXCHS8GB -RDSIZE=64m](https://github.com/user-attachments/assets/f52299eb-4512-4cbb-997a-45b40f0153de)

#### Output of FATINFO.G4B and test with FATINFO.G4B /T on FAT32 partition from last example above
![FATINFO G4B (rd,0) after MKFATIMG G4B --CHS=1025-255-63 (rd) -V=MYMAXCHS8GB -RDSIZE=64m](https://github.com/user-attachments/assets/2dcc1af2-d906-4a33-8d85-a3e006743f50)
![FATINFO G4B (rd,0) -T after MKFATIMG G4B --CHS=1025-255-63 (rd) -V=MYMAXCHS8GB -RDSIZE=64m](https://github.com/user-attachments/assets/501b0a85-9d7d-46c2-9f2e-93e1a0842e64)
