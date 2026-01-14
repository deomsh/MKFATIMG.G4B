## MKFATIMG.G4B v0.8.2 (20260114), by deomsh
<pre><code>Use 1:    MKFATIMG.G4B --size=n|--sectors=N|--CHS=C/H/S FILE switches
Switches: [/HDD[:nk|:nm]|/FDD] [/FAT12[:nk]|/FAT16[:nk]|/FAT32[:nk|:nm]]
          [/V:VOLUMENAME] [/BOOT[:OS]] [/SPC:n] [/RSRV:n] [/ROOTENTR:n]
          [/HEADS:H] [/SPT:S] [/CON] [/RDBASE:n] [/RDSIZE:n] [/Q|/T] [/Y]
          [/NOTACTIVE] [/LOG] [/R] [/NOTRM] [/FHD] [/HIDDSEC:n]

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
Make Floppy Disk or Hard Disk Images with FAT filesystem (Image-creation on FAT Device only)
Make full FAT filesystem on Grub4dos Ram-Disk: (rd)
Always preview with fat-info on (rd)
Size can be set in (k/m/g) bytes, in sectors or with C/H/S
Geometry auto, switches to set Number of Heads/ Sectors per Track
IMG-type auto (below 4MB Floppy, above MBR-Hard Drive) or forced with switch
FAT-type auto (<16MB: FAT12, <512MB: FAT16, above FAT32) or forced with switch
Always MS-DOS uuid written
FAT Volume Name written with switch (with valid time/ date)
Boot Code with switch (many Boot Codes onboard, default MS-DOS 7)
Switch to force Logical Partition (Boot Code can be added)
Files can be copied to Grub4dos Ram-Disk before making Image-file
Quiet operation possible with switch
For maximum compatibily on Hard Drive always one empty test cylinder is set
Switch to make predefined Floppies (more than 60 in range 4KB-240MB)
Switch to make IBM PC AT/ PS/2 Hard Disk Types 1-14/ 16-44 
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

To copy MS-DOS System Files to Ram-Disk first:
Use COPYSYS.G4B: https://github.com/deomsh/COPYSYS.G4B
Copy Ram-Disk afterwards to Image file (see 'Use 4' above)</code></pre> 

### HISTORY
V0.8.2:  
BUGFIX: always set /HDD with switch /LOG and incompatibility with other switches  
BUGFIX: calculation of Sectors Per FAT12  

V0.8.1:  
BUGFIX: IBMBIO.COM not booting on floppie with /BOOT:MSDOS20  

V0.8:  
NEW: Boot Codes for MSDOS2/3, compatible with Grubutil 'fat'  
NEW: all Boot Codes for MS-DOS based on MSDOS 4.00/ Freedos/ Grldr/ Syslinux  
NEW: switch /HDD:nk or /HDD:nm to align partition, nk=32k/64k/128k/256k/512k or nm=1m/2m/4m/8m (experimental)  
CHANGE: better FAT32 calculations  
CHANGE: no E5-vollabel  
BUGFIX: sometimes wrong Sectors per FAT with switch /rootentr:1263 and above  
BUGFIX: sometimes negative padding sectors on FAT32  
BUGFIX: /D:34+ sector echo HEX  
BUGFIX: --sector=n bad echo  
BUGFIX: Reserved sectors up to 0xFFFF instead below 0x1000 [typo?]  

V0.7:  
NEW: with switch /BOOT on Floppy always (newly added) MSWIN4.0 Boot Code, includes WINBOOT.SYS (FAT12/ FAT16 only)  
NEW: switch /HIDDSEC:n to set (fake) number of hidden sectors on Floppy
NEW: max size 2048g, on (partial) Ram-Disk only (size rounded down)  
NEW: partition/ volume --size=n/ --sectors=n leading to more than 1024 cylinders allowed (partition type auto 0E/0C)  
NEW: with --CHS=C/H/S max 1025 cylinders allowed on hard disk (one for mandatory test cylinder)  
NEW: cleaning memory with dd and check with cmp (max 3GB)  
NEW: switch /RDSIZE:n to make partial Ram-Disk in memory (n: max 4GB or auto with n=MAX)  
NEW: switch /SPC:n now max 128 sectors per cluster (max was 64)  
NEW: switch /FAT32:nk or /FAT16:nk or /FAT12:nk to align FAT (FAT32/16 n=1k-16k, FAT12 n=1-2k)  
NEW: space(s) after Y/ N in dialogs auto-removed  
CHANGE: Default rdbase changed to 290m (seems better with switch /LOG and with smaller sizes)  
CHANGE: fat info screen gives uuid/ vol with switch /NOTRM  
BETTER: alignment of Help (in grub4dos textmode)  
BUGFIX: no exit with (rd)/ - / was ignored (harmless)  
BUGFIX: fat driver echo with target (rd) using switch /T (harmless)  
BUGFIX: on FAT16 max 65624 clusters (instead of 65524 => Microsoft White Paper)  
BUGFIX: max about 7400 Directory Root Entries for Grub4dos' uuid/ vol to mount partition => safe(r) max set to 4096  

V0.6:  
NEW: FAT (and FATLFN.LLL + ATTRIBFT.LLL if used) not unloaded afterwards if already loaded with insmod  

V0.5:  
Switch '/Y' to suppress Make/ Overwrite dialogs (use with /Q: silent overwrite file too)  
BUGFIX: if ATTRIBFT.lll/ FATLFN.LLL is missing, working again for 8.3 Short File names  

V0.4:  
Long File Names supported if Libraries are present  
Switch '/ROOTENTR:n' with predefined Floppies too (switch '/F:floppysize')  
Rootentries set to 240 on all predefined Floppies >1440k <=3840k for compatibility with MS-DOS <=6.22  
Lowest base memory of Ram-Disk: 64m, Default now 256m  
OEM-name added to fat-info  
New Boot Codes: MSDOS33 and MSDOS40 (in PBR)  

V0.3: first published version  

### SCREENSHOTS
![MKFATIMG G4B v0 8 VERSION and TEXTSTAT](https://github.com/user-attachments/assets/ebaddb57-3194-4be0-9fce-1d210bab1670)

#### Small Help:
![MKFATIMG G4B Small-help v0 8](https://github.com/user-attachments/assets/c66b6196-8b65-41b9-9daf-8fe6e5f96a69)

#### Example of making Image with Argument --size=n and Auto-settings (here Geometry, HDD and FAT16)
![MKFATIMG G4B v0 8 --size=512m (hd0,0)-MYIMAGE IMG (HDD FAT16)](https://github.com/user-attachments/assets/9b906c1b-6231-4505-9a2b-7cc346502102)

#### Example of making preset Floppy Disk Image with switch Volume-name and Contiguous with switch /CON 
![MKFATIMG G4B v0 7 (hd0,0)-MyImage IMG -F=2880 -V=F2880KIMAGE (FDD FAT12, contiguous)](https://github.com/user-attachments/assets/45a8c0ec-b7d6-453b-82fb-3d7f831c75e6)

#### Example of making Bootable Image with Argument --size=n and force FAT16 with switch (Auto-settings Geometry, HDD and MS-DOS 5-7 Boot Code here)
![MKFATIMG G4B --size=2g -H2GFAT16 IMG -V=HDD2GBFAT16 -FAT16 -BOOT](https://github.com/user-attachments/assets/4c0fda97-fb09-44f8-a83e-ecf3721a7830)

#### Example of making Image with Argument --sectors and Long File Name on Root and Auto-settings (here Geometry, HDD and FAT16)
![MKFATIMG G4B --sectors=0x100000 &#39;&#39;-Image with max 1m Sectors IMG&#39;&#39;](https://github.com/user-attachments/assets/eee45de1-91cd-4b34-8362-6768551ea695)

#### Example of making preset 'IBM PC AT/ PS/2' (Hard) Disk-Type Image with changed Geometry and set Bootable (default MS-DOS 5-7) and forced Not-active
![MKFATIMG G4B -HDDTYPE IMG -D=25 -HEADS=16 -SPT=63 -BOOT -NOTACTIVE](https://github.com/user-attachments/assets/43d1132f-8e49-47c8-ad32-2470ecdcbade)

#### Example of making El Torito Boot Floppy with GRUB Boot Code on Ram-Disk and Image-production afterwards with switch /COPY - more verbose with switch /T
![MKFATIMG G4B --CHS=24-2-15 (rd) -BOOT=GRUB -T with (hd1,0)-BOOTCD IMG -COPY](https://github.com/user-attachments/assets/89465e94-95b6-43f3-829e-18aa8518f284)

#### Example of making partial max CHS-Partition on Ram-Disk with switch /RDSIZE:n (Auto-setting HDD with one unused test cylinder after partition, still FAT32 Partition type 0B)
![MKFATIMG G4B --CHS=1025-255-63 (rd) -V=MYMAXCHS8GB -RDSIZE=64m](https://github.com/user-attachments/assets/f52299eb-4512-4cbb-997a-45b40f0153de)

#### Output of FATINFO.G4B and test with FATINFO.G4B /T on FAT32 partition from last example above
![FATINFO G4B (rd,0) after MKFATIMG G4B --CHS=1025-255-63 (rd) -V=MYMAXCHS8GB -RDSIZE=64m](https://github.com/user-attachments/assets/2dcc1af2-d906-4a33-8d85-a3e006743f50)
![FATINFO G4B (rd,0) -T after MKFATIMG G4B --CHS=1025-255-63 (rd) -V=MYMAXCHS8GB -RDSIZE=64m](https://github.com/user-attachments/assets/501b0a85-9d7d-46c2-9f2e-93e1a0842e64)

#### Example of making partial max LBA-Partition on Ram-Disk with switch /RDSIZE:n (Auto-setting HDD with one unused test cylinder after partition, now FAT32 Partition type 0C)
![MKFATIMG G4B v0 8 --size=2048g (rd0) -V=MYMAXLBA2TB -BOOT -RDSIZE=64m,728m](https://github.com/user-attachments/assets/6b7c3df3-52e8-49b4-812b-22d34964954a)

#### Example of using a partial, aligned LBA-Partition on Ram-Disk with switch /RDSIZE:n to partition and format a fully prepared USB Flash-Drive using dd
<img width="1049" height="930" alt="SANDISK 64GB USB original with DiskManagement and Settings in VMWare" src="https://github.com/user-attachments/assets/10955e79-d318-4872-86e7-88b0f9fca800" />
<img width="1398" height="1199" alt="Prepare SANDISK 64GB USB with new FAT32 partition and MSDOS71 System and basic files" src="https://github.com/user-attachments/assets/68a153fb-740a-407a-b4ea-16688ad55656" />
<img width="1392" height="915" alt="SANDISK 64GB USB with new FAT32 partition booted on 960GM-GS3" src="https://github.com/user-attachments/assets/df9f7bf2-baca-4a68-95fe-abecedd539c5" />







