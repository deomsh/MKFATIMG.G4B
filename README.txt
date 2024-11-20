MKFATIMG.G4B v0.4 (20241120), by deomsh
Use 1:    MKFATIMG.G4B --size=n|--sectors=N|--CHS=C/H/S FILE switches
Switches: [/HDD|/FDD] [/FAT12|/FAT16|/FAT32] [/V:VOLUMENAME] [/BOOT[:OS]]
          [/NOTACTIVE] [/LOG] [/CON] [/R] [/SPT:S] [/HEADS:H] [/ROOTENTR:n]
          [/RSRV:n] [/SPC:N] [/NOTRM] [/FHD] [/RDBASE:n] [/Q|/T]
Use 2:    MKFATIMG.G4B FILE /F:floppysize switches
Switches: [/V:VOLUMENAME] [/BOOT[:OS]] [/FAT16|/FAT32] [/CON] [/RSRV:n]
          [/ROOTENTR:n] [/FHD] [/RDBASE:n] [/Q|/T]
Use 3:    MKFATIMG.G4B FILE /D:disktype switches
Switches: [/SPT:S] [/HEADS:H] [/LOG] [/BOOT[:OS]] [/NOTACTIVE] [/R] [/CON]
          [/FAT12|/FAT16|/FAT32] [/V:VOLUMENAME] [/ROOTENTR:n] [/RSRV:n]
          [/RDBASE:n] [/Q|/T]
Use 4:    MKFATIMG.G4B FILE /COPY [/CON] [/Q|/T]
Help:     MKFATIMG.G4B /?|--?|?


Features
Make floppie or harddrive images with FAT filesystem (image-creation on FAT device only)
Make full FAT filesystem on grub4dos ram-disk: (rd)
Always preview with fat-info on (rd)
Size can be set in (k/m/g) bytes, in sectors or with C/H/S
Geometry auto, switches to set Number of Heads/ Sectors per Track
IMG-type auto (below 4MB floppy, above MBR-harddrive) or with switch
FAT-type auto (<16MB: FAT12, <512MB: FAT16, above FAT32) or with switch
Always MS-DOS uuid written
FAT-Volumename written with switch (has valid time/ date)
Boot-code with switch (many boot-codes onboard, default MS-DOS)
Switch to set logical partition (boot-code can be added)
Files can be copied to grub4dos ram-disk before making image-file
Quiet operation possible with switch
For maximum compatibily on hardrive always one empty test cylinder set
Switch to make predefined floppies (more than 60 in range 4KB-240MB)
Switch to make IBM PC AT/ PS/2 Harddisk Types 1-14/ 16-44 
Switch to set Sectors per Cluster
Switch to set Reserved Sectors (>=32 on FAT32)
Switch to set Root Directory Entries (FAT12/ FAT16)
Experimental switch to force contiguous image
More switches available for special purposes
Copy files to ram-disk using Grubutil FAT with target on
 Floppie: (rd)/
 Hardrive: (rd,0)/ or (rd,4)/ on grub4dos; (rd)/ on grub4dos for UEFI
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

History
V0.4:
Long File Names supported if Libraries are present
Switch '/ROOTENTR:n' with predifined floppies too (switch '/F:floppysize')
Rootentries set to 240 on all predefined floppies >1440k <=3840k for
 compatibility with MS-DOS <=6.22
Lowest base memory of ram-disk: 64m, Default now 256m
OEM-name added to fat-info
New bootcodes: MSDOS33 and MSDOS40 (in PBR)

V0.3: first published version
