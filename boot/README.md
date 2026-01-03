To compile used NASM Boot Codes make a Fork of: https://github.com/mrexodia/x86-real-mode-bootloader

Upload Boot Code to you Fork and add lines for making FILE.bin to Makefile first (all case-sensitive!)
  Example: 
    MSBOOT.bin: MSBOOT.nasm
      nasm -f bin MSBOOT.nasm -o MSBOOT.bin

To compile use CODESPACE

Compile in Terminal with: make FILE.bin
  Example: make MSBOOT.bin

If downloading binary is a problem, open first in VS Code Desktop.




