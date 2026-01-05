## Compiling NASM Boot Codes

To compile your own NASM boot codes with this project:

1. [Fork this repository](https://github.com/mrexodia/x86-real-mode-bootloader/fork) (mrexodia's 'x86 Real Mode Bootloader')
2. Add your boot code files to your fork (like `MSBOOT.nasm`)
3. Add a build rule to the Makefile. The recipe line requires a TAB:
   ```makefile
   MSBOOT.bin: MSBOOT.nasm
   	nasm -f bin MSBOOT.nasm -o MSBOOT.bin
   ```
4. Open GitHub Codespaces, or use VS Code with Dev Containers
5. Compile in the terminal:
   ```bash
   make MSBOOT.bin
   ```

If downloading binaries from Codespaces doesn't work properly, try VS Code Desktop.
