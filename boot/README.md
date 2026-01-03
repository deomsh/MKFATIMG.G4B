## Compiling NASM Boot Codes

To compile your own NASM boot codes from this project:

1. [Fork this repository](https://github.com/mrexodia/x86-real-mode-bootloader/fork)

2. Add your boot code files to your fork (e.g., `MSBOOT.nasm`)

3. Add a build rule to the Makefile (must use TAB indentation):
   ```makefile
   MSBOOT.bin: MSBOOT.nasm
   	nasm -f bin MSBOOT.nasm -o MSBOOT.bin
   ```

4. Open GitHub Codespaces or use VS Code with Dev Containers

5. Compile in the terminal:
   ```bash
   make MSBOOT.bin
   ```

**Note**: If you have trouble downloading binaries from Codespaces, use VS Code Desktop instead.
