# Untitled-Nes-Project
Nintendo Entertainment System Project


This is a project I'm working on to build an NES game. We'll see where it goes.

The .NES is the latest build I've uploaded of the playable rom.

.ASM is the assembly language file

To compile this, I use CC65

https://cc65.github.io

The default NES linker only assigns 26 bytes to the zero page instead of the full 256. Change the config header to fix this problem or it'll throw up an error
