GB_EMULATOR_PATH = /Applications/SameBoy.app/Contents/MacOS/SameBoy
ROM_PATH = target/hello-world.gb

ASM = rgbasm
ASM_FLAGS = -i lib

LINKER = rgblink

FIXER = rgbfix
FIXER_FLAGS = -v -p 0

all: clean hello-world.gb

run: all
	$(GB_EMULATOR_PATH) $(ROM_PATH)

main.o:
	$(ASM) $(ASM_FLAGS) -o target/main.o src/main.asm

hello-world.gb: main.o
	$(LINKER) -o target/hello-world.gb target/main.o
	$(FIXER) $(FIXER_FLAGS) $(ROM_PATH) 
clean:
	rm -f target/*

.PHONY: all run clean
