del target/*.gb
del target/*.o

rgbasm -i lib -o target/main.o src/main.asm
rgblink -o target/hello-world.gb target/main.o

rgbfix -v -p 0 target/hello-world.gb