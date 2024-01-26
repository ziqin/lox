CFLAGS=-std=c11 -Wall -Wextra

clox: main.o chunk.o memory.o debug.o value.o vm.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

main.o: common.h chunk.h debug.h vm.h

chunk.o: chunk.h memory.h

chunk.h: common.h value.h

memory.o: memory.h

memory.h: common.h

debug.o: chunk.h debug.h value.h

debug.h: chunk.h

value.o: memory.h value.h

value.h: common.h

vm.o: common.h debug.h vm.h

vm.h: chunk.h value.h

.PHONY: clean
clean:
	rm -rf *.o
