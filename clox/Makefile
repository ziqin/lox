CFLAGS=-std=c11 -Wall -Wextra

clox: main.o chunk.o memory.o debug.o value.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

main.o: common.h chunk.h debug.h

chunk.o: chunk.h memory.h

chunk.h: common.h value.h

memory.o: memory.h

memory.h: common.h

debug.o: chunk.h debug.h value.h

debug.h: chunk.h

value.o: memory.h value.h

value.h: common.h

.PHONY: clean
clean:
	rm -rf *.o
