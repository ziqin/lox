CFLAGS=-std=c11 -Wall -Wextra -Werror

clox: main.o chunk.o memory.o debug.o value.o vm.o compiler.o scanner.o object.o table.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

main.o: common.h chunk.h debug.h vm.h

chunk.o: chunk.h memory.h

chunk.h: common.h value.h

memory.o: memory.h vm.h

memory.h: common.h object.h

debug.o: chunk.h debug.h value.h

debug.h: chunk.h

value.o: object.h memory.h value.h

value.h: common.h

vm.o: common.h compiler.h debug.h object.h memory.h vm.h

vm.h: chunk.h table.h value.h

compiler.o: common.h compiler.h scanner.h

compiler.h: vm.h object.h

scanner.o: common.h scanner.h

object.o: memory.h object.h table.h value.h vm.h

object.h: common.h value.h

table.o: memory.h object.h table.h value.h

table.h: common.h value.h

.PHONY: clean
clean:
	rm -rf *.o
