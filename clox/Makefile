CFLAGS=-std=c11 -Wall -Wextra -Werror

clox: main.o chunk.o memory.o debug.o value.o vm.o compiler.o scanner.o object.o table.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

main.o: common.h chunk.h debug.h vm.h

chunk.o: chunk.h memory.h vm.h

chunk.h: common.h value.h
	@touch $@

memory.o: compiler.h memory.h object.h vm.h debug.h

memory.h: common.h object.h
	@touch $@

debug.o: chunk.h debug.h object.h value.h

debug.h: chunk.h
	@touch $@

value.o: object.h memory.h value.h

value.h: common.h
	@touch $@

vm.o: chunk.h common.h compiler.h debug.h object.h memory.h vm.h

vm.h: chunk.h object.h table.h value.h
	@touch $@

compiler.o: chunk.h common.h compiler.h memory.h object.h scanner.h

compiler.h: vm.h object.h
	@touch $@

scanner.o: common.h scanner.h

object.o: chunk.h memory.h object.h table.h value.h vm.h

object.h: common.h chunk.h table.h value.h
	@touch $@

table.o: memory.h object.h table.h value.h

table.h: common.h value.h
	@touch $@

.PHONY: clean
clean:
	rm -rf *.o
