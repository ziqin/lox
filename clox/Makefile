CFLAGS=-std=c11 -Wall -Wextra -Werror -O2

clox: main.o chunk.o memory.o debug.o value.o vm.o compiler.o scanner.o object.o table.o
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

main.o: common.h chunk.h.gch debug.h.gch vm.h.gch

chunk.o: chunk.h.gch memory.h.gch vm.h.gch

chunk.h.gch: chunk.h common.h value.h.gch
	$(CC) $(CFLAGS) -o $@ $<

memory.o: compiler.h.gch memory.h.gch object.h.gch vm.h.gch debug.h.gch

memory.h.gch: memory.h common.h object.h.gch
	$(CC) $(CFLAGS) -o $@ $<

debug.o: chunk.h.gch debug.h.gch object.h.gch value.h.gch

debug.h.gch: debug.h chunk.h.gch
	$(CC) $(CFLAGS) -o $@ $<

value.o: object.h.gch memory.h.gch value.h.gch

value.h.gch: value.h common.h
	$(CC) $(CFLAGS) -o $@ $<

vm.o: chunk.h.gch common.h compiler.h.gch debug.h.gch object.h.gch memory.h.gch vm.h.gch

vm.h.gch: vm.h chunk.h.gch object.h.gch table.h.gch value.h.gch
	$(CC) $(CFLAGS) -o $@ $<

compiler.o: chunk.h.gch common.h compiler.h.gch memory.h.gch object.h.gch scanner.h

compiler.h.gch: compiler.h vm.h.gch object.h.gch
	$(CC) $(CFLAGS) -o $@ $<

scanner.o: common.h scanner.h

object.o: chunk.h.gch memory.h.gch object.h.gch table.h.gch value.h.gch vm.h.gch

object.h.gch: object.h common.h chunk.h.gch table.h.gch value.h.gch
	$(CC) $(CFLAGS) -o $@ $<

table.o: memory.h.gch object.h.gch table.h.gch value.h.gch

table.h.gch: table.h common.h value.h.gch
	$(CC) $(CFLAGS) -o $@ $<

.PHONY: clean
clean:
	rm -f clox *.o *.gch
