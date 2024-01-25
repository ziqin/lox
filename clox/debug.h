#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

void disassembleChunk(Chunk* chunk, const char* name);

// Disassembles the instruction at the given offset.
// Returns the offset of the next instruction.
int disassembleInstruction(Chunk* chunk, int offset);

#endif
