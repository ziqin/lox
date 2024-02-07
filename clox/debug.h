#ifndef clox_debug_h
#define clox_debug_h

#include "chunk.h"

void disassembleChunk(const Chunk* chunk, const char* name);

// Disassembles the instruction at the given offset.
// Returns the offset of the next instruction.
int disassembleInstruction(const Chunk* chunk, int offset);

#endif
