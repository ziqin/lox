#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_CONSTANT_LONG,
  OP_RETURN,
} OpCode;

// A dynamic array of instructions.
typedef struct {
  int count;
  int capacity;
  uint8_t* code;
  int* lines; // The line information is kepted in a separate array instead of
              // interleaving it in the bytecode itself. Since line information
              // is only used when a runtime error occurs, we don't want it
              // between the instructions, taking up precious space in the CPU
              // cache and causing more cache misses as the interpreter skips
              // past it to get to the opcodes and operands it cares about.
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);

// Appends a constant to the constant array.
// Returns the index where the constant was appended.
int addConstant(Chunk* chunk, Value value);

// Adds value to chunk's constant array and writes an appropriate instruction
// to load the constant.
// Potential instructions:
// - OP_CONSTANT: followed by 1 octet as index
// - OP_CONSTANT_LONG: followed by 3 octets in little endian as index
void writeConstant(Chunk* chunk, Value value, int line);

#endif
