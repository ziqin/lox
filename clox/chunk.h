#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_CONSTANT_LONG,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NEGATE,
  OP_RETURN,
} OpCode;

typedef struct CodeLinePair CodeLinePair;

// A dynamic array storing the line information.
// Compression scheme: For a consecutive segment of instructions with the same
// source line number, only 1 CodeLinePair is stored.
typedef struct {
  int count;
  int capacity;
  CodeLinePair* data;
} SourceLines;

// A dynamic array of instructions.
typedef struct {
  int count;
  int capacity;
  uint8_t* code;
  SourceLines lines; // The line information is stored separately instead of
                     // interleaving it in the bytecode itself. Since line
                     // information is only used when a runtime error occurs,
                     // this design improves CPU caching.
  ValueArray constants;
} Chunk;

void initChunk(Chunk* chunk);
void freeChunk(Chunk* chunk);
void writeChunk(Chunk* chunk, uint8_t byte, int line);

// Given the index of an instruction,
// Returns the line where the instruction occurs.
// Time complexity: The insertion process implies an ascending order of bytecode
// offset in chunk->lines, which enables O(log(offset)) lookup.
int getLine(Chunk* chunk, int offset);

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
