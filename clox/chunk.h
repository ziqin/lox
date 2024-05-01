#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

typedef enum {
  OP_CONSTANT,
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_POP,
  OP_GET_LOCAL,
  OP_GET_GLOBAL,
  OP_DEFINE_GLOBAL,
  OP_SET_LOCAL,
  OP_SET_GLOBAL,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_GET_PROPERTY,
  OP_SET_PROPERTY,
  OP_GET_SUPER,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_NEGATE,
  OP_PRINT,
  OP_JUMP,
  OP_JUMP_IF_FALSE,
  OP_LOOP,
  OP_CALL,
  OP_INVOKE,
  OP_SUPER_INVOKE,
  OP_CLOSURE,
  OP_CLOSE_UPVALUE,
  OP_RETURN,
  OP_CLASS,
  OP_INHERIT,
  OP_METHOD,
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
int getLine(const Chunk* chunk, int offset);

// Appends a constant to the constant array.
// Returns the index where the constant was appended.
int addConstant(Chunk* chunk, Value value);

#endif
