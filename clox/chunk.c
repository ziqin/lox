#include <stdlib.h>

#include "chunk.h"
#include "memory.h"

void initChunk(Chunk* chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  chunk->lines = NULL;
  initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  FREE_ARRAY(int, chunk->lines, chunk->capacity);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
  if (chunk->capacity < chunk->count + 1) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity,
        chunk->capacity);
    chunk->lines = GROW_ARRAY(int, chunk->lines, oldCapacity, chunk->capacity);
  }

  chunk->code[chunk->count] = byte;
  chunk->lines[chunk->count] = line;
  chunk->count++;
}

int addConstant(Chunk* chunk, Value value) {
  writeValueArray(&chunk->constants, value);
  return chunk->constants.count - 1;
}

void writeConstant(Chunk* chunk, Value value, int line) {
  int constantIndex = addConstant(chunk, value);
  if (constantIndex <= 0xff) {
    writeChunk(chunk, OP_CONSTANT, line);
    writeChunk(chunk, constantIndex, line);
  } else if (constantIndex <= 0xffffff) {
    writeChunk(chunk, OP_CONSTANT_LONG, line);
    // The constant's index in little endian.
    writeChunk(chunk, (constantIndex & 0x0000ff), line);
    writeChunk(chunk, (constantIndex & 0x00ff00) >> 8, line);
    writeChunk(chunk, (constantIndex & 0xff0000) >> 16, line);
  } else {
    // Too many constants to add.
    exit(1);
  }
}
