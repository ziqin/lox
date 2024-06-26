#include <assert.h>
#include <stdlib.h>

#include "chunk.h"
#include "memory.h"
#include "vm.h"

struct CodeLinePair {
  int offset; // The start of bytecode offset corresponding to the source line.
  int line; // Line number in Lox source code.
};

static void initSourceLines(SourceLines* lines) {
  lines->count = 0;
  lines->capacity = 0;
  lines->data = NULL;
}

static void freeSourceLines(SourceLines* lines) {
  FREE_ARRAY(CodeLinePair, lines->data, lines->capacity);
  initSourceLines(lines);
}

static void writeSourceLines(SourceLines* lines, int offset, int line) {
  if (lines->capacity < lines->count + 1) {
    int oldCapacity = lines->capacity;
    lines->capacity = GROW_CAPACITY(oldCapacity);
    lines->data = GROW_ARRAY(CodeLinePair, lines->data, oldCapacity,
        lines->capacity);
  }
  CodeLinePair* pair = &lines->data[lines->count++];
  pair->offset = offset;
  pair->line = line;
}

void initChunk(Chunk* chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  initSourceLines(&chunk->lines);
  initValueArray(&chunk->constants);
}

void freeChunk(Chunk* chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  freeSourceLines(&chunk->lines);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}

void writeChunk(Chunk* chunk, uint8_t byte, int line) {
  if (chunk->capacity < chunk->count + 1) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code = GROW_ARRAY(uint8_t, chunk->code, oldCapacity,
        chunk->capacity);
  }

  chunk->code[chunk->count] = byte;

  // Appends only when the line is different from the counterpart of the last
  // instruction.
  if (chunk->lines.count == 0 ||
      chunk->lines.data[chunk->lines.count - 1].line != line) {
    // Elements of chunk->lines are thus ordered by bytecode offset.
    writeSourceLines(&chunk->lines, chunk->count, line);
  }

  chunk->count++;
}

// Binary search
int getLine(const Chunk* chunk, int offset) {
  assert(chunk->lines.count > 0 && chunk->lines.data != NULL);
  int left = 0, mid = 0, right = chunk->lines.count;
  while (left + 1 < right) {
    mid = left + (right - left) / 2;
    if (chunk->lines.data[mid].offset <= offset) {
      left = mid;
    } else {
      right = mid;
    }
  }
  return chunk->lines.data[left].line;
}

int addConstant(Chunk* chunk, Value value) {
  push(value);
  writeValueArray(&chunk->constants, value);
  pop();
  return chunk->constants.count - 1;
}
