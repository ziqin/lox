#include <assert.h>
#include <stdio.h>

#include "chunk.h"
#include "debug.h"
#include "value.h"

void disassembleChunk(Chunk* chunk, const char* name) {
  printf("== %s ==\n", name);

  for (int offset = 0; offset < chunk->count;) {
    offset = disassembleInstruction(chunk, offset);
  }
}

// Prints out the name of the opcode, prints the constant index from the
// subsequent byte in the chunk, and displays the actual constant value.
// Returns the byte offset of the next instruction.
static int constantInstruction(const char* name, Chunk* chunk, int offset) {
  uint8_t instruction = chunk->code[offset];
  int constant;
  if (instruction == OP_CONSTANT) {
    constant = chunk->code[offset + 1];
  } else {
    assert(instruction == OP_CONSTANT_LONG);
    uint8_t* operand = &chunk->code[offset + 1];
    constant = operand[0] | (operand[1] << 8) | (operand[2] << 16);
  }
  printf("%-16s %4d '", name, constant);
  printValue(chunk->constants.values[constant]);
  printf("'\n");
  return offset + (instruction == OP_CONSTANT ? 2 : 4);
}

// Prints the name of the opcode.
// Returns the next byte offset past this instruction.
static int simpleInstruction(const char* name, int offset) {
  printf("%s\n", name);
  return offset + 1;
}

int disassembleInstruction(Chunk* chunk, int offset) {
  printf("%04d ", offset);
  int line = getLine(chunk, offset);
  if (offset > 0 && line == getLine(chunk, offset - 1)) {
    printf("   | ");
  } else {
    printf("%4d ", line);
  }

  uint8_t instruction = chunk->code[offset];
  switch (instruction) {
    case OP_CONSTANT:
      return constantInstruction("OP_CONSTANT", chunk, offset);
    case OP_CONSTANT_LONG:
      return constantInstruction("OP_CONSTANT_LONG", chunk, offset);
    case OP_NIL:
      return simpleInstruction("OP_NIL", offset);
    case OP_TRUE:
      return simpleInstruction("OP_TRUE", offset);
    case OP_FALSE:
      return simpleInstruction("OP_FALSE", offset);
    case OP_EQUAL:
      return simpleInstruction("OP_EQUAL", offset);
    case OP_GREATER:
      return simpleInstruction("OP_GREATER", offset);
    case OP_LESS:
      return simpleInstruction("OP_LESS", offset);
    case OP_ADD:
      return simpleInstruction("OP_ADD", offset);
    case OP_SUBTRACT:
      return simpleInstruction("OP_SUBTRACT", offset);
    case OP_MULTIPLY:
      return simpleInstruction("OP_MULTIPLY", offset);
    case OP_DIVIDE:
      return simpleInstruction("OP_DIVIDE", offset);
    case OP_NOT:
      return simpleInstruction("OP_NOT", offset);
    case OP_NEGATE:
      return simpleInstruction("OP_NEGATE", offset);
    case OP_RETURN:
      return simpleInstruction("OP_RETURN", offset);
    default:
      printf("Unknown opcode %d\n", instruction);
      return offset + 1;
  }
}
