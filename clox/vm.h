#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#include "value.h"

#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  ObjClosure* closure;
  uint8_t* ip; // The caller stores its own ip. When we return from a function,
               // the VM will jump to the ip of the caller's CallFrame and
               // resume from there.
  Value* slots; // Points into the VM's value stack at the first slot that this
                // function can use.
} CallFrame;

typedef struct {
  CallFrame frames[FRAMES_MAX];
  int frameCount;

  Value stack[STACK_MAX];
  Value* stackTop; // Points to where the next value to be pushed will go.
  Table globals;
  Table strings;  // A set for string interning. The keys are all we care about.
  ObjString* initString; // Take advantage of string interning to speed up init.
  ObjUpvalue* openUpvalues; // Head ptr to the ordered list of open upvalues.

  size_t bytesAllocated;
  size_t nextGC;
  Obj* objects; // A linked list storing every Obj.
  int grayCount;
  int grayCapacity;
  Obj** grayStack;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR
} InterpretResult;

extern VM vm;

void initVM();
void freeVM();
InterpretResult interpret(const char* source);
void push(Value value);
Value pop();

#endif
