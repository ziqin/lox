#include <stdio.h>
#include <string.h>

#include "chunk.h"
#include "memory.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType) \
  (type*)allocateObject(sizeof(type), objectType)

static void* allocateObject(size_t size, ObjType type) {
  Obj* object = reallocate(NULL, 0, size);
  object->type = type;
  object->isMarked = false;

  // Every time we allocate an Obj, we insert it in the list.
  object->next = vm.objects;
  vm.objects = object;

#ifdef DEBUG_LOG_GC
  printf("%p allocate %zu for %d\n", (void*)object, size, type);
#endif

  return object;
}

ObjClosure* newClosure(ObjFunction* function) {
  ObjUpvalue** upvalues = ALLOCATE(ObjUpvalue*, function->upvalueCount);
  for (int i = 0; i < function->upvalueCount; i++) {
    upvalues[i] = NULL;
  }

  ObjClosure* closure = ALLOCATE_OBJ(ObjClosure, OBJ_CLOSURE);
  closure->function = function;
  closure->upvalues = upvalues;
  closure->upvalueCount = function->upvalueCount;
  return closure;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;
  function->upvalueCount = 0;
  function->name = NULL;
  initChunk(&function->chunk);
  return function;
}

ObjNative* newNative(NativeFn function, int arity) {
  ObjNative* native = ALLOCATE_OBJ(ObjNative, OBJ_NATIVE);
  native->arity = arity;
  native->function = function;
  return native;
}

// FNA-1a algorithm.
static uint32_t hashString(const char* key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}

ObjString* copyString(const char* chars, int length) {
  uint32_t hash = hashString(chars, length);
  ObjString* interned = tableFindString(&vm.strings, chars, length, hash);
  if (interned != NULL) return interned;

  ObjString* string = allocateObject(STRING_SIZE(length), OBJ_STRING);
  string->length = length;
  string->hash = hash;
  memcpy(string->chars, chars, sizeof(char) * length);
  string->chars[length] = '\0';

  push(OBJ_VAL(string));
  tableSet(&vm.strings, string, NIL_VAL);
  pop();

  return string;
}

ObjString* concatStrings(const ObjString* a, const ObjString* b) {
  int length = a->length + b->length;
  ObjString* result = allocateObject(STRING_SIZE(length), OBJ_STRING);
  memcpy(result->chars, a->chars, sizeof(char) * a->length);
  memcpy(result->chars + a->length, b->chars, sizeof(char) * b->length);

  result->hash = hashString(result->chars, length);
  ObjString* interned = tableFindString(&vm.strings, result->chars,
      length, result->hash);
  if (interned != NULL) {
    // Remove the temporary object from the all-objects list.
    vm.objects = vm.objects->next;
    reallocate(result, STRING_SIZE(length), 0);
    return interned;
  }

  result->length = length;
  result->chars[length] = '\0';

  push(OBJ_VAL(result));
  tableSet(&vm.strings, result, NIL_VAL);
  pop();

  return result;
}

ObjUpvalue* newUpvalue(Value* slot) {
  ObjUpvalue* upvalue = ALLOCATE_OBJ(ObjUpvalue, OBJ_UPVALUE);
  upvalue->closed = NIL_VAL;
  upvalue->location = slot;
  upvalue->next = NULL;
  return upvalue;
}

static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars);
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_CLOSURE:
      // From the user's perspective, the difference between ObjFunction and
      // ObjClosure is purely a hidden implementation detail.
      printFunction(AS_CLOSURE(value)->function);
      break;
    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;
    case OBJ_NATIVE:
      printf("<native fn>");
      break;
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
    case OBJ_UPVALUE:
      // Upvalues aren't first-class values that a Lox user can directly access
      // in a program, so this branch is actually unreachable.
      printf("upvalue");
      break;
  }
}
