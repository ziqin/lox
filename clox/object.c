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

  // Every time we allocate an Obj, we insert it in the list.
  object->next = vm.objects;
  vm.objects = object;
  return object;
}

ObjFunction* newFunction() {
  ObjFunction* function = ALLOCATE_OBJ(ObjFunction, OBJ_FUNCTION);
  function->arity = 0;
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
  tableSet(&vm.strings, string, NIL_VAL);
  return string;
}

static void printFunction(ObjFunction* function) {
  if (function->name == NULL) {
    printf("<script>");
    return;
  }
  printf("<fn %s>", function->name->chars);
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
  tableSet(&vm.strings, result, NIL_VAL);
  return result;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_FUNCTION:
      printFunction(AS_FUNCTION(value));
      break;
    case OBJ_NATIVE:
      printf("<native fn>");
      break;
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
  }
}
