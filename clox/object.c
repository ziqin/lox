#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
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

ObjString* copyString(const char* chars, int length) {
  ObjString* string = allocateObject(STRING_SIZE(length), OBJ_STRING);
  string->length = length;
  memcpy(string->chars, chars, sizeof(char) * length);
  string->chars[length] = '\0';
  return string;
}

ObjString* concatStrings(const ObjString* a, const ObjString* b) {
  int length = a->length + b->length;
  ObjString* result = allocateObject(STRING_SIZE(length), OBJ_STRING);
  result->length = length;
  memcpy(result->chars, a->chars, sizeof(char) * a->length);
  memcpy(result->chars + a->length, b->chars, sizeof(char) * b->length);
  result->chars[length] = '\0';
  return result;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
    case OBJ_STRING:
      printf("%s", AS_CSTRING(value));
      break;
  }
}
