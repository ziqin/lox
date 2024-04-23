#include <stdlib.h>

#include "memory.h"
#include "object.h"
#include "vm.h"

void* reallocate(void* pointer, size_t UNUSED(oldSize), size_t newSize) {
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void* result = realloc(pointer, newSize);
  if (result == NULL) exit(1);
  return result;
}

static void freeObject(Obj* object) {
  switch (object->type) {
    case OBJ_CLOSURE: {
      // We free only the ObjClosure itself, not the ObjFunction, because the
      // closure doesn't own the function. There may be multiple closures that
      // all reference the same function, and none of them claims any special
      // privilege over it. We can't free the ObjFunction until all objects
      // referencing it are gone, including even the surrounding function whose
      // constant table contains it.
      // ObjClosure doesn't own the ObjUpvalue objects themselves, but it does
      // own the array containing pointers to those upvalues.
      ObjClosure* closure = (ObjClosure*)object;
      FREE_ARRAY(ObjUpvalue*, closure->upvalues, closure->upvalueCount);
      FREE(ObjClosure, object);
      break;
    }
    case OBJ_FUNCTION: {
      ObjFunction* function = (ObjFunction*)object;
      freeChunk(&function->chunk);
      FREE(ObjFunction, object);
      break;
    }
    case OBJ_NATIVE:
      FREE(ObjNative, object);
      break;
    case OBJ_STRING: {
      ObjString* string = (ObjString*)object;
      reallocate(object, STRING_SIZE(string->length), 0);
      break;
    }
    case OBJ_UPVALUE:
      // Multiple closures can close over the same variable, so ObjUpvalue does
      // not own the variable it references.
      FREE(ObjUpvalue, object);
      break;
  }
}

void freeObjects() {
  Obj* object = vm.objects;
  while (object != NULL) {
    Obj* next = object->next;
    freeObject(object);
    object = next;
  }
}
