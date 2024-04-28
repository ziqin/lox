#ifndef clox_object_h
#define clox_object_h

#include "common.h"
#include "chunk.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value)        (AS_OBJ(value)->type)

#define IS_CLASS(value)        isObjType(value, OBJ_STRING)
#define IS_CLOSURE(value)      isObjType(value, OBJ_CLOSURE)
#define IS_FUNCTION(value)     isObjType(value, OBJ_FUNCTION)
#define IS_INSTANCE(value)     isObjType(value, OBJ_INSTANCE)
#define IS_NATIVE(value)       isObjType(value, OBJ_NATIVE)
#define IS_STRING(value)       isObjType(value, OBJ_STRING)

#define AS_CLASS(value)        ((ObjClass*)AS_OBJ(value))
#define AS_CLOSURE(value)      ((ObjClosure*)AS_OBJ(value))
#define AS_FUNCTION(value)     ((ObjFunction*)AS_OBJ(value))
#define AS_INSTANCE(value)     ((ObjInstance*)AS_OBJ(value))
#define AS_NATIVE(value)       ((ObjNative*)AS_OBJ(value))
#define AS_STRING(value)       ((ObjString*)AS_OBJ(value))
#define AS_CSTRING(value)      (((ObjString*)AS_OBJ(value))->chars)

// One more byte for the tailing '\0'.
#define STRING_SIZE(length) (sizeof(ObjString) + sizeof(char[(length) + 1]))

typedef enum {
  OBJ_CLASS,
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_INSTANCE,
  OBJ_NATIVE,
  OBJ_STRING,
  OBJ_UPVALUE,
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj* next;
};

// ObjFunction objects are created by the front end during compilation. At
// runtime, all the VM does is load the function object from a constant table
// and bind it to a name.
typedef struct {
  Obj obj;
  int arity;
  int upvalueCount;
  Chunk chunk;
  ObjString* name;
} ObjFunction;

// args points to the first argument on the stack.
typedef Value (*NativeFn)(int argCount, Value* args);

typedef struct {
  Obj obj;
  int arity;
  NativeFn function;
} ObjNative;

struct ObjString {
  Obj obj;
  int length;
  uint32_t hash; // We can cache the hash because strings are immutable in Lox.
  char chars[];
};

typedef struct ObjUpvalue {
  Obj obj;
  Value* location; // Points to the closed-over variable.
  Value closed;
  struct ObjUpvalue* next;
} ObjUpvalue;

typedef struct {
  Obj obj;
  ObjFunction* function;
  ObjUpvalue** upvalues; // A dynamic array of pointers to upvalues.
  int upvalueCount; // The count is redundant because it's also tracked by
                    // ObjFunction, but GC may need to know the size of an
                    // ObjClosure's upvalue array size after the closure's
                    // corresponding ObjFunction has already been freed.
} ObjClosure;

typedef struct {
  Obj obj;
  ObjString* name;
} ObjClass;

typedef struct {
  Obj obj;
  ObjClass* klass;
  Table fields;
} ObjInstance;

ObjClass* newClass(ObjString* name);
ObjClosure* newClosure(ObjFunction* function);
ObjFunction* newFunction();
ObjInstance* newInstance(ObjClass* klass);
ObjNative* newNative(NativeFn function, int arity);
ObjString* copyString(const char* chars, int length);
ObjString* concatStrings(const ObjString* a, const ObjString* b);
ObjUpvalue* newUpvalue(Value* slot);
void printObject(Value value);

static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
