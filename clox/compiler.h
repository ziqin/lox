#ifndef clox_compiler_h
#define clox_compiler_h

#include "vm.h"
#include "object.h"

// By returning NULL we signal an error
ObjFunction* compile(const char* source);

#endif
