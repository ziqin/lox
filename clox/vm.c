#include <stdarg.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "debug.h"
#include "object.h"
#include "memory.h"
#include "vm.h"

// Ignore __attribute__ in compilers incompatible with GCC.
#ifndef __GNUC__
#define __attribute__(a)
#endif

VM vm;

static void runtimeError(const char* format, ...)
  __attribute__ ((format (printf, 1, 2)));

// Returns the elapsed time since the program started running, in seconds.
static Value clockNative(int UNUSED(argCount), Value* UNUSED(args)) {
  return NUMBER_VAL((double)clock() / CLOCKS_PER_SEC);
}

static void resetStack() {
  vm.stackTop = vm.stack;
  vm.frameCount = 0;
}

void runtimeError(const char* format, ...) {
  va_list args;
  va_start(args, format);
  vfprintf(stderr, format, args);
  va_end(args);
  fputs("\n", stderr);

  for (int i = vm.frameCount - 1; i >= 0; i--) {
    CallFrame* frame = &vm.frames[i];
    ObjFunction* function = frame->function;
    ptrdiff_t instruction = frame->ip - 1 - frame->function->chunk.code;
    fprintf(stderr, "[line %d] in ",
            getLine(&function->chunk, (int)instruction));
    if (function->name == NULL) {
      fprintf(stderr, "script\n");
    } else {
      fprintf(stderr, "%s()\n", function->name->chars);
    }
  }

  resetStack();
}

static void defineNative(const char* name, NativeFn function, int arity) {
  // Both copyString() and newNative() dynamically allocate memory. That means
  // they can potentially trigger a collection. By storing them on the value
  // stack, we ensure the collector knows we're not done with the name and
  // ObjFunction so that it doesn't free them out from under us.
  push(OBJ_VAL(copyString(name, (int)strlen(name))));
  push(OBJ_VAL(newNative(function, arity)));
  tableSet(&vm.globals, AS_STRING(vm.stack[0]), vm.stack[1]);
  pop();
  pop();
}

void initVM() {
  resetStack();
  vm.objects = NULL;
  initTable(&vm.globals);
  initTable(&vm.strings);

  defineNative("clock", clockNative, 0);
}

void freeVM() {
  freeTable(&vm.globals);
  freeTable(&vm.strings);
  freeObjects();
}

void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}

Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}

static Value peek(int distance) {
  return vm.stackTop[-1 - distance];
}

static bool call(ObjFunction* function, int argCount) {
  if (argCount != function->arity) {
    runtimeError("Expected %d arguments but got %d.",
        function->arity, argCount);
    return false;
  }

  if (vm.frameCount == FRAMES_MAX) {
    runtimeError("Stack overflow.");
    return false;
  }

  CallFrame* frame = &vm.frames[vm.frameCount++];
  frame->function = function;
  frame->ip = function->chunk.code;
  // The top of the caller's stack contains the function being called followed
  // by the arguments in order. The bottom of the callee's stack overlaps so
  // that the parameter slots exactly line up with where the argument values
  // already live. This means that we don't need to do any work to "bind an
  // argument to a parameter". There's no copying values between slots or across
  // environments. The arguments are already exactly where they need to be.
  frame->slots = vm.stackTop - argCount - 1;
  return true;
}

static bool callNative(ObjNative* native, int argCount) {
  if (argCount != native->arity) {
    runtimeError("Expected %d arguments but got %d.", native->arity, argCount);
    return false;
  }
  NativeFn nativeFn = native->function;
  Value result = nativeFn(argCount, vm.stackTop - argCount);
  vm.stackTop -= argCount + 1;
  push(result);
  return true;
}

static bool callValue(Value callee, int argCount) {
  // Because Lox is dynamically typed, there's nothing to prevent a user from
  // writing bad code like:
  //   var notAFunction = 123;
  //   notAFunction();
  // If that happens, the runtime needs to safely report an error and halt. So
  // the first thing we do is check the type of the value that we're trying to
  // call.
  if (IS_OBJ(callee)) {
    switch (OBJ_TYPE(callee)) {
      case OBJ_FUNCTION:
        return call(AS_FUNCTION(callee), argCount);
      case OBJ_NATIVE:
        return callNative(AS_NATIVE(callee), argCount);
      default:
        break; // Non-callable object type.
    }
  }
  runtimeError("Can only call functions and classes.");
  return false;
}

// Lox follows Ruby in that nil and false are falsey and every other value
// behaves like true.
static bool isFalsey(Value value) {
  return IS_NIL(value) || (IS_BOOL(value) && !AS_BOOL(value));
}

static void concatenate() {
  ObjString* b = AS_STRING(pop());
  ObjString* a = AS_STRING(pop());
  ObjString* result = concatStrings(a, b);
  push(OBJ_VAL(result));
}

static InterpretResult run() {
  CallFrame* frame = &vm.frames[vm.frameCount - 1];

#define READ_BYTE() (*frame->ip++)

#define READ_SHORT() \
  (frame->ip += 2, (uint16_t)((frame->ip[-2] << 8) | frame->ip[-1]))

#define READ_CONSTANT() (frame->function->chunk.constants.values[READ_BYTE()])

#define READ_CONSTANT_LONG()                                    \
  (frame->ip += 3,                                              \
  frame->function->chunk.constants.values                      \
    [frame->ip[-3] << 16 | frame->ip[-2] << 8 | frame->ip[-1]])

#define READ_STRING() AS_STRING(READ_CONSTANT())
#define READ_STRING_LONG() AS_STRING(READ_CONSTANT_LONG())

// When the operands themselves are calculated, the left is evaluated first,
// then the right. That means the left operand gets pushed before the right
// operand. SO the right operand will be on the top of the stack. Thus, the
// first value we pop is b.
#define BINARY_OP(valueType, op)                      \
  do {                                                \
    if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) { \
      runtimeError("Operands must be numbers.");      \
      return INTERPRET_RUNTIME_ERROR;                 \
    }                                                 \
    double b = AS_NUMBER(pop());                      \
    double a = AS_NUMBER(pop());                      \
    push(valueType(a op b));                          \
  } while (false)

  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(&frame->function->chunk,
        (int)(frame->ip - frame->function->chunk.code));
#endif

    uint8_t instruction;
    switch (instruction = READ_BYTE()) {
      case OP_CONSTANT: {
        Value constant = READ_CONSTANT();
        push(constant);
        break;
      }
      case OP_CONSTANT_LONG: {
        Value constant = READ_CONSTANT_LONG();
        push(constant);
        break;
      }
      case OP_NIL:      push(NIL_VAL); break;
      case OP_TRUE:     push(BOOL_VAL(true)); break;
      case OP_FALSE:    push(BOOL_VAL(false)); break;
      case OP_POP:      pop(); break;
      case OP_GET_LOCAL: {
        uint8_t slot = READ_BYTE();
        // The other bytecode instructions only look for data at the top of the
        // stack, so we need to push the local's value onto the stack even
        // though it's already on the stack lower down somewhere.
        push(frame->slots[slot]);
        break;
      }
      case OP_SET_LOCAL: {
        uint8_t slot = READ_BYTE();
        // Assignment is an expression, and every expression produces a value.
        // The value of an assignment expression is the assigned value itself,
        // so the VM just leaves the value on the stack.
        frame->slots[slot] = peek(0);
        break;
      }
      case OP_GET_GLOBAL: {
        ObjString* name = READ_STRING();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      case OP_GET_GLOBAL_LONG: {
        ObjString* name = READ_STRING_LONG();
        Value value;
        if (!tableGet(&vm.globals, name, &value)) {
          runtimeError("Undefined variable '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        push(value);
        break;
      }
      // Note that we don't pop the value until after we add it to the hash
      // table. That ensures the VM can still find the value if a garbage
      // collection is triggered right in the middle of adding it to the hash
      // table. That's a distinct possibility since the hash table requires
      // dynamic allocation when it resizes.
      case OP_DEFINE_GLOBAL: {
        ObjString* name = READ_STRING();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      case OP_DEFINE_GLOBAL_LONG: {
        ObjString* name = READ_STRING_LONG();
        tableSet(&vm.globals, name, peek(0));
        pop();
        break;
      }
      // The call to tableSet() stores the value in the global variable table
      // even if the variable wasn't previously defined. That fact is visible in
      // a REPL session, since it keeps running even after the runtime error is
      // reported. So we also take care to delete that zombie value from the
      // table.
      case OP_SET_GLOBAL: {
        ObjString* name = READ_STRING();
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variables '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SET_GLOBAL_LONG: {
        ObjString* name = READ_STRING_LONG();
        if (tableSet(&vm.globals, name, peek(0))) {
          tableDelete(&vm.globals, name);
          runtimeError("Undefined variables '%s'.", name->chars);
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_EQUAL: {
        Value b = pop();
        Value a = pop();
        push(BOOL_VAL(valuesEqual(a, b)));
        break;
      }
      case OP_GREATER:  BINARY_OP(BOOL_VAL, >); break;
      case OP_LESS:     BINARY_OP(BOOL_VAL, <); break;
      case OP_ADD: {
        if (IS_NUMBER(peek(0)) && IS_NUMBER(peek(1))) {
          double b = AS_NUMBER(pop());
          double a = AS_NUMBER(pop());
          push(NUMBER_VAL(a + b));
        } else if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
          concatenate();
        } else {
          runtimeError("Operands must be two numbers or two strings.");
          return INTERPRET_RUNTIME_ERROR;
        }
        break;
      }
      case OP_SUBTRACT: BINARY_OP(NUMBER_VAL, -); break;
      case OP_MULTIPLY: BINARY_OP(NUMBER_VAL, *); break;
      case OP_DIVIDE:   BINARY_OP(NUMBER_VAL, /); break;
      case OP_NOT:      push(BOOL_VAL(isFalsey(pop()))); break;
      case OP_NEGATE:
        if (!IS_NUMBER(peek(0))) {
          runtimeError("Operand must be a number.");
          return INTERPRET_RUNTIME_ERROR;
        }
        push(NUMBER_VAL(-AS_NUMBER(pop())));
        break;
      case OP_PRINT: {
        printValue(pop());
        printf("\n");
        break;
      }
      case OP_JUMP: {
        uint16_t offset = READ_SHORT();
        frame->ip += offset;
        break;
      }
      case OP_JUMP_IF_FALSE: {
        uint16_t offset = READ_SHORT();
        if (isFalsey(peek(0))) frame->ip += offset;
        break;
      }
      case OP_LOOP: {
        uint16_t offset = READ_SHORT();
        frame->ip -= offset;
        break;
      }
      case OP_CALL: {
        int argCount = READ_BYTE();
        if (!callValue(peek(argCount), argCount)) {
          return INTERPRET_RUNTIME_ERROR;
        }
        // If callValue() is successful, there will be a new frame on the
        // CallFrame stack for the called function. The run() function has its
        // own cached pointer to the current frame, so we need to update that.
        // Since the bytecode dispatch loop reads from the frame variable, when
        // the VM goes to execute the next instruction, it will read the ip from
        // the newly called function's CallFrame and jump to its code.
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
      case OP_RETURN: {
        Value result = pop();
        vm.frameCount--;
        if (vm.frameCount == 0) {
          pop();
          return INTERPRET_OK;
        }

        // Discards all of the slots the callee was using for its parameters and
        // local variables. That includes the same slots the caller used to pass
        // the arguments. Now that the call is done, the caller doesn't need
        // them anymore. This means the top of the stack ends up right at the
        // beginning of the returning function's stack window.
        vm.stackTop = frame->slots;
        push(result);
        // On the next iteration of the bytecode dispatch loop, execution will
        // jump back to the caller, right where it left off, immediately after
        // the OP_CALL instruction.
        frame = &vm.frames[vm.frameCount - 1];
        break;
      }
    }
  }

#undef READ_BYTE
#undef READ_SHORT
#undef READ_CONSTANT
#undef READ_STRING
#undef READ_STRING_LONG
#undef BINARY_OP
}

InterpretResult interpret(const char* source) {
  ObjFunction* function = compile(source);
  if (function == NULL) return INTERPRET_COMPILE_ERROR;

  push(OBJ_VAL(function));
  call(function, 0);

  return run();
}
