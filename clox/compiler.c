#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
#include "memory.h"
#include "object.h"
#include "scanner.h"

#ifdef DEBUG_PRINT_CODE
#include "debug.h"
#endif

typedef struct {
  Token current;
  Token previous;
  bool hadError;
  bool panicMode;
} Parser;

// All of Lox's precedence levels in order from lowest to highest.
typedef enum {
  PREC_NONE,
  PREC_ASSIGNMENT,  // =
  PREC_OR,          // or
  PREC_AND,         // and
  PREC_EQUALITY,    // == !=
  PREC_COMPARISON,  // < > <= >=
  PREC_TERM,        // + -
  PREC_FACTOR,      // * /
  PREC_UNARY,       // ! -
  PREC_CALL,        // . ()
  PREC_PRIMARY
} Precedence;

typedef void (*ParseFn)(bool canAssign);

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

typedef struct {
  Token name;
  int depth;
  bool isCaptured;
} Local;

typedef struct {
  uint8_t index; // Which local slot the upvalue is capturing.
  bool isLocal; // Whether the closure captures a local variable or an upvalue
                // from the surrounding function.
} Upvalue;

typedef enum {
  TYPE_FUNCTION,
  TYPE_INITIALIZER,
  TYPE_METHOD,
  TYPE_SCRIPT
} FunctionType;

typedef struct Compiler {
  // A linked list as a stack of the series of nested Compilers structs.
  // Each Compiler points back to the Compiler for the function that encloses
  // it, all the way back to the root Compiler for the top-level code.
  // The linked list of Compilers threads through the C stack. Using the native
  // stack for Compiler structs does mean our compiler has a practical limit on
  // how deeply nested function declarations can be. Go too far and you could
  // overflow the C stack. If we want the compiler to be more robust against
  // pathological or even malicious code, it would be good to have our compiler
  // artificially limit the amount of function nesting it permits.
  struct Compiler* enclosing;
  ObjFunction* function;
  FunctionType type;

  // A hard limit on the number of locals that can be in scope at once.
  Local locals[UINT8_COUNT];
  // Tracks how many locals are in scope.
  int localCount;
  // The OP_GET_UPVALUE and OP_SET_UPVALUE instructions encode an upvalue index
  // using a single byte operand, so the maximum number of unique variables a
  // function can close over is restricted to UINT8_COUNT;
  Upvalue upvalues[UINT8_COUNT];
  // The number of blocks surrounding the current bit of code we're compiling.
  int scopeDepth;
} Compiler;

typedef struct ClassCompiler {
  struct ClassCompiler* enclosing;
  bool hasSuperclass;
} ClassCompiler;

static Parser parser;
static Compiler* current = NULL;
static ClassCompiler* currentClass = NULL;

static Chunk* currentChunk() {
  return &current->function->chunk;
}

static void errorAt(Token* token, const char* message) {
  if (parser.panicMode) return;
  parser.panicMode = true;
  fprintf(stderr, "[line %d] Error", token->line);

  if (token->type == TOKEN_EOF) {
    fprintf(stderr, " at end");
  } else if (token->type == TOKEN_ERROR) {
    // Nothing.
  } else {
    fprintf(stderr, " at '%.*s'", token->length, token->start);
  }

  fprintf(stderr, ": %s\n", message);
  parser.hadError = true;
}

static void error(const char* message) {
  errorAt(&parser.previous, message);
}

static void errorAtCurrent(const char* message) {
  errorAt(&parser.current, message);
}

static void advance() {
  parser.previous = parser.current;

  for (;;) {
    parser.current = scanToken();
    if (parser.current.type != TOKEN_ERROR) break;

    errorAtCurrent(parser.current.start);
  }
}

static void consume(TokenType type, const char* message) {
  if (parser.current.type == type) {
    advance();
    return;
  }
  errorAtCurrent(message);
}

// Returns true if the current token has the given type.
static bool check(TokenType type) {
  return parser.current.type == type;
}

// If the current token has the given type, we consume the token and return
// true. Otherwise we have the token alone and return false.
static bool match(TokenType type) {
  if (!check(type)) return false;
  advance();
  return true;
}

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitLoop(int loopStart) {
  emitByte(OP_LOOP);

  // The +2 is to take into acount the size of the OP_LOOP instruction's own
  // operands which we also need to jump over.
  int offset = currentChunk()->count - loopStart + 2;
  if (offset > UINT16_MAX) error("Loop body too large.");

  emitByte((offset >> 8) & 0xff);
  emitByte(offset & 0xff);
}

// The function returns the offset of the emitted instruction in the chunk.
static int emitJump(uint8_t instruction) {
  emitByte(instruction);
  // We use two bytes for the jump offset operand. A 16-bit offset lets us jump
  // over up to 65,535 bytes of code.
  emitByte(0xff);
  emitByte(0xff);
  return currentChunk()->count - 2;
}

static void patchJump(int offset) {
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = currentChunk()->count - offset - 2;

  if (jump > UINT16_MAX) {
    error("Too much code to jump over.");
  }

  currentChunk()->code[offset] = (jump >> 8) & 0xff;
  currentChunk()->code[offset + 1] = jump & 0xff;
}

static void emitReturn() {
  if (current->type == TYPE_INITIALIZER) {
    emitBytes(OP_GET_LOCAL, 0); // Slot 0 contains the instance.
  } else {
    emitByte(OP_NIL);
  }

  emitByte(OP_RETURN);
}

static uint8_t makeConstant(Value value) {
  int constant = addConstant(currentChunk(), value);
  if (constant > 0xffffff) {
    error("Too many constants in one chunk.");
    return 0;
  }
  return constant;
}

static void emitConstant(Value value) {
  uint8_t constant = makeConstant(value);
  emitBytes(OP_CONSTANT, constant);
}

static void initCompiler(Compiler* compiler, FunctionType type) {
  compiler->enclosing = current;
  compiler->function = NULL;
  compiler->type = type;
  compiler->localCount = 0;
  compiler->scopeDepth = 0;
  compiler->function = newFunction();
  current = compiler;
  if (type != TYPE_SCRIPT) {
    // The lexeme points directly into the original source code string, which
    // may get freed once the code is finished compiling. The function object we
    // create in the compiler outlives the compiler and persists until runtime.
    // So it needs its own heap-allocated name string that it can keep around.
    current->function->name = copyString(parser.previous.start,
                                         parser.previous.length);
  }

  // The compiler implicitly claims stack slot 0 for the VM's own internal use.
  Local* local = &current->locals[current->localCount++];
  local->depth = 0;
  local->isCaptured = false;
  if (type != TYPE_FUNCTION) {
    local->name.start = "this";
    local->name.length = 4;
  } else {
    local->name.start = "";
    local->name.length = 0;
  }
}

static ObjFunction* endCompiler() {
  emitReturn();
  ObjFunction* function = current->function;

#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), function->name != NULL
        ? function->name->chars : "<script>");
  }
#endif

  current = current->enclosing;
  return function;
}

static void beginScope() {
  current->scopeDepth++;
}

static void endScope() {
  current->scopeDepth--;

  while (current->localCount > 0 &&
      current->locals[current->localCount - 1].depth > current->scopeDepth) {
    if (current->locals[current->localCount - 1].isCaptured) {
      // If a variable is captured by a closure, we will instead emit a
      // different instruction to hoist that variable out of the stack and into
      // its corresponding upvalue.
      emitByte(OP_CLOSE_UPVALUE);
    } else {
      // Local variables occupy slots on the stack at runtime. When a local
      // variable goes out of scope, that slot is no longer needed and should be
      // freed. So, for each variable that we discard, we also emit an OP_POP
      // instruction to pop it from the stack.
      emitByte(OP_POP);
    }
    current->localCount--;
  }
}

static void expression();
static void statement();
static void declaration();
static const ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static uint8_t identifierConstant(const Token* name) {
  return makeConstant(OBJ_VAL(copyString(name->start, name->length)));
}

static bool identifiersEqual(const Token* a, const Token* b) {
  if (a->length != b->length) return false;
  return memcmp(a->start, b->start, a->length) == 0;
}

// Return the stack slot index of the variable with the given name, or -1 to
// signal that it wasn't found and should be assumed to be a global variable.
static int resolveLocal(Compiler* compiler, const Token* name) {
  // The locals array in the compiler has the exact same layout as the VM's
  // stack will have at runtime.
  // We walk the array backward so that we find the last declared variable with
  // the identifier. That ensures that inner local variables correctly shadow
  // locals with the same nmae in surrounding scopes.
  for (int i = compiler->localCount - 1; i >= 0; i--) {
    Local* local = &compiler->locals[i];
    if (identifiersEqual(name, &local->name)) {
      if (local->depth == -1) {
        error("Can't read local variable in its own initializer.");
      }
      return i;
    }
  }

  return -1;
}

// The compiler keeps an array of upvalue structures to track the closed-over
// identifiers that it has resolved in the body of each function. The indexes in
// the compiler's array match the indexes where upvalues will live in the
// ObjClosure at runtime.
// Returns the index of the created upvalue in the function's upvalue list,
// which becomes the operand to OP_GET_UPVALUE and OP_SET_UPVALUE.
static int addUpvalue(Compiler* compiler, uint8_t index, bool isLocal) {
  // It stores the count directly in the ObjFunction itself because we'll also
  // need that number for use at runtime.
  int upvalueCount = compiler->function->upvalueCount;

  // A closure may reference the same variable in a surrounding function
  // multiple times. To avoid wasting time and memory creating a separate
  // upvalue for each identifier expression in that case, we first check to see
  // if the function already has an upvalue that closes over that variable
  // before we add a new upvalue.
  for (int i = 0; i < upvalueCount; i++) {
    const Upvalue* upvalue = &compiler->upvalues[i];
    if (upvalue->index == index && upvalue->isLocal == isLocal) {
      return i;
    }
  }

  if (upvalueCount == UINT8_COUNT) {
    error("Too many closure variables in function.");
    return 0;
  }

  compiler->upvalues[upvalueCount].isLocal = isLocal;
  compiler->upvalues[upvalueCount].index = index;
  return compiler->function->upvalueCount++;
}

// Looks for a local variable declared in any of the surrounding functions.
// Returns an "upvalue index" for a found variable, or -1 to indicate the
// variable wasn't found (Hopefully it's global).
static int resolveUpvalue(Compiler* compiler, const Token* name) {
  // We've reached the outermost function without finding a local variable.
  if (compiler->enclosing == NULL) return -1;

  // We need to resolve local variables that are declared in surrounding
  // functions beyond the immediately enclosing one.
  int local = resolveLocal(compiler->enclosing, name);
  if (local != -1) {
    compiler->enclosing->locals[local].isCaptured = true;
    return addUpvalue(compiler, (uint8_t)local, true);
  }

  // We need to be able to capture variables that have already left the stack.
  // The solution is to allow a closure to capture either a local variable or an
  // existing upvalue in the immediately enclosing function. If a deeply nested
  // function references a local variable declared several hops away, we'll
  // thread it through all of the intermediate functions by having each function
  // capture an upvalue for the next function to grab.
  // A function captures -- either a local or upvalue -- only from the
  // immediately surrounding function, which is guaranteed to still be around at
  // the point that the inner function declaration executes.
  int upvalue = resolveUpvalue(compiler->enclosing, name);
  if (upvalue != -1) {
    return addUpvalue(compiler, (uint8_t)upvalue, false);
  }

  return -1;
}

static void addLocal(Token name) {
  if (current->localCount == UINT8_COUNT) {
    error("Too many local variables in function.");
    return;
  }

  Local* local = &current->locals[current->localCount++];
  local->name = name;
  local->depth = -1; // Indicate the uninitialized state.
  local->isCaptured = false;
}

static void declareVariable() {
  if (current->scopeDepth == 0) return;

  Token* name = &parser.previous;
  for (int i = current->localCount - 1; i >= 0; i--) {
    Local* local = &current->locals[i];
    if (local->depth != -1 && local->depth < current->scopeDepth) {
      break;
    }

    if (identifiersEqual(name, &local->name)) {
      error("Already a variable with this name in this scope.");
    }
  }

  addLocal(*name);
}

// Returns the index in the constant table for the name of a global variable, or
// 0 as a dummy table index for a local variable.
static uint8_t parseVariable(const char* errorMessage) {
  consume(TOKEN_IDENTIFIER, errorMessage);

  declareVariable();
  if (current->scopeDepth > 0) return 0;

  return identifierConstant(&parser.previous);
}

static void markInitialized() {
  if (current->scopeDepth == 0) return;
  current->locals[current->localCount - 1].depth = current->scopeDepth;
}

static void defineVariable(uint8_t global) {
  // In a local scope, we don't need to emit code to store a local variable.
  // At runtime, the code for the variable's initializer (or the implicit nil if
  // the user omitted an initializer) has already been executed, and the value
  // is sitting right on top of the stack as the only remaining temporary. The
  // temporary simply becomes the local variable.
  if (current->scopeDepth > 0) {
    markInitialized();
    return;
  }

  emitBytes(OP_DEFINE_GLOBAL, global);
}

static uint8_t argumentList() {
  uint8_t argCount = 0;
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      expression();
      // Since we stuff the argument count into the bytecode as a single-byte
      // operand, we can only go up to 255.
      if (argCount == 255) {
        error("Can't have more than 255 arguments.");
      }
      argCount++;
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  return argCount;
}

static void and_(bool UNUSED(canAssign)) {
  // At the point this is called, the left-hand side expression has already been
  // compiled. That means at runtime, its value will be on top of the stack. If
  // that value is falsey, then we know the entire "and" must be false, so we
  // skip the right operand and leave the left-hand side value as the result of
  // the entire expression.
  int endJump = emitJump(OP_JUMP_IF_FALSE);

  emitByte(OP_POP);
  parsePrecedence(PREC_AND);

  patchJump(endJump);
}

static void binary(bool UNUSED(canAssign)) {
  // The left operand is already compiled and left on the stack.
  // Now the compiler should compile the right operand before emitting the
  // instruction for the operator.
  TokenType operatorType = parser.previous.type;
  const ParseRule* rule = getRule(operatorType);
  // We use one higher level of precedence for the right operand because the
  // binary operators are left-associative.
  // (For right-associative operator like assignment, we would parse with the
  // same precedence as the current operator.)
  parsePrecedence((Precedence)(rule->precedence + 1));

  // a != b  ==>  !(a == b)
  // a >= b  ==>  !(a < b)    (NaN ignored)
  // a <= b  ==>  !(a > b)    (NaN ignored)
  switch (operatorType) {
    case TOKEN_BANG_EQUAL:    emitBytes(OP_EQUAL, OP_NOT); break;
    case TOKEN_EQUAL_EQUAL:   emitByte(OP_EQUAL); break;
    case TOKEN_GREATER:       emitByte(OP_GREATER); break;
    case TOKEN_GREATER_EQUAL: emitBytes(OP_LESS, OP_NOT); break;
    case TOKEN_LESS:          emitByte(OP_LESS); break;
    case TOKEN_LESS_EQUAL:    emitBytes(OP_GREATER, OP_NOT); break;
    case TOKEN_PLUS:          emitByte(OP_ADD); break;
    case TOKEN_MINUS:         emitByte(OP_SUBTRACT); break;
    case TOKEN_STAR:          emitByte(OP_MULTIPLY); break;
    case TOKEN_SLASH:         emitByte(OP_DIVIDE); break;
    default: return; // Unreachable.
  }
}

// A function call expression is kind of an infix ( operator. You have a
// high-precedence expression on the left for the thing being called---usually
// just a single identifier. Then the ( in the middle, followed by the argument
// expressions separated by commas, and a final ) to wrap it up at the end.
static void call(bool UNUSED(canAssign)) {
  // We've already consumed the ( token.
  uint8_t argCount = argumentList();
  emitBytes(OP_CALL, argCount);
}

static void dot(bool canAssign) {
  consume(TOKEN_IDENTIFIER, "Expect property name after '.'.");
  uint8_t name = identifierConstant(&parser.previous);

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(OP_SET_PROPERTY, name);
  } else if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    emitBytes(OP_INVOKE, name);
    emitByte(argCount);
  } else {
    emitBytes(OP_GET_PROPERTY, name);
  }
}

static void literal(bool UNUSED(canAssign)) {
  switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL:   emitByte(OP_NIL); break;
    case TOKEN_TRUE:  emitByte(OP_TRUE); break;
    default: return; // Unreachable.
  }
}

static void grouping(bool UNUSED(canAssign)) {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number(bool UNUSED(canAssign)) {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void or_(bool UNUSED(canAssign)) {
  int elseJump = emitJump(OP_JUMP_IF_FALSE);
  // In an "or" expression, if the left-hand side is truthy, then we skip over
  // the right operand. Thus we need to jump when a value is truthy.
  int endJump = emitJump(OP_JUMP);

  patchJump(elseJump);
  emitByte(OP_POP);

  parsePrecedence(PREC_OR);
  patchJump(endJump);
}

static void string(bool UNUSED(canAssign)) {
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
          parser.previous.length - 2)));
}

static void namedVariable(Token name, bool canAssign) {
  uint8_t getOp, setOp;
  uint8_t arg;
  int resolved = resolveLocal(current, &name);
  if (resolved != -1) {
    arg = (uint8_t)resolved;
    getOp = OP_GET_LOCAL;
    setOp = OP_SET_LOCAL;
  } else if ((resolved = resolveUpvalue(current, &name)) != -1) {
    arg = (uint8_t)resolved;
    getOp = OP_GET_UPVALUE;
    setOp = OP_SET_UPVALUE;
  } else {
    arg = identifierConstant(&name);
    getOp = OP_GET_GLOBAL;
    setOp = OP_SET_GLOBAL;
  }

  if (canAssign && match(TOKEN_EQUAL)) {
    expression();
    emitBytes(setOp, arg);
  } else {
    emitBytes(getOp, arg);
  }
}

static void variable(bool canAssign) {
  namedVariable(parser.previous, canAssign);
}

static Token syntheticToken(const char* text) {
  Token token;
  token.start = text;
  token.length = (int)strlen(text);
  return token;
}

static void super_(bool UNUSED(canAssign)) {
  // A super call is meaningful only inside the body of a method (or in a
  // function nested inside a method), and only inside the method of a class
  // that has a superclass.
  if (currentClass == NULL) {
    error("Can't use 'super' outside of a class.");
  } else if (!currentClass->hasSuperclass) {
    error("Can't use 'super' in a class with no superclass.");
  }

  // Unlike 'this', a 'super' token is not a standalone expression. Instead, the
  // dot and method name following it are inseparable parts of the syntax.
  consume(TOKEN_DOT, "Expect '.' after 'super'.");
  consume(TOKEN_IDENTIFIER, "Expect superclass method name.");
  uint8_t name = identifierConstant(&parser.previous);

  // Generates code to look up the current receiver stored in the hidden
  // variable "this" and push it onto the stack.
  namedVariable(syntheticToken("this"), false);
  // Emits code to look up the superclass from its "super" variable and push
  // that on top.
  if (match(TOKEN_LEFT_PAREN)) {
    uint8_t argCount = argumentList();
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_SUPER_INVOKE, name);
    emitByte(argCount);
  } else {
    namedVariable(syntheticToken("super"), false);
    emitBytes(OP_GET_SUPER, name);
  }
}

static void this_(bool UNUSED(canAssign)) {
  // Prevent misuse of "this"
  if (currentClass == NULL) {
    error("Can't use 'this' outside of a class.");
    return;
  }

  // We treat "this" as a lexically scoped local variable whose value gets
  // magically initialized.
  variable(false);
}

static void unary(bool UNUSED(canAssign)) {
  TokenType operatorType = parser.previous.type;

  // Compile the operand.
  parsePrecedence(PREC_UNARY);

  switch (operatorType) {
    case TOKEN_BANG: emitByte(OP_NOT); break;
    case TOKEN_MINUS: emitByte(OP_NEGATE); break;
    default: return; // Unreachable.
  }
}

static const ParseRule rules[] = {
  [TOKEN_LEFT_PAREN]    = {grouping, call,   PREC_CALL},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     dot,    PREC_CALL},
  [TOKEN_MINUS]         = {unary,    binary, PREC_TERM},
  [TOKEN_PLUS]          = {NULL,     binary, PREC_TERM},
  [TOKEN_SEMICOLON]     = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SLASH]         = {NULL,     binary, PREC_FACTOR},
  [TOKEN_STAR]          = {NULL,     binary, PREC_FACTOR},
  [TOKEN_BANG]          = {unary,    NULL,   PREC_NONE},
  [TOKEN_BANG_EQUAL]    = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_EQUAL]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EQUAL_EQUAL]   = {NULL,     binary, PREC_EQUALITY},
  [TOKEN_GREATER]       = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_GREATER_EQUAL] = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS]          = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_LESS_EQUAL]    = {NULL,     binary, PREC_COMPARISON},
  [TOKEN_IDENTIFIER]    = {variable, NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     and_,   PREC_AND},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     or_,    PREC_OR},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {super_,   NULL,   PREC_NONE},
  [TOKEN_THIS]          = {this_,    NULL,   PREC_NONE},
  [TOKEN_TRUE]          = {literal,  NULL,   PREC_NONE},
  [TOKEN_VAR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_WHILE]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ERROR]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_EOF]           = {NULL,     NULL,   PREC_NONE},
};

static void parsePrecedence(Precedence precedence) {
  advance();
  // The first token may turn out to be nested as an operand inside one or more
  // infix expressions, but it nevertheless belongs to some kind of prefix
  // expression, by definition.
  ParseFn prefixRule = getRule(parser.previous.type)->prefix;
  if (prefixRule == NULL) {
    error("Expect expression.");
    return;
  }

  // Since assignment is the lowest-precedence expression, the only time we
  // allow an assignment is when parsing an assignment expression or top-level
  // expression like in an expression statement.
  bool canAssign = precedence <= PREC_ASSIGNMENT;
  prefixRule(canAssign);

  // We look for an infix parser for the next token after parsing the prefix
  // expression. If we find one, it means the prefix expression we already
  // compiled might be an operand for it. But only if the call to
  // parsePrecedence() has a precedence that is low enough to permit that infix
  // operator.
  // After the first turn (if any), we loop back around and see if the next
  // token is also a valid infix operator that can take the entire preceding
  // expression as its operand.
  while (precedence <= getRule(parser.current.type)->precedence) {
    advance();
    ParseFn infixRule = getRule(parser.previous.type)->infix;
    infixRule(/* unused */ canAssign);
  }

  // If the = doesn't get consumed as part of the expression, nothing else is
  // going to consume it.
  if (canAssign && match(TOKEN_EQUAL)) {
    error("Invalid assignment target.");
  }
}

static const ParseRule* getRule(TokenType type) {
  return &rules[type];
}

static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

// block -> "{" declaration* "}" ;
static void block() {
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    declaration();
  }

  consume(TOKEN_RIGHT_BRACE, "Expect '}' after block.");
}

static void function(FunctionType type) {
  Compiler compiler;
  initCompiler(&compiler, type);
  // This beginScope() doesn't have a corresponding endScope() call. Because we
  // end Compiler completely when we reach the end of the function body, there's
  // no need to close the lingering outermost scope.
  beginScope();

  consume(TOKEN_LEFT_PAREN, "Expect '(' after function name.");
  if (!check(TOKEN_RIGHT_PAREN)) {
    do {
      current->function->arity++;
      if (current->function->arity > 255) {
        errorAtCurrent("Can't have more than 255 parameters.");
      }
      uint8_t constant = parseVariable("Expect parameter name.");
      defineVariable(constant);
    } while (match(TOKEN_COMMA));
  }
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
  consume(TOKEN_LEFT_BRACE, "Expect '{' before function body.");
  block();

  ObjFunction* function = endCompiler();
  emitBytes(OP_CLOSURE, makeConstant(OBJ_VAL(function)));

  // By the time the compiler reaches the end of a function declaration, every
  // variable reference has been resolved as either a local, an upvalue, or a
  // global.

  // The OP_CLOSURE instruction has a variably sized encoding.
  for (int i = 0; i < function->upvalueCount; i++) {
    emitByte(compiler.upvalues[i].isLocal ? 1 : 0);
    emitByte(compiler.upvalues[i].index);
  }
}

static void method() {
  consume(TOKEN_IDENTIFIER, "Expect method name.");
  uint8_t constant = identifierConstant(&parser.previous);

  FunctionType type = TYPE_METHOD;
  if (parser.previous.length == 4 &&
      memcmp(parser.previous.start, "init", 4) == 0) {
    type = TYPE_INITIALIZER;
  }

  function(type);
  emitBytes(OP_METHOD, constant);
}

static void classDeclaration() {
  consume(TOKEN_IDENTIFIER, "Expect class name.");
  Token className = parser.previous;
  uint8_t nameConstant = identifierConstant(&parser.previous);
  declareVariable();

  emitBytes(OP_CLASS, nameConstant);
  // We define the variable before the body, so that users can refer to the
  // containing class inside the bodies of its own methods.
  defineVariable(nameConstant);

  ClassCompiler classCompiler;
  classCompiler.enclosing = currentClass;
  classCompiler.hasSuperclass = false;
  currentClass = &classCompiler;

  if (match(TOKEN_LESS)) {
    consume(TOKEN_IDENTIFIER, "Expect superclass name.");
    // Looks up the superclass by name and pushes it onto the stack.
    variable(false);

    if (identifiersEqual(&className, &parser.previous)) {
      error("A class can't inherit from itself.");
    }

    beginScope();
    addLocal(syntheticToken("super"));
    defineVariable(0);

    // Load the subclass doing inheriting onto the stack.
    namedVariable(className, false);
    emitByte(OP_INHERIT);
    classCompiler.hasSuperclass = true;
  }

  // Right before compiling the class body, we load the class variable onto the
  // stack, so that the VM knows which class to bind the method to when it
  // reaches an OP_METHOD instruction.
  namedVariable(className, false);
  consume(TOKEN_LEFT_BRACE, "Expect '{' before class body.");
  // Lox doesn't have field declarations, so anything before the closing brace
  // at the end of the class body must be a method.
  while (!check(TOKEN_RIGHT_BRACE) && !check(TOKEN_EOF)) {
    method();
  }
  consume(TOKEN_RIGHT_BRACE, "Expect '}' after class body.");
  // Once we've reached the end of the methods, we no longer need the class on
  // the stack.
  emitByte(OP_POP);

  if (classCompiler.hasSuperclass) {
    endScope();
  }

  currentClass = currentClass->enclosing;
}

static void funDeclaration() {
  // A function declaration at the top level will bind the function to a global
  // variable. Inside a block or other function, a function declaration creates
  // a local variable.
  uint8_t global = parseVariable("Expect function name.");
  // It's safe for a function to refer to its own name inside its body. You
  // can't call the function and execute the body until after it's fully
  // defined, so you'll never see the variable in an unintialized state.
  // Practiacally speaking, it's useful to allow this in order to support
  // recursive local functions.
  markInitialized();
  function(TYPE_FUNCTION);
  defineVariable(global);
}

static void varDeclaration() {
  uint8_t global = parseVariable("Expect variable name.");

  if (match(TOKEN_EQUAL)) {
    expression();
  } else {
    // Desugars a variable declaration like:
    //   var a;
    // into:
    //   var a = nil;
    emitByte(OP_NIL);
  }
  consume(TOKEN_SEMICOLON, "Expect ';' after variable declaration.");

  defineVariable(global);
}

static void expressionStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after expression.");
  emitByte(OP_POP);
}

static void forStatement() {
  beginScope();
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'for'.");
  if (match(TOKEN_SEMICOLON)) {
    // No initializer.
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    expressionStatement();
  }

  int loopStart = currentChunk()->count;
  int exitJump = -1;
  if (!match(TOKEN_SEMICOLON)) {
    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after loop condition.");

    // Jump out of the loop if the condition is false.
    exitJump = emitJump(OP_JUMP_IF_FALSE);
    emitByte(OP_POP); // Condition.
  }

  if (!match(TOKEN_RIGHT_PAREN)) {
    // We can't compile the increment clause after the for's body, since our
    // compiler only makes a single pass over the code. Instead, we'll jump over
    // the increment, run the body, jump back up to the increment, run it, and
    // then go to the next iteration.
    int bodyJump = emitJump(OP_JUMP);
    int incrementStart = currentChunk()->count;
    expression();
    emitByte(OP_POP); // We only execute the increment clause for its side
                      // effect, so we also emit a pop to discard its value.
    consume(TOKEN_RIGHT_PAREN, "Expect ')' after for clauses.");

    emitLoop(loopStart);
    loopStart = incrementStart;
    patchJump(bodyJump);
  }

  statement();
  emitLoop(loopStart);

  if (exitJump != -1) {
    patchJump(exitJump);
    emitByte(OP_POP); // Condition.
  }

  endScope();
}

static void ifStatement() {
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int thenJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();

  int elseJump = emitJump(OP_JUMP);

  patchJump(thenJump);
  emitByte(OP_POP);

  if (match(TOKEN_ELSE)) statement();
  patchJump(elseJump);
}

static void printStatement() {
  expression();
  consume(TOKEN_SEMICOLON, "Expect ';' after value.");
  emitByte(OP_PRINT);
}

static void returnStatement() {
  if (current->type == TYPE_SCRIPT) {
    error("Can't return from top-level code.");
  }

  if (match(TOKEN_SEMICOLON)) {
    // If there is no return value, the statement implicitly returns nil.
    emitReturn();
  } else {
    if (current->type == TYPE_INITIALIZER) {
      error("Can't return a value from an initializer.");
      // We still go ahead and compile the value afterwards so that the compiler
      // doesn't get confused by the trailing expression and report a bunch of
      // cascaded errors.
    }

    expression();
    consume(TOKEN_SEMICOLON, "Expect ';' after return value.");
    emitByte(OP_RETURN);
  }
}

static void whileStatement() {
  // Record the offset in the bytecode right before the condition expression
  // we're about to compile.
  int loopStart = currentChunk()->count;
  consume(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after condition.");

  int exitJump = emitJump(OP_JUMP_IF_FALSE);
  emitByte(OP_POP);
  statement();
  emitLoop(loopStart);

  patchJump(exitJump);
  emitByte(OP_POP);
}

// We skip tokens indiscriminately until we reach something that looks like a
// statement boundary. We recognize the boundary by looking for a preceding
// token that can end a statement, like a semicolon. Or we'll look for a
// subsequent token that begins a statement, usually one of the control flow or
// declaration keywords.
static void synchronize() {
  parser.panicMode = false;

  while (parser.current.type != TOKEN_EOF) {
    if (parser.previous.type == TOKEN_SEMICOLON) return;
    switch (parser.current.type) {
      case TOKEN_CLASS:
      case TOKEN_FUN:
      case TOKEN_VAR:
      case TOKEN_FOR:
      case TOKEN_IF:
      case TOKEN_WHILE:
      case TOKEN_PRINT:
      case TOKEN_RETURN:
        return;

      default:
        ; // Do nothing.
    }

    advance();
  }
}

// declaration -> funDecl
//              | varDecl
//              | statement ;
static void declaration() {
  if (match(TOKEN_CLASS)) {
    classDeclaration();
  } else if (match(TOKEN_FUN)) {
    funDeclaration();
  } else if (match(TOKEN_VAR)) {
    varDeclaration();
  } else {
    statement();
  }

  if (parser.panicMode) synchronize();
}

// statement -> exprStmt
//            | printStmt
//            | ifStmt
//            | returnStmt
//            | whileStmt
//            | forStmt
//            | block ;
static void statement() {
  if (match(TOKEN_PRINT)) {
    printStatement();
  } else if (match(TOKEN_FOR)) {
    forStatement();
  } else if (match(TOKEN_IF)) {
    ifStatement();
  } else if (match(TOKEN_RETURN)) {
    returnStatement();
  } else if (match(TOKEN_WHILE)) {
    whileStatement();
  } else if (match(TOKEN_LEFT_BRACE)) {
    beginScope();
    block();
    endScope();
  } else {
    expressionStatement();
  }
}

ObjFunction* compile(const char* source) {
  initScanner(source);
  Compiler compiler;
  initCompiler(&compiler, TYPE_SCRIPT);

  parser.hadError = false;
  parser.panicMode = false;

  advance();

  while (!match(TOKEN_EOF)) {
    declaration();
  }

  ObjFunction* function = endCompiler();
  return parser.hadError ? NULL : function;
}

void markCompilerRoots() {
  Compiler* compiler = current;
  while (compiler != NULL) {
    markObject((Obj*)compiler->function);
    compiler = compiler->enclosing;
  }
}
