#include <stdio.h>
#include <stdlib.h>

#include "chunk.h"
#include "common.h"
#include "compiler.h"
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

typedef void (*ParseFn)();

typedef struct {
  ParseFn prefix;
  ParseFn infix;
  Precedence precedence;
} ParseRule;

static Parser parser;
static Chunk* compilingChunk;

static Chunk* currentChunk() {
  return compilingChunk;
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

static void emitByte(uint8_t byte) {
  writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
  emitByte(byte1);
  emitByte(byte2);
}

static void emitReturn() {
  emitByte(OP_RETURN);
}

static void emitConstant(Value value) {
  int written = writeConstant(currentChunk(), value, parser.previous.line);
  if (written == -1) {
    error("Too many constants in one chunk.");
  }
}

static void endCompiler() {
  emitReturn();
#ifdef DEBUG_PRINT_CODE
  if (!parser.hadError) {
    disassembleChunk(currentChunk(), "code");
  }
#endif
}

static void expression();
static const ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);

static void binary() {
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

static void literal() {
  switch (parser.previous.type) {
    case TOKEN_FALSE: emitByte(OP_FALSE); break;
    case TOKEN_NIL:   emitByte(OP_NIL); break;
    case TOKEN_TRUE:  emitByte(OP_TRUE); break;
    default: return; // Unreachable.
  }
}

static void grouping() {
  expression();
  consume(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void number() {
  double value = strtod(parser.previous.start, NULL);
  emitConstant(NUMBER_VAL(value));
}

static void string() {
  emitConstant(OBJ_VAL(copyString(parser.previous.start + 1,
          parser.previous.length - 2)));
}

static void unary() {
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
  [TOKEN_LEFT_PAREN]    = {grouping, NULL,   PREC_NONE},
  [TOKEN_RIGHT_PAREN]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_LEFT_BRACE]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RIGHT_BRACE]   = {NULL,     NULL,   PREC_NONE},
  [TOKEN_COMMA]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_DOT]           = {NULL,     NULL,   PREC_NONE},
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
  [TOKEN_IDENTIFIER]    = {NULL,     NULL,   PREC_NONE},
  [TOKEN_STRING]        = {string,   NULL,   PREC_NONE},
  [TOKEN_NUMBER]        = {number,   NULL,   PREC_NONE},
  [TOKEN_AND]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_CLASS]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_ELSE]          = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FALSE]         = {literal,  NULL,   PREC_NONE},
  [TOKEN_FOR]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_FUN]           = {NULL,     NULL,   PREC_NONE},
  [TOKEN_IF]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_NIL]           = {literal,  NULL,   PREC_NONE},
  [TOKEN_OR]            = {NULL,     NULL,   PREC_NONE},
  [TOKEN_PRINT]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_RETURN]        = {NULL,     NULL,   PREC_NONE},
  [TOKEN_SUPER]         = {NULL,     NULL,   PREC_NONE},
  [TOKEN_THIS]          = {NULL,     NULL,   PREC_NONE},
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
  prefixRule();

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
    infixRule();
  }
}

static const ParseRule* getRule(TokenType type) {
  return &rules[type];
}

static void expression() {
  parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(const char* source, Chunk* chunk) {
  initScanner(source);
  compilingChunk = chunk;

  parser.hadError = false;
  parser.panicMode = false;

  advance();
  expression();
  consume(TOKEN_EOF, "Expect end of expression.");
  endCompiler();
  return !parser.hadError;
}
