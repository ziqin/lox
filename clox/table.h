#ifndef clox_table_h
#define clox_table_h

#include "common.h"
#include "value.h"

typedef struct {
  ObjString* key;
  Value value;
} Entry;

typedef struct {
  int count; // The number of non-empty entries and tombstones.
  int capacity;
  Entry* entries;
} Table;

void initTable(Table* table);
void freeTable(Table* table);
bool tableGet(const Table* table, const ObjString* key, Value* value);
bool tableSet(Table* table, ObjString* key, Value value);
bool tableDelete(Table* table, const ObjString* key);
void tableAddAll(const Table* from, Table* to);
ObjString* tableFindString(const Table* table, const char* chars,
                           int length, uint32_t hash);

#endif
