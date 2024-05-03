#ifndef clox_common_h
#define clox_common_h

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __GNUC__
#define UNUSED(a) UNUSED_ ## a __attribute__((unused))
#else
#define UNUSED(a) UNUSED_ ## a
#endif

#define NAN_BOXING

//#define DEBUG_PRINT_CODE
//#define DEBUG_TRACE_EXECUTION

//#define DEBUG_STRESS_GC
//#define DEBUG_LOG_GC

#define UINT8_COUNT (UINT8_MAX + 1)

#endif
