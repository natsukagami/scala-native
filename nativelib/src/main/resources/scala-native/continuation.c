#include "gc/shared/ScalaNativeGC.h"
#include <setjmp.h>
#include <stddef.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

// assume gcc or clang
#define __noinline __attribute__((noinline))
#define __noreturn __attribute__((noreturn))
#define __returnstwice __attribute__((returns_twice))

typedef struct ContHandler {
    void *stack_btm;
    void *ret;
    int label;
    void *buf[20];
} ContHandler;

typedef struct ContFragment {
    void *fragment;
    ptrdiff_t fragment_size;
    void *top;
} ContFragment;

static __noinline void *stack_top(void *top) { return top; }

__returnstwice ContHandler *cont_save_handler() {
    ContHandler *h = alloca(sizeof(ContHandler));
    void *base = NULL;
    h->stack_btm =
        stack_top(&base); // might not be enough, you might wanna capture stack
                          // all the way up to the last fn
    h->ret = NULL;
    // if (!setjmp(h->buf))
    return h;
    // printf("here\n");
    // return NULL;
}

ContFragment *cont_capture_to_handler(ContHandler *handler) {
    void *base = NULL;
    // ContResumption *r = scalanative_alloc(NULL, sizeof(ContResumption));
    ContFragment *f = malloc(sizeof(ContFragment));
    f->top = stack_top(&base);
    assert(f->top < handler->stack_btm); // stack grows downwards, right?
    f->fragment_size = handler->stack_btm - f->top;
    f->fragment = malloc(f->fragment_size);
    // r->stack_fragment = scalanative_alloc(NULL, r->fragment_size);
    memcpy(f->fragment, f->top, f->fragment_size);
    printf("!!copying %lu bytes, from %p to %p (handler = %p)\n",
           f->fragment_size, f->top, handler->stack_btm, handler);
    // TODO: what to do with jmp_buf?
    return f;
}

// extern void _lh_longjmp(void *jmpbuf, int arg);

// void do_resume(void *fragment, ptrdiff_t fragment_size, void *f_stacktop,
// void *#)
