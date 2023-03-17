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
    void *stack_top;
    void *ret;
    jmp_buf buf;
} ContHandler;

typedef struct ContFragment {
    void *fragment;
    ptrdiff_t fragment_size;
} ContFragment;

static __noinline void *stack_top(void *top) { return top; }

__returnstwice ContHandler *cont_save_handler() {
    ContHandler *h = alloca(sizeof(ContHandler));
    void *base = NULL;
    h->stack_top =
        stack_top(&base); // might not be enough, you might wanna capture stack
                          // all the way up to the last fn
    h->ret = NULL;
    // if (!setjmp(h->buf))
    return h;
    // printf("here\n");
    // return NULL;
}

__noreturn void resume_to_handler(ContHandler *h) {
    printf("jumping to %p\n", h->stack_top);
    longjmp(h->buf, 1);
}

ContFragment cont_capture_to_handler(ContHandler *handler) {
    void *base = NULL;
    void *top = stack_top(&base);
    assert(top < handler->stack_top); // stack grows downwards, right?
    // ContResumption *r = scalanative_alloc(NULL, sizeof(ContResumption));
    ContFragment f = {.fragment_size = handler->stack_top - top,
                      .fragment = NULL};
    printf("!copying %lu bytes\n", f.fragment_size);
    f.fragment = malloc(f.fragment_size);
    // r->stack_fragment = scalanative_alloc(NULL, r->fragment_size);
    memcpy(f.fragment, top, f.fragment_size);
    // TODO: what to do with jmp_buf?
    return f;
}

// void cont_resume(ContResumption *r) {}
