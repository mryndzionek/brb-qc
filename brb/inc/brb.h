#ifndef BRB_H_
#define BRB_H_

#include <stddef.h>
#include <stdint.h>

typedef struct
{
    size_t size;
    unsigned int a_start, a_end;
    unsigned int b_end;
    int b_inuse;
    uint8_t data[];
} brb_t;

brb_t *brb_new(const size_t size);
void brb_init(brb_t* me, const size_t size);
void brb_free(brb_t *me);
int brb_offer(brb_t *me, const uint8_t *data, const int size);
uint8_t *brb_peek(const brb_t* me, const size_t len);
uint8_t *brb_poll(brb_t* me, const size_t size);
int brb_size(const brb_t* me);
int brb_is_empty(const brb_t* me);
int brb_used(const brb_t* cb);
int brb_unused(const brb_t* me);

#endif /* BRB_H_ */
