#include "stdio.h"
#include <stdlib.h>

#include <string.h>
#include "brb.h"

static size_t brb_sizeof(const size_t size)
{
    return sizeof(brb_t) + size;
}

int brb_unused(const brb_t* me)
{
    if (1 == me->b_inuse)
        /* distance between region B and region A */
        return me->a_start - me->b_end;
    else
        return me->size - me->a_end;
}

int brb_size(const brb_t* me)
{
    return me->size;
}

int brb_used(const brb_t* me)
{
    return (me->a_end - me->a_start) + me->b_end;
}

void brb_init(brb_t* me, const size_t size)
{
    me->a_start = me->a_end = me->b_end = 0;
    me->size = size;
    me->b_inuse = 0;
}

brb_t *brb_new(const size_t size)
{
    brb_t *me = malloc(brb_sizeof(size));
    if (!me)
        return NULL;
    brb_init(me, size);
    return me;
}

void brb_free(brb_t* me)
{
    free(me);
}

int brb_is_empty(const brb_t* me)
{
    return me->a_start == me->a_end;
}

/* find out if we should turn on region B
 * ie. is the distance from A to buffer's end less than B to A? */
static void __check_for_switch_to_b(brb_t* me)
{
    if (me->size - me->a_end < me->a_start - me->b_end)
        me->b_inuse = 1;
}

int brb_offer(brb_t* me, const uint8_t *data, const int size)
{
    /* not enough space */
    if (brb_unused(me) < size)
        return 0;

    if (1 == me->b_inuse)
    {
        memcpy(me->data + me->b_end, data, size);
        me->b_end += size;
    }
    else
    {
        memcpy(me->data + me->a_end, data, size);
        me->a_end += size;
    }

    __check_for_switch_to_b(me);
    return size;
}

uint8_t *brb_peek(const brb_t* me, const size_t size)
{
    /* make sure we can actually peek at this data */
    if (me->size <= me->a_start + size)
        return NULL;

    if (brb_is_empty(me))
        return NULL;

    return (unsigned char*)me->data + me->a_start;
}

uint8_t *brb_poll(brb_t* me, const size_t size)
{
    if (brb_is_empty(me))
        return NULL;

    /* make sure we can actually poll this data */
    if (me->a_start + size > me->a_end)
        return NULL;

    void *end = me->data + me->a_start;
    me->a_start += size;

    /* we seem to be empty.. */
    if (me->a_start == me->a_end)
    {
        /* replace a with region b */
        if (1 == me->b_inuse)
        {
            me->a_start = 0;
            me->a_end = me->b_end;
            me->b_end = me->b_inuse = 0;
        }
        else
            /* safely move cursor back to the start because we are empty */
            me->a_start = me->a_end = 0;
    }

    __check_for_switch_to_b(me);
    return end;
}
