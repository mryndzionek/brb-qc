#include "brb_interface.h"

#include "brb.h"

static brb_t *pbrb;

void rbiSetUp(size_t s) {
    pbrb = brb_new(s);
}

int rbiOffer(const uint8_t *data, const int size) {
    return brb_offer(pbrb, data, size);
}

uint8_t *rbiPoll(const size_t size) {
    return brb_poll(pbrb, size);
}

int rbiUsed(void) {
    return brb_used(pbrb);
}

void rbiTearDown(void) {
    brb_free(pbrb);
}
