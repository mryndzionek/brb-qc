#ifndef BRB_INTERFACE_H_
#define BRB_INTERFACE_H_

#include <stddef.h>
#include <stdint.h>

void rbiSetUp(size_t s);
int rbiOffer(const uint8_t *data, const int size);
uint8_t *rbiPoll(const size_t size);
int rbiUsed(void);
void rbiTearDown(void);

#endif /* BRB_INTERFACE_H_ */
