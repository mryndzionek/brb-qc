#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "brb.h"

int main(void) {
	brb_t *cb;

	cb = brb_new(13);

	{
		uint8_t a[] = {221, 76};
		brb_offer(cb, a, 2);
		uint8_t *b = brb_poll(cb, 2);
		int ret = memcmp(a, b, 2);
		printf("%d\r\n", ret);
	}

	brb_free(cb);

	return EXIT_SUCCESS;
}
