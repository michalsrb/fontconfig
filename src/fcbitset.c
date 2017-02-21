/*
 * fontconfig/src/fcbitset.c
 *
 * TODO: Add copyright.
 *
 */

#include "fcint.h"
#include <stdlib.h>

struct _FcBitset {
    size_t size;
    size_t ones;
    FcChar8 data[];
};

FcBitset *
FcBitsetCreate (size_t size)
{
    size_t size_in_bytes = (size + 7) / 8;

    FcBitset *bitset = (FcBitset *) malloc(sizeof(FcBitset) + size_in_bytes);
    if (!bitset)
	return NULL;

    bitset->size = size;

    return bitset;
}

void
FcBitsetDestroy (FcBitset *bitset)
{
    assert(bitset->size != 0);

    bitset->size = 0;
    free (bitset);
}

void
FcBitsetClear (FcBitset *bitset, FcBool value)
{
    assert(bitset->size != 0);

    size_t size_in_bytes = (bitset->size + 7) / 8;

    if (value)
	bitset->ones = bitset->size;
    else
	bitset->ones = 0;

    memset(bitset->data, value ? 255 : 0, size_in_bytes);
}

void
FcBitsetSet (FcBitset *bitset, size_t index, FcBool value)
{
    assert(index < bitset->size);

    FcChar8 bit = (1 << (index % 8));

    if (value) {
	if (!(bitset->data[index / 8] & bit))
	    bitset->ones++;

	bitset->data[index / 8] |= bit;
    } else {
	if (bitset->data[index / 8] & bit)
	    bitset->ones--;

	bitset->data[index / 8] &= ~bit;
    }
}

FcBool
FcBitsetGet (const FcBitset *bitset, size_t index)
{
    assert(index < bitset->size);

    return bitset->data[index / 8] & (1 << (index % 8));
}

size_t
FcBitsetCountOnes (const FcBitset *bitset)
{
    assert(bitset->size != 0);

    return bitset->ones;
}
