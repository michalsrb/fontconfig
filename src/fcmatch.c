/*
 * fontconfig/src/fcmatch.c
 *
 * Copyright © 2000 Keith Packard
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of the author(s) not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  The authors make no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * THE AUTHOR(S) DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 */

#include "fcint.h"

static double
FcCompareNumber (FcValue *value1, FcPrepValue *prep1, FcValue *value2, FcPrepValue *prep2)
{
    double  v1, v2, v;

    switch ((int) value1->type) {
    case FcTypeInteger:
	v1 = (double) value1->u.i;
	break;
    case FcTypeDouble:
	v1 = value1->u.d;
	break;
    default:
	return -1.0;
    }
    switch ((int) value2->type) {
    case FcTypeInteger:
	v2 = (double) value2->u.i;
	break;
    case FcTypeDouble:
	v2 = value2->u.d;
	break;
    default:
	return -1.0;
    }
    v = v2 - v1;
    if (v < 0)
	v = -v;
    return v;
}

static FcPrepValue
FcPreprocessString (FcValue *v)
{
    FcPrepValue prep;
    prep.type = FcPrepStrHashIgnoreCase;
    prep.str_hash = FcStrHashIgnoreCase(FcValueString(v));
    return prep;
}

static double
FcCompareString (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    if (p1->type == FcPrepStrHashIgnoreCase &&
	p2->type == FcPrepStrHashIgnoreCase)
    {
	// If hashes are not matching, return fast
	if (p1->str_hash != p2->str_hash)
	    return 1.0;
    }

    return (double) FcStrCmpIgnoreCase (FcValueString(v1), FcValueString(v2)) != 0;
}

static FcPrepValue
FcPreprocessFamily (FcValue *v)
{
    FcPrepValue prep;
    prep.type = FcPrepStrHashIgnoreBlanksAndCase;
    prep.str_hash = FcStrHashIgnoreBlanksAndCase(FcValueString(v));
    return prep;
}

static double
FcCompareFamily (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    if (likely(p1->type == FcPrepStrHashIgnoreBlanksAndCase &&
	       p2->type == FcPrepStrHashIgnoreBlanksAndCase))
    {
	// If hashes are not matching, return fast
	if (likely(p1->str_hash != p2->str_hash))
	    return 1.0;
    }

    /* rely on the guarantee in FcPatternObjectAddWithBinding that
     * families are always FcTypeString. */
    const FcChar8* v1_string = FcValueString(v1);
    const FcChar8* v2_string = FcValueString(v2);

    if (FcToLower(*v1_string) != FcToLower(*v2_string) &&
	*v1_string != ' ' && *v2_string != ' ')
       return 1.0;

    return (double) FcStrCmpIgnoreBlanksAndCase (v1_string, v2_string) != 0;
}

static double
FcComparePostScript (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    const FcChar8 *v1_string = FcValueString (v1);
    const FcChar8 *v2_string = FcValueString (v2);
    int n;
    size_t len;

    if (FcToLower (*v1_string) != FcToLower (*v2_string) &&
	*v1_string != ' ' && *v2_string != ' ')
	return 1.0;

    n = FcStrMatchIgnoreCaseAndDelims (v1_string, v2_string, (const FcChar8 *)" -");
    len = strlen ((const char *)v1_string);

    return (double)(len - n) / (double)len;
}

static double
FcCompareLang (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    FcLangResult    result;
    FcValue value1 = FcValueCanonicalize(v1), value2 = FcValueCanonicalize(v2);

    switch ((int) value1.type) {
    case FcTypeLangSet:
	switch ((int) value2.type) {
	case FcTypeLangSet:
	    result = FcLangSetCompare (value1.u.l, value2.u.l);
	    break;
	case FcTypeString:
	    result = FcLangSetHasLang (value1.u.l,
				       value2.u.s);
	    break;
	default:
	    return -1.0;
	}
	break;
    case FcTypeString:
	switch ((int) value2.type) {
	case FcTypeLangSet:
	    result = FcLangSetHasLang (value2.u.l, value1.u.s);
	    break;
	case FcTypeString:
	    result = FcLangCompare (value1.u.s,
				    value2.u.s);
	    break;
	default:
	    return -1.0;
	}
	break;
    default:
	return -1.0;
    }
    switch (result) {
    case FcLangEqual:
	return 0;
    case FcLangDifferentCountry:
	return 1;
    case FcLangDifferentLang:
    default:
	return 2;
    }
}

static double
FcCompareBool (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    if (v2->type != FcTypeBool || v1->type != FcTypeBool)
	return -1.0;
    return (double) v2->u.b != v1->u.b;
}

static double
FcCompareCharSet (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    return (double) FcCharSetSubtractCount (FcValueCharSet(v1), FcValueCharSet(v2));
}

static double
FcCompareSize (FcValue *value1, FcPrepValue *p1, FcValue *value2, FcPrepValue *p2)
{
    double  v1, v2, v;

    switch ((int) value1->type) {
    case FcTypeInteger:
	v1 = value1->u.i;
	break;
    case FcTypeDouble:
	v1 = value1->u.d;
	break;
    default:
	return -1;
    }
    switch ((int) value2->type) {
    case FcTypeInteger:
	v2 = value2->u.i;
	break;
    case FcTypeDouble:
	v2 = value2->u.d;
	break;
    default:
	return -1;
    }
    if (v2 == 0)
	return 0;
    v = v2 - v1;
    if (v < 0)
	v = -v;
    return v;
}

static double
FcCompareSizeRange (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    FcValue value1 = FcValueCanonicalize (v1);
    FcValue value2 = FcValueCanonicalize (v2);
    FcRange *r1 = NULL, *r2 = NULL;
    double ret = -1.0;

    switch ((int) value1.type) {
    case FcTypeDouble:
	r1 = FcRangeCreateDouble (value1.u.d, value1.u.d);
	break;
    case FcTypeRange:
	r1 = FcRangeCopy (value1.u.r);
	break;
    default:
	goto bail;
    }
    switch ((int) value2.type) {
    case FcTypeDouble:
	r2 = FcRangeCreateDouble (value2.u.d, value2.u.d);
	break;
    case FcTypeRange:
	r2 = FcRangeCopy (value2.u.r);
	break;
    default:
	goto bail;
    }

    if (FcRangeIsInRange (r1, r2))
	ret = 0.0;
    else
	ret = FC_MIN (fabs (r1->end - r2->begin), fabs (r1->begin - r2->end));

bail:
    if (r1)
	FcRangeDestroy (r1);
    if (r2)
	FcRangeDestroy (r2);

    return ret;
}

static FcPrepValue
FcPreprocessFilename (FcValue *v)
{
    FcPrepValue prep;

    prep.type = FcPrepStrFilename;

    const FcChar8* str = FcValueString(v);
    prep.str_hash = FcStrHashIgnoreBlanksAndCase(str);
    prep.filename_has_globs = (strchr((const char *)str, '*') != NULL && strchr((const char *)str, '?') != NULL);

    return prep;
}

static double
FcCompareFilename (FcValue *v1, FcPrepValue *p1, FcValue *v2, FcPrepValue *p2)
{
    FcBool may_have_globs = FcTrue;
    FcBool may_be_same = FcTrue;

    if (p1->type == FcPrepStrFilename)
    {
	may_have_globs = p1->filename_has_globs;

	if (p2->type == FcPrepStrFilename)
	{
	    may_be_same = (p1->str_hash == p2->str_hash);
	}
    }

    const FcChar8 *s1 = FcValueString (v1), *s2 = FcValueString (v2);
    if (may_be_same && FcStrCmp (s1, s2) == 0)
	return 0.0;
    else if (may_be_same && FcStrCmpIgnoreCase (s1, s2) == 0)
	return 1.0;
    else if (may_have_globs && FcStrGlobMatch (s1, s2))
	return 2.0;
    else
	return 3.0;
}


/* Define priorities to -1 for objects that don't have a compare function. */

#define PRI_NULL(n)				\
    PRI_ ## n ## _STRONG = -1,			\
    PRI_ ## n ## _WEAK = -1,
#define PRI1(n)
#define PRI_FcCompareFamily(n)		PRI1(n)
#define PRI_FcCompareString(n)		PRI1(n)
#define PRI_FcCompareNumber(n)		PRI1(n)
#define PRI_FcCompareSize(n)		PRI1(n)
#define PRI_FcCompareBool(n)		PRI1(n)
#define PRI_FcCompareFilename(n)	PRI1(n)
#define PRI_FcCompareCharSet(n)		PRI1(n)
#define PRI_FcCompareLang(n)		PRI1(n)
#define PRI_FcComparePostScript(n)	PRI1(n)
#define PRI_FcCompareSizeRange(n)	PRI1(n)

#define FC_OBJECT(NAME, Type, Cmp, Prep)	PRI_##Cmp(NAME)

typedef enum _FcMatcherPriorityDummy {
#include "fcobjs.h"
} FcMatcherPriorityDummy;

#undef FC_OBJECT


/* Canonical match priority order. */

#undef PRI1
#define PRI1(n)					\
    PRI_ ## n,					\
    PRI_ ## n ## _STRONG = PRI_ ## n,		\
    PRI_ ## n ## _WEAK = PRI_ ## n

typedef enum _FcMatcherPriority {
    PRI1(FILE),
    PRI1(FONTFORMAT),
    PRI1(SCALABLE),
    PRI1(COLOR),
    PRI1(FOUNDRY),
    PRI1(CHARSET),
    PRI_FAMILY_STRONG,
    PRI_POSTSCRIPT_NAME_STRONG,
    PRI1(LANG),
    PRI_FAMILY_WEAK,
    PRI_POSTSCRIPT_NAME_WEAK,
    PRI1(SYMBOL),
    PRI1(SPACING),
    PRI1(SIZE),
    PRI1(PIXEL_SIZE),
    PRI1(STYLE),
    PRI1(SLANT),
    PRI1(WEIGHT),
    PRI1(WIDTH),
    PRI1(DECORATIVE),
    PRI1(ANTIALIAS),
    PRI1(RASTERIZER),
    PRI1(OUTLINE),
    PRI1(FONTVERSION),
    PRI_END
} FcMatcherPriority;

#undef PRI1

typedef struct _FcMatcher {
    FcObject	object;
    double	(*compare) (FcValue *value1, FcPrepValue *prep1, FcValue *value2, FcPrepValue *prep2);
    FcPrepValue	(*preprocess) (FcValue *value);
    int		strong, weak;
} FcMatcher;

/*
 * Order is significant, it defines the precedence of
 * each value, earlier values are more significant than
 * later values
 */
#define FC_OBJECT(NAME, Type, Cmp, Prep)	{ FC_##NAME##_OBJECT,	Cmp,	Prep,	PRI_##NAME##_STRONG,	PRI_##NAME##_WEAK },
static const FcMatcher _FcMatchers [] = {
    { FC_INVALID_OBJECT, NULL, NULL, -1, -1 },
#include "fcobjs.h"
};
#undef FC_OBJECT

static const FcMatcher*
FcObjectToMatcher (FcObject object,
		   FcBool   include_lang)
{
    if (include_lang)
    {
	switch (object) {
	case FC_FAMILYLANG_OBJECT:
	case FC_STYLELANG_OBJECT:
	case FC_FULLNAMELANG_OBJECT:
	    object = FC_LANG_OBJECT;
	    break;
	}
    }
    if (object > FC_MAX_BASE_OBJECT ||
	!_FcMatchers[object].compare ||
	_FcMatchers[object].strong == -1 ||
	_FcMatchers[object].weak == -1)
	return NULL;

    return _FcMatchers + object;
}

static FcBool
FcCompareValueList (FcObject	     object,
		    const FcMatcher *match,
		    FcValueListPtr   v1orig,	/* pattern */
		    FcValueListPtr   v2orig,	/* target */
		    FcValue         *bestValue,
		    double          *value_strong,
		    double          *value_weak,
		    int             *n,
		    FcResult        *result)
{
    FcValueListPtr  v1, v2;
    double    	    v, best, bestStrong, bestWeak;
    int		    j, k, pos = 0;

    if (!match)
    {
	if (bestValue)
	    *bestValue = FcValueCanonicalize(&v2orig->value);
	if (n)
	    *n = 0;
	return FcTrue;
    }

    best = 1e99;
    bestStrong = 1e99;
    bestWeak = 1e99;
    for (v2 = v2orig, k = 0; v2; v2 = FcValueListNext(v2), k++)
    {
	for (v1 = v1orig, j = 0; v1; v1 = FcValueListNext(v1), j++)
	{
	    v = (match->compare) (&v1->value, &v1->prep_value, &v2->value, &v2->prep_value);
	    if (unlikely(v < 0))
	    {
		*result = FcResultTypeMismatch;
		return FcFalse;
	    }
	    v = v * 1000 + j;
	    if (unlikely(v < best))
	    {
		if (bestValue)
		    *bestValue = FcValueCanonicalize(&v2->value);
		best = v;
		pos = k;
	    }
	    if (unlikely(v1->binding == FcValueBindingStrong))
	    {
		if (v < bestStrong)
		    bestStrong = v;
	    }
	    else
	    {
		if (v < bestWeak)
		    bestWeak = v;
	    }
	}
    }
    if (FcDebug () & FC_DBG_MATCHV)
    {
	printf (" %s: %g ", FcObjectName (object), best);
	FcValueListPrint (v1orig);
	printf (", ");
	FcValueListPrint (v2orig);
	printf ("\n");
    }

    if (value_strong) {
	if (value_strong == value_weak)
	    *value_strong += best;
	else
	    *value_strong += bestStrong;
    }
    if (value_weak) {
	if (value_strong != value_weak)
	    *value_weak += bestWeak;
    }

    if (n)
	*n = pos;

    return FcTrue;
}

/*
 * Return a value indicating the distance between the two lists of
 * values
 */

static FcBool
FcCompare (FcPattern	*pat,
	   FcPattern	*fnt,
	   double	*value,
	   FcResult	*result)
{
    int		    i, i1, i2;

    for (i = 0; i < PRI_END; i++)
	value[i] = 0.0;

    i1 = 0;
    i2 = 0;
    while (i1 < pat->num && i2 < fnt->num)
    {
	FcPatternElt *elt_i1 = &FcPatternElts(pat)[i1];
	FcPatternElt *elt_i2 = &FcPatternElts(fnt)[i2];

	i = FcObjectCompare(elt_i1->object, elt_i2->object);
	if (i > 0)
	    i2++;
	else if (i < 0)
	    i1++;
	else
	{
	    const FcMatcher *match = FcObjectToMatcher (elt_i1->object, FcFalse);
	    double *value_strong = (match ? &value[match->strong] : NULL);
	    double *value_weak = (match ? &value[match->weak] : NULL);

	    if (!FcCompareValueList (elt_i1->object, match,
				     FcPatternEltValues(elt_i1),
				     FcPatternEltValues(elt_i2),
				     NULL, value_strong, value_weak,
				     NULL, result))
		return FcFalse;
	    i1++;
	    i2++;
	}
    }
    return FcTrue;
}

FcPattern *
FcFontRenderPrepare (FcConfig	    *config,
		     FcPattern	    *pat,
		     FcPattern	    *font)
{
    FcPattern	    *new;
    int		    i;
    FcPatternElt    *fe, *pe;
    FcValue	    v;
    FcResult	    result;

    assert (pat != NULL);
    assert (font != NULL);

    new = FcPatternCreate ();
    if (!new)
	return NULL;
    for (i = 0; i < font->num; i++)
    {
	fe = &FcPatternElts(font)[i];
	if (fe->object == FC_FAMILYLANG_OBJECT ||
	    fe->object == FC_STYLELANG_OBJECT ||
	    fe->object == FC_FULLNAMELANG_OBJECT)
	{
	    /* ignore those objects. we need to deal with them
	     * another way */
	    continue;
	}
	if (fe->object == FC_FAMILY_OBJECT ||
	    fe->object == FC_STYLE_OBJECT ||
	    fe->object == FC_FULLNAME_OBJECT)
	{
	    FcPatternElt    *fel, *pel;

	    FC_ASSERT_STATIC ((FC_FAMILY_OBJECT + 1) == FC_FAMILYLANG_OBJECT);
	    FC_ASSERT_STATIC ((FC_STYLE_OBJECT + 1) == FC_STYLELANG_OBJECT);
	    FC_ASSERT_STATIC ((FC_FULLNAME_OBJECT + 1) == FC_FULLNAMELANG_OBJECT);

	    fel = FcPatternObjectFindElt (font, fe->object + 1);
	    pel = FcPatternObjectFindElt (pat, fe->object + 1);

	    if (fel && pel)
	    {
		/* The font has name languages, and pattern asks for specific language(s).
		 * Match on language and and prefer that result.
		 * Note:  Currently the code only give priority to first matching language.
		 */
		int n = 1, j;
		FcValueListPtr l1, l2, ln = NULL, ll = NULL;
		const FcMatcher *match = FcObjectToMatcher (pel->object, FcTrue);

		if (!FcCompareValueList (pel->object, match,
					 FcPatternEltValues (pel),
					 FcPatternEltValues (fel), NULL, NULL, NULL, &n, &result))
		{
		    FcPatternDestroy (new);
		    return NULL;
		}

		for (j = 0, l1 = FcPatternEltValues (fe), l2 = FcPatternEltValues (fel);
		     l1 != NULL || l2 != NULL;
		     j++, l1 = l1 ? FcValueListNext (l1) : NULL, l2 = l2 ? FcValueListNext (l2) : NULL)
		{
		    if (j == n)
		    {
			if (l1)
			    ln = FcValueListPrepend (ln,
						     FcValueCanonicalize (&l1->value),
						     FcValueBindingStrong);
			if (l2)
			    ll = FcValueListPrepend (ll,
						     FcValueCanonicalize (&l2->value),
						     FcValueBindingStrong);
		    }
		    else
		    {
			if (l1)
			    ln = FcValueListAppend (ln,
						    FcValueCanonicalize (&l1->value),
						    FcValueBindingStrong);
			if (l2)
			    ll = FcValueListAppend (ll,
						    FcValueCanonicalize (&l2->value),
						    FcValueBindingStrong);
		    }
		}
		FcPatternObjectListAdd (new, fe->object, ln, FcFalse);
		FcPatternObjectListAdd (new, fel->object, ll, FcFalse);

		continue;
	    }
	    else if (fel)
	    {
		/* Pattern doesn't ask for specific language.  Copy all for name and
		 * lang. */
		FcValueListPtr l1, l2;

		l1 = FcValueListDuplicate (FcPatternEltValues (fe));
		l2 = FcValueListDuplicate (FcPatternEltValues (fel));
		FcPatternObjectListAdd (new, fe->object, l1, FcFalse);
		FcPatternObjectListAdd (new, fel->object, l2, FcFalse);

		continue;
	    }
	}

	pe = FcPatternObjectFindElt (pat, fe->object);
	if (pe)
	{
	    const FcMatcher *match = FcObjectToMatcher (pe->object, FcFalse);
	    if (!FcCompareValueList (pe->object, match,
				     FcPatternEltValues(pe),
				     FcPatternEltValues(fe), &v, NULL, NULL, NULL, &result))
	    {
		FcPatternDestroy (new);
		return NULL;
	    }
	    FcPatternObjectAdd (new, fe->object, v, FcFalse);
	}
	else
	{
	    FcPatternObjectListAdd (new, fe->object,
				    FcValueListDuplicate (FcPatternEltValues (fe)),
				    FcTrue);
	}
    }
    for (i = 0; i < pat->num; i++)
    {
	pe = &FcPatternElts(pat)[i];
	fe = FcPatternObjectFindElt (font, pe->object);
	if (!fe &&
	    pe->object != FC_FAMILYLANG_OBJECT &&
	    pe->object != FC_STYLELANG_OBJECT &&
	    pe->object != FC_FULLNAMELANG_OBJECT)
	{
	    FcPatternObjectListAdd (new, pe->object,
				    FcValueListDuplicate (FcPatternEltValues(pe)),
				    FcFalse);
	}
    }

    FcConfigSubstituteWithPat (config, new, pat, FcMatchFont);
    return new;
}

static FcPattern *
FcFontSetMatchInternal (FcFontSet   **sets,
			int	    nsets,
			FcPattern   *p,
			FcResult    *result)
{
    FcPattern *best = NULL;
    int set;
    int index;
    int f;

    if (FcDebug () & FC_DBG_MATCH)
    {
	printf ("Match ");
	FcPatternPrint (p);
    }

    if (FcDebug () & FC_DBG_MATCHV)
    {
	index = 0;
	for (set = 0; set < nsets; set++)
	    for (f = 0; f < sets[set]->nfont; f++, index++)
	    {
		printf ("Font %d ", index);
		FcPatternPrint (sets[set]->fonts[f]);
	    }
    }

    // Preprocess pattern
    FcPreprocessPattern(p);

    // Count fonts in all sets
    size_t font_count = 0;
    for (set = 0; set < nsets; set++)
	font_count += sets[set]->nfont;

    // Handle special case when there are no fonts at all
    if (font_count == 0)
	goto out0;

    // Create bitset that marks fonts that should be considered, i.e. weren't excluded yet
    FcBitset *possible_matches = FcBitsetCreate(font_count);
    if (!possible_matches) {
	*result = FcResultOutOfMemory;
	goto out0;
    }

    FcBitsetClear(possible_matches, FcTrue);

    // Create bitset that marks fonts that had best score in current test
    FcBitset *best_matches_so_far = FcBitsetCreate(font_count);
    if (!best_matches_so_far) {
	*result = FcResultOutOfMemory;
	goto out1;
    }

    // Iterate over all matchers ordered by priority
    int priority;
    for (priority = 0; priority < PRI_END; priority++)
    {
	if (FcDebug () & FC_DBG_MATCHV)
	    printf ("Priority %d:\n", priority);

	// Find the matcher for given priority
	const FcMatcher *matcher = NULL;
	for (matcher = &_FcMatchers[0]; matcher->weak != priority && matcher->strong != priority; matcher++);
	assert(matcher);

	// Skip to next one if the pattern doesn't have the object
	const FcPatternElt *p_elt = FcPatternObjectFindElt(p, matcher->object);
	if (!p_elt)
	    continue;

	// The best_so_far bitset keeps track of all fonts that share the best score so far while we iterate over them
	FcBitsetClear(best_matches_so_far, FcFalse);
	double best_score_so_far = 1e99;

	// Iterate over all fonts in all sets
	for (set = 0, index = 0; set < nsets; set++)
	{
	    FcFontSet *s = sets[set];
	    if (!s)
		continue;

	    int f;
	    for (f = 0; f < s->nfont; f++, index++)
	    {
		FcPattern *font = s->fonts[f];

		// Skip the font if it was already excluded by matchers with higher priority
		if (!FcBitsetGet(possible_matches, index))
		    continue;

		// Compare the font's value lists to the pattern's value list and measure distance.
		// If the font doesn't contain such object, it is considered as distance 0 (best).
		double score_strong = 0.0, score_weak = 0.0;
		const FcPatternElt *font_elt = FcPatternObjectFindElt(font, matcher->object);
		if (font_elt)
		{
		    if (!FcCompareValueList (matcher->object, matcher,
					    FcPatternEltValues(p_elt),
					    FcPatternEltValues(font_elt),
					    NULL, &score_strong, &score_weak,
					    NULL, result))
		    {
			best = NULL;
			goto out2;
		    }
		}

		double score = (FcPatternEltValues(p_elt)->binding == FcValueBindingStrong ? score_strong : score_weak);

		// If this font was better match than the best so far, forget them and remember new best.
		if (score < best_score_so_far)
		{
		    best = font;

		    FcBitsetClear(best_matches_so_far, FcFalse);
		    best_score_so_far = score;
		}

		// If this font is at least as good as the best ones so far, remember it.
		if (score == best_score_so_far)
		{
		    FcBitsetSet(best_matches_so_far, index, FcTrue);
		}
	    }
	}

	if (FcDebug () & FC_DBG_MATCHV)
	{
	    printf ("Best ");
	    FcBitsetPrint (best_matches_so_far);
	}


	// If we managed to narrow the search down to one font, it is stored in `best` variable, go out.
	if (FcBitsetCountOnes(best_matches_so_far) <= 1)
	    break;

	// If we got here, there are multiple fonts in `best_matches_so_far` set, we need to narrow them down more in next iteration.
	// Swap `best_matches_so_far` with `possible_matches`. The `best_matches_so_far` is new narrowed down version of `possible_matches` and old `possible_matches` will be reset and reused as in next iteration.
	FcBitset *tmp = best_matches_so_far;
	best_matches_so_far = possible_matches;
	possible_matches = tmp;
    }

    if (FcDebug () & FC_DBG_MATCH)
    {
	printf ("Best ");
	FcPatternPrint (best);
    }

    /* assuming that 'result' is initialized with FcResultNoMatch
     * outside this function */
    if (best)
	*result = FcResultMatch;

out2:
    FcBitsetDestroy(best_matches_so_far);
out1:
    FcBitsetDestroy(possible_matches);

out0:
    return best;
}

FcPattern *
FcFontSetMatch (FcConfig    *config,
		FcFontSet   **sets,
		int	    nsets,
		FcPattern   *p,
		FcResult    *result)
{
    FcPattern	    *best;

    assert (sets != NULL);
    assert (p != NULL);
    assert (result != NULL);

    *result = FcResultNoMatch;

    if (!config)
    {
	config = FcConfigGetCurrent ();
	if (!config)
	    return 0;
    }
    best = FcFontSetMatchInternal (sets, nsets, p, result);
    if (best)
	return FcFontRenderPrepare (config, p, best);
    else
	return NULL;
}

FcPattern *
FcFontMatch (FcConfig	*config,
	     FcPattern	*p,
	     FcResult	*result)
{
    FcFontSet	*sets[2];
    int		nsets;
    FcPattern   *best;

    assert (p != NULL);
    assert (result != NULL);

    *result = FcResultNoMatch;

    if (!config)
    {
	config = FcConfigGetCurrent ();
	if (!config)
	    return 0;
    }
    nsets = 0;
    if (config->fonts[FcSetSystem])
	sets[nsets++] = config->fonts[FcSetSystem];
    if (config->fonts[FcSetApplication])
	sets[nsets++] = config->fonts[FcSetApplication];

    best = FcFontSetMatchInternal (sets, nsets, p, result);
    if (best)
	return FcFontRenderPrepare (config, p, best);
    else
	return NULL;
}

typedef struct _FcSortNode {
    FcPattern	*pattern;
    double	score[PRI_END];
} FcSortNode;

static int
FcSortCompare (const void *aa, const void *ab)
{
    FcSortNode  *a = *(FcSortNode **) aa;
    FcSortNode  *b = *(FcSortNode **) ab;
    double	*as = &a->score[0];
    double	*bs = &b->score[0];
    double	ad = 0, bd = 0;
    int         i;

    i = PRI_END;
    while (i-- && (ad = *as++) == (bd = *bs++))
	;
    return ad < bd ? -1 : ad > bd ? 1 : 0;
}

static FcBool
FcSortWalk (FcSortNode **n, int nnode, FcFontSet *fs, FcCharSet **csp, FcBool trim)
{
    FcBool ret = FcFalse;
    FcCharSet *cs;
    int i;

    cs = 0;
    if (trim || csp)
    {
	cs = FcCharSetCreate ();
	if (cs == NULL)
	    goto bail;
    }

    for (i = 0; i < nnode; i++)
    {
	FcSortNode	*node = *n++;
	FcBool		adds_chars = FcFalse;

	/*
	 * Only fetch node charset if we'd need it
	 */
	if (cs)
	{
	    FcCharSet	*ncs;

	    if (FcPatternGetCharSet (node->pattern, FC_CHARSET, 0, &ncs) !=
		FcResultMatch)
	        continue;

	    if (!FcCharSetMerge (cs, ncs, &adds_chars))
		goto bail;
	}

	/*
	 * If this font isn't a subset of the previous fonts,
	 * add it to the list
	 */
	if (!i || !trim || adds_chars)
	{
	    FcPatternReference (node->pattern);
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Add ");
		FcPatternPrint (node->pattern);
	    }
	    if (!FcFontSetAdd (fs, node->pattern))
	    {
		FcPatternDestroy (node->pattern);
		goto bail;
	    }
	}
    }
    if (csp)
    {
	*csp = cs;
	cs = 0;
    }

    ret = FcTrue;

bail:
    if (cs)
	FcCharSetDestroy (cs);

    return ret;
}

void
FcFontSetSortDestroy (FcFontSet *fs)
{
    FcFontSetDestroy (fs);
}

FcFontSet *
FcFontSetSort (FcConfig	    *config FC_UNUSED,
	       FcFontSet    **sets,
	       int	    nsets,
	       FcPattern    *p,
	       FcBool	    trim,
	       FcCharSet    **csp,
	       FcResult	    *result)
{
    FcFontSet	    *ret;
    FcFontSet	    *s;
    FcSortNode	    *nodes;
    FcSortNode	    **nodeps, **nodep;
    int		    nnodes;
    FcSortNode	    *new;
    int		    set;
    int		    f;
    int		    i;
    int		    nPatternLang;
    FcBool    	    *patternLangSat;
    FcValue	    patternLang;

    assert (sets != NULL);
    assert (p != NULL);
    assert (result != NULL);

    /* There are some implementation that relying on the result of
     * "result" to check if the return value of FcFontSetSort
     * is valid or not.
     * So we should initialize it to the conservative way since
     * this function doesn't return NULL anymore.
     */
    if (result)
	*result = FcResultNoMatch;

    if (FcDebug () & FC_DBG_MATCH)
    {
	printf ("Sort ");
	FcPatternPrint (p);
    }

    FcPreprocessPattern(p);

    nnodes = 0;
    for (set = 0; set < nsets; set++)
    {
	s = sets[set];
	if (!s)
	    continue;
	nnodes += s->nfont;
    }
    if (!nnodes)
	return FcFontSetCreate ();

    for (nPatternLang = 0;
	 FcPatternGet (p, FC_LANG, nPatternLang, &patternLang) == FcResultMatch;
	 nPatternLang++)
	;
	
    /* freed below */
    nodes = malloc (nnodes * sizeof (FcSortNode) +
		    nnodes * sizeof (FcSortNode *) +
		    nPatternLang * sizeof (FcBool));
    if (!nodes)
	goto bail0;
    nodeps = (FcSortNode **) (nodes + nnodes);
    patternLangSat = (FcBool *) (nodeps + nnodes);

    new = nodes;
    nodep = nodeps;
    for (set = 0; set < nsets; set++)
    {
	s = sets[set];
	if (!s)
	    continue;
	for (f = 0; f < s->nfont; f++)
	{
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Font %d ", f);
		FcPatternPrint (s->fonts[f]);
	    }
	    new->pattern = s->fonts[f];
	    if (!FcCompare (p, new->pattern, new->score, result))
		goto bail1;
	    if (FcDebug () & FC_DBG_MATCHV)
	    {
		printf ("Score");
		for (i = 0; i < PRI_END; i++)
		{
		    printf (" %g", new->score[i]);
		}
		printf ("\n");
	    }
	    *nodep = new;
	    new++;
	    nodep++;
	}
    }

    nnodes = new - nodes;

    qsort (nodeps, nnodes, sizeof (FcSortNode *),
	   FcSortCompare);

    for (i = 0; i < nPatternLang; i++)
	patternLangSat[i] = FcFalse;

    for (f = 0; f < nnodes; f++)
    {
	FcBool	satisfies = FcFalse;
	/*
	 * If this node matches any language, go check
	 * which ones and satisfy those entries
	 */
	if (nodeps[f]->score[PRI_LANG] < 2000)
	{
	    for (i = 0; i < nPatternLang; i++)
	    {
		FcValue	    nodeLang;
		
		if (!patternLangSat[i] &&
		    FcPatternGet (p, FC_LANG, i, &patternLang) == FcResultMatch &&
		    FcPatternGet (nodeps[f]->pattern, FC_LANG, 0, &nodeLang) == FcResultMatch)
		{
		    double  compare = FcCompareLang (&patternLang, NULL, &nodeLang, NULL);
		    if (compare >= 0 && compare < 2)
		    {
			if (FcDebug () & FC_DBG_MATCHV)
			{
			    FcChar8 *family;
			    FcChar8 *style;

			    if (FcPatternGetString (nodeps[f]->pattern, FC_FAMILY, 0, &family) == FcResultMatch &&
				FcPatternGetString (nodeps[f]->pattern, FC_STYLE, 0, &style) == FcResultMatch)
				printf ("Font %s:%s matches language %d\n", family, style, i);
			}
			patternLangSat[i] = FcTrue;
			satisfies = FcTrue;
			break;
		    }
		}
	    }
	}
	if (!satisfies)
	{
	    nodeps[f]->score[PRI_LANG] = 10000.0;
	}
    }

    /*
     * Re-sort once the language issues have been settled
     */
    qsort (nodeps, nnodes, sizeof (FcSortNode *),
	   FcSortCompare);

    ret = FcFontSetCreate ();
    if (!ret)
	goto bail1;

    if (!FcSortWalk (nodeps, nnodes, ret, csp, trim))
	goto bail2;

    free (nodes);

    if (FcDebug() & FC_DBG_MATCH)
    {
	printf ("First font ");
	FcPatternPrint (ret->fonts[0]);
    }
    if (ret->nfont > 0)
	*result = FcResultMatch;

    return ret;

bail2:
    FcFontSetDestroy (ret);
bail1:
    free (nodes);
bail0:
    return 0;
}

FcFontSet *
FcFontSort (FcConfig	*config,
	    FcPattern	*p,
	    FcBool	trim,
	    FcCharSet	**csp,
	    FcResult	*result)
{
    FcFontSet	*sets[2];
    int		nsets;

    assert (p != NULL);
    assert (result != NULL);

    *result = FcResultNoMatch;

    if (!config)
    {
	config = FcConfigGetCurrent ();
	if (!config)
	    return 0;
    }
    nsets = 0;
    if (config->fonts[FcSetSystem])
	sets[nsets++] = config->fonts[FcSetSystem];
    if (config->fonts[FcSetApplication])
	sets[nsets++] = config->fonts[FcSetApplication];
    return FcFontSetSort (config, sets, nsets, p, trim, csp, result);
}


void
FcPreprocessPattern (FcPattern *pat) {
    int i;
    for (i = 0; i < pat->num; i++)
    {
	FcPatternElt *elt = &FcPatternElts(pat)[i];

	const FcMatcher *matcher = FcObjectToMatcher(elt->object, FcFalse);
	if (!matcher || !matcher->preprocess)
	    continue;

	FcValueListPtr vl;
	for (vl = elt->values; vl; vl = FcValueListNext(vl))
	{
	    vl->prep_value = (matcher->preprocess)(&vl->value);
	}
    }
}

#define __fcmatch__
#include "fcaliastail.h"
#undef __fcmatch__
