/*
 * fontconfig/src/fcobjs.h
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
/* DON'T REORDER!  The order is part of the cache signature. */
FC_OBJECT (FAMILY,		FcTypeString,	FcCompareFamily,	FcPreprocessFamily)
FC_OBJECT (FAMILYLANG,		FcTypeString,	NULL,			NULL)
FC_OBJECT (STYLE,		FcTypeString,	FcCompareString,	FcPreprocessString)
FC_OBJECT (STYLELANG,		FcTypeString,	NULL,			NULL)
FC_OBJECT (FULLNAME,		FcTypeString,	NULL,			NULL)
FC_OBJECT (FULLNAMELANG,	FcTypeString,	NULL,			NULL)
FC_OBJECT (SLANT,		FcTypeInteger,	FcCompareNumber,	NULL)
FC_OBJECT (WEIGHT,		FcTypeInteger,	FcCompareNumber,	NULL)
FC_OBJECT (WIDTH,		FcTypeInteger,	FcCompareNumber,	NULL)
FC_OBJECT (SIZE,		FcTypeRange,	FcCompareSizeRange,	NULL)
FC_OBJECT (ASPECT,		FcTypeDouble,	NULL,			NULL)
FC_OBJECT (PIXEL_SIZE,		FcTypeDouble,	FcCompareSize,		NULL)
FC_OBJECT (SPACING,		FcTypeInteger,	FcCompareNumber,	NULL)
FC_OBJECT (FOUNDRY,		FcTypeString,	FcCompareString,	FcPreprocessString)
FC_OBJECT (ANTIALIAS,		FcTypeBool,	FcCompareBool,		NULL)
FC_OBJECT (HINT_STYLE,		FcTypeInteger,	NULL,			NULL)
FC_OBJECT (HINTING,		FcTypeBool,	NULL,			NULL)
FC_OBJECT (VERTICAL_LAYOUT,	FcTypeBool,	NULL,			NULL)
FC_OBJECT (AUTOHINT,		FcTypeBool,	NULL,			NULL)
FC_OBJECT (GLOBAL_ADVANCE,	FcTypeBool,	NULL,			NULL)	/* deprecated */
FC_OBJECT (FILE,		FcTypeString,	FcCompareFilename,	FcPreprocessFilename)
FC_OBJECT (INDEX,		FcTypeInteger,	NULL,			NULL)
FC_OBJECT (RASTERIZER,		FcTypeString,	FcCompareString,	FcPreprocessString)	/* deprecated */
FC_OBJECT (OUTLINE,		FcTypeBool,	FcCompareBool,		NULL)
FC_OBJECT (SCALABLE,		FcTypeBool,	FcCompareBool,		NULL)
FC_OBJECT (DPI,			FcTypeDouble,	NULL,			NULL)
FC_OBJECT (RGBA,		FcTypeInteger,	NULL,			NULL)
FC_OBJECT (SCALE,		FcTypeDouble,	NULL,			NULL)
FC_OBJECT (MINSPACE,		FcTypeBool,	NULL,			NULL)
FC_OBJECT (CHARWIDTH,		FcTypeInteger,	NULL,			NULL)
FC_OBJECT (CHAR_HEIGHT,		FcTypeInteger,	NULL,			NULL)
FC_OBJECT (MATRIX,		FcTypeMatrix,	NULL,			NULL)
FC_OBJECT (CHARSET,		FcTypeCharSet,	FcCompareCharSet,	NULL)
FC_OBJECT (LANG,		FcTypeLangSet,	FcCompareLang,		NULL)
FC_OBJECT (FONTVERSION,		FcTypeInteger,	FcCompareNumber,	NULL)
FC_OBJECT (CAPABILITY,		FcTypeString,	NULL,			NULL)
FC_OBJECT (FONTFORMAT,		FcTypeString,	FcCompareString,	FcPreprocessString)
FC_OBJECT (EMBOLDEN,		FcTypeBool,	NULL,			NULL)
FC_OBJECT (EMBEDDED_BITMAP,	FcTypeBool,	NULL,			NULL)
FC_OBJECT (DECORATIVE,		FcTypeBool,	FcCompareBool,		NULL)
FC_OBJECT (LCD_FILTER,		FcTypeInteger,	NULL,			NULL)
FC_OBJECT (NAMELANG,		FcTypeString,	NULL,			NULL)
FC_OBJECT (FONT_FEATURES,	FcTypeString,	NULL,			NULL)
FC_OBJECT (PRGNAME,		FcTypeString,	NULL,			NULL)
FC_OBJECT (HASH,		FcTypeString,	NULL,			NULL)	/* deprecated */
FC_OBJECT (POSTSCRIPT_NAME,	FcTypeString,	FcComparePostScript,	NULL)
FC_OBJECT (COLOR,		FcTypeBool,	FcCompareBool,		NULL)
FC_OBJECT (SYMBOL,		FcTypeBool,	FcCompareBool,		NULL)
/* ^-------------- Add new objects here. */
