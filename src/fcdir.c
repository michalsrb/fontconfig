/*
 * fontconfig/src/fcdir.c
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
#include "fcftint.h"
#include <ft2build.h>
#include FT_FREETYPE_H
#include <dirent.h>

FcBool
FcFileIsDir (const FcChar8 *file)
{
    struct stat	    statb;

    if (FcStat (file, &statb) != 0)
	return FcFalse;
    return S_ISDIR(statb.st_mode);
}

FcBool
FcFileIsLink (const FcChar8 *file)
{
#if HAVE_LSTAT
    struct stat statb;

    if (lstat ((const char *)file, &statb) != 0)
	return FcFalse;
    return S_ISLNK (statb.st_mode);
#else
    return FcFalse;
#endif
}

FcBool
FcFileIsFile (const FcChar8 *file)
{
    struct stat statb;

    if (FcStat (file, &statb) != 0)
	return FcFalse;
    return S_ISREG (statb.st_mode);
}

static FcBool
FcFileScanFontConfig (FcFontSet		*set,
		      FcBlanks		*blanks,
		      const FcChar8	*file,
		      FcConfig		*config)
{
    FT_Library	ftLibrary;
    FT_Face	face;
    FcPattern	*font;
    FcBool	ret = FcTrue;
    int		num_faces = 0;
    int		num_instances = 0;
    int		face_num = 0;
    int		instance_num = 0;
    int		id;
    const FcChar8 *sysroot = FcConfigGetSysRoot (config);

    if (FT_Init_FreeType (&ftLibrary))
	return FcFalse;

    do
    {
	font = 0;
	/*
	 * Nothing in the cache, scan the file
	 */
	if (FcDebug () & FC_DBG_SCAN)
	{
	    printf ("\tScanning file %s...", file);
	    fflush (stdout);
	}

	id = ((instance_num << 16) + face_num);
	if (FT_New_Face (ftLibrary, (char *) file, id, &face))
	    return FcFalse;
	num_faces = face->num_faces;
	num_instances = face->style_flags >> 16;
	font = FcFreeTypeQueryFace (face, file, id, blanks);
	FT_Done_Face (face);

	if (FcDebug () & FC_DBG_SCAN)
	    printf ("done\n");
	/*
	 * Get rid of sysroot here so that targeting scan rule may contains FC_FILE pattern
	 * and they should usually expect without sysroot.
	 */
	if (font && sysroot)
	{
	    size_t len = strlen ((const char *)sysroot);
	    FcChar8 *f = NULL;

	    if (FcPatternObjectGetString (font, FC_FILE_OBJECT, 0, &f) == FcResultMatch &&
		strncmp ((const char *)f, (const char *)sysroot, len) == 0)
	    {
		FcChar8 *s = FcStrdup (f);
		FcPatternObjectDel (font, FC_FILE_OBJECT);
		if (s[len] != '/')
		    len--;
		else if (s[len+1] == '/')
		    len++;
		FcPatternObjectAddString (font, FC_FILE_OBJECT, &s[len]);
		FcStrFree (s);
	    }
	}

	/*
	 * Edit pattern with user-defined rules
	 */
	if (font && config && !FcConfigSubstitute (config, font, FcMatchScan))
	{
	    FcPatternDestroy (font);
	    font = NULL;
	    ret = FcFalse;
	}

	/*
	 * Add the font
	 */
	if (font)
	{
	    if (FcDebug() & FC_DBG_SCANV)
	    {
		printf ("Final font pattern:\n");
		FcPatternPrint (font);
	    }
	    if (!FcFontSetAdd (set, font))
	    {
		FcPatternDestroy (font);
		font = NULL;
		ret = FcFalse;
	    }
	}
	else
	    ret = FcFalse;

	if (instance_num < num_instances)
	    instance_num++;
	else
	{
	    face_num++;
	    instance_num = 0;
	}
    } while (font && ret && face_num < num_faces);

    FT_Done_FreeType (ftLibrary);

    return ret;
}

FcBool
FcFileScanConfig (FcFontSet	*set,
		  FcStrSet	*dirs,
		  FcBlanks	*blanks,
		  const FcChar8	*file,
		  FcConfig	*config)
{
    if (FcFileIsDir (file))
    {
	const FcChar8 *sysroot = FcConfigGetSysRoot (config);
	const FcChar8 *d = file;
	size_t len;

	if (sysroot)
	{
		len = strlen ((const char *)sysroot);
		if (strncmp ((const char *)file, (const char *)sysroot, len) == 0)
		{
			if (file[len] != '/')
				len--;
			else if (file[len+1] == '/')
				len++;
			d = &file[len];
		}
	}
	return FcStrSetAdd (dirs, d);
    }
    else
    {
	if (set)
	    return FcFileScanFontConfig (set, blanks, file, config);
	else
	    return FcTrue;
    }
}

FcBool
FcFileScan (FcFontSet	    *set,
	    FcStrSet	    *dirs,
	    FcFileCache	    *cache FC_UNUSED,
	    FcBlanks	    *blanks,
	    const FcChar8   *file,
	    FcBool	    force FC_UNUSED)
{
    return FcFileScanConfig (set, dirs, blanks, file, FcConfigGetCurrent ());
}

/*
 * Strcmp helper that takes pointers to pointers, copied from qsort(3) manpage
 */
static int
cmpstringp(const void *p1, const void *p2)
{
    return strcmp(* (char **) p1, * (char **) p2);
}

FcBool
FcDirScanConfig (FcFontSet	*set,
		 FcStrSet	*dirs,
		 FcBlanks	*blanks,
		 const FcChar8	*dir,
		 FcBool		force, /* XXX unused */
		 FcConfig	*config)
{
    DIR			*d;
    struct dirent	*e;
    FcStrSet		*files;
    FcChar8		*file;
    FcChar8		*base;
    FcBool		ret = FcTrue;
    int			i;

    if (!force)
	return FcFalse;

    if (!set && !dirs)
	return FcTrue;

    if (!blanks)
	blanks = FcConfigGetBlanks (config);

    /* freed below */
    file = (FcChar8 *) malloc (strlen ((char *) dir) + 1 + FC_MAX_FILE_LEN + 1);
    if (!file) {
	ret = FcFalse;
	goto bail;
    }

    strcpy ((char *) file, (char *) dir);
    strcat ((char *) file, "/");
    base = file + strlen ((char *) file);

    if (FcDebug () & FC_DBG_SCAN)
	printf ("\tScanning dir %s\n", dir);
	
    d = opendir ((char *) dir);
    if (!d)
    {
	/* Don't complain about missing directories */
	if (errno != ENOENT)
	    ret = FcFalse;
	goto bail;
    }

    files = FcStrSetCreateEx (FCSS_ALLOW_DUPLICATES | FCSS_GROW_BY_64);
    if (!files)
    {
	ret = FcFalse;
	goto bail1;
    }
    while ((e = readdir (d)))
    {
	if (e->d_name[0] != '.' && strlen (e->d_name) < FC_MAX_FILE_LEN)
	{
	    strcpy ((char *) base, (char *) e->d_name);
	    if (!FcStrSetAdd (files, file)) {
		ret = FcFalse;
		goto bail2;
	    }
	}
    }

    /*
     * Sort files to make things prettier
     */
    qsort(files->strs, files->num, sizeof(FcChar8 *), cmpstringp);

    /*
     * Scan file files to build font patterns
     */
    for (i = 0; i < files->num; i++)
	FcFileScanConfig (set, dirs, blanks, files->strs[i], config);

bail2:
    FcStrSetDestroy (files);
bail1:
    closedir (d);
bail:
    if (file)
	free (file);

    return ret;
}

FcBool
FcDirScan (FcFontSet	    *set,
	   FcStrSet	    *dirs,
	   FcFileCache	    *cache, /* XXX unused */
	   FcBlanks	    *blanks,
	   const FcChar8    *dir,
	   FcBool	    force /* XXX unused */)
{
    if (cache || !force)
	return FcFalse;

    return FcDirScanConfig (set, dirs, blanks, dir, force, FcConfigGetCurrent ());
}

/*
 * Scan the specified directory and construct a cache of its contents
 */
FcCache *
FcDirCacheScan (const FcChar8 *dir, FcConfig *config)
{
    FcStrSet		*dirs;
    FcFontSet		*set;
    FcCache		*cache = NULL;
    struct stat		dir_stat;
    const FcChar8	*sysroot = FcConfigGetSysRoot (config);
    FcChar8		*d;
    int			fd = -1;
    int			i;

    if (sysroot)
	d = FcStrBuildFilename (sysroot, dir, NULL);
    else
	d = FcStrdup (dir);

    if (FcDebug () & FC_DBG_FONTSET)
	printf ("cache scan dir %s\n", d);

    if (FcStatChecksum (d, &dir_stat) < 0)
	goto bail;

    set = FcFontSetCreate();
    if (!set)
	goto bail;

    dirs = FcStrSetCreateEx (FCSS_GROW_BY_64);
    if (!dirs)
	goto bail1;

    fd = FcDirCacheLock (dir, config);
    /*
     * Scan the dir
     */
    if (!FcDirScanConfig (set, dirs, NULL, d, FcTrue, config))
	goto bail2;

    /*
     * Preprocess all fonts
     */
    for (i = 0; i < set->nfont; i++)
    {
	FcPreprocessPattern (set->fonts[i]);
    }

    /*
     * Build the cache object
     */
    cache = FcDirCacheBuild (set, dir, &dir_stat, dirs);
    if (!cache)
	goto bail2;

    /*
     * Write out the cache file, ignoring any troubles
     */
    FcDirCacheWrite (cache, config);

 bail2:
    FcDirCacheUnlock (fd);
    FcStrSetDestroy (dirs);
 bail1:
    FcFontSetDestroy (set);
 bail:
    FcStrFree (d);

    return cache;
}

FcCache *
FcDirCacheRescan (const FcChar8 *dir, FcConfig *config)
{
    FcCache *cache;
    FcCache *new = NULL;
    struct stat dir_stat;
    FcStrSet *dirs;
    const FcChar8 *sysroot = FcConfigGetSysRoot (config);
    FcChar8 *d = NULL;
    int fd = -1;

    cache = FcDirCacheLoad (dir, config, NULL);
    if (!cache)
	goto bail;

    if (sysroot)
	d = FcStrBuildFilename (sysroot, dir, NULL);
    else
	d = FcStrdup (dir);
    if (FcStatChecksum (d, &dir_stat) < 0)
	goto bail;
    dirs = FcStrSetCreateEx (FCSS_GROW_BY_64);
    if (!dirs)
	goto bail;

    fd = FcDirCacheLock (dir, config);
    /*
     * Scan the dir
     */
    if (!FcDirScanConfig (NULL, dirs, NULL, d, FcTrue, config))
	goto bail1;
    /*
     * Rebuild the cache object
     */
    new = FcDirCacheRebuild (cache, &dir_stat, dirs);
    if (!new)
	goto bail1;
    FcDirCacheUnload (cache);
    /*
     * Write out the cache file, ignoring any troubles
     */
    FcDirCacheWrite (new, config);

bail1:
    FcDirCacheUnlock (fd);
    FcStrSetDestroy (dirs);
bail:
    if (d)
	FcStrFree (d);

    return new;
}

/*
 * Read (or construct) the cache for a directory
 */
FcCache *
FcDirCacheRead (const FcChar8 *dir, FcBool force, FcConfig *config)
{
    FcCache		*cache = NULL;

    /* Try to use existing cache file */
    if (!force)
	cache = FcDirCacheLoad (dir, config, NULL);

    /* Not using existing cache file, construct new cache */
    if (!cache)
	cache = FcDirCacheScan (dir, config);

    return cache;
}

FcBool
FcDirSave (FcFontSet *set FC_UNUSED, FcStrSet * dirs FC_UNUSED, const FcChar8 *dir FC_UNUSED)
{
    return FcFalse; /* XXX deprecated */
}
#define __fcdir__
#include "fcaliastail.h"
#undef __fcdir__
