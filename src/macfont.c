/* Font driver on Mac OS.
   Copyright (C) 2009  YAMAMOTO Mitsuharu

This file is part of GNU Emacs Mac port.

GNU Emacs Mac port is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

GNU Emacs Mac port is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs Mac port.  If not, see <http://www.gnu.org/licenses/>.  */

#include <config.h>

#include "lisp.h"
#include "dispextern.h"
#include "macterm.h"
#include "frame.h"
#include "blockinput.h"
#include "character.h"
#include "charset.h"
#include "composite.h"
#include "fontset.h"
#include "font.h"
#include "macfont.h"

/* Symbolic types of this font-driver.  */
Lisp_Object macfont_driver_type;

#if USE_CORE_TEXT
/* Core Text, for Mac OS X 10.5 and later.  */
Lisp_Object Qmac_ct;

static double mac_ctfont_get_advance_width_for_glyph P_ ((CTFontRef, CGGlyph));
static CGRect mac_ctfont_get_bounding_rect_for_glyph P_ ((CTFontRef, CGGlyph));
static CFArrayRef mac_ctfont_create_available_families P_ ((void));
static CTLineRef mac_ctfont_create_line_with_string_and_font P_ ((CFStringRef,
								  CTFontRef));
#endif
#if USE_NS_FONT_DESCRIPTOR
/* Core Text emulation by NSFontDescriptor, for Mac OS X 10.4. */
Lisp_Object Qmac_fd;
#endif
#if USE_NS_FONT_MANAGER
/* Core Text emulation by NSFontManager, for Mac OS X 10.2 - 10.3. */
Lisp_Object Qmac_fm;
#endif

/* The actual structure for Mac font that can be casted to struct font.  */

struct macfont_info
{
  struct font font;
  FontRef macfont;
  CGFontRef cgfont;
  struct macfont_cache *cache;
  struct font_metrics **metrics;
  short metrics_nrows;
  unsigned synthetic_italic_p : 1;
  unsigned synthetic_bold_p : 1;
  unsigned synthetic_mono_p : 1;
  unsigned no_antialias_p : 1;
};

#define METRICS_NCOLS_PER_ROW	(128)

enum metrics_status
  {
    METRICS_INVALID = -1,    /* metrics entry is invalid */
    METRICS_WIDTH_VALID = -2 /* width is valid but others are invalid */
  };

#define METRICS_STATUS(metrics)	((metrics)->ascent + (metrics)->descent)
#define METRICS_SET_STATUS(metrics, status) \
  ((metrics)->ascent = 0, (metrics)->descent = (status))

enum {FONT_SLANT_SYNTHETIC_ITALIC = 200}; /* FC_SLANT_ITALIC + 100 */
enum {FONT_WEIGHT_SYNTHETIC_BOLD = 200};  /* FC_WEIGHT_BOLD */
enum {FONT_SPACING_SYNTHETIC_MONO = FONT_SPACING_MONO};

static const CGAffineTransform synthetic_italic_atfm = {1, 0, 0.25, 1, 0, 0};
static const CGFloat synthetic_bold_factor = 0.024;

static Boolean cfnumber_get_font_symbolic_traits_value P_ ((CFNumberRef,
							    FontSymbolicTraits *));
static void macfont_store_descriptor_attributes P_ ((FontDescriptorRef,
						     Lisp_Object));
static Lisp_Object macfont_descriptor_entity P_ ((FontDescriptorRef,
						  Lisp_Object,
						  FontSymbolicTraits));
static CFStringRef macfont_create_family_with_symbol P_ ((Lisp_Object));
static int macfont_glyph_extents P_ ((struct font *, CGGlyph,
				      struct font_metrics *, int *));
static CFMutableDictionaryRef macfont_create_attributes_with_spec P_ ((Lisp_Object));
static Boolean macfont_supports_charset_and_languages_p P_ ((FontDescriptorRef,
							     CFCharacterSetRef,
							     Lisp_Object,
							     CFArrayRef));
static CFIndex macfont_closest_traits_index P_ ((CFArrayRef,
						 FontSymbolicTraits));


/* Mac font driver.  */

static struct
{
  /* registry name */
  char *name;
  /* characters to distinguish the charset from the others */
  int uniquifier[6];
  /* additional constraint by language */
  CFStringRef lang;
  /* set on demand */
  CFCharacterSetRef cf_charset;
  CFStringRef cf_charset_string;
} cf_charset_table[] =
  { { "iso8859-1", { 0x00A0, 0x00A1, 0x00B4, 0x00BC, 0x00D0 } },
    { "iso8859-2", { 0x00A0, 0x010E }},
    { "iso8859-3", { 0x00A0, 0x0108 }},
    { "iso8859-4", { 0x00A0, 0x00AF, 0x0128, 0x0156, 0x02C7 }},
    { "iso8859-5", { 0x00A0, 0x0401 }},
    { "iso8859-6", { 0x00A0, 0x060C }},
    { "iso8859-7", { 0x00A0, 0x0384 }},
    { "iso8859-8", { 0x00A0, 0x05D0 }},
    { "iso8859-9", { 0x00A0, 0x00A1, 0x00BC, 0x011E }},
    { "iso8859-10", { 0x00A0, 0x00D0, 0x0128, 0x2015 }},
    { "iso8859-11", { 0x00A0, 0x0E01 }},
    { "iso8859-13", { 0x00A0, 0x201C }},
    { "iso8859-14", { 0x00A0, 0x0174 }},
    { "iso8859-15", { 0x00A0, 0x00A1, 0x00D0, 0x0152 }},
    { "iso8859-16", { 0x00A0, 0x0218}},
    { "gb2312.1980-0", { 0x4E13 }, CFSTR ("zh-Hans")},
    { "big5-0", { /* 0xF6B1 in ftfont.c */ 0xF7E5 }, CFSTR ("zh-Hant") },
    { "jisx0208.1983-0", { 0x4E55 }, CFSTR ("ja")},
    { "ksc5601.1987-0", { 0xAC00 }, CFSTR ("ko")},
    { "cns11643.1992-1", { 0xFE32 }, CFSTR ("zh-Hant")},
    { "cns11643.1992-2", { 0x4E33, 0x7934 }},
    { "cns11643.1992-3", { 0x201A9 }},
    { "cns11643.1992-4", { 0x20057 }},
    { "cns11643.1992-5", { 0x20000 }},
    { "cns11643.1992-6", { 0x20003 }},
    { "cns11643.1992-7", { 0x20055 }},
    { "gbk-0", { 0x4E06 }, CFSTR ("zh-Hans")},
    { "jisx0212.1990-0", { 0x4E44 }},
    { "jisx0213.2000-1", { 0xFA10 }, CFSTR ("ja")},
    { "jisx0213.2000-2", { 0xFA49 }},
    { "jisx0213.2004-1", { 0x20B9F }},
    { "viscii1.1-1", { 0x1EA0, 0x1EAE, 0x1ED2 }, CFSTR ("vi")},
    { "tis620.2529-1", { 0x0E01 }, CFSTR ("th")},
    { "windows-1251", { 0x0401, 0x0490 }, CFSTR ("ru")},
    { "koi8-r", { 0x0401, 0x2219 }, CFSTR ("ru")},
    { "mulelao-1", { 0x0E81 }, CFSTR ("lo")},
    { "unicode-sip", { 0x20000 }},
    { NULL }
  };

static INLINE Lisp_Object
macfont_intern_prop_cfstring (cfstring)
     CFStringRef cfstring;
{
  Lisp_Object string = cfstring_to_lisp_nodecode (cfstring);

  return font_intern_prop (SDATA (string), SBYTES (string), 1);
}

static INLINE CFIndex
macfont_store_utf32char_to_unichars (c, unichars)
     UTF32Char c;
     UniChar *unichars;
{
  if (c < 0x10000)
    {
      unichars[0] = c;

      return 1;
    }
  else
    {
      c -= 0x10000;
      unichars[0] = (c >> 10) + 0xD800;
      unichars[1] = (c & 0x3FF) + 0xDC00;

      return 2;
    }
}

static Boolean
cfnumber_get_font_symbolic_traits_value (number, sym_traits)
     CFNumberRef number;
     FontSymbolicTraits *sym_traits;
{
  SInt64 sint64_value;

  /* Getting symbolic traits with kCFNumberSInt32Type is lossy on Mac
     OS 10.6 when the value is greater than or equal to 1 << 31.  */
  if (CFNumberGetValue (number, kCFNumberSInt64Type, &sint64_value))
    {
      *sym_traits = (FontSymbolicTraits) sint64_value;

      return true;
    }

  return false;
}

static void
macfont_store_descriptor_attributes (desc, spec_or_entity)
     FontDescriptorRef desc;
     Lisp_Object spec_or_entity;
{
  CFStringRef str;
  CFDictionaryRef dict;
  CFNumberRef num;
  CGFloat floatval;

  str = mac_font_descriptor_copy_attribute (desc,
					    MAC_FONT_FAMILY_NAME_ATTRIBUTE);
  if (str)
    {
      ASET (spec_or_entity, FONT_FAMILY_INDEX,
	    macfont_intern_prop_cfstring (str));
      CFRelease (str);
    }
  dict = mac_font_descriptor_copy_attribute (desc, MAC_FONT_TRAITS_ATTRIBUTE);
  if (dict)
    {
      struct {
	enum font_property_index index;
	CFStringRef trait;
	CGFloat pos_scale, neg_scale;
      } numeric_traits[] =
	  {{FONT_WEIGHT_INDEX, MAC_FONT_WEIGHT_TRAIT, 250, 115},
	   {FONT_SLANT_INDEX, MAC_FONT_SLANT_TRAIT, 1000, 1000},
	   {FONT_WIDTH_INDEX, MAC_FONT_WIDTH_TRAIT, 100, 100}};
      int i;

      for (i = 0; i < sizeof (numeric_traits) / sizeof (numeric_traits[0]); i++)
	{
	  num = CFDictionaryGetValue (dict, numeric_traits[i].trait);
	  if (num && CFNumberGetValue (num, kCFNumberCGFloatType, &floatval))
	    FONT_SET_STYLE (spec_or_entity, numeric_traits[i].index,
			    make_number (floatval
					 * (floatval >= 0
					    ? numeric_traits[i].pos_scale
					    : numeric_traits[i].neg_scale)
					 + 100));
	}

      num = CFDictionaryGetValue (dict, MAC_FONT_SYMBOLIC_TRAIT);
      if (num)
	{
	  FontSymbolicTraits sym_traits;
	  int spacing;

	  cfnumber_get_font_symbolic_traits_value (num, &sym_traits);
	  spacing = (sym_traits & MAC_FONT_MONO_SPACE_TRAIT
		     ? FONT_SPACING_MONO : FONT_SPACING_PROPORTIONAL);
	  ASET (spec_or_entity, FONT_SPACING_INDEX, make_number (spacing));
	}

      CFRelease (dict);
    }
  num = mac_font_descriptor_copy_attribute (desc, MAC_FONT_SIZE_ATTRIBUTE);
  if (num && CFNumberGetValue (num, kCFNumberCGFloatType, &floatval))
    {
      ASET (spec_or_entity, FONT_SIZE_INDEX, make_number (floatval));
      CFRelease (num);
    }
  else
    ASET (spec_or_entity, FONT_SIZE_INDEX, make_number (0));
}

static Lisp_Object
macfont_descriptor_entity (desc, extra, synth_sym_traits)
     FontDescriptorRef desc;
     Lisp_Object extra;
     FontSymbolicTraits synth_sym_traits;
{
  Lisp_Object entity;
  CFDictionaryRef dict;
  FontSymbolicTraits sym_traits = 0;
  CFStringRef name;

  entity = font_make_entity ();

  ASET (entity, FONT_TYPE_INDEX, macfont_driver.type);
  ASET (entity, FONT_REGISTRY_INDEX, Qiso10646_1);

  macfont_store_descriptor_attributes (desc, entity);

  dict = mac_font_descriptor_copy_attribute (desc, MAC_FONT_TRAITS_ATTRIBUTE);
  if (dict)
    {
      CFNumberRef num = CFDictionaryGetValue (dict, MAC_FONT_SYMBOLIC_TRAIT);

      if (num)
	cfnumber_get_font_symbolic_traits_value (num, &sym_traits);
      CFRelease (dict);
    }
  if (EQ (AREF (entity, FONT_SIZE_INDEX), make_number (0)))
    ASET (entity, FONT_AVGWIDTH_INDEX, make_number (0));
  ASET (entity, FONT_EXTRA_INDEX, Fcopy_sequence (extra));
  name = mac_font_descriptor_copy_attribute (desc, MAC_FONT_NAME_ATTRIBUTE);
  font_put_extra (entity, QCfont_entity,
		  make_save_value ((void *) name, sym_traits));
  if (synth_sym_traits & MAC_FONT_ITALIC_TRAIT)
    FONT_SET_STYLE (entity, FONT_SLANT_INDEX,
		    make_number (FONT_SLANT_SYNTHETIC_ITALIC));
  if (synth_sym_traits & MAC_FONT_BOLD_TRAIT)
    FONT_SET_STYLE (entity, FONT_WEIGHT_INDEX,
		    make_number (FONT_WEIGHT_SYNTHETIC_BOLD));
  if (synth_sym_traits & MAC_FONT_MONO_SPACE_TRAIT)
    ASET (entity, FONT_SPACING_INDEX,
	  make_number (FONT_SPACING_SYNTHETIC_MONO));

  return entity;
}

static CFStringRef
macfont_create_family_with_symbol (symbol)
     Lisp_Object symbol;
{
  static CFArrayRef families = NULL;
  CFStringRef result = NULL, family_name;
  int using_cache_p = 1;
  CFComparatorFunction family_name_comparator;

  family_name =
    cfstring_create_with_utf8_cstring (SDATA (SYMBOL_NAME (symbol)));
  if (family_name == NULL)
    return NULL;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1060
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
  if (CTFontManagerCompareFontFamilyNames != NULL)
#endif
    {
      family_name_comparator = CTFontManagerCompareFontFamilyNames;
    }
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
  else		     /* CTFontManagerCompareFontFamilyNames == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1060 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
    {
      family_name_comparator = mac_font_family_compare;
    }
#endif

  while (1)
    {
      CFIndex i, count;

      if (families == NULL)
	{
	  families = mac_font_create_available_families ();
	  using_cache_p = 0;
	  if (families == NULL)
	    break;
	}

      count = CFArrayGetCount (families);
      i = CFArrayBSearchValues (families, CFRangeMake (0, count),
				(const void *) family_name,
				family_name_comparator, 0);
      if (i < count)
	{
	  CFStringRef name = CFArrayGetValueAtIndex (families, i);

	  if ((*family_name_comparator) (name, family_name, 0)
	      == kCFCompareEqualTo)
	    result = CFRetain (name);
	}

      if (result || !using_cache_p)
	break;
      else
	{
	  CFRelease (families);
	  families = NULL;
	}
    }

  CFRelease (family_name);

  return result;
}

static int
macfont_glyph_extents (font, glyph, metrics, advance_delta)
     struct font *font;
     CGGlyph glyph;
     struct font_metrics *metrics;
     int *advance_delta;
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  FontRef macfont = macfont_info->macfont;
  int row, col;
  struct font_metrics *cache;
  int width;

  row = glyph / METRICS_NCOLS_PER_ROW;
  col = glyph % METRICS_NCOLS_PER_ROW;
  if (row >= macfont_info->metrics_nrows)
    {
      macfont_info->metrics =
	xrealloc (macfont_info->metrics,
		  sizeof (struct font_metrics *) * (row + 1));
      bzero (macfont_info->metrics + macfont_info->metrics_nrows,
	     (sizeof (struct font_metrics *)
	      * (row + 1 - macfont_info->metrics_nrows)));
      macfont_info->metrics_nrows = row + 1;
    }
  if (macfont_info->metrics[row] == NULL)
    {
      struct font_metrics *new;
      int i;

      new = xmalloc (sizeof (struct font_metrics) * METRICS_NCOLS_PER_ROW);
      for (i = 0; i < METRICS_NCOLS_PER_ROW; i++)
	METRICS_SET_STATUS (new + i, METRICS_INVALID);
      macfont_info->metrics[row] = new;
    }
  cache = macfont_info->metrics[row] + col;

  if (METRICS_STATUS (cache) == METRICS_INVALID)
    {
      cache->width =
	mac_font_get_advance_width_for_glyph (macfont, glyph) + 0.5;
      METRICS_SET_STATUS (cache, METRICS_WIDTH_VALID);
      /* For synthetic mono fonts, cache->width holds the advance
	 delta value.  */
      if (macfont_info->synthetic_mono_p)
	cache->width = (font->pixel_size - cache->width) / 2;
    }
  if (macfont_info->synthetic_mono_p)
    width = font->pixel_size;
  else
    width = cache->width;

  if (metrics)
    {
      if (METRICS_STATUS (cache) == METRICS_WIDTH_VALID)
	{
	  CGRect bounds = mac_font_get_bounding_rect_for_glyph (macfont, glyph);

	  if (macfont_info->synthetic_italic_p)
	    {
	      /* We assume the members a, b, c, and d in
		 synthetic_italic_atfm are non-negative.  */
	      bounds.origin =
		CGPointApplyAffineTransform (bounds.origin,
					     synthetic_italic_atfm);
	      bounds.size =
		CGSizeApplyAffineTransform (bounds.size, synthetic_italic_atfm);
	    }
	  if (macfont_info->synthetic_bold_p)
	    {
	      CGFloat d =
		- synthetic_bold_factor * mac_font_get_size (macfont) / 2;

	      bounds = CGRectInset (bounds, d, d);
	    }
	  bounds = CGRectIntegral (bounds);
	  if (macfont_info->synthetic_mono_p)
	    bounds.origin.x += cache->width;
	  cache->lbearing = CGRectGetMinX (bounds);
	  cache->rbearing = CGRectGetMaxX (bounds);
	  cache->ascent = CGRectGetMaxY (bounds);
	  cache->descent = -CGRectGetMinY (bounds);
	}
      *metrics = *cache;
      if (macfont_info->synthetic_mono_p)
	metrics->width = width;
    }

  if (advance_delta)
    *advance_delta = (macfont_info->synthetic_mono_p ? cache->width : 0);

  return width;
}

static CFMutableDictionaryRef macfont_cache_dictionary;

/* Threshold used in row_nkeys_or_perm.  This must be less than or
   equal to the number of rows that are invalid as BMP (i.e., from
   U+D800 to U+DFFF).  */
#define ROW_PERM_OFFSET	(8)

struct macfont_cache
{
  int reference_count;
  CFCharacterSetRef cf_charset;
  struct {
    /* The cached glyph for a BMP character c is stored in
       matrix[row_nkeys_or_perm[c / 256] - ROW_PERM_OFFSET][c % 256]
       if row_nkeys_or_perm[c / 256] >= ROW_PERM_OFFSET.  */
    unsigned char row_nkeys_or_perm[256];
    CGGlyph **matrix;

    /* Number of rows for which the BMP cache is allocated so far.
       I.e., matrix[0] ... matrix[nrows - 1] are non-NULL.  */
    int nrows;

    /* The cached glyph for a character c is stored as a value for the
       key c.  However, the glyph for a BMP characrer c is not stored
       here if row_nkeys_or_perm[c / 256] >= ROW_PERM_OFFSET.  */
    CFMutableDictionaryRef dictionary;
  } glyph;
};

static struct macfont_cache *macfont_lookup_cache P_ ((CFStringRef));
static struct macfont_cache *macfont_retain_cache P_ ((struct macfont_cache *));
static void macfont_release_cache P_ ((struct macfont_cache *));
static CGGlyph macfont_get_glyph_for_character P_ ((struct font *, UTF32Char));

static struct macfont_cache *
macfont_lookup_cache (key)
     CFStringRef key;
{
  struct macfont_cache *cache;

  if (macfont_cache_dictionary == NULL)
    {
      macfont_cache_dictionary =
	CFDictionaryCreateMutable (NULL, 0,
				   &kCFTypeDictionaryKeyCallBacks, NULL);
      cache = NULL;
    }
  else
    cache = ((struct macfont_cache *)
	     CFDictionaryGetValue (macfont_cache_dictionary, key));

  if (cache == NULL)
    {
      FontRef macfont = mac_font_create_with_name (key, 0);

      if (macfont)
	{
	  cache = xmalloc (sizeof (struct macfont_cache));
	  bzero (cache, sizeof (struct macfont_cache));
	  cache->cf_charset = mac_font_copy_character_set (macfont);
	  CFDictionaryAddValue (macfont_cache_dictionary, key,
				(const void *) cache);
	  CFRelease (macfont);
	}
    }

  return cache;
}

static struct macfont_cache *
macfont_retain_cache (cache)
     struct macfont_cache *cache;
{
  cache->reference_count++;

  return cache;
}

static void
macfont_release_cache (cache)
     struct macfont_cache *cache;
{
  if (--cache->reference_count == 0)
    {
      int i;

      for (i = 0; i < cache->glyph.nrows; i++)
	xfree (cache->glyph.matrix[i]);
      xfree (cache->glyph.matrix);
      if (cache->glyph.dictionary)
	CFRelease (cache->glyph.dictionary);
      bzero (&cache->glyph, sizeof (cache->glyph));
    }
}

static CGGlyph
macfont_get_glyph_for_character (font, c)
     struct font *font;
     UTF32Char c;
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  FontRef macfont = macfont_info->macfont;
  struct macfont_cache *cache = macfont_info->cache;

  if (c < 0x10000)
    {
      int row = c / 256;
      int nkeys_or_perm = cache->glyph.row_nkeys_or_perm[row];

      if (nkeys_or_perm < ROW_PERM_OFFSET)
	{
	  UniChar unichars[256], ch;
	  CGGlyph *glyphs;
	  int i, len;
	  int nrows;

	  if (row != 0)
	    {
	      uintptr_t glyph;
	      CGGlyph g;

	      if (cache->glyph.dictionary == NULL)
		cache->glyph.dictionary =
		  CFDictionaryCreateMutable (NULL, 0, NULL, NULL);
	      if (CFDictionaryGetValueIfPresent (cache->glyph.dictionary,
						 (const void *) (uintptr_t) c,
						 (const void **) &glyph))
		return glyph;

	      ch = c;
	      if (!mac_font_get_glyphs_for_characters (macfont, &ch, &g, 1))
		g = kCGFontIndexInvalid;

	      if (nkeys_or_perm + 1 != ROW_PERM_OFFSET)
		{
		  CFDictionaryAddValue (cache->glyph.dictionary,
					(const void *) (uintptr_t) c,
					(const void *) (uintptr_t) g);
		  cache->glyph.row_nkeys_or_perm[row] = nkeys_or_perm + 1;

		  return g;
		}
	    }

	  len = 0;
	  for (i = 0; i < 256; i++)
	    {
	      ch = row * 256 + i;
	      if (CFCharacterSetIsLongCharacterMember (cache->cf_charset, ch))
		unichars[len++] = ch;
	      if (nkeys_or_perm > 0
		  && CFDictionaryContainsKey (cache->glyph.dictionary,
					      (const void *) (uintptr_t) ch))
		{
		  CFDictionaryRemoveValue (cache->glyph.dictionary,
					   (const void *) (uintptr_t) ch);
		  nkeys_or_perm--;
		}
	    }

	  glyphs = xmalloc (sizeof (CGGlyph) * 256);
	  if (len > 0)
	    {
	      mac_font_get_glyphs_for_characters (macfont, unichars,
						  glyphs, len);
	      while (i > len)
		{
		  int next = unichars[len - 1] % 256;

		  while (--i > next)
		    glyphs[i] = kCGFontIndexInvalid;

		  len--;
		  glyphs[i] = glyphs[len];
		  if (len == 0)
		    break;
		}
	    }
	  if (i > len)
	    while (i-- > 0)
	      glyphs[i] = kCGFontIndexInvalid;

	  nrows = cache->glyph.nrows;
	  nkeys_or_perm = nrows + ROW_PERM_OFFSET;
	  cache->glyph.row_nkeys_or_perm[row] = nkeys_or_perm;
	  nrows++;
	  cache->glyph.matrix = xrealloc (cache->glyph.matrix,
					  sizeof (CGGlyph *) * nrows);
	  cache->glyph.matrix[nrows - 1] = glyphs;
	  cache->glyph.nrows = nrows;
	}

      return cache->glyph.matrix[nkeys_or_perm - ROW_PERM_OFFSET][c % 256];
    }
  else
    {
      uintptr_t glyph;

      if (cache->glyph.dictionary == NULL)
	cache->glyph.dictionary =
	  CFDictionaryCreateMutable (NULL, 0, NULL, NULL);
      if (!CFDictionaryGetValueIfPresent (cache->glyph.dictionary,
					  (const void *) (uintptr_t) c,
					  (const void **) &glyph))
	{
	  UniChar unichars[2];
	  CGGlyph glyphs[2];

	  unichars[0] = ((c - 0x10000) >> 10) + 0xD800;
	  unichars[1] = (c & 0x3FF) + 0xDC00;
	  if (mac_font_get_glyphs_for_characters (macfont, unichars, glyphs, 2))
	    glyph = glyphs[0];
	  else
	    glyph = kCGFontIndexInvalid;

	  CFDictionaryAddValue (cache->glyph.dictionary,
				(const void *) (uintptr_t) c,
				(const void *) glyph);
	}

      return glyph;
    }
}

static Lisp_Object macfont_get_cache P_ ((FRAME_PTR));
static Lisp_Object macfont_list P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object macfont_match P_ ((Lisp_Object, Lisp_Object));
static Lisp_Object macfont_list_family P_ ((Lisp_Object));
static void macfont_free_entity P_ ((Lisp_Object));
static Lisp_Object macfont_open P_ ((FRAME_PTR, Lisp_Object, int));
static void macfont_close P_ ((FRAME_PTR, struct font *));
static int macfont_has_char P_ ((Lisp_Object, int));
static unsigned macfont_encode_char P_ ((struct font *, int));
static int macfont_text_extents P_ ((struct font *, unsigned *, int,
				     struct font_metrics *));
static int macfont_draw P_ ((struct glyph_string *, int, int, int, int, int));
static Lisp_Object macfont_shape P_ ((Lisp_Object));

struct font_driver macfont_driver =
  {
    0,				/* Qmac_ct, Qmac_fd, or Qmac_fm */
    0,				/* case insensitive */
    macfont_get_cache,
    macfont_list,
    macfont_match,
    macfont_list_family,
    macfont_free_entity,
    macfont_open,
    macfont_close,
    NULL,			/* prepare_face */
    NULL,			/* done_face */
    macfont_has_char,
    macfont_encode_char,
    macfont_text_extents,
    macfont_draw,
    NULL,			/* get_bitmap */
    NULL,			/* free_bitmap */
    NULL,			/* get_outline */
    NULL,			/* free_outline */
    NULL,			/* anchor_point */
    NULL,			/* otf_capability */
    NULL,			/* otf_drive */
    NULL,			/* start_for_frame */
    NULL,			/* end_for_frame */
    macfont_shape,
    NULL,			/* check */
    NULL			/* get_variation_glyphs */
  };

static Lisp_Object
macfont_get_cache (f)
     FRAME_PTR f;
{
  Display_Info *dpyinfo = FRAME_MAC_DISPLAY_INFO (f);

  return (dpyinfo->name_list_element);
}

static int
macfont_get_charset (registry)
     Lisp_Object registry;
{
  char *str = (char *) SDATA (SYMBOL_NAME (registry));
  char *re = alloca (SBYTES (SYMBOL_NAME (registry)) * 2 + 1);
  Lisp_Object regexp;
  int i, j;

  for (i = j = 0; i < SBYTES (SYMBOL_NAME (registry)); i++, j++)
    {
      if (str[i] == '.')
	re[j++] = '\\';
      else if (str[i] == '*')
	re[j++] = '.';
      re[j] = str[i];
      if (re[j] == '?')
	re[j] = '.';
    }
  re[j] = '\0';
  regexp = make_unibyte_string (re, j);
  for (i = 0; cf_charset_table[i].name; i++)
    if (fast_c_string_match_ignore_case (regexp, cf_charset_table[i].name) >= 0)
      break;
  if (! cf_charset_table[i].name)
    return -1;
  if (! cf_charset_table[i].cf_charset)
    {
      int *uniquifier = cf_charset_table[i].uniquifier;
      UniChar *unichars = alloca (sizeof (cf_charset_table[i].uniquifier));
      CFIndex count = 0;
      CFStringRef string;
      CFMutableCharacterSetRef charset = CFCharacterSetCreateMutable (NULL);

      if (! charset)
	return -1;
      for (j = 0; uniquifier[j]; j++)
	{
	  count += macfont_store_utf32char_to_unichars (uniquifier[j],
							unichars + count);
	  CFCharacterSetAddCharactersInRange (charset,
					      CFRangeMake (uniquifier[j], 1));
	}

      string = CFStringCreateWithCharacters (NULL, unichars, count);
      if (! string)
	{
	  CFRelease (charset);
	  return -1;
	}
#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1030
#if MAC_OS_X_VERSION_MIN_REQUIRED == 1020
      if (CFCharacterSetCreateCopy != NULL)
#endif
	{
	  cf_charset_table[i].cf_charset = CFCharacterSetCreateCopy (NULL,
								     charset);
	  CFRelease (charset);
	}
#if MAC_OS_X_VERSION_MIN_REQUIRED == 1020
      else		       /* CFCharacterSetCreateCopy == NULL */
#endif
#endif
#if MAC_OS_X_VERSION_MAX_ALLOWED < 1030 || MAC_OS_X_VERSION_MIN_REQUIRED == 1020
	{
	  cf_charset_table[i].cf_charset = charset;
	}
#endif
      /* CFCharacterSetCreateWithCharactersInString does not handle
	 surrogate pairs properly as of Mac OS X 10.5.  */
     cf_charset_table[i].cf_charset_string = string;
    }
  return i;
}

struct OpenTypeSpec
{
  Lisp_Object script;
  unsigned int script_tag, langsys_tag;
  int nfeatures[2];
  unsigned int *features[2];
};

#define OTF_SYM_TAG(SYM, TAG)					\
  do {								\
    unsigned char *p = SDATA (SYMBOL_NAME (SYM));		\
    TAG = (p[0] << 24) | (p[1] << 16) | (p[2] << 8) | p[3];	\
  } while (0)

#define OTF_TAG_STR(TAG, P)			\
  do {						\
    (P)[0] = (char) (TAG >> 24);		\
    (P)[1] = (char) ((TAG >> 16) & 0xFF);	\
    (P)[2] = (char) ((TAG >> 8) & 0xFF);	\
    (P)[3] = (char) (TAG & 0xFF);		\
    (P)[4] = '\0';				\
  } while (0)

static struct OpenTypeSpec *
macfont_get_open_type_spec (Lisp_Object otf_spec)
{
  struct OpenTypeSpec *spec = xmalloc (sizeof (struct OpenTypeSpec));
  Lisp_Object val;
  int i, j, negative;

  if (! spec)
    return NULL;
  spec->script = XCAR (otf_spec);
  if (! NILP (spec->script))
    {
      OTF_SYM_TAG (spec->script, spec->script_tag);
      val = assq_no_quit (spec->script, Votf_script_alist);
      if (CONSP (val) && SYMBOLP (XCDR (val)))
	spec->script = XCDR (val);
      else
	spec->script = Qnil;
    }
  else
    spec->script_tag = 0x44464C54; 	/* "DFLT" */
  otf_spec = XCDR (otf_spec);
  val = XCAR (otf_spec);
  if (! NILP (val))
    OTF_SYM_TAG (val, spec->langsys_tag);
  else
    spec->langsys_tag = 0;
  spec->nfeatures[0] = spec->nfeatures[1] = 0;
  for (i = 0; i < 2; i++)
    {
      Lisp_Object len;

      otf_spec = XCDR (otf_spec);
      if (NILP (otf_spec))
	break;
      val = XCAR (otf_spec);
      if (NILP (val))
	continue;
      len = Flength (val);
      spec->features[i] = xmalloc (sizeof (int) * XINT (len));
      if (! spec->features[i])
	{
	  if (i > 0 && spec->features[0])
	    free (spec->features[0]);
	  free (spec);
	  return NULL;
	}
      for (j = 0, negative = 0; CONSP (val); val = XCDR (val))
	{
	  if (NILP (XCAR (val)))
	    negative = 1;
	  else
	    {
	      unsigned int tag;

	      OTF_SYM_TAG (XCAR (val), tag);
	      spec->features[i][j++] = negative ? tag & 0x80000000 : tag;
	    }
	}
      spec->nfeatures[i] = j;
    }
  return spec;
}

static CFMutableDictionaryRef
macfont_create_attributes_with_spec (spec)
     Lisp_Object spec;
{
  Lisp_Object tmp, extra;
  CFMutableArrayRef langarray = NULL;
  CFCharacterSetRef charset = NULL;
  CFStringRef charset_string = NULL;
  CFMutableDictionaryRef attributes = NULL, traits = NULL;
  Lisp_Object script = Qnil;
  Lisp_Object registry;
  int cf_charset_idx, i;
  struct OpenTypeSpec *otspec = NULL;
  struct {
    enum font_property_index index;
    CFStringRef trait;
    CGFloat pos_scale, neg_scale;
  } numeric_traits[] = {{FONT_WEIGHT_INDEX, MAC_FONT_WEIGHT_TRAIT, 250, 115},
			{FONT_SLANT_INDEX, MAC_FONT_SLANT_TRAIT, 1000, 1000},
			{FONT_WIDTH_INDEX, MAC_FONT_WIDTH_TRAIT, 100, 100}};

  registry = AREF (spec, FONT_REGISTRY_INDEX);
  if (NILP (registry)
      || EQ (registry, Qascii_0)
      || EQ (registry, Qiso10646_1)
      || EQ (registry, Qunicode_bmp))
    cf_charset_idx = -1;
  else
    {
      CFStringRef lang;

      cf_charset_idx = macfont_get_charset (registry);
      if (cf_charset_idx < 0)
	goto err;
      charset = cf_charset_table[cf_charset_idx].cf_charset;
      charset_string = cf_charset_table[cf_charset_idx].cf_charset_string;
      lang = cf_charset_table[cf_charset_idx].lang;
      if (lang)
	{
	  langarray = CFArrayCreateMutable (NULL, 0, &kCFTypeArrayCallBacks);
	  if (! langarray)
	    goto err;
	  CFArrayAppendValue (langarray, lang);
	}
    }

  for (extra = AREF (spec, FONT_EXTRA_INDEX);
       CONSP (extra); extra = XCDR (extra))
    {
      Lisp_Object key, val;

      tmp = XCAR (extra);
      key = XCAR (tmp), val = XCDR (tmp);
      if (EQ (key, QClang))
	{
	  if (! langarray)
	    langarray = CFArrayCreateMutable (NULL, 0, &kCFTypeArrayCallBacks);
	  if (! langarray)
	    goto err;
	  if (SYMBOLP (val))
	    val = list1 (val);
	  for (; CONSP (val); val = XCDR (val))
	    if (SYMBOLP (XCAR (val)))
	      {
		CFStringRef lang =
		  cfstring_create_with_utf8_cstring (SDATA (SYMBOL_NAME
							    (XCAR (val))));

		if (lang == NULL)
		  goto err;
		CFArrayAppendValue (langarray, lang);
		CFRelease (lang);
	      }
	}
      else if (EQ (key, QCotf))
	{
	  otspec = macfont_get_open_type_spec (val);
	  if (! otspec)
	    goto err;
	  script = otspec->script;
	}
      else if (EQ (key, QCscript))
	script = val;
    }

  if (! NILP (script) && ! charset)
    {
      Lisp_Object chars = assq_no_quit (script, Vscript_representative_chars);

      if (CONSP (chars) && CONSP (CDR (chars)))
	{
	  CFMutableStringRef string = CFStringCreateMutable (NULL, 0);
	  CFMutableCharacterSetRef cs = CFCharacterSetCreateMutable (NULL);

	  if (! string || !cs)
	    {
	      if (string)
		CFRelease (string);
	      else if (cs)
		CFRelease (cs);
	      goto err;
	    }
	  for (chars = XCDR (chars); CONSP (chars); chars = XCDR (chars))
	    if (CHARACTERP (XCAR (chars)))
	      {
		UniChar unichars[2];
		CFIndex count =
		  macfont_store_utf32char_to_unichars (XUINT (XCAR (chars)),
						       unichars);
		CFRange range = CFRangeMake (XUINT (XCAR (chars)), 1);

		CFStringAppendCharacters (string, unichars, count);
		CFCharacterSetAddCharactersInRange (cs, range);
	      }
	  charset = cs;
	  /* CFCharacterSetCreateWithCharactersInString does not
	     handle surrogate pairs properly as of Mac OS X 10.5.  */
	  charset_string = string;
	}
    }

  attributes = CFDictionaryCreateMutable (NULL, 0,
					  &kCFTypeDictionaryKeyCallBacks,
					  &kCFTypeDictionaryValueCallBacks);
  if (! attributes)
    goto err;

  tmp = AREF (spec, FONT_FAMILY_INDEX);
  if (SYMBOLP (tmp) && ! NILP (tmp))
    {
      CFStringRef family = macfont_create_family_with_symbol (tmp);

      if (! family)
	goto err;
      CFDictionaryAddValue (attributes, MAC_FONT_FAMILY_NAME_ATTRIBUTE,
			    family);
      CFRelease (family);
    }

  traits = CFDictionaryCreateMutable (NULL, 4,
				      &kCFTypeDictionaryKeyCallBacks,
				      &kCFTypeDictionaryValueCallBacks);
  if (! traits)
    goto err;

  for (i = 0; i < sizeof (numeric_traits) / sizeof (numeric_traits[0]); i++)
    {
      tmp = AREF (spec, numeric_traits[i].index);
      if (INTEGERP (tmp))
	{
	  CGFloat floatval;
	  CFNumberRef num;

	  floatval = (XINT (tmp) >> 8) - 100; // XXX
	  if (floatval >= 0)
	    floatval /= numeric_traits[i].pos_scale;
	  else
	    floatval /= numeric_traits[i].neg_scale;
	  if (floatval > 1.0)
	    floatval = 1.0;
	  else if (floatval < -1.0)
	    floatval = -1.0;
	  num = CFNumberCreate (NULL, kCFNumberCGFloatType, &floatval);
	  if (! num)
	    goto err;
	  CFDictionaryAddValue (traits, numeric_traits[i].trait, num);
	  CFRelease (num);
	}
    }
  if (CFDictionaryGetCount (traits))
    CFDictionaryAddValue (attributes, MAC_FONT_TRAITS_ATTRIBUTE, traits);

  if (charset)
    CFDictionaryAddValue (attributes, MAC_FONT_CHARACTER_SET_ATTRIBUTE,
			  charset);
  if (charset_string)
    CFDictionaryAddValue (attributes, MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE,
			  charset_string);
  if (langarray)
    CFDictionaryAddValue (attributes, MAC_FONT_LANGUAGES_ATTRIBUTE, langarray);

  goto finish;

 err:
  if (attributes)
    {
      CFRelease (attributes);
      attributes = NULL;
    }

 finish:
  if (langarray) CFRelease (langarray);
  if (charset && cf_charset_idx < 0) CFRelease (charset);
  if (charset_string && cf_charset_idx < 0) CFRelease (charset_string);
  if (traits) CFRelease (traits);
  if (otspec)
    {
      if (otspec->nfeatures[0] > 0)
	free (otspec->features[0]);
      if (otspec->nfeatures[1] > 0)
	free (otspec->features[1]);
      free (otspec);
    }

  return attributes;
}

static Boolean
macfont_supports_charset_and_languages_p (desc, charset, chars, languages)
     FontDescriptorRef desc;
     CFCharacterSetRef charset;
     Lisp_Object chars;
     CFArrayRef languages;
{
  Boolean result = true;

  if (charset || VECTORP (chars))
    {
      CFCharacterSetRef desc_charset =
	mac_font_descriptor_copy_attribute (desc,
					    MAC_FONT_CHARACTER_SET_ATTRIBUTE);

      if (desc_charset == NULL)
	result = false;
      else
	{
	  if (charset)
	    result = CFCharacterSetIsSupersetOfSet (desc_charset, charset);
	  else 			/* VECTORP (chars) */
	    {
	      int j;

	      for (j = 0; j < ASIZE (chars); j++)
		if (NATNUMP (AREF (chars, j))
		    && CFCharacterSetIsLongCharacterMember (desc_charset,
							    XFASTINT (AREF (chars, j))))
		  break;
	      if (j == ASIZE (chars))
		result = false;
	    }
	  CFRelease (desc_charset);
	}
    }
  if (result && languages)
    result = mac_font_descriptor_supports_languages (desc, languages);

  return result;
}

static CFIndex
macfont_closest_traits_index (traits_array, target)
     CFArrayRef traits_array;
     FontSymbolicTraits target;
{
  CFIndex i, result = -1, count = CFArrayGetCount (traits_array);
  int min_distance = (1 << 3);

  for (i = 0; i < count; i++)
    {
      FontSymbolicTraits traits, diff;
      int distance = 0;

      traits = ((FontSymbolicTraits) (uintptr_t)
		CFArrayGetValueAtIndex (traits_array, i));
      diff = (target ^ traits);
      /* We prefer synthetic bold of italic to synthetic italic of
	 bold when both bold and italic are available but bold-italic
	 is not available.  */
      if (diff & MAC_FONT_BOLD_TRAIT)
	distance |= (1 << 0);
      if (diff & MAC_FONT_ITALIC_TRAIT)
	distance |= (1 << 1);
      if (diff & MAC_FONT_MONO_SPACE_TRAIT)
	distance |= (1 << 2);
      if (distance < min_distance)
	{
	  min_distance = distance;
	  result = i;
	}
    }

  return result;
}

static Lisp_Object
macfont_list (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object val = Qnil, family, extra;
  int i, j, n;
  CFStringRef family_name = NULL;
  CFMutableDictionaryRef attributes = NULL, traits;
  Lisp_Object chars = Qnil;
  int spacing = -1;
  FontSymbolicTraits synth_sym_traits = 0;
  CFArrayRef families;
  CFIndex families_count, last_resort_index = -1;
  CFCharacterSetRef charset = NULL;
  CFArrayRef languages = NULL;

  BLOCK_INPUT;

  family = AREF (spec, FONT_FAMILY_INDEX);
  if (! NILP (family))
    {
      family_name = macfont_create_family_with_symbol (family);
      if (family_name == NULL)
	goto finish;
    }

  attributes = macfont_create_attributes_with_spec (spec);
  if (! attributes)
    goto finish;

  charset = ((CFCharacterSetRef)
	     CFDictionaryGetValue (attributes,
				   MAC_FONT_CHARACTER_SET_ATTRIBUTE));
  if (charset)
    {
      CFRetain (charset);
      CFDictionaryRemoveValue (attributes, MAC_FONT_CHARACTER_SET_ATTRIBUTE);
    }
  else
    {
      val = assq_no_quit (QCscript, AREF (spec, FONT_EXTRA_INDEX));
      if (! NILP (val))
	{
	  val = assq_no_quit (XCDR (val), Vscript_representative_chars);
	  if (CONSP (val) && VECTORP (XCDR (val)))
	    chars = XCDR (val);
	}
      val = Qnil;
    }

  languages = ((CFArrayRef)
	       CFDictionaryGetValue (attributes, MAC_FONT_LANGUAGES_ATTRIBUTE));
  if (languages)
    {
      CFRetain (languages);
      CFDictionaryRemoveValue (attributes, MAC_FONT_LANGUAGES_ATTRIBUTE);
    }

  if (INTEGERP (AREF (spec, FONT_SPACING_INDEX)))
    spacing = XINT (AREF (spec, FONT_SPACING_INDEX));

  traits = ((CFMutableDictionaryRef)
	    CFDictionaryGetValue (attributes, MAC_FONT_TRAITS_ATTRIBUTE));

  n = FONT_SLANT_NUMERIC (spec);
  if (n < 0 || n == FONT_SLANT_SYNTHETIC_ITALIC)
    {
      synth_sym_traits |= MAC_FONT_ITALIC_TRAIT;
      if (traits)
	CFDictionaryRemoveValue (traits, MAC_FONT_SLANT_TRAIT);
    }

  n = FONT_WEIGHT_NUMERIC (spec);
  if (n < 0 || n == FONT_WEIGHT_SYNTHETIC_BOLD)
    {
      synth_sym_traits |= MAC_FONT_BOLD_TRAIT;
      if (traits)
	CFDictionaryRemoveValue (traits, MAC_FONT_WEIGHT_TRAIT);
    }

  if (languages
      && (spacing < 0 || spacing == FONT_SPACING_SYNTHETIC_MONO))
    {
      CFStringRef language = CFArrayGetValueAtIndex (languages, 0);

      if (CFStringHasPrefix (language, CFSTR ("ja"))
	  || CFStringHasPrefix (language, CFSTR ("ko"))
	  || CFStringHasPrefix (language, CFSTR ("zh")))
	synth_sym_traits |= MAC_FONT_MONO_SPACE_TRAIT;
    }

  /* Create array of families.  */
  if (family_name)
    families = CFArrayCreate (NULL, (const void **) &family_name,
			      1, &kCFTypeArrayCallBacks);
  else
    {
      CFStringRef preferred_family;
      CFIndex families_count, preferred_family_index = -1;

      families = mac_font_create_available_families ();
      if (families == NULL)
	goto err;

      families_count = CFArrayGetCount (families);
      if (EQ (AREF (spec, FONT_REGISTRY_INDEX), Qiso10646_1))
	last_resort_index = families_count;

      /* Move preferred family to the front if exists.  */
      preferred_family =
	mac_font_create_preferred_family_for_attributes (attributes);
      if (preferred_family)
	{
	  preferred_family_index =
	    CFArrayGetFirstIndexOfValue (families,
					 CFRangeMake (0, families_count),
					 preferred_family);
	  CFRelease (preferred_family);
	}
      if (last_resort_index >= 0 || preferred_family_index > 0)
	{
	  CFMutableArrayRef mutable_families =
	    CFArrayCreateMutableCopy (NULL,
				      (last_resort_index >= 0
				       ? families_count + 1 : families_count),
				      families);
	  if (mutable_families)
	    {
	      if (last_resort_index >= 0)
		CFArrayAppendValue (mutable_families, CFSTR ("LastResort"));
	      if (preferred_family_index > 0)
		CFArrayExchangeValuesAtIndices (mutable_families,
						0, preferred_family_index);
	      CFRelease (families);
	      families = mutable_families;
	    }
	}
    }

  val = Qnil;
  extra = AREF (spec, FONT_EXTRA_INDEX);
  families_count = CFArrayGetCount (families);
  for (i = 0; i < families_count; i++)
    {
      CFStringRef family_name = CFArrayGetValueAtIndex (families, i);
      FontDescriptorRef pat_desc;
      CFArrayRef descs;
      CFIndex descs_count;
      CFMutableArrayRef filtered_descs, traits_array;
      Lisp_Object entity;

      if (i == last_resort_index && !NILP (val))
	break;

      CFDictionarySetValue (attributes, MAC_FONT_FAMILY_NAME_ATTRIBUTE,
			    family_name);
      pat_desc = mac_font_descriptor_create_with_attributes (attributes);
      if (! pat_desc)
	goto err;

      descs = mac_font_descriptor_create_matching_font_descriptors (pat_desc,
								    NULL);
      if (! descs)
	goto err;

      CFRelease (pat_desc);

      descs_count = CFArrayGetCount (descs);
      if (descs_count == 0
	  || (i != last_resort_index
	      && !macfont_supports_charset_and_languages_p (CFArrayGetValueAtIndex (descs, 0),
							    charset, chars,
							    languages)))
	{
	  CFRelease (descs);
	  continue;
	}

      filtered_descs =
	CFArrayCreateMutable (NULL, descs_count, &kCFTypeArrayCallBacks);
      traits_array = CFArrayCreateMutable (NULL, descs_count, NULL);
      for (j = 0; j < descs_count; j++)
	{
	  FontDescriptorRef desc = CFArrayGetValueAtIndex (descs, j);
	  CFDictionaryRef dict;
	  CFNumberRef num;
	  FontSymbolicTraits sym_traits;

	  dict = mac_font_descriptor_copy_attribute (desc,
						     MAC_FONT_TRAITS_ATTRIBUTE);
	  if (dict == NULL)
	    continue;

	  num = CFDictionaryGetValue (dict, MAC_FONT_SYMBOLIC_TRAIT);
	  CFRelease (dict);
	  if (num == NULL
	      || !cfnumber_get_font_symbolic_traits_value (num, &sym_traits))
	    continue;

	  if (spacing >= 0
	      && !(synth_sym_traits & MAC_FONT_MONO_SPACE_TRAIT)
	      && (((sym_traits & MAC_FONT_MONO_SPACE_TRAIT) != 0)
		  != (spacing >= FONT_SPACING_MONO)))
	    continue;

	  if (i != last_resort_index && j > 0
	      && !macfont_supports_charset_and_languages_p (desc, charset,
							    chars, languages))
	    continue;

	  CFArrayAppendValue (filtered_descs, desc);
	  CFArrayAppendValue (traits_array,
			      (const void *) (uintptr_t) sym_traits);
	}

      CFRelease (descs);
      descs = filtered_descs;
      descs_count = CFArrayGetCount (descs);

      for (j = 0; j < descs_count; j++)
	{
	  FontDescriptorRef desc = CFArrayGetValueAtIndex (descs, j);
	  FontSymbolicTraits sym_traits =
	    ((FontSymbolicTraits) (uintptr_t)
	     CFArrayGetValueAtIndex (traits_array, j));
	  FontSymbolicTraits mask_min, mask_max, imask, bmask, mmask;

	  mask_min = ((synth_sym_traits ^ sym_traits)
		      & (MAC_FONT_ITALIC_TRAIT | MAC_FONT_BOLD_TRAIT));
	  if (FONT_SLANT_NUMERIC (spec) < 0)
	    mask_min &= ~MAC_FONT_ITALIC_TRAIT;
	  if (FONT_WEIGHT_NUMERIC (spec) < 0)
	    mask_min &= ~MAC_FONT_BOLD_TRAIT;

	  mask_max = (synth_sym_traits & ~sym_traits);
	  if (spacing >= 0)
	    mask_min |= (mask_max & MAC_FONT_MONO_SPACE_TRAIT);

	  for (mmask = (mask_min & MAC_FONT_MONO_SPACE_TRAIT);
	       mmask <= (mask_max & MAC_FONT_MONO_SPACE_TRAIT);
	       mmask += MAC_FONT_MONO_SPACE_TRAIT)
	    for (bmask = (mask_min & MAC_FONT_BOLD_TRAIT);
		 bmask <= (mask_max & MAC_FONT_BOLD_TRAIT);
		 bmask += MAC_FONT_BOLD_TRAIT)
	      for (imask = (mask_min & MAC_FONT_ITALIC_TRAIT);
		   imask <= (mask_max & MAC_FONT_ITALIC_TRAIT);
		   imask += MAC_FONT_ITALIC_TRAIT)
		{
		  FontSymbolicTraits synth = (imask | bmask | mmask);

		  if (synth == 0
		      || j == macfont_closest_traits_index (traits_array,
							    (sym_traits | synth)))
		    {
		      entity = macfont_descriptor_entity (desc, extra, synth);
		      if (! NILP (entity))
			val = Fcons (entity, val);
		    }
		}
	}

      CFRelease (traits_array);
      CFRelease (descs);
    }

  CFRelease (families);
  val = Fnreverse (val);
  goto finish;
 err:
  val = Qnil;

 finish:
  font_add_log ("macfont-list", spec, val);
  if (charset) CFRelease (charset);
  if (languages) CFRelease (languages);
  if (attributes) CFRelease (attributes);
  if (family_name) CFRelease (family_name);

  UNBLOCK_INPUT;

  return val;
}

static Lisp_Object
macfont_match (frame, spec)
     Lisp_Object frame, spec;
{
  Lisp_Object entity = Qnil;
  CFMutableDictionaryRef attributes;
  FontDescriptorRef pat_desc = NULL, desc = NULL;

  BLOCK_INPUT;

  attributes = macfont_create_attributes_with_spec (spec);
  if (attributes)
    {
      pat_desc = mac_font_descriptor_create_with_attributes (attributes);
      CFRelease (attributes);
    }
  if (pat_desc)
    {
      desc = mac_font_descriptor_create_matching_font_descriptor (pat_desc,
								  NULL);
      CFRelease (pat_desc);
    }
  if (desc)
    {
      entity = macfont_descriptor_entity (desc, AREF (spec, FONT_EXTRA_INDEX),
					  0);
      CFRelease (desc);
    }
  UNBLOCK_INPUT;

  font_add_log ("macfont-match", spec, entity);
  return entity;
}

static Lisp_Object
macfont_list_family (frame)
     Lisp_Object frame;
{
  Lisp_Object list = Qnil;
  CFArrayRef families;

  BLOCK_INPUT;

  families = mac_font_create_available_families ();
  if (families)
    {
      CFIndex i, count = CFArrayGetCount (families);

      for (i = 0; i < count; i++)
	list = Fcons (macfont_intern_prop_cfstring (CFArrayGetValueAtIndex (families, i)), list);
      CFRelease (families);
    }

  UNBLOCK_INPUT;

  return list;
}

static void
macfont_free_entity (entity)
     Lisp_Object entity;
{
  Lisp_Object val = assq_no_quit (QCfont_entity,
				  AREF (entity, FONT_EXTRA_INDEX));
  CFStringRef name = XSAVE_VALUE (XCDR (val))->pointer;

  BLOCK_INPUT;
  CFRelease (name);
  UNBLOCK_INPUT;
}

extern Lisp_Object QCantialias;

static Lisp_Object
macfont_open (f, entity, pixel_size)
     FRAME_PTR f;
     Lisp_Object entity;
     int pixel_size;
{
  Lisp_Object val, font_object;
  CFStringRef font_name;
  struct macfont_info *macfont_info = NULL;
  struct font *font;
  int size;
  FontRef macfont;
  FontSymbolicTraits sym_traits;
  char name[256];
  int len, i, total_width;
  CGGlyph glyph;
  CGFloat ascent, descent, leading;
  CFStringRef family_name;

  val = assq_no_quit (QCfont_entity, AREF (entity, FONT_EXTRA_INDEX));
  if (! CONSP (val)
      || XTYPE (XCDR (val)) != Lisp_Misc
      || XMISCTYPE (XCDR (val)) != Lisp_Misc_Save_Value)
    return Qnil;
  font_name = XSAVE_VALUE (XCDR (val))->pointer;
  sym_traits = XSAVE_VALUE (XCDR (val))->integer;

  size = XINT (AREF (entity, FONT_SIZE_INDEX));
  if (size == 0)
    size = pixel_size;

  BLOCK_INPUT;
  macfont = mac_font_create_with_name (font_name, size);
  UNBLOCK_INPUT;
  if (! macfont)
    return Qnil;

  font_object = font_make_object (VECSIZE (struct macfont_info), entity, size);
  ASET (font_object, FONT_TYPE_INDEX, macfont_driver.type);
  len = font_unparse_xlfd (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_NAME_INDEX, make_string (name, len));
  len = font_unparse_fcname (entity, size, name, 256);
  if (len > 0)
    ASET (font_object, FONT_FULLNAME_INDEX, make_string (name, len));
  else
    ASET (font_object, FONT_FULLNAME_INDEX,
	  AREF (font_object, FONT_NAME_INDEX));
  font = XFONT_OBJECT (font_object);
  font->pixel_size = size;
  font->driver = &macfont_driver;
  font->encoding_charset = font->repertory_charset = -1;

  BLOCK_INPUT;

  macfont_info = (struct macfont_info *) font;
  macfont_info->macfont = macfont;
  macfont_info->cgfont = mac_font_copy_graphics_font (macfont);
  macfont_info->cache = macfont_lookup_cache (font_name);
  macfont_retain_cache (macfont_info->cache);
  macfont_info->metrics = NULL;
  macfont_info->metrics_nrows = 0;
  macfont_info->synthetic_italic_p = 0;
  macfont_info->synthetic_bold_p = 0;
  macfont_info->synthetic_mono_p = 0;
  macfont_info->no_antialias_p = 0;
  if (!(sym_traits & MAC_FONT_ITALIC_TRAIT)
      && FONT_SLANT_NUMERIC (entity) == FONT_SLANT_SYNTHETIC_ITALIC)
    macfont_info->synthetic_italic_p = 1;
  if (!(sym_traits & MAC_FONT_BOLD_TRAIT)
      && FONT_WEIGHT_NUMERIC (entity) == FONT_WEIGHT_SYNTHETIC_BOLD)
    macfont_info->synthetic_bold_p = 1;
  if (!(sym_traits & MAC_FONT_MONO_SPACE_TRAIT)
      && INTEGERP (AREF (entity, FONT_SPACING_INDEX))
      && (XINT (AREF (entity, FONT_SPACING_INDEX))
	  == FONT_SPACING_SYNTHETIC_MONO))
    macfont_info->synthetic_mono_p = 1;
  val = assq_no_quit (QCantialias, AREF (entity, FONT_EXTRA_INDEX));
  if (CONSP (val))
    macfont_info->no_antialias_p = NILP (XCDR (val));
  else
    {
      int threshold;
      Boolean valid_p;

      threshold =
	CFPreferencesGetAppIntegerValue (CFSTR ("AppleAntiAliasingThreshold"),
					 kCFPreferencesCurrentApplication,
					 &valid_p);
      if (valid_p && size <= threshold)
	macfont_info->no_antialias_p = 1;
    }

  glyph = macfont_get_glyph_for_character (font, ' ');
  if (glyph != kCGFontIndexInvalid)
    font->space_width = macfont_glyph_extents (font, glyph, NULL, NULL);
  else
    /* dirty workaround */
    font->space_width = pixel_size;

  total_width = font->space_width;
  for (i = 1; i < 95; i++)
    {
      glyph = macfont_get_glyph_for_character (font, ' ' + i);
      if (glyph == kCGFontIndexInvalid)
	break;
      total_width += macfont_glyph_extents (font, glyph, NULL, NULL);
    }
  if (i == 95)
    font->average_width = total_width / 95;
  else
    font->average_width = font->space_width; /* XXX */

  ascent = mac_font_get_ascent (macfont);
  descent = mac_font_get_descent (macfont);
  leading = mac_font_get_leading (macfont);
  /* AppKit and WebKit do some adjustment to the heights of Courier,
     Helvetica, and Times.  */
  family_name = mac_font_copy_family_name (macfont);
  if (family_name)
    {
      if ((CFStringCompare (family_name, CFSTR ("Courier"), 0)
	   == kCFCompareEqualTo)
	  || (CFStringCompare (family_name, CFSTR ("Helvetica"), 0)
	      == kCFCompareEqualTo)
	  || (CFStringCompare (family_name, CFSTR ("Times"), 0)
	      == kCFCompareEqualTo))
	ascent += (ascent + descent) * .15;
      else if (CFStringHasPrefix (family_name, CFSTR ("Hiragino")))
	{
	  leading *= .25;
	  ascent += leading;
	}
      CFRelease (family_name);
    }
  font->ascent = ascent + 0.5;
  font->descent = descent + leading + 0.5;
  font->height = font->ascent + font->descent;

  font->underline_position = - mac_font_get_underline_position (macfont) + 0.5;
  font->underline_thickness = mac_font_get_underline_thickness (macfont) + 0.5;

  UNBLOCK_INPUT;

  /* Unfortunately Xft doesn't provide a way to get minimum char
     width.  So, we use space_width instead.  */
  font->min_width = font->space_width; /* XXX */

  font->baseline_offset = 0;
  font->relative_compose = 0;
  font->default_ascent = 0;
  font->vertical_centering = 0;

  return font_object;
}

static void
macfont_close (f, font)
     FRAME_PTR f;
     struct font *font;
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  int i;

  BLOCK_INPUT;
  CFRelease (macfont_info->macfont);
  CGFontRelease (macfont_info->cgfont);
  macfont_release_cache (macfont_info->cache);
  for (i = 0; i < macfont_info->metrics_nrows; i++)
    if (macfont_info->metrics[i])
      xfree (macfont_info->metrics[i]);
  if (macfont_info->metrics)
    xfree (macfont_info->metrics);
  UNBLOCK_INPUT;
}

static int
macfont_has_char (font, c)
     Lisp_Object font;
     int c;
{
  int result = -1;

  BLOCK_INPUT;
  if (FONT_ENTITY_P (font))
    {
      Lisp_Object val;
      CFStringRef name;
      struct macfont_cache *cache;
      CFCharacterSetRef charset = NULL;

      val = assq_no_quit (QCfont_entity, AREF (font, FONT_EXTRA_INDEX));
      val = XCDR (val);
      name = XSAVE_VALUE (val)->pointer;
      cache = macfont_lookup_cache (name);
      result = CFCharacterSetIsLongCharacterMember (cache->cf_charset,
						    (UTF32Char) c);
    }
  else
    {
      struct macfont_info *macfont_info;

      macfont_info = (struct macfont_info *) XFONT_OBJECT (font);
      result =
	CFCharacterSetIsLongCharacterMember (macfont_info->cache->cf_charset,
					     (UTF32Char) c);
    }
  UNBLOCK_INPUT;

  return result;
}

static unsigned
macfont_encode_char (font, c)
     struct font *font;
     int c;
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  CGGlyph glyph;

  BLOCK_INPUT;
  glyph = macfont_get_glyph_for_character (font, c);
  UNBLOCK_INPUT;

  return glyph != kCGFontIndexInvalid ? glyph : FONT_INVALID_CODE;
}

static int
macfont_text_extents (font, code, nglyphs, metrics)
     struct font *font;
     unsigned *code;
     int nglyphs;
     struct font_metrics *metrics;
{
  int width, i;

  BLOCK_INPUT;
  width = macfont_glyph_extents (font, code[0], metrics, NULL);
  for (i = 1; i < nglyphs; i++)
    {
      struct font_metrics m;
      int w = macfont_glyph_extents (font, code[i], metrics ? &m : NULL, NULL);

      if (metrics)
	{
	  if (width + m.lbearing < metrics->lbearing)
	    metrics->lbearing = width + m.lbearing;
	  if (width + m.rbearing > metrics->rbearing)
	    metrics->rbearing = width + m.rbearing;
	  if (m.ascent > metrics->ascent)
	    metrics->ascent = m.ascent;
	  if (m.descent > metrics->descent)
	    metrics->descent = m.descent;
	}
      width += w;
    }
  UNBLOCK_INPUT;

  if (metrics)
    metrics->width = width;

  return width;
}

static int
macfont_draw (s, from, to, x, y, with_background)
     struct glyph_string *s;
     int from, to, x, y, with_background;
{
  FRAME_PTR f = s->f;
  struct face *face = s->face;
  struct macfont_info *macfont_info = (struct macfont_info *) s->font;
  FontRef macfont = macfont_info->macfont;
  CGContextRef context;
  int len = to - from;
  int i;

  BLOCK_INPUT;

  context = mac_begin_cg_clip (f, s->gc);

#if 0
  if (s->num_clips > 0)
    {
      CGRect clips[2];

      for (i = 0; i < s->num_clips; i++)
	clips[i] = mac_rect_make (f, s->clip[i].left, s->clip[i].top,
				  s->clip[i].right - s->clip[i].left,
				  s->clip[i].bottom - s->clip[i].top);
      CGContextClipToRects (context, clips, s->num_clips);
    }
#endif

  if (with_background)
    {
      CG_SET_FILL_COLOR_WITH_GC_BACKGROUND (context, s->gc);
      CGContextFillRect	(context,
			 mac_rect_make (f, x, y - FONT_BASE (face->font),
					s->width, FONT_HEIGHT (face->font)));
    }

  if (macfont_info->cgfont)
    {
      CGGlyph *glyphs = alloca (sizeof (CGGlyph) * len);
      CGSize *advances = alloca (sizeof (CGSize) * len);
      CGFloat font_size = mac_font_get_size (macfont);
      CGAffineTransform atfm;
      int advance_delta = 0;

      for (i = len - 1; i >= 0; i--)
	{
	  int width, last_advance_delta = advance_delta;

	  glyphs[i] = ((XCHAR2B_BYTE1 (s->char2b + from + i) << 8)
		       | XCHAR2B_BYTE2 (s->char2b + from + i));
	  width = (s->padding_p ? 1
		   : macfont_glyph_extents (s->font, glyphs[i],
					    NULL, &advance_delta));
	  advances[i].width = width + last_advance_delta - advance_delta;
	  advances[i].height = 0;
	}

      CGContextScaleCTM (context, 1, -1);
      CG_SET_FILL_COLOR_WITH_GC_FOREGROUND (context, s->gc);
      if (macfont_info->synthetic_italic_p)
	atfm = synthetic_italic_atfm;
      else
	atfm = CGAffineTransformIdentity;
      if (macfont_info->synthetic_bold_p)
	{
	  CGContextSetTextDrawingMode (context, kCGTextFillStroke);
	  CGContextSetLineWidth (context, synthetic_bold_factor * font_size);
	  CG_SET_STROKE_COLOR_WITH_GC_FOREGROUND (context, s->gc);
	}
      if (macfont_info->no_antialias_p)
	CGContextSetShouldAntialias (context, false);

      CGContextSetFont (context, macfont_info->cgfont);
      CGContextSetFontSize (context, font_size);
      CGContextSetTextMatrix (context, atfm);
      CGContextSetTextPosition (context, x + advance_delta, -y);
      /* The symbol CGContextShowGlyphsWithAdvances seems to exist
	 even in Mac OS X 10.2.  */
      CGContextShowGlyphsWithAdvances (context, glyphs, advances, len);
    }

  mac_end_cg_clip (f);

  UNBLOCK_INPUT;

  return len;
}

#if USE_CORE_TEXT

Boolean
mac_ctfont_descriptor_supports_languages (descriptor, languages)
     CTFontDescriptorRef descriptor;
     CFArrayRef languages;
{
  Boolean result = true;
  CFArrayRef desc_languages =
    CTFontDescriptorCopyAttribute (descriptor, kCTFontLanguagesAttribute);

  if (desc_languages == NULL)
    result = false;
  else
    {
      CFIndex desc_languages_count, i, languages_count;

      desc_languages_count = CFArrayGetCount (desc_languages);
      languages_count = CFArrayGetCount (languages);
      for (i = 0; i < languages_count; i++)
	if (!CFArrayContainsValue (desc_languages,
				   CFRangeMake (0, desc_languages_count),
				   CFArrayGetValueAtIndex (languages, i)))
	  {
	    result = false;
	    break;
	  }
      CFRelease (desc_languages);
    }

  return result;
}

CFStringRef
mac_ctfont_create_preferred_family_for_attributes (attributes)
     CFDictionaryRef attributes;
{
  CFStringRef result = NULL;
  CFStringRef charset_string =
    CFDictionaryGetValue (attributes, MAC_FONT_CHARACTER_SET_STRING_ATTRIBUTE);
  CFIndex length;

  if (charset_string
      && (length = CFStringGetLength (charset_string)) > 0)
    {
      CTFontRef last_resort =
	CTFontCreateWithName (CFSTR ("LastResort"), 0, NULL);

      if (last_resort)
	{
	  CTFontRef font = CTFontCreateForString (last_resort, charset_string,
						  CFRangeMake (0, length));

	  if (font)
	    {
	      result = CTFontCopyAttribute (font, kCTFontFamilyNameAttribute);

	      if (CFStringCompare (result, CFSTR ("LastResort"), 0)
		  == kCFCompareEqualTo)
		{
		  CFRelease (result);
		  result = NULL;
		}
	      CFRelease (font);
	    }
	  CFRelease (last_resort);
	}
    }

  return result;
}

static INLINE double
mac_ctfont_get_advance_width_for_glyph (font, glyph)
     CTFontRef font;
     CGGlyph glyph;
{
  return CTFontGetAdvancesForGlyphs (font, kCTFontDefaultOrientation,
				     &glyph, NULL, 1);
}

static INLINE CGRect
mac_ctfont_get_bounding_rect_for_glyph (font, glyph)
     CTFontRef font;
     CGGlyph glyph;
{
  return CTFontGetBoundingRectsForGlyphs (font, kCTFontDefaultOrientation,
					  &glyph, NULL, 1);
}

static CFArrayRef
mac_ctfont_create_available_families ()
{
  CFMutableArrayRef families = NULL;

#if MAC_OS_X_VERSION_MAX_ALLOWED >= 1060
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
  if (CTFontManagerCopyAvailableFontFamilyNames != NULL)
#endif
    {
      CFArrayRef orig_families = CTFontManagerCopyAvailableFontFamilyNames ();

      if (orig_families)
	{
	  CFIndex i, count = CFArrayGetCount (orig_families);

	  families = CFArrayCreateMutable (NULL, count, &kCFTypeArrayCallBacks);
	  if (families)
	    for (i = 0; i < count; i++)
	      {
		CFStringRef family = CFArrayGetValueAtIndex (orig_families, i);

		if (!CFStringHasPrefix (family, CFSTR ("."))
		    && (CTFontManagerCompareFontFamilyNames (family,
							     CFSTR ("LastResort"),
							     NULL)
			!= kCFCompareEqualTo))
		  CFArrayAppendValue (families, family);
	      }
	  CFRelease (orig_families);
	}
    }
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
  else	       /* CTFontManagerCopyAvailableFontFamilyNames == NULL */
#endif
#endif	/* MAC_OS_X_VERSION_MAX_ALLOWED >= 1060 */
#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
    {
      CTFontCollectionRef collection;
      CFArrayRef descs = NULL;

      collection = CTFontCollectionCreateFromAvailableFonts (NULL);
      if (collection)
	{
	  descs = CTFontCollectionCreateMatchingFontDescriptors (collection);
	  CFRelease (collection);
	}
      if (descs)
	{
	  CFIndex i, count = CFArrayGetCount (descs);

	  families = CFArrayCreateMutable (NULL, count, &kCFTypeArrayCallBacks);
	  if (families)
	    for (i = 0; i < count; i++)
	      {
		FontDescriptorRef desc = CFArrayGetValueAtIndex (descs, i);
		CFStringRef name =
		  mac_font_descriptor_copy_attribute (desc,
						      MAC_FONT_FAMILY_NAME_ATTRIBUTE);

		if (name)
		  {
		    CFIndex p, limit = CFArrayGetCount (families);

		    p = CFArrayBSearchValues (families, CFRangeMake (0, limit),
					      (const void *) name,
					      mac_font_family_compare,
					      0);
		    if (p >= limit)
		      CFArrayAppendValue (families, name);
		    else if (mac_font_family_compare
			     (CFArrayGetValueAtIndex (families, p),
			      name, 0) != kCFCompareEqualTo)
		      CFArrayInsertValueAtIndex (families, p, name);
		    CFRelease (name);
		  }
	      }
	  CFRelease (descs);
	}
    }
#endif

  return families;
}

static CTLineRef
mac_ctfont_create_line_with_string_and_font (string, macfont)
     CFStringRef string;
     CTFontRef macfont;
{
  CFStringRef keys[] = {kCTFontAttributeName, kCTKernAttributeName};
  CFTypeRef values[] = {NULL, NULL};
  CFDictionaryRef attributes = NULL;
  CFAttributedStringRef attr_string = NULL;
  CTLineRef ctline = NULL;
  float float_zero = 0.0f;

  values[0] = macfont;
  values[1] = CFNumberCreate (NULL, kCFNumberFloatType, &float_zero);
  if (values[1])
    {
      attributes = CFDictionaryCreate (NULL, (const void **) keys,
				       (const void **) values,
				       sizeof (keys) / sizeof (keys[0]),
				       &kCFTypeDictionaryKeyCallBacks,
				       &kCFTypeDictionaryValueCallBacks);
      CFRelease (values[1]);
    }
  if (attributes)
    {
      attr_string = CFAttributedStringCreate (NULL, string, attributes);
      CFRelease (attributes);
    }
  if (attr_string)
    {
      ctline = CTLineCreateWithAttributedString (attr_string);
      CFRelease (attr_string);
    }

  return ctline;
}

CFIndex
mac_ctfont_shape (font, string, glyph_layouts, glyph_len)
     CTFontRef font;
     CFStringRef string;
     struct mac_glyph_layout *glyph_layouts;
     CFIndex glyph_len;
{
  CFIndex used, result = 0;
  CTLineRef ctline = mac_ctfont_create_line_with_string_and_font (string, font);

  if (ctline == NULL)
    return 0;

  used = CTLineGetGlyphCount (ctline);
  if (used <= glyph_len)
    {
      CFArrayRef ctruns = CTLineGetGlyphRuns (ctline);
      CFIndex i, j, k, ctrun_count = CFArrayGetCount (ctruns);
      CFRange comp_range = CFRangeMake (0, 0);
      CGFloat total_advance = 0, comp_offset;

      comp_offset = CTLineGetOffsetForStringIndex (ctline, 0, NULL);
      for (i = j = k = 0; k < ctrun_count; k++)
	{
	  CTRunRef ctrun = CFArrayGetValueAtIndex (ctruns, k);
	  CFIndex glyph_count = CTRunGetGlyphCount (ctrun);
	  CFRange range;

	  for (range = CFRangeMake (0, 1); range.location < glyph_count;
	       range.location++, i++)
	    {
	      CFIndex index;
	      CGFloat offset;
	      CGPoint position;

	      CTRunGetStringIndices (ctrun, range, &index);
	      offset = CTLineGetOffsetForStringIndex (ctline, index, NULL);
	      if (offset != comp_offset)
		{
		  CGPoint pos = CGPointMake (offset, 0);

		  /* Glyph indices are not always increasing in a
		     composed character (e.g., the first one of
		     "Hindi" in its native name).  */
		  comp_range.length =
		    (CTLineGetStringIndexForPosition (ctline, pos)
		     - comp_range.location);
		  for (; j < i; j++)
		    glyph_layouts[j].comp_range = comp_range;
		  comp_range.location += comp_range.length;
		  comp_offset = offset;
		}

	      glyph_layouts[i].string_index = index;
	      CTRunGetGlyphs (ctrun, range, &glyph_layouts[i].glyph_id);

	      CTRunGetPositions (ctrun, range, &position);
	      glyph_layouts[i].advance_delta = position.x - total_advance;
	      glyph_layouts[i].baseline_delta = position.y;
	      glyph_layouts[i].advance =
		CTRunGetTypographicBounds (ctrun, range, NULL, NULL, NULL);
	      total_advance += glyph_layouts[i].advance;
	    }
	}
      comp_range.length = CFStringGetLength (string) - comp_range.location;
      for (; j < i; j++)
	glyph_layouts[j].comp_range = comp_range;

      result = used;
    }
  CFRelease (ctline);

  return result;
}

#endif	/* USE_CORE_TEXT */

Lisp_Object
macfont_shape (lgstring)
     Lisp_Object lgstring;
{
  struct font *font;
  struct macfont_info *macfont_info;
  FontRef macfont;
  EMACS_UINT glyph_len, len, i, j;
  CFIndex nonbmp_len;
  UniChar *unichars;
  CFIndex *nonbmp_indices;
  CFStringRef string;
  CFIndex used = 0;
  struct mac_glyph_layout *glyph_layouts;

  CHECK_FONT_GET_OBJECT (LGSTRING_FONT (lgstring), font);
  macfont_info = (struct macfont_info *) font;
  macfont = macfont_info->macfont;

  glyph_len = LGSTRING_GLYPH_LEN (lgstring);
  nonbmp_len = 0;
  for (i = 0; i < glyph_len; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);

      if (NILP (lglyph))
	break;
      if (LGLYPH_CHAR (lglyph) >= 0x10000)
	nonbmp_len++;
    }
  len = i;

  unichars = alloca (sizeof (UniChar) * (len + nonbmp_len));
  nonbmp_indices = alloca (sizeof (CFIndex) * (nonbmp_len + 1));
  for (i = j = 0; i < len; i++)
    {
      UTF32Char c = LGLYPH_CHAR (LGSTRING_GLYPH (lgstring, i));

      if (c < 0x10000)
	unichars[i + j] = c;
      else
	{
	  c -= 0x10000;
	  unichars[i + j] = (c >> 10) + 0xD800;
	  nonbmp_indices[j++] = i;
	  unichars[i + j] = (c & 0x3FF) + 0xDC00;
	}
    }
  nonbmp_indices[j] = len;	/* sentinel */

  BLOCK_INPUT;

  string = CFStringCreateWithCharactersNoCopy (NULL, unichars, len + nonbmp_len,
					       kCFAllocatorNull);
  if (string)
    {
      glyph_layouts = alloca (sizeof (struct mac_glyph_layout) * glyph_len);
      used = mac_font_shape (macfont, string, glyph_layouts, glyph_len);
      CFRelease (string);
    }

  UNBLOCK_INPUT;

  if (used == 0)
    return Qnil;

  BLOCK_INPUT;

  for (i = 0; i < used; i++)
    {
      Lisp_Object lglyph = LGSTRING_GLYPH (lgstring, i);
      struct mac_glyph_layout *gl = glyph_layouts + i;
      EMACS_UINT from, to;
      struct font_metrics metrics;
      int xoff, yoff, wadjust;

      if (NILP (lglyph))
	{
	  lglyph = Fmake_vector (make_number (LGLYPH_SIZE), Qnil);
	  LGSTRING_SET_GLYPH (lgstring, i, lglyph);
	}

      from = gl->comp_range.location;
      /* Convert UTF-16 index to UTF-32.  */
      for (j = 0; nonbmp_indices[j] < from; j++)
	from--;
      LGLYPH_SET_FROM (lglyph, from);

      to = from + gl->comp_range.length;
      /* Convert UTF-16 index to UTF-32.  */
      for (; nonbmp_indices[j] < to; j++)
	to--;
      LGLYPH_SET_TO (lglyph, to - 1);

      if (unichars[gl->string_index] >= 0xD800
	  && unichars[gl->string_index] < 0xDC00)
	LGLYPH_SET_CHAR (lglyph, (((unichars[gl->string_index] - 0xD800) << 10)
				  + (unichars[gl->string_index + 1] - 0xDC00)
				  + 0x10000));
      else
	LGLYPH_SET_CHAR (lglyph, unichars[gl->string_index]);

      LGLYPH_SET_CODE (lglyph, gl->glyph_id);

      macfont_glyph_extents (font, gl->glyph_id, &metrics, NULL);
      LGLYPH_SET_WIDTH (lglyph, metrics.width);
      LGLYPH_SET_LBEARING (lglyph, metrics.lbearing);
      LGLYPH_SET_RBEARING (lglyph, metrics.rbearing);
      LGLYPH_SET_ASCENT (lglyph, metrics.ascent);
      LGLYPH_SET_DESCENT (lglyph, metrics.descent);

      xoff = lround (gl->advance_delta);
      yoff = lround (- gl->baseline_delta);
      wadjust = lround (gl->advance);
      if (xoff != 0 || yoff != 0 || wadjust != metrics.width)
	{
	  Lisp_Object vec;

	  vec = Fmake_vector (make_number (3), Qnil);
	  ASET (vec, 0, make_number (xoff));
	  ASET (vec, 1, make_number (yoff));
	  ASET (vec, 2, make_number (wadjust));
	  LGLYPH_SET_ADJUSTMENT (lglyph, vec);
	}
    }

  UNBLOCK_INPUT;

  return make_number (used);
}

#if MAC_OS_X_VERSION_MIN_REQUIRED < 1060
static INLINE int
mac_font_family_group (family)
     CFStringRef family;
{
  if (CFStringHasPrefix (family, CFSTR ("#")))
    return 2;
  else
    {
      CFRange range;

      range = CFStringFind (family, CFSTR ("Apple"),
			    kCFCompareCaseInsensitive | kCFCompareAnchored);
      if (range.location != kCFNotFound)
	return 1;

      return 0;
    }
}

CFComparisonResult
mac_font_family_compare (val1, val2, context)
     const void *val1;
     const void *val2;
     void *context;
{
  CFStringRef family1 = (CFStringRef) val1, family2 = (CFStringRef) val2;
  int group1, group2;

  group1 = mac_font_family_group (family1);
  group2 = mac_font_family_group (family2);
  if (group1 < group2)
    return kCFCompareLessThan;
  if (group1 > group2)
    return kCFCompareGreaterThan;
  return CFStringCompare (family1, family2, kCFCompareCaseInsensitive);
}
#endif	/* MAC_OS_X_VERSION_MIN_REQUIRED < 1060 */

void *
macfont_get_nsctfont (font)
     struct font *font;
{
  struct macfont_info *macfont_info = (struct macfont_info *) font;
  FontRef macfont = macfont_info->macfont;

  return (void *) macfont;
}

Lisp_Object
macfont_nsctfont_to_spec (font)
     void *font;
{
  Lisp_Object spec = Qnil;
  FontDescriptorRef desc = mac_nsctfont_copy_font_descriptor (font);

  if (desc)
    {
      spec = font_make_spec ();
      macfont_store_descriptor_attributes (desc, spec);
      CFRelease (desc);
    }

  return spec;
}

void
mac_register_font_driver (f)
     struct frame *f;
{
  if (NILP (macfont_driver.type))
    {
      UInt32 response;
      OSErr err;

      BLOCK_INPUT;
      err = Gestalt (gestaltSystemVersion, &response);
      UNBLOCK_INPUT;
      if (err == noErr)
	{
#if USE_CORE_TEXT
	  if (response >= 0x1050)
	    macfont_driver.type = Qmac_ct;
	  else
#endif
#if USE_NS_FONT_DESCRIPTOR
	  if (response >= 0x1040)
	    macfont_driver.type = Qmac_fd;
	  else
#endif
#if USE_NS_FONT_MANAGER
	  if (response >= 0x1020)
	    macfont_driver.type = Qmac_fm;
	  else
#endif
	    abort ();
	}
      else
	abort ();

      macfont_driver_type = macfont_driver.type;
    }

  register_font_driver (&macfont_driver, f);
}


void
syms_of_macfont ()
{
#if USE_CORE_TEXT
  {
    static struct font_driver mac_ctfont_driver;

    DEFSYM (Qmac_ct, "mac-ct");
    mac_ctfont_driver = macfont_driver;
    mac_ctfont_driver.type = Qmac_ct;
    register_font_driver (&mac_ctfont_driver, NULL);
  }
#endif
#if USE_NS_FONT_DESCRIPTOR
  {
    static struct font_driver mac_fdfont_driver;

    DEFSYM (Qmac_fd, "mac-fd");
    mac_fdfont_driver = macfont_driver;
    mac_fdfont_driver.type = Qmac_fd;
    register_font_driver (&mac_fdfont_driver, NULL);
  }
#endif
#if USE_NS_FONT_MANAGER
  {
    static struct font_driver mac_fmfont_driver;

    DEFSYM (Qmac_fm, "mac-fm");
    mac_fmfont_driver = macfont_driver;
    mac_fmfont_driver.type = Qmac_fm;
    register_font_driver (&macfont_driver, NULL);
  }
#endif

  macfont_driver_type = macfont_driver.type = Qnil;
  staticpro (&macfont_driver_type);
}
