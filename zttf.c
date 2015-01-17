/*

Based on ttf2png - True Type Font to PNG converter

Copyright (c) 2004-2008 Mikko Rasa

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <getopt.h>
#include <ft2build.h>
#include FT_FREETYPE_H

typedef struct sImage
{
	unsigned w, h;
	char     *data;
} Image;

typedef struct sGlyph
{
	unsigned code;
	Image    image;
	unsigned x, y;
	int      offset_x;
	int      offset_y;
	int      advance;
} Glyph;

typedef struct sFont
{
	unsigned size;
	int      ascent;
	int      descent;
	unsigned n_glyphs;
	Glyph    *glyphs;
	Image    image;
} Font;

unsigned round_to_pot(unsigned);
void usage();
void init_font(Font *, FT_Face, unsigned, unsigned, int);
void render_pixels(Font *, const char *id);
void render_data(Font *, const char *id);

char verbose=0;

int main(int argc, char **argv)
{
	char *fn;
        char *id;
	int  begin=0;
	int  end=127;
	int  size;
	char autohinter=0;

	FT_Library freetype;
	FT_Face    face;

	int  err;
	int  i;

	Font font;

	if(argc != 4)
	{
		usage();
		return 1;
	}

	fn=argv[1];
        size=atol(argv[2]);
        id=argv[3];

	err=FT_Init_FreeType(&freetype);
	if(err)
	{
		fprintf(stderr, "Couldn't initialize FreeType library\n");
		return 1;
	}

	err=FT_New_Face(freetype, fn, 0, &face);
	if(err)
	{
		fprintf(stderr, "Couldn't load font file\n");
		if(err==FT_Err_Unknown_File_Format)
			fprintf(stderr, "Unknown file format\n");
		return 1;
	}

        const char *name=FT_Get_Postscript_Name(face);
        printf(";; font name: %s\n", name);
        printf(";; glyphs:    %ld\n", face->num_glyphs);
        printf(";; size:      %d\n", size);
        printf("\n");

	err=FT_Set_Pixel_Sizes(face, 0, size);
	if(err)
	{
		fprintf(stderr, "Couldn't set size\n");
		return 1;
	}

	font.size=size;
	init_font(&font, face, begin, end, autohinter);
        render_pixels(&font, id);
        render_data(&font, id);

	for(i=0; i<font.n_glyphs; ++i)
		free(font.glyphs[i].image.data);
	free(font.glyphs);

	FT_Done_Face(face);
	FT_Done_FreeType(freetype);

	return 0;
}

void usage()
{
	printf("usage: zttf font.tff pixel-size code-name\n");
}

void init_font(Font *font, FT_Face face, unsigned first, unsigned last, int autohinter)
{
	unsigned i;
	unsigned size=0;

	font->ascent=(face->size->metrics.ascender+63)>>6;
	font->descent=(face->size->metrics.descender+63)>>6;

	font->n_glyphs=0;
	font->glyphs=NULL;
	for(i=first; i<=last; ++i)
	{
		unsigned  n;
		FT_Bitmap *bmp=&face->glyph->bitmap;
		int       x, y;
		int       flags=0;
		Glyph     *glyph;

                if(font->n_glyphs>=size)
		{
			size+=16;
			font->glyphs=(Glyph *)realloc(font->glyphs, size*sizeof(Glyph));
		}

		glyph=&font->glyphs[font->n_glyphs++];
                glyph->code = i;

		n=FT_Get_Char_Index(face, i);
		if(!n) {
                  glyph->image.data = NULL;
                  continue;
                }

		if(autohinter)
			flags|=FT_LOAD_FORCE_AUTOHINT;
		FT_Load_Glyph(face, n, flags);
		FT_Render_Glyph(face->glyph, FT_RENDER_MODE_NORMAL);

		if(bmp->pixel_mode!=FT_PIXEL_MODE_GRAY)
		{
			fprintf(stderr, "Warning: Glyph %u skipped, not grayscale\n", n);
			continue;
		}

		glyph->image.w=bmp->width;
		glyph->image.h=bmp->rows;
		glyph->image.data=(char *)malloc(bmp->width*bmp->rows);
		glyph->offset_x=face->glyph->bitmap_left;
		glyph->offset_y=face->glyph->bitmap_top-bmp->rows;
		glyph->advance=(int)(face->glyph->advance.x+32)/64;

		if(bmp->pitch<0)
		{
			for(y=0; y<bmp->rows; ++y) for(x=0; x<bmp->width; ++x)
				glyph->image.data[x+(glyph->image.h-1-y)*glyph->image.w]=bmp->buffer[x-y*bmp->pitch];
		}
		else
		{
			for(y=0; y<bmp->rows; ++y) for(x=0; x<bmp->width; ++x)
				glyph->image.data[x+y*glyph->image.w]=bmp->buffer[x+y*bmp->pitch];
		}
	}
}

void render_pixels(Font *font, const char *id)
{
	unsigned i;

	for(i=0; i<font->n_glyphs; ++i)
	{
		Glyph    *glyph;
		int      cx, cy;
		unsigned x, y;

		glyph=&font->glyphs[i];

                if (glyph->image.data == NULL)
                  continue;

                printf ("(code %s.%d", id, glyph->code);
                printf ("\n  (1 %d %d %d %d %d 0 0 0)",
                        glyph->image.w,
                        glyph->image.h,
                        glyph->offset_x,
                        font->ascent - glyph->offset_y - glyph->image.h,
                        glyph->advance);
		for(y=0; y<glyph->image.h; ++y)
                  {
                    printf ("\n  (1");
                    for(x=0; x<glyph->image.w; ++x)
                      printf (" 0x%02x",
                              glyph->image.data[x+y*glyph->image.w] & 0xFF);
                    printf (")");
		}
                printf(")\n\n");
	}
}

void render_data(Font *font, const char *id)
{
  unsigned i;

  printf ("(code %s.glyphs", id);
  for(i=0; i<font->n_glyphs; ++i)
    {
      const Glyph *g=&font->glyphs[i];
      if (g->image.data)
        {
          printf ("\n  (8 %s.%d)", id, g->code);
        }
      else
        {
          printf ("\n  (8 0)");
        }
    }
  printf (")\n\n");

  printf ("(code %s\n", id);
  printf ("  (8 %s.glyphs)\n", id);
  printf ("  (1 %d %d))\n", font->ascent, font->descent);
}
