#define STB_IMAGE_IMPLEMENTATION
#define STRUSE_IMPLEMENTATION

#include "external/struse/struse.h"
#include "external/stb/stb_image.h"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "help.h"

uint8_t* LoadBinary( const char* name, size_t &size )
{
	FILE *f;
	if( fopen_s( &f, name, "rb" ) == 0 ) {
		fseek( f, 0, SEEK_END );
		size_t sizeAdd = ftell( f );
		fseek( f, 0, SEEK_SET );
		uint8_t* add = ( uint8_t* )calloc( 1, sizeAdd );
		fread( add, sizeAdd, 1, f );
		fclose( f );
		size = sizeAdd;
		return add;
	}
	return nullptr;
}

#define MAX_FONTS 16
#define MAX_STYLES 128
#define MAX_PAGES 64
#define MAX_IMAGES 128

static const char* aFontLabels[ 4 ] = { "FontBitmapLo", "FontBitmapHi", "FontWidthLo", "FontWidthHi" };

Image aImages[ MAX_IMAGES ];
Font aFonts[ MAX_FONTS ];
Style aStyles[ MAX_STYLES ];
Page aPages[ MAX_PAGES ];

int numImages = 0;
int numFonts = 0;
int numStyles = 0;
int numPages = 0;
int bytesCompressed = 0;

// REPLACE THIS IF CREATING ART USING A DIFFERENT PALETTE
static const uint8_t palette[ 16 ][ 3 ] = {
	{ 0, 0, 0 },		// #000000
	{ 255, 255, 255 },	// #FFFFFF
	{ 137, 64, 54 },	// #880000
	{ 122, 191, 199 },	// #AAFFEE
	{ 138, 70, 174 },	// #CC44CC
	{ 104, 169, 65 },	// #00CC55
	{ 62, 49, 162 },	// #0000AA
	{ 208, 220, 113 },	// #EEEE77
	{ 144, 95, 37 },	// #DD8855
	{ 92, 71, 0 },		// #664400
	{ 187, 119, 109 },	// #FF7777
	{ 85, 85, 85 },		// #555555
	{ 128, 128, 128 },	// #808080
	{ 172, 234, 136 },	// #AAFF66
	{ 124, 112, 218 },	// #0088FF
	{ 171, 171, 171 }	// #ABABAB
};

#define MAX_CHARS 256
#define abs8(a) ( (a)&0x80 ? (0x100-a) : (a) )
bool ConvertFont( Font* font, strref path )
{
	int x, y, n;
	strown<_MAX_PATH> filename;
	filename.append(path).append(font->FontFile);
	uint8_t *data = stbi_load(filename.c_str(), &x, &y, &n, 0 );

	if( !data ) {
		printf( "could not load " STRREF_FMT "\n", STRREF_ARG(font->FontFile) );
		return false;
	}

	uint8_t *out = ( uint8_t* )malloc( 8 * 256 ), *o = out;
	uint8_t widths[ MAX_CHARS ];
	uint8_t *lp = data + x * y * n - n;


	uint8_t clrCol[ 3 ] = { lp[ 0 ], lp[ 1 ], lp[ 2 ] };
	bool setValue = ( uint16_t( data[ x*y*n - 3 ] ) + uint16_t( data[ x*y*n - 3 ] ) + uint16_t( data[ x*y*n - 3 ] ) ) < ( 128 * 3 );

	int count = 0;

	bool wasEmpty = false;
	int index = 0;
	int bw = x / 8;
	int bh = y / 8;
	while( !wasEmpty && index < MAX_CHARS && ( !count || index < count ) ) {
		uint8_t* block = data + ( index / bw ) * ( 8 * n * x ) + ( index % bw ) * ( 8 * n );
		uint8_t chr[ 8 ] = {};
		uint8_t wid = 0, hgt = 0;
		for( uint8_t py = 0; py < 8; ++py ) {
			for( uint8_t px = 0; px < 8; ++px ) {
				uint16_t d = uint16_t( abs8( block[ 0 ] - clrCol[ 0 ] ) ) + uint16_t( abs8( block[ 1 ] - clrCol[ 1 ] ) ) + uint16_t( abs8( block[ 2 ] - clrCol[ 2 ] ) );
				bool set = d > 16;
				if( set && px >= wid ) { wid = px + 1; }
				if( set && py >= hgt ) { hgt = py + 1; }
				if( set ) { chr[ py ] |= 1 << ( ( ~px ) & 7 ); }
				block += n;
			}
			block += n * ( x - 8 );
		}
		if( ( wid > 0 && hgt > 0 ) || count ) {
			wasEmpty = false;
			widths[ index++ ] = wid;
			for( int ch = 0; ch<8; ++ch ) { *o++ = chr[ ch ]; }
		}
		else { break; }
	}
	strown<_MAX_PATH> outBin(font->BinFile);
	outBin.append(".bin");
	FILE* f;
	if( fopen_s( &f, outBin.c_str(), "wb" ) == 0 ) {
		fwrite( out, o - out, 1, f );
		fclose( f );
	}
	outBin.copy(font->BinFile);
	outBin.append(".wid");
	if( fopen_s( &f, outBin.c_str(), "wb" ) == 0 ) {
		fwrite( widths, index, 1, f );
		fclose( f );
	}

	free(out);

	return true;
}

struct sMemChunk {
	int16_t start, end;
	sMemChunk() {}
	sMemChunk( uint16_t s, uint16_t e ) : start( s ), end( e ) {}
};

struct sMergeChunk
{
	uint16_t target;
	uint8_t length;
	uint8_t offset;
	sMergeChunk() {}
	sMergeChunk( uint16_t t, uint8_t l, uint8_t o ) : target(t), length(l), offset(o) {}
};

uint8_t* TryCompress( uint8_t* data, uint16_t size, uint16_t* compressed )
{
	// back to front decode
	// 1. byte length
	// 2. length >= $80
	//	2.1 copy (length&$7f) bytes from next byte
	// 2. Length < $80
	//  2.1 byte offset
	//	2.2 copy length bytes from offset byte + 1
	// 3. next byte 0 => done

	// encode
	// find largest copy, mark duplicate bytes are taken

	std::vector< sMergeChunk > copies;
	std::vector< sMemChunk > chunks;
	chunks.reserve( 256 );
	chunks.push_back( sMemChunk( 0, size ) );

	bool dupe = true;
	while( dupe ) {
		dupe = false;
		sMemChunk best(0, 0);
		int16_t bestSrc = 0;
		int16_t bestLen = 0;
		size_t bestChunk = 0;
		for( size_t c = 0, n = chunks.size(); c < n && bestLen < 127; ++c ) {
			int16_t start = chunks[c].start;
			int16_t end = chunks[c].end;
			// can only read 256 bytes ahead (offset+1)
			for( int16_t b = end - 2; b >= start && bestLen < 127; --b ) {
				int16_t match = b + 255;
				if( match >( int16_t )size ) { match = size - 1; }
				int16_t left = size - match;
				if( left > ( end - b ) ) { left = end - b; }
				for( int16_t d = match; d > b; --d ) {
					int16_t count = 0;
					for( int16_t e = 0; e < left; ++e ) {
						if( data[ b + e ] != data[ d + e ] ) { break; }
						++count;
					}
					if( count > bestLen ) {
						bestLen = count;
						best.start = b;
						best.end = b + count;
						bestChunk = c;
						bestSrc = d;
					}
					++left;
					if( left > ( end - b ) ) { left = end - b; }
					if( bestLen >= 127 ) { break; }
				}
			}
		}
		if( bestLen > 2 ) {
			// split up chunks
			// - if entire chunk delete it
			// - if best>start && best<end chunk end=start, insert new chunk best-end before
			// - if at end of chunk update end
			// - if at start of chunk update start
			dupe = true;
			sMemChunk& curr = chunks[bestChunk];
			if( best.start == curr.start && best.end == curr.end ) {
				chunks.erase( chunks.begin() + bestChunk );
			} else if( best.start == curr.start ) {
				curr.start = best.end;
			} else if( best.end == curr.end ) {
				curr.end = best.start;
			} else {
				sMemChunk was = curr;
				curr.end = best.start;
				chunks.insert( chunks.begin() + bestChunk, sMemChunk( best.end, was.end ) );
			}
			bool inserted = false;
			for( size_t i = 0, n = copies.size(); i < n; ++i ) {
				if( copies[ i ].target > best.start ) {
					copies.insert( copies.begin() + i, sMergeChunk(best.start, (uint8_t)bestLen, ( uint8_t )( bestSrc - best.start) ) );
					inserted = true;
					break;
				}
			}
			if( !inserted ) {
				copies.push_back( sMergeChunk( best.start, ( uint8_t )bestLen, ( uint8_t )( bestSrc - best.start ) ) );
			}
		}
	}
	size_t compSize = 2 + copies.size() * 2 + 1; // 2 bytes for size, 2 bytes for each copy + 1 terminator byte
	for( size_t o = 0; o < chunks.size(); ++o ) {
		sMemChunk &c = chunks[o];
		compSize += 1 + c.end - c.start; // 1 byte for each orig + orig data
	}

	uint8_t* compd = (uint8_t*)malloc( compSize ), *co = compd;
	// size
	*co++ = uint8_t(size&0xff);
	*co++ = uint8_t(size>>8);
	// insert copies and origs as needed
	uint16_t addr = size;
	for( size_t c = 0, n = copies.size(); c < n; ++c ) {
		sMergeChunk* copy = &copies[copies.size()-1-c];
		uint16_t end = copy->target + copy->length;
		while( addr > end ) {
			uint8_t len = addr - end;
			uint8_t run = len < 128 ? len : 127;
			*co++ = run;
			addr -= run;
			uint16_t src = addr + run;
			for( uint16_t og = 0; og < run; ++og ) {
				*co++ = data[--src];	// stored backwards to make decomp easier
			}
		}
		assert( copy->target == ( addr - copy->length ));
		*co++ = -copy->length;
		*co++ = copy->offset;
		addr -= copy->length;
	}
	while( addr > 0 ) {
		uint16_t len = addr;
		uint8_t run = len < 128 ? uint8_t(len) : 127;
		*co++ = run;
		addr -= run;
		uint16_t src = addr + run;
		for( uint16_t og = 0; og < run; ++og ) {
			*co++ = data[ --src ];	// stored backwards to make decomp easier
		}
	}
	*co++ = 0; // termin8r
	*compressed = (uint16_t)compSize;

	// double check
	co = compd;
	addr = uint16_t(*co) + (uint16_t(co[1])<<8); co += 2;
	assert( addr == size );
	while( *co ) {
		assert( size_t(co-compd) < compSize );
		int8_t len = *co++;
		if( len > 0 ) {
			addr -= len;
			for( int c = 0; c < len; ++c ) {
				if( co[c] != data[ addr + len - 1 - c ] ) {
					assert( false );
				}
			}
			co += len;
		} else {
			len = -len;
			addr -= len;
			uint8_t offs = *co++;
			uint16_t dupe = addr + offs;
			for( int c = 0; c < len; ++c ) {
				assert( data[addr + c] == data[ dupe + c ] );
			}
		}
	}
	assert( addr == 0 );
	assert( (co-compd) == (compSize-1));

	return compd;
}

bool ConvertImage( Image* img, strref path )
{
	int x, y, n;

	strown<_MAX_PATH> filename;
	filename.append(path).append(img->ImageFile);
	uint8_t *data = stbi_load(filename.c_str(), &x, &y, &n, 0 );

	if( !data ) {
		printf( "could not load " STRREF_FMT "\n", STRREF_ARG( img->ImageFile ) );
		return false;
	}

	uint8_t* raw = (uint8_t*)malloc( x * y ), *dst = raw;
	uint8_t* src = data;
	for( int p = 0, n = x*y; p < n; ++p ) {
		uint8_t col[ 3 ] = { src[ 0 ], src[ 1 ], src[ 2 ] };
		int minOffs = 1 << 29;
		uint8_t b = 0xff;
		for( int c = 0; c < 16; c++ ) {
			int d =
				abs( int( col[ 0 ] ) - int( palette[ c ][ 0 ] ) ) +
				abs( int( col[ 1 ] ) - int( palette[ c ][ 1 ] ) ) +
				abs( int( col[ 2 ] ) - int( palette[ c ][ 2 ] ) );
			if( d < minOffs ) {
				minOffs = d;
				b = c;
			}
		}
		*dst++ = b;
		src += 3;
	}

	int xc = x / 8;
	int yc = y / 8;

	uint8_t* spr = nullptr;

	// extract sprites and remove their footprint
	if( img->numSprites ) {
		spr = (uint8_t*)calloc( img->numSprites, 64 );
		uint8_t* csp = spr;
		for( int s = 0, ns = ( int )img->numSprites; s < ns; ++s ) {
			uint32_t c = img->sprites[s].col;
			uint32_t xpos = img->sprites[s].x;
			uint32_t ypos = img->sprites[s].y;

			int32_t x0 = xpos/8;
			int32_t x1 = (xpos+23)/8;
			int32_t y0 = ypos / 8;
			int32_t y1 = ( ypos + 20 ) / 8;
			if( x1 > xc ) { x1 = xc; }
			if( y1 > yc ) { y1 = yc; }
			for( int32_t y = y0; y <= y1; ++y ) {
				for( int32_t x = x0; x <= x1; ++x ) {
					// find a color that is different in char
					uint8_t* rc = raw + y * 8 * xc * 8 + x * 8;
					uint8_t sc = 0;
					for( int32_t yt = 0; yt < 8; ++yt ) {
						for( int32_t xt = 0; xt < 8; ++xt ) {
							if( *rc != c ) {
								sc = *rc;
								yt = 8;
								break;
							}
							++rc;
						}
						rc += xc*8 - 8;
					}
					rc = raw + y * 8 * xc * 8 + x * 8;
					for( int32_t yt = 0; yt < 8; ++yt ) {
						for( int32_t xt = 0; xt < 8; ++xt ) {
							if( *rc == c ) {
								int32_t px = x * 8 + xt - xpos;
								int32_t py = y * 8 + yt - ypos;
								if( px >= 0 && py >= 0 && px < 24 && py < 21 ) {
									*rc = sc;
									csp[py*3 + px/8] |= 1<<(7-(px&7) );
								}
							}
							++rc;
						}
						rc += xc * 8 - 8;
					}
				}
			}
			csp += 64;
		}
	}

	img->width = xc * 8;
	img->height = yc * 8;
	img->bitmap = (uint8_t*)malloc(xc * yc * 8);
	img->screen = (uint8_t*)malloc( xc * yc );
	uint8_t* bm = img->bitmap;
	uint8_t* sc = img->screen;
	for( int yt = 0; yt < yc; ++yt ) {
		for( int xt = 0; xt < xc; ++xt ) {
			uint8_t* sr = raw + yt * 8 * x + xt * 8;
			uint8_t c[ 2 ] = {}, nc = 0;
			for( int yp = 0; yp < 8; ++yp ) {
				uint8_t b = 0;
				for( int xp = 0; xp < 8; ++xp ) {
					if( !nc ) { c[ 0 ] = *sr; ++nc; }
					else if( *sr != c[0] ) { c[ 1 ] = *sr; }
					b <<= 1;
					b |= (*sr == c[ 0 ]) ? 0 : 1;
					++sr;
				}
				sr += x - 8;
				*bm++ = b;
			}
			// try to keep color consistent between char edges
			bool reverse = false;
			if (xt) {
				reverse = c[0] == (sc[-1] >> 4) || c[1] == (sc[-1] & 0xf);
			} else if( yt ) {
				reverse = c[0] == (sc[-xc] >> 4) || c[1] == (sc[-xc] & 0xf);
			}
			if (reverse) {
				for (int r = 0; r < 8; ++r) { bm[r - 8] = ~bm[r - 8]; }
				uint8_t t = c[0]; c[0] = c[1]; c[1] = t;
			}

			*sc++ = (c[ 0 ] & 0xf) | (c[ 1 ] << 4);
		}
	}
	free( data );
	free( raw );



	strown<_MAX_PATH> binFile(path);
	binFile.append( img->ImageFile.after_last_or_full( '/', '\\' ).before_or_full( '.' ) ).append( ".bimp" );
	img->bitmapOutFile = _strdup( binFile.c_str() );
	FILE* f;
	if( fopen_s( &f, binFile.c_str(), "wb" ) == 0 ) {
		uint16_t compSize;
		uint8_t* screenComp = TryCompress( img->bitmap, xc*yc * 8, &compSize );
		if( compSize < ( xc * yc * 8 ) ) {
			img->compBitmap = true;
			fwrite( screenComp, compSize, 1, f );
			printf( "Image " STRREF_FMT " bitmap compression saved 0x%x bytes\n", STRREF_ARG( img->ImageName ), xc*yc*8 - compSize);
			bytesCompressed += xc*yc*8 - compSize;
		} else {
			fwrite( img->bitmap, xc * yc * 8, 1, f );
		}
		free( screenComp );
		fclose( f );
	}
	binFile.copy(path);
	binFile.append( img->ImageFile.after_last_or_full( '/', '\\' ).before_or_full( '.' ) ).append( ".scrn" );
	img->screenOutFile = _strdup( binFile.c_str() );
	if( fopen_s( &f, binFile.c_str(), "wb" ) == 0 ) {
		uint16_t compSize;
		uint8_t* screenComp = TryCompress( img->screen, xc*yc, &compSize );
		if( compSize < ( xc * yc ) ) {
			img->compScreen = true;
			fwrite( screenComp, compSize, 1, f );
			printf( "Image " STRREF_FMT " screen compression saved 0x%x bytes\n", STRREF_ARG( img->ImageName ), xc*yc - compSize );
			bytesCompressed += xc*yc - compSize;
		} else {
			fwrite( img->screen, xc * yc, 1, f );
		}
		free( screenComp );
		fclose( f );
	}
	if( spr ) {
		binFile.copy(path);
		binFile.append( img->ImageFile.after_last_or_full( '/', '\\' ).before_or_full( '.' ) ).append( ".sprit" );
		img->spriteOutFile = _strdup( binFile.c_str() );
		if( fopen_s( &f, binFile.c_str(), "wb" ) == 0 ) {
			uint16_t compSize;
			uint8_t* spriteComp = TryCompress( spr, img->numSprites * 64, &compSize );
			if( compSize < ( img->numSprites * 64 ) ) {
				img->compSprites = true;
				fwrite( spriteComp, compSize, 1, f );
				printf( "Image " STRREF_FMT " sprite compression saved 0x%x bytes\n", STRREF_ARG( img->ImageName ), img->numSprites * 64 - compSize );
				bytesCompressed += img->numSprites * 64 - compSize;
			} else {
				fwrite( spr, img->numSprites * 64, 1, f );
			}
			free( spriteComp );
			fclose( f );
		}
	}
	return true;
}

bool ParseFont( Font* font, strref fontInfo )
{
	bool success = true;
	while( fontInfo ) {
		fontInfo.skip_whitespace();
		if( fontInfo[ 0 ] == '/' && fontInfo[ 1 ] == '/' ) { fontInfo.line(); }
		else if( strref label = fontInfo.split_label() ) {
			fontInfo.skip_whitespace();
			if( label.same_str( "File" ) ) {
				if( fontInfo.get_first() == '"' ) {
					font->FontFile = fontInfo.skip_quote_xml();
				} else { success = false; }
			} else if( label.same_str( "Bin" ) ) {
				if( fontInfo.get_first() == '"' ) {
					font->BinFile = fontInfo.skip_quote_xml();
				} else { success = false; }
			} else if( label.same_str( "Order" ) ) {
				if( fontInfo.get_first() == '"' ) {
					font->FontOrder = fontInfo.skip_quote_xml();
				} else { success = false; }
			}
		} else { break; }
	}
	return success;
}

bool ParseComplexImage( Image* img, strref imageInfo )
{
	bool success = true;
	while( imageInfo ) {
		imageInfo.skip_whitespace();
		if( imageInfo.get_first() == '"' ) {
			img->ImageFile = imageInfo.skip_quote_xml();
		} else {
			strref label = imageInfo.split_label();
			if( label.same_str( "sprite" ) ) {
				imageInfo.skip_whitespace();
				if( img->numSprites < 8 ) {
					ImageSprite* imgSpr = &img->sprites[ img->numSprites++ ];
					imgSpr->col = imageInfo.atoi_skip();
					imageInfo.grab_char(',');
					imageInfo.skip_whitespace();
					imgSpr->x = imageInfo.atoi_skip();
					imageInfo.grab_char( ',' );
					imageInfo.skip_whitespace();
					imgSpr->y = imageInfo.atoi_skip();
					imageInfo.skip_whitespace();
				} else { success = false; break; }
			} else if( label.valid() ) {
				success = false;
				break;
			}
		}
	}
	return success;
}


bool ParsePageImage( PageImage* img, strref imageInfo )
{
	// must contain with x, y, image name
	imageInfo.skip_whitespace();
	img->posX = imageInfo.atoi_skip();
	imageInfo.skip_whitespace();
	if( imageInfo.get_first() == ',' ) { ++imageInfo; imageInfo.skip_whitespace(); }
	img->posY = imageInfo.atoi_skip();
	imageInfo.skip_whitespace();
	if( imageInfo.get_first() == ',' ) { ++imageInfo; imageInfo.skip_whitespace(); }
	if( imageInfo ) {
		imageInfo.trim_whitespace();
		for( int i=0; i<numImages; ++i ) {
			if( imageInfo.same_str( aImages[ i ].ImageName ) ) {
				img->imageIdx = i;
				return true;
			}
		}
	}

	return false;
}

bool ParseParagraph( Paragraph* para, int lastFont, strref text )
{
	bool success = true;
	para->text = text;
	para->font = lastFont;
	strref fontOrder = aFonts[ lastFont ].FontOrder;
	int len = 0;
	while( text && len < 255 ) {
		size_t c = text.pop_utf8();
		if( c == ' ' ) { ++len; }
		else {
			strref fo = fontOrder;
			while( fo ) {
				size_t o = fo.pop_utf8();
				if ( o == '\\' ) {
					o = fo.pop_utf8(); }
				if( o == c ) {
					++len;
					break;
				}
			}
		}
	}
	para->length = len;
	return success;
}

bool ParseBlock( Block* block, strref blockInfo )
{
	bool success = true;
	int currFont = 0;
	// must start with x, y, width, height
	blockInfo.skip_whitespace();
	block->posX = blockInfo.atoi_skip();
	blockInfo.skip_whitespace();
	if( blockInfo.get_first() == ',' ) { ++blockInfo; blockInfo.skip_whitespace(); }
	block->posY = blockInfo.atoi_skip();
	blockInfo.skip_whitespace();
	if( blockInfo.get_first() == ',' ) { ++blockInfo; blockInfo.skip_whitespace(); }
	block->width = blockInfo.atoi_skip();
	blockInfo.skip_whitespace();
	if( blockInfo.get_first() == ',' ) { ++blockInfo; blockInfo.skip_whitespace(); }
	block->height = blockInfo.atoi_skip();
	blockInfo.skip_whitespace();
	if( blockInfo.get_first() == ',' ) { ++blockInfo; blockInfo.skip_whitespace(); }

	while( blockInfo && success ) {
		blockInfo.skip_whitespace();
		strref label = blockInfo.split_label();
		blockInfo.skip_whitespace();
		if( label.same_str( "color" ) ) {
			SetColor* col = new SetColor();
			col->background = blockInfo.atoi_skip();
			blockInfo.skip_whitespace();
			col->color = blockInfo.atoi_skip();
			block->data.push_back( col );
		} else if( label.same_str( "divider" ) ) {
			Divider* div = new Divider();
			block->data.push_back( div );
		} else if( label.same_str( "Style" ) ) {
			SetStyle* style = new SetStyle();
			if( strref styleName = blockInfo.split_label() ) {
				for( int s = 0; s < numStyles; ++s ) {
					if( aStyles[ s ].name.same_str( styleName ) ) {
						style->style = s;
						currFont = aStyles[s].font;
						break;
					}
				}
			}
			strref AlignName = blockInfo.split_label();
			if( AlignName.same_str( "Left" ) ) { style->align = -1; }
			else if( AlignName.same_str( "Center" ) || AlignName.same_str( "Centre" ) ) { style->align = 0; }
			else if( AlignName.same_str( "Right" ) ) { style->align = 1; }
			block->data.push_back( style );
		} else if( label.same_str( "Paragraph" ) ) {
			if( blockInfo.get_first() == '"' ) {
				Paragraph* para = new Paragraph();
				success = ParseParagraph( para, currFont, blockInfo.skip_quote_xml() );
				block->data.push_back( para );
			} else { success = false; }
		} else { success = false; }
	}
	return success;
}

bool ParsePage( strref pageInfo )
{
	bool success = true;
	Page& page = aPages[ numPages-1 ];
	while( pageInfo )
	{
		pageInfo.skip_whitespace();
		if( pageInfo[0] == '/' && pageInfo[1] == '/' ) { pageInfo.line(); }
		else {
			strref label = pageInfo.split_label();
			label.trim_whitespace();
			pageInfo.skip_whitespace();
			if( label.same_str( "Image" ) ) {
				if( pageInfo.get_first() == '{' ) {
					PageImage* img = new PageImage();
					page.data.push_back(img);
					success = ParsePageImage( img, pageInfo.scoped_block_utf8_comment_skip() );
				} else { success = false; }
			} else if( label.same_str( "Block") ) {
				if( pageInfo.get_first() == '{' ) {
					Block* block = new Block();
					page.data.push_back( block );
					success = ParseBlock( block, pageInfo.scoped_block_utf8_comment_skip() );
				}
				else { success = false; }
			} else { success = false; }
		}
	}
	return true;
}

bool ParseStyle( strref page )
{
	Style &style = aStyles[ numStyles - 1 ];
	while( strref label = page.split_label() ) {
		label.trim_whitespace(); page.skip_whitespace();
		if( label.same_str( "font" ) ) {
			strref font = page.split_label();
			for( int f = 0; f < numFonts; ++f ) {
				if( font.same_str( aFonts[ f ].FontName ) ) {
					style.font = f;
					break;
				}
			}
		} else if( label.same_str("lineSpace") ) {
			style.lineSpace = page.atoi_skip();
		} else if( label.same_str("spaceWidth") ) {
			style.spaceWidth = page.atoi_skip();
		} else if( label.same_str("charSpacing") ) {
			style.charSpacing = page.atoi_skip();
		} else {
			return false;
		}
		page.skip_whitespace();
	}
	return true;
}

int main( int argc, char* argv[] )
{
	if( argc < 2 ) {
		printf("Usage:\nHelpConv helpscript.txt\n" );
		return 0;
	}

	strref filepath(argv[1]);
	int pathEnd = filepath.find_last('/', '\\');
	strref path = filepath.get_substr(0, pathEnd + 1);;
	strref file = filepath.get_substr(pathEnd+1, filepath.get_len()-pathEnd-1).before_or_full('.');

	size_t size;
	if( uint8_t* data = LoadBinary( argv[ 1 ], size ) ) {
		strref file((const char*)data, strl_t(size)), prev = file;

		if( data[ 0 ] == 0xef && data[ 1 ] == 0xbb && data[ 2 ] == 0xbf ) { file += 3; }
		while( file ) {
			strref prev = file;
			file.skip_whitespace();
			if( file.get_first() == '/' && file[ 1 ] == '/' ) { file.line(); }
			else {
				strref label = file.split_label();
				label.trim_whitespace();
				if( label.same_str( "Image" ) ) {
					assert( numImages < MAX_FONTS );
					aImages[ numImages ].ImageName = file.split_label();
					if( file.get_first() == '"' ) {
						aImages[ numImages ].ImageFile = file.skip_quote_xml();
					} else if( file.get_first() == '{' ) {
						ParseComplexImage( &aImages[ numImages ], file.scoped_block_utf8_comment_skip() );
					}
					++numImages;
				}
				else if( label.same_str( "Font" ) ) {
					assert( numFonts < MAX_FONTS );
					aFonts[ numFonts ].FontName = file.split_label();
					file.skip_whitespace();
					if( file.get_first() == '{' ) {
						ParseFont( &aFonts[numFonts], file.scoped_block_utf8_comment_skip() );
					}
					++numFonts;
				}
				else if( label.same_str( "Style" ) ) {
					assert( numStyles < MAX_STYLES );
					aStyles[ numStyles ].name = file.split_label();
					++numStyles;
					file.skip_whitespace();
					if( file.get_first() == '{' ) {
						ParseStyle(file.scoped_block_utf8_comment_skip());
					}
				}
				else if( label.same_str( "Page" ) ) {
					aPages[ numPages ].PageName = file.split_label();
					++numPages;
					file.skip_whitespace();
					if( file.get_first() == '{' ) {
						ParsePage( file.scoped_block_utf8_comment_skip() );
					}
				}
			}
		}
	}

	for( int f = 0; f < numFonts; ++f ) {
		ConvertFont( &aFonts[ f ], path );
	}

	for( int i = 0; i < numImages; ++i ) {
		ConvertImage( &aImages[ i ], path );
	}

	char* srcRaw = (char*)malloc( 1024 * 1024 );
	strovl src( srcRaw, 1024 * 1024 );
	src.append( "PageCount:\n\tdc.b " ).append_num( numPages, 0, 10 ).append( "\n" );
	src.append( "PagesLo:\n\tdc.b " );
	for( int p = 0; p < numPages; ++p ) {
		if( p ) { src.append( ", " ); }
		src.append( "<" ).append( aPages[ p ].PageName );
	}
	src.append( "\n" );

	src.append( "PagesHi:\n\tdc.b " );
	for( int p = 0; p < numPages; ++p ) {
		if( p ) { src.append( ", " ); }
		src.append( ">" ).append( aPages[ p ].PageName );
	}
	src.append( "\n\n" );


	for( int ft = 0; ft < 4; ++ft ) {
		src.append( aFontLabels[ ft ] ).append( ":\n\tdc.b\t" );
		for( int f = 0; f < numFonts; ++f ) {
			if( f ) { src.append( ", " ); }
			src.append( (ft & 1) ? '>' : '<' );
			src.append( (ft & 2) ? "FontWidth_" : "FontBitmap_" );
			src.append( aFonts[ f ].FontName );
		}
		src.append( "\n\n" );
	}

	src.append( "\nStylesLo:\n\tdc.b " );
	for( int s = 0; s < numStyles; ++s ) {
		if( s ) { src.append( ", " ); }
		src.append( "<" ).append( "Style_" ).append( aStyles[ s ].name );
	}
	src.append( "\nStylesHi:\n\tdc.b " );
	for( int s = 0; s < numStyles; ++s ) {
		if( s ) { src.append( ", " ); }
		src.append( ">" ).append( "Style_" ).append( aStyles[ s ].name );
	}
	src.append( "\n\n" );
	for( int s = 0; s < numStyles; ++s ) {
		strref fontName = numFonts > aStyles[ s ].font ? aFonts[ aStyles[ s ].font ].FontName : strref( "<no font>" );
		src.append( "Style_" ).append( aStyles[ s ].name ).append(":\n");
		src.append( "\tdc.b " ).append_num( aStyles[ s ].font, 0, 10 );
		src.append( ", " ).append_num( aStyles[ s ].lineSpace, 0, 10 );
		src.append( ", " ).append_num( aStyles[ s ].spaceWidth, 0, 10 );
		src.append( ", " ).append_num( aStyles[ s ].charSpacing, 0, 10 );
		src.append( "\n\n" );
	}

	int paragraphIndex = 0;

	for( int p = 0; p < numPages; ++p ) {
		src.append( aPages[ p ].PageName ).append( ":\n" );
		Page& page = aPages[ p ];
		for( size_t i = 0; i < page.data.size(); ++i ) {
			PageData* data = page.data[ i ];
			src.append( "\tdc.b PageData." );
			switch( data->type ) {
				case PageData::Page_Image: {
					PageImage* img = (PageImage*)data;
					Image* raw = &aImages[ img->imageIdx ];
					strref imgName = img->imageIdx < numImages ? aImages[ img->imageIdx ].ImageName : "<no img>";
					src.append( "Image\n" );
					src.append( "\tdc.b $").append_num( (raw->compBitmap ? 1 : 0 ) | (raw->compScreen ? 2 : 0), 2, 16 ).append( "\n" );
					src.append( "\tdc.b <Bitmap_" ).append( imgName ).append( ", >Bitmap_" ).append( imgName ).append( "\n" );
					src.append( "\tdc.b <Screen_" ).append( imgName ).append( ", >Screen_" ).append( imgName ).append( "\n" );
					src.sprintf_append( "\tdc.b %d, %d, %d\n", img->posX & 0xff, img->posX>>8, img->posY );
					src.sprintf_append( "\tdc.b %d, %d\n\n", aImages[ img->imageIdx ].width, aImages[ img->imageIdx ].height );
					if( aImages[ img->imageIdx ].numSprites ) {
						int num = aImages[ img->imageIdx ].numSprites;
						ImageSprite* spr = aImages[ img->imageIdx ].sprites;
						src.append( "\tdc.b PageData.Sprites\n\tdc.b <Sprite_" ).append( imgName ).append( ", >Sprite_" ).append( imgName ).append( "\n" );
						src.append( "\tdc.b $").append( raw->compSprites ? "01\n" : "00\n" );
						src.append( "\tdc.b " ).append_num( num, 0, 10 ).append( "\n" );
						int hiMask = 0;
						for( int s = 0; s < num; ++s ) {
							if( (spr[ s ].x + img->posX + 24) >= 256 ) { hiMask |= 1 << s; }
						}
						src.append( "\tdc.b " ).append_num( hiMask, 0, 10 ).append( "\n" );
						for( int s = 0; s < num; ++s ) {
							src.sprintf_append( "\tdc.b $%x, $%02x, $%02x\n", spr[s].col, (spr[s].x + img->posX + 24) & 0xff, spr[s].y + img->posY + 50);
						}
						src.append( "\n" );
					}
					break;
				}
				case PageData::Page_Block: {
					src.append( "Block\n" );
					Block* block = (Block*)data;
					src.sprintf_append( "\tdc.b $%02x, $%02x, $%02x, $%02x, $%02x\n",
						block->posX & 0xff, block->posX >> 8, block->posY, block->width, block->height );
					for( size_t b = 0; b < block->data.size(); ++b ) {
						BlockData* bldt = block->data[ b ];
						switch( bldt->type ) {
							case BlockData::Block_Color: {
								SetColor* sec = (SetColor*)bldt;
								src.append( "\tdc.b PageData.Color, $" ).append_num( sec->color*16 + sec->background, 2, 16 ).append( "\n" );
								break;
							}
							case BlockData::Block_Divider: {
								src.append( "\tdc.b PageData.Divider\n" );
								break;
							}
							case BlockData::Block_SetStyle: {
								SetStyle* set = ( SetStyle* )bldt;
								src.sprintf_append( "\tdc.b PageData.Style, %d, %d\n", set->style, set->align );
								break;
							}
							case BlockData::Block_Paragraph: {
								Paragraph* para = (Paragraph*)bldt;
								src.append( "\tdc.b PageData.Paragraph\n" );
								src.sprintf_append( "\tdc.b %d, <Paragraph_%d, >Paragraph_%d\n",
									(uint8_t)para->length, paragraphIndex, paragraphIndex );
								++paragraphIndex;
								break;
							}
						}
					}
					src.append( "\tdc.b PageData.End\n" );
					break;
				}
			}
		}
		src.append( "\tdc.b PageData.End\n\n;--------------------------------------\n\n" );
	}
	//	src.append( "\tdc.b PageData.End\n" );
	src.append( "\n\n" );

	paragraphIndex = 0;
	for( int p = 0; p < numPages; ++p ) {
		Page& page = aPages[ p ];
		for( size_t i = 0; i < page.data.size(); ++i ) {
			PageData* data = page.data[ i ];
			if( data->type == PageData::Page_Block ) {
				Block* block = (Block*)data;
				for( size_t b = 0; b < block->data.size(); ++b ) {
					BlockData* bldt = block->data[ b ];
					if( bldt->type == BlockData::Block_Paragraph ) {
						Paragraph* para = (Paragraph*)bldt;
						src.append( "Paragraph_" ).append_num( paragraphIndex, 0, 10 ).append( ":\n\tdc.b " );
						++paragraphIndex;
						strref fontOrder = aFonts[ para->font ].FontOrder;
						strref text = para->text;
						int len = 0;
						bool fs = true;
						while( text && len < 255 ) {
							size_t c = text.pop_utf8();
							if( c == ' ' ) { src.append( fs ? "$00" : ", $00" ); fs = false; }
							else {
								strref fo = fontOrder;
								int chr = 1;
								while( fo ) {
									size_t o = fo.pop_utf8();
									if( o == '\\' ) { o = fo.pop_utf8(); }
									if( o == c ) {
										if( !fs ) { src.append( ", " ); }
										else { fs = false; }
										src.append( "$" ).append_num( chr, 2, 16 );
										break;
									}
									++chr;
								}
							}
						}
						src.append( "\n\n" );
					}
				}
			}
		}
	}

	for( int f = 0; f < numFonts; ++f ) {
		src.append( "FontBitmap_" ).append( aFonts[ f ].FontName ).append( ":\n" );
		src.append( "\tincbin \"" ).append( aFonts[ f ].FontName ).append( ".bin\"\n" );
		src.append( "FontWidth_" ).append( aFonts[ f ].FontName ).append( ":\n" );
		src.append( "\tincbin \"" ).append( aFonts[ f ].FontName ).append( ".wid\"\n" );
		src.append( "\n\n" );
	}

	for( int i = 0; i < numImages; ++i ) {
		src.append( "Bitmap_" ).append( aImages[ i ].ImageName ).append( ":\n" );
		src.append( "\tincbin \"" ).append( aImages[ i ].bitmapOutFile ).append( "\"\n" );
		src.append( "Screen_" ).append( aImages[ i ].ImageName ).append( ":\n" );
		src.append( "\tincbin \"" ).append( aImages[ i ].screenOutFile ).append( "\"\n" );
		if( aImages[ i ].spriteOutFile ) {
			src.append( "Sprite_" ).append( aImages[ i ].ImageName ).append( ":\n" );
			src.append( "\tincbin \"" ).append( aImages[ i ].spriteOutFile ).append( "\"\n" );
		}
		src.append( "\n\n" );
	}

	FILE *f;
	strown<_MAX_PATH> save_as;
	save_as.append(path).append(file).append(".s");
	if( fopen_s( &f, save_as.c_str(), "w" ) == 0 ) {
		fwrite( src.get(), src.get_len(), 1, f );
		fclose( f );
	}
	printf("Total bytes compressed away: 0x%x\n", bytesCompressed);
 	return 0;
}

