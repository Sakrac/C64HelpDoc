#pragma once
#include <inttypes.h>
#include <vector>

#define HELP_MAX_PATH 128

struct Font {
	strref FontFile;
	strref FontName;
	strref BinFile;
	strref FontOrder;
};

struct Style {
	strref name;
	int font;
	uint8_t lineSpace;
	uint8_t spaceWidth;
	uint8_t charSpacing;
};

struct ImageSprite
{
	uint8_t col, x, y;
};

struct Image {
	enum { MAX_IMAGE_SPRITE = 8 };
	strref ImageFile;
	strref ImageName;
	uint8_t width;
	uint8_t height;
	uint8_t numSprites;
	uint8_t* bitmap;
	uint8_t* screen;

	bool compBitmap;
	bool compScreen;
	bool compSprites;

	ImageSprite sprites[ MAX_IMAGE_SPRITE ];
	char* screenOutFile;
	char* bitmapOutFile;
	char* spriteOutFile;

	Image() : width( 0 ), height( 0 ), numSprites( 0 ), bitmap( nullptr ), screen( nullptr ),
		compBitmap( false ), compScreen( false ), compSprites( false ),
		screenOutFile(0), bitmapOutFile(0), spriteOutFile(0) {}
};

struct BlockData
{
	enum Type {
		Block_Color,
		Block_Background,
		Block_Paragraph,
		Block_Divider,
		Block_SetStyle
	};
	Type type;
};

struct Paragraph : public BlockData
{
	int font;
	int8_t length;
	strref text;
	Paragraph() : font(0), length(0) { type = Block_Paragraph; }
};

struct SetColor : public BlockData
{
	uint8_t color;
	uint8_t background;

	SetColor() : color( 14 ), background( 6 ) { type = Block_Color; }
};

struct SetStyle : public BlockData
{
	int style;
	int8_t align;
	SetStyle() : style( 0 ), align( -1 ) { type = Block_SetStyle; }
};

struct Divider : public BlockData
{
	Divider() { type = Block_Divider; }
};

struct PageData
{
	enum Type {
		Page_Block,
		Page_Image
	};
	Type type;
};

struct Block : public PageData
{
	uint16_t posX;
	uint8_t posY;
	uint8_t width;
	uint8_t height;
	std::vector< BlockData* > data;

	Block() : posX(0), posY(0), width(150), height(200) { type = Page_Block; }
};

struct PageImage : public PageData
{
	int imageIdx;
	uint16_t posX;
	uint8_t posY;
	PageImage() : imageIdx(-1), posX(0), posY(0) { type = Page_Image; }
};


struct Page {
	strref PageName;
	std::vector< PageData* > data;
};