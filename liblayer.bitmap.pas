unit libLayer.Bitmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

type
  TLayerBitmap = class
  private
    FDC: HDC;
    FBitmap: HBitmap;
    FPtr: Pointer;
    FWidth, FHeight: Int32;
  public
    property Ptr: Pointer read FPtr;
    property DC: HDC read FDC;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;

    constructor Create(AWidth, AHeight: Int32);
    destructor Destroy; override;
  end;

implementation

constructor TLayerBitmap.Create(AWidth, AHeight: Int32);
var
  bi: Windows.BITMAPINFO;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FPtr := nil;

  ZeroMemory(@bi, SizeOf(BITMAPINFO));
  bi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  bi.bmiHeader.biWidth := AWidth;
  bi.bmiHeader.biHeight := -AHeight;
  bi.bmiHeader.biPlanes := 1;
  bi.bmiHeader.biBitCount := 32;
  bi.bmiHeader.biCompression := BI_RGB;
  bi.bmiHeader.biSizeImage := 0;
  bi.bmiHeader.biClrUsed := 0;

  FDC := Windows.CreateCompatibleDC(0);
  FBitmap := Windows.CreateDIBSection(FDC, bi, DIB_RGB_COLORS, FPtr, 0, 0);

  SelectObject(FDC, FBitmap);
end;

destructor TLayerBitmap.Destroy;
begin
  FPtr := nil;
  DeleteObject(FBitmap);

  inherited Destroy;
end;

end.

