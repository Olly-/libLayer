unit lapeMufasaLayer;

{$mode objfpc}{$H+}
{$macro on}
{$define callconv := {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl{$ELSE}{$ENDIF}{$ENDIF}
                     {$IFDEF LINUX}{$IFDEF CPU32}cdecl{$ELSE}{$ENDIF}{$ENDIF}}

interface

uses
  Classes, SysUtils, MufasaLayer;

type
  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

  TBox = record X1, Y1, X2, Y2: Integer; end;
  PBox = ^TBox;

  lpMufasaLayer = packed record
    Bitmap: Pointer;
    LayerHandle: PtrUInt;
    Width, Height: Integer;
    __Layer: TMufasaLayer;
  end;
  lpPMufasaLayer = ^lpMufasaLayer;

procedure lpMufasaLayer_Init(const Params: PParamArray); callconv;
procedure lpMufasaLayer_Free(const Params: PParamArray); callconv;
procedure lpMufasaLayer_GetBitmapPtr(const Params: PParamArray; const Result: Pointer); callconv;
procedure lpMufasaLayer_Paint(const Params: PParamArray); callconv;
procedure lpMufasaLayer_PaintArea(const Params: PParamArray); callconv;

implementation

procedure lpMufasaLayer_Init(const Params: PParamArray); callconv;
begin
  with lpPMufasaLayer(Params^[0])^ do
  begin
    __Layer  := TMufasaLayer.Create(PPtrUInt(Params^[1])^, PBoolean(Params^[2])^, PString(Params^[3])^);
   LayerHandle := __Layer.Window;
  end;
end;

procedure lpMufasaLayer_Free(const Params: PParamArray); callconv;
begin
  lpPMufasaLayer(Params^[0])^.__Layer.Free;
end;

procedure lpMufasaLayer_GetBitmapPtr(const Params: PParamArray; const Result: Pointer); callconv;
begin
  with lpPMufasaLayer(Params^[0])^ do
  begin
    PPointer(Result)^ := __Layer.GetBitmapPtr(PInteger(Params^[1])^, PInteger(Params^[2])^);
    Width := __Layer.Width;
    Height := __Layer.Height;
  end;
end;

procedure lpMufasaLayer_Paint(const Params: PParamArray); cdecl;
begin
  lpPMufasaLayer(Params^[0])^.__Layer.Paint;
end;

procedure lpMufasaLayer_PaintArea(const Params: PParamArray); callconv;
begin
  with PBox(Params^[0])^ do
    lpPMufasaLayer(Params^[0])^.__Layer.Paint(X1, Y1, X1 + (X2 - X1) + 1, Y1 + (Y2 - Y1) + 1);
end;

end.

