library libMufasaLayer;

{$mode objfpc}{$H+}
{$macro on}
{$define callconv := {$IFDEF WINDOWS}{$IFDEF CPU32}cdecl{$ELSE}{$ENDIF}{$ENDIF}
                     {$IFDEF LINUX}{$IFDEF CPU32}cdecl{$ELSE}{$ENDIF}{$ENDIF}}

uses
  Classes, SysUtils, lapeMufasaLayer;

var
  OldMemoryManager: TMemoryManager;
  MemIsSet: Boolean = False;

function GetPluginABIVersion: Integer; callconv; export;
begin
  Result := 2;
end;

procedure SetPluginMemManager(MemMgr : TMemoryManager); callconv; export;
begin
  if (MemIsSet) then
    Exit;
  GetMemoryManager(OldMemoryManager);
  SetMemoryManager(MemMgr);
  MemIsSet := True;
end;

procedure OnDetach; callconv; export;
begin
  SetMemoryManager(OldMemoryManager);
end;

function GetTypeCount(): Integer; callconv; export;
begin
  Result := 1;
end;

function GetTypeInfo(x: Integer; var sType, sTypeDef: PChar): integer; callconv; export;
begin
  StrPCopy(sType, 'TMufasaLayer');
  StrPCopy(sTypeDef, 'packed record'              + LineEnding +
                        'Bitmap: TMufasaBitmap;'  + LineEnding +
                        'LayerHandle: PtrUInt;'   + LineEnding +
                        'Width, Height: Integer;' + LineEnding +
                        '__Layer: Pointer;'       + LineEnding +
                     'end;'
          );

  Result := x;
end;

function GetFunctionCount(): Integer; callconv; export;
begin
  Result := 5;
end;

function GetFunctionInfo(x: Integer; var ProcAddr: Pointer; var ProcDef: PChar): Integer; callconv; export;
begin
  case x of
    0:
      begin
        ProcAddr := @lpMufasaLayer_Init;
        StrPCopy(ProcDef, 'procedure TMufasaLayer.Init(Target: PtrUInt; AutoDraw: Boolean; ExePath: String); Native;');
      end;
    1:
      begin
        ProcAddr := @lpMufasaLayer_GetBitmapPtr;
        StrPCopy(ProcDef, 'function TMufasaLayer.GetBitmapPtr(AWidth, AHeight: Integer): Pointer; Native;');
      end;
    2:
      begin
        ProcAddr := @lpMufasaLayer_Paint;
        StrPCopy(ProcDef, 'procedure TMufasaLayer.Paint; Native;');
      end;
    3:
      begin
        ProcAddr := @lpMufasaLayer_Paint;
        StrPCopy(ProcDef, 'procedure TMufasaLayer.Paint(Area: TBox); overload; Native;');
      end;
    4:
      begin
        ProcAddr := @lpMufasaLayer_Free;
        StrPCopy(ProcDef, 'procedure TMufasaLayer.Free; Native;');
      end;
  end;

  Result := x;
end;

exports GetPluginABIVersion;
exports SetPluginMemManager;
exports GetTypeCount;
exports GetTypeInfo;
exports GetFunctionCount;
exports GetFunctionInfo;
exports OnDetach;

begin
end.
