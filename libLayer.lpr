library libLayer;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils,
  libLayer.Window;

// Lape Wrappers \\

type
  PParamArray = ^TParamArray;
  TParamArray = array[Word] of Pointer;

procedure Lape_Layer_Init(const Params: PParamArray); cdecl;
begin
  PLayer(Params^[0])^ := TLayer.Create(PPtrUInt(Params^[1])^, PUInt32(Params^[2])^);
end;

procedure Lape_Layer_Free(const Params: PParamArray); cdecl;
begin
  PLayer(Params^[0])^.Free();
end;

procedure Lape_Layer_OnClick(const Params: PParamArray); cdecl;
type
  PLayerClickEvent = ^TLayerClickEvent;
begin
  PLayer(Params^[0])^.OnClick := PLayerClickEvent(Params^[1])^;
end;

procedure Lape_Layer_Data(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PPointer(Result)^ := PLayer(Params^[0])^.Data;
end;

procedure Lape_Layer_Width(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := PLayer(Params^[0])^.Width;
end;

procedure Lape_Layer_Height(const Params: PParamArray; const Result: Pointer); cdecl;
begin
  PInt32(Result)^ := PLayer(Params^[0])^.Height;
end;

procedure Lape_Layer_Paint(const Params: PParamArray); cdecl;
begin
  PLayer(Params^[0])^.Paint();
end;

procedure Lape_Layer_PaintInterval(const Params: PParamArray); cdecl;
begin
  PLayer(Params^[0])^.PaintInterval := PInt32(Params^[1])^;
end;

procedure Lape_Layer_BeginUpdate(const Params: PParamArray); cdecl;
begin
  PLayer(Params^[0])^.BeginUpdate();
end;

procedure Lape_Layer_EndUpdate(const Params: PParamArray); cdecl;
begin
  PLayer(Params^[0])^.EndUpdate();
end;

// Plugin Exports \\

function GetPluginABIVersion: Int32; cdecl; export;
begin
  Result := 2;
end;

function GetFunctionCount(): Int32; cdecl; export;
begin
  Result := 10;
end;

function GetFunctionInfo(Index: Int32; var Addr: Pointer; var Decl: PChar): Int32; cdecl; export;
begin
  case Index of
    0:
      begin
        Addr := @Lape_Layer_Init;
        StrPCopy(Decl, 'procedure TLayer.Init(Window: PtrUInt; ScriptThread: UInt32); native;');
      end;
    1:
      begin
        Addr := @Lape_Layer_Free;
        StrPCopy(Decl, 'procedure TLayer.Free; native;');
      end;
    2:
      begin
        Addr := @Lape_Layer_OnClick;
        StrPCopy(Decl, 'procedure TLayer.OnClick(Method: Pointer); native;');
      end;
    3:
      begin
        Addr := @Lape_Layer_Data;
        StrPCopy(Decl, 'function TLayer.Data: PRGB32; native;');
      end;
    4:
      begin
        Addr := @Lape_Layer_Width;
        StrPCopy(Decl, 'function TLayer.Width: Int32; native;');
      end;
    5:
      begin
        Addr := @Lape_Layer_Height;
        StrPCopy(Decl, 'function TLayer.Height: Int32; native;');
      end;
    6:
      begin
        Addr := @Lape_Layer_Paint;
        StrPCopy(Decl, 'procedure TLayer.Paint; native;');
      end;
    7:
      begin
        Addr := @Lape_Layer_PaintInterval;
        StrPCopy(Decl, 'procedure TLayer.PaintInterval(Interval: Int32); native;');
      end;
    8:
      begin
        Addr := @Lape_Layer_BeginUpdate;
        StrPCopy(Decl, 'procedure TLayer.BeginUpdate; native;');
      end;
    9:
      begin
        Addr := @Lape_Layer_EndUpdate;
        StrPCopy(Decl, 'procedure TLayer.EndUpdate; native;');
      end;
  end;

  Result := Index;
end;

function GetTypeCount(): Int32; cdecl; export;
begin
  Result := 1;
end;

function GetTypeInfo(Index: Int32; var Name, Def: PChar): Int32; cdecl; export;
begin
  case Index of
    0:
      begin
        StrPCopy(Name, 'TLayer');
        StrPCopy(Def,  'type Pointer');
      end;
  end;

  Result := Index;
end;

procedure SetPluginMemManager(NewMemoryManager: TMemoryManager); cdecl; export;
begin
  SetMemoryManager(NewMemoryManager);
end;

exports GetPluginABIVersion;
exports GetFunctionCount;
exports GetFunctionInfo;
exports GetTypeCount;
exports GetTypeInfo;
exports SetPluginMemManager;

begin
end.

