unit libLayer.Hooks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JwaWindows,
  libLayer.Window;

type
  TLayerHooks = class
  private
    FEventHook: HHOOK;
    FMouseHook: HHOOK;
    FReferenceCount: Int32;

    procedure AddHooks;
    procedure RemoveHooks;
  public
    procedure IncRef;
    procedure DecRef;

    constructor Create;
    destructor Destroy; override;
  end;

var
  LayerHooks: TLayerHooks;

implementation

uses
  Types;

procedure OnEventHook(Hook: HWINEVENTHOOK; Event: DWORD; Window: HWND; idObject: LONG; idChild: LONG; EventThread: DWORD; EventTime: DWORD); stdcall;
var
  Layer: TLayer;
begin
  if (idObject = OBJID_WINDOW) and (Event = EVENT_OBJECT_LOCATIONCHANGE) then
  begin
    Layer := GetLayer(Window);

    if (Layer <> nil) then
      Layer.SetBounds(WindowRect(Window));
  end;
end;

function OnMouseHook(Code: Int32; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  P: POINT;
  Layer: TLayer;
  Block: Boolean = False;
  Handle: HWND;
begin
  if (wParam = WM_LBUTTONDOWN) then
  begin
    P := MSLLHOOKSTRUCT(Pointer(lParam)^).Pt;
    Handle := WindowFromPoint(P);
    Layer := GetLayer(GetAncestor(Handle, GA_ROOT));

    if ((Layer <> nil) and (Layer.OnClick <> nil)) and ((Handle = Layer.Target) or (Layer.HasChild(Handle))) then
    begin
      ScreenToClient(Layer.Target, P);
      P.X -= Layer.Offset.X;
      P.Y -= Layer.Offset.Y;

      Layer.OnClick(P.X, P.Y, Block);
      if (Block) then
        Exit(-1);
    end;
  end;

  Exit(CallNextHookEx(0, Code, wParam, lParam));
end;

procedure TLayerHooks.AddHooks;
begin
  FEventHook := SetWinEventHook(EVENT_OBJECT_LOCATIONCHANGE, EVENT_OBJECT_LOCATIONCHANGE, 0, @OnEventHook, 0, 0, WINEVENT_OUTOFCONTEXT);
  FMouseHook := SetWindowsHookEx(WH_MOUSE_LL, @OnMouseHook, 0, 0);
end;

procedure TLayerHooks.RemoveHooks;
begin
  UnhookWinEvent(FEventHook);
  UnhookWindowsHookEx(FMouseHook);
end;

procedure TLayerHooks.IncRef;
begin
  if (FReferenceCount = 0) then
    AddHooks();

  Inc(FReferenceCount);
end;

procedure TLayerHooks.DecRef;
begin
  if (FReferenceCount > 0) then
    Dec(FReferenceCount);

  if (FReferenceCount = 0) then
    RemoveHooks();
end;

constructor TLayerHooks.Create;
begin
  FReferenceCount := 0;
end;

destructor TLayerHooks.Destroy;
begin
  while (FReferenceCount > 0) do
    DecRef();

  inherited Destroy;
end;

initialization
  LayerHooks := TLayerHooks.Create();

finalization
  LayerHooks.Free();

end.

