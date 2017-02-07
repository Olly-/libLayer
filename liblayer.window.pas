unit libLayer.Window;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JwaWindows,
  libLayer.Bitmap, libLayer.PaintThread;

type
  TLayerClickEvent = procedure(X, Y: Int32; var Block: Boolean); cdecl;

  TLayer = class
  private
    FChildren: array of HWND;
    FPaintInterval: Int32;
    FPaintThread: TPaintThread;
    FWindow: HWND;
    FTarget: HWND;
    FOffset: POINT;
    FOnClick: TLayerClickEvent;
    FBitmap: TLayerBitmap;
    FWidth, FHeight: Int32;

    function GetData: Pointer;
    procedure SetPaintInterval(Interval: Int32);
  public
    property Window: HWND read FWindow;
    property Target: HWND read FTarget;
    property OnClick: TLayerClickEvent read FOnClick write FOnClick;
    property Width: Int32 read FWidth;
    property Height: Int32 read FHeight;
    property Data: Pointer read GetData;
    property Bitmap: TLayerBitmap read FBitmap;
    property PaintInterval: Int32 read FPaintInterval write SetPaintInterval;
    property Offset: POINT read FOffset;

    procedure AddChild(Handle: HWND);
    function HasChild(Handle: HWND): Boolean;

    procedure SetLayered;
    procedure SetParent(Parent: HWND);
    procedure SetTransparentColor(Color: Int32);
    procedure SetBounds(Rect: RECT);
    procedure Show;
    procedure Hide;
    procedure Close;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Paint;

    constructor Create(ATarget: HWND);
    destructor Destroy; override;
  end;

  PLayer = ^TLayer;

  function WindowRect(Handle: HWND): RECT;

  function GetLayerHandle(Handle: HWND): HWND;
  function GetLayer(Handle: HWND): TLayer;

implementation

uses
  fgl, syncobjs, DwmApi,
  libLayer.Hooks;

type
  TLayerList = class(specialize TFPGObjectList<TLayer>)
  private
    FCriticalSection: TCriticalSection;
  public
    function Add(const Item: TLayer): Integer;
    procedure Remove(const Item: TLayer);

    destructor Destroy; override;
    constructor Create(AFreeObjects: Boolean = True);
  end;

var
  LayerList: TLayerList;

function TLayerList.Add(const Item: TLayer): Integer;
begin
  FCriticalSection.Enter();

  try
    inherited Add(Item);
  finally
    FCriticalSection.Leave();
  end;
end;

procedure TLayerList.Remove(const Item: TLayer);
begin
  FCriticalSection.Enter();

  try
    inherited Remove(Item);
  finally
    FCriticalSection.Leave();
  end;
end;

destructor TLayerList.Destroy;
begin
  while (Count > 0) do
    First.Free();

  FCriticalSection.Free();

  inherited Destroy();
end;

constructor TLayerList.Create(AFreeObjects: Boolean);
begin
  FCriticalSection := TCriticalSection.Create();

  inherited Create(AFreeObjects);
end;

function GetLayerHandle(Handle: HWND): HWND;
const
  CLASS_NAME = 'TLayer';
  CLASS_SIZE = 6;
var
  Arr: array[0..CLASS_SIZE + 1] of Char;
begin
  Handle := GetWindow(Handle, GW_ENABLEDPOPUP);
  if (Handle <> 0) and (isWindow(Handle)) and (GetClassName(Handle, @Arr[0], Length(Arr)) = CLASS_SIZE) and (UTF8Encode(Arr) = CLASS_NAME) then
    Exit(Handle);

  Exit(0);
end;

function GetLayer(Handle: HWND): TLayer;
var
  Ptr: Pointer;
begin
  Handle := GetLayerHandle(Handle);
  if (Handle > 0) then
  begin
    Ptr := Pointer(PtrUInt(GetWindowLongPtr(Handle, GWL_USERDATA)));
    if (not IsBadReadPtr(Ptr, SizeOf(Pointer))) then // Make sure it's in our memory space
      Exit(TLayer(Ptr));
  end;

  Exit(nil);
end;

function GetChildren(Handle: HWND; lParam: LPARAM): BOOL; stdcall;
begin
  PLayer(PtrUInt(lParam))^.addChild(Handle);
  Exit(True);
end;

function WindowRect(Handle: HWND): RECT;
begin
  Result := Default(RECT);

  if (Win32MajorVersion >= 6) and (DwmCompositionEnabled) and (GetAncestor(Handle, GA_ROOT) = Handle) then
    DwmGetWindowAttribute(Handle, DWMWA_EXTENDED_FRAME_BOUNDS, @Result, SizeOf(Result))
  else
    GetWindowRect(Handle, Result);
end;

function WindowProc(Handle: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  if (Message = WM_ACTIVATEAPP) and (not IsWindow(GetWindowLongPtr(Handle, GWL_HWNDPARENT))) then // seems to catch target close
    PostMessage(Handle, WM_CLOSE, 0, 0);

  if (Message = WM_SHOWWINDOW) and (lParam = SW_PARENTCLOSING) then
    { nothing }
  else
    Result := DefWindowProc(Handle, Message, wParam, lParam);
end;

function TLayer.GetData: Pointer;
begin
  if (FBitmap = nil) then
    Exit(nil)
  else
    Exit(FBitmap.Ptr);
end;

procedure TLayer.SetPaintInterval(Interval: Int32);
begin
  if (FPaintInterval = Interval) then
    Exit;
  FPaintInterval := Interval;

  if (FPaintThread <> nil) then
  begin
    FPaintThread.Terminate();
    FPaintThread.WaitFor();
    FPaintThread.Free();
    FPaintThread := nil;
  end;

  if (FPaintInterval > 0) then
    FPaintThread := TPaintThread.Create(FPaintInterval, @Paint);
end;

procedure TLayer.AddChild(Handle: HWND);
begin
  SetLength(FChildren, Length(FChildren) + 1);
  FChildren[High(FChildren)] := Handle;
end;

function TLayer.HasChild(Handle: HWND): Boolean;
var
  i: Int32;
begin
  for i := 0 to High(FChildren) do
    if (FChildren[i] = Handle) then
      Exit(True);

  Exit(False);
end;

procedure TLayer.SetLayered;
begin
  SetWindowLong(FWindow, GWL_EXSTYLE, WS_EX_LAYERED or WS_EX_TRANSPARENT);
end;

procedure TLayer.SetParent(Parent: HWND);
begin
  SetWindowLong(FWindow, GWL_HWNDPARENT, Parent);
end;

procedure TLayer.SetTransparentColor(Color: Int32);
begin
  SetLayeredWindowAttributes(FWindow, Color, 0, LWA_COLORKEY);
end;

procedure TLayer.SetBounds(Rect: RECT);
begin
  SetWindowPos(FWindow, 0, Rect.Left, Rect.Top, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
end;

procedure TLayer.Show;
begin
  ShowWindow(FWindow, SW_SHOWNOACTIVATE);
end;

procedure TLayer.Hide;
begin
  ShowWindow(FWindow, SW_HIDE);
end;

procedure TLayer.Close;
begin
  SendMessage(FWindow, WM_CLOSE, 0, 0);
end;

procedure TLayer.BeginUpdate;
begin
  if (FPaintThread <> nil) then
    FPaintThread.Paused := True;
end;

procedure TLayer.EndUpdate;
begin
  if (FPaintThread <> nil) then
    FPaintThread.Paused := False;
end;

procedure TLayer.Paint;
var
  DC: HDC;
begin
  if (FBitmap <> nil) then
  begin
    DC := GetWindowDC(FWindow);
    BitBlt(DC, FOffset.X, FOffset.Y, FBitmap.Width, FBitmap.Height, FBitmap.DC, 0, 0, SRCCOPY);
    ReleaseDC(FWindow, DC);
  end;
end;

constructor TLayer.Create(ATarget: HWND);

  procedure CreateWindow;
  var
    WindowClass: WNDCLASS;
    R: RECT;
  begin
    with WindowClass do
    begin
      Style := 0;
      lpfnWndProc := @WindowProc;
      cbClsExtra := 0;
      cbWndExtra := 0;
      hInstance := System.HINSTANCE;
      hIcon := 0;
      hCursor := LoadCursor(0, IDC_ARROW);
      hbrBackground := GetStockObject(BLACK_BRUSH);
      lpszMenuName := nil;
      lpszClassName := PChar(String(ClassName));
    end;

    RegisterClass(WindowClass);

    R := WindowRect(FTarget);

    FWidth := R.Right - R.Left;
    FHeight := R.Bottom - R.Top;
    FWindow := JwaWindows.CreateWindow(
                 PChar(String(ClassName)), PChar(String(ClassName)), WS_POPUP or WS_SYSMENU,
                 R.Left, R.Top, FWidth, FHeight, 0, 0, System.HINSTANCE, nil
               );

    SetWindowLongPtr(FWindow, GWL_USERDATA, PtrUInt(Self));
  end;

var
  R, RR: TRect;
  Root: HWND;
begin
  FTarget := ATarget;
  FBitmap := nil;
  FOnClick := nil;
  FPaintThread := nil;
  FPaintInterval := 0;
  FWidth := 0;
  FHeight := 0;
  FOffset.X := 0;
  FOffset.Y := 0;

  Root := GetAncestor(FTarget, GA_ROOT);
  if (Root <> FTarget) then // Target is a child window, so we will offset painting
  begin
    R := WindowRect(FTarget);
    RR := WindowRect(Root);

    FOffset.X := R.Left - RR.Left;
    FOffset.Y := R.Top - RR.Top;

    FTarget := Root;
  end;

  // Get children
  SetLength(FChildren, 0);
  EnumChildWindows(FTarget, @GetChildren, PtrUInt(@Self));

  // Close old layer if needed
  if (GetLayerHandle(FTarget) > 0) then
    SendMessage(GetLayerHandle(FTarget), WM_CLOSE, 0, 0);

  // Create new layer
  CreateWindow();

  // Setup layer window
  Show();
  SetLayered();
  SetTransparentColor(0);
  SetParent(FTarget);

  // Add hooks
  LayerHooks.IncRef();

  // Create Image
  FBitmap := TLayerBitmap.Create(FWidth, FHeight);

  // Add to a list so we can free unfree'd ones
  LayerList.Add(Self);
end;

destructor TLayer.Destroy;
begin
  Close();
  LayerHooks.DecRef();
  LayerList.Remove(Self);

  if (FPaintThread <> nil) then
  begin
    FPaintThread.Terminate();
    FPaintThread.WaitFor();
    FPaintThread.Free();
  end;

  if (FBitmap <> nil) then
    FBitmap.Free();

  inherited Destroy();
end;

initialization
  if (Win32MajorVersion >= 6) then
    InitDwmLibrary();

  LayerList := TLayerList.Create(False);

finalization
  LayerList.Free();

end.

