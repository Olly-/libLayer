unit MufasaLayer;

{.$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, Windows, Interfaces, Graphics, Forms;

type
  TProcObj = procedure of object;

  { TPaintThread }

  TPaintThread = class(TThread)
  protected
    procedure Execute; override;
  public
    FMethod: TProcObj;
    constructor Create(AMethod: TProcObj); overload;
  end;


  { TMufasaLayer }

  PMufasaLayer = ^TMufasaLayer;
  TMufasaLayer = class(TObject)
  private
    FProcess: TProcess;
    FThread: TPaintThread;
    FWindow: HWND;
    FWindowDC: HDC;
    FIsChildWindow: Boolean;
    FChildOffset: TPoint;
    FMainWindow: HWND;

    imageWidth, imageHeight: Integer;
    imageHDC: HDC;
    imageHBitmap: HBitmap;
  public
    property Window: HWND read FWindow;
    property Width: Integer read imageWidth;
    property Height: Integer read imageHeight;

    function GetBitmapPtr(AWidth, AHeight: Integer): Pointer;
    procedure Paint;
    procedure Paint(X, Y, AWidth, AHeight: Integer); overload;

    constructor Create(ATarget: PtrUInt; AutoPaint: Boolean; APath: String);
    destructor Destroy; override;
  end;

implementation

{ TPaintThread }

uses
  JwaWindows;

constructor TPaintThread.Create(AMethod: TProcObj);
begin
  inherited Create(False);

  FMethod := AMethod;
end;

procedure TPaintThread.Execute;
begin
  while (not Terminated) do
  begin
    FMethod();
    Sleep(35);
  end;
end;


{ TMufasaLayer }

constructor TMufasaLayer.Create(ATarget: PtrUInt; AutoPaint: Boolean; APath: String);
var
  l: TStringList;
  Str: String;
  i: Integer;
  r: TRect;
begin
  inherited Create;

  FThread := nil;
  FWindow := 0;
  FWindowDC := 0;
  FChildOffset.X := 0;
  FChildOffset.Y := 0;
  imageHBitmap := 0;
  imageHDC := 0;

  FIsChildWindow := GetParent(ATarget) <> 0;
  if (FIsChildWindow) then
  begin
    FMainWindow := GetParent(ATarget);
    while True do
    begin
      if (GetParent(FMainWindow) = 0) then
         Break;
      FMainWindow := GetParent(FMainWindow);
    end;

    GetWindowRect(ATarget, r);
    FChildOffset := r.TopLeft;
    Windows.ScreenToClient(FMainWindow, FChildOffset);
    ATarget := FMainWindow;

    Writeln('TMufasaLayer.Create: Child window[', FChildOffset.X, ', ', FChildOffset.Y, ']');
  end;

  FProcess := TProcess.Create(nil);
  try
    FProcess.Executable := APath + 'mLayer.exe';
    FProcess.Parameters.Add('-target');
    FProcess.Parameters.Add(IntToStr(ATarget));
    FProcess.Options := [poUsePipes, poStderrToOutPut];
    FProcess.Execute;

    Writeln('Started mLayer.exe');
  except
    on e: Exception do
    begin
      Writeln('TProcess exception: ' + e.Message);
      Exit;
    end;
  end;

  l := TStringList.Create;
  try
    while FProcess.Running do
    begin
      l.LoadFromStream(FProcess.Output);
      if (l.Count <> 0) then Break;
      Sleep(500);
    end;
    if (Pos('Handle=', l.Text) > 0) then
    begin
      Str := '';
      for i := 1 to Length(l.Text) do
        if (l.Text[i] in ['0'..'9']) then Str += l.Text[i];
      FWindow := StrToInt(Str);
      FWindowDC := GetWindowDC(FWindow);
    end;

    if (FWindow = 0) then
      raise Exception.Create('Failed to get a response from mLayer.exe');
    Writeln('Layer window: ', FWindow);
  except
    on e: Exception do
    begin
      Writeln('TMufasaLayer.Create: Exception: ' + e.Message);
      raise;
    end;
  end;
  l.Free;

  if (AutoPaint) then
    FThread := TPaintThread.Create(@Self.Paint);
end;

function TMufasaLayer.GetBitmapPtr(AWidth, AHeight: Integer): Pointer;
var
  bi: Windows.BITMAPINFO;
begin
  Result := nil;

  ZeroMemory(@bi, SizeOf(BITMAPINFO));
  bi.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  bi.bmiHeader.biWidth := AWidth;
  bi.bmiHeader.biHeight := -AHeight;
  bi.bmiHeader.biPlanes := 1;
  bi.bmiHeader.biBitCount := 32;
  bi.bmiHeader.biCompression := BI_RGB;
  bi.bmiHeader.biSizeImage := 0;
  bi.bmiHeader.biClrUsed := 0;

  imageWidth := AWidth;
  imageHeight := AHeight;
  imageHDC := CreateCompatibleDC(0);
  imageHBITMAP := Windows.CreateDIBSection(imageHDC, bi, DIB_RGB_COLORS, Result, 0, 0);

  SelectObject(imageHDC, imageHBITMAP);
end;

procedure TMufasaLayer.Paint;
begin
  if (imageHDC > 0) then
    BitBlt(FWindowDC, FChildOffset.X, FChildOffset.Y, imageWidth, imageHeight, imageHDC, 0, 0, SRCCOPY);
end;

procedure TMufasaLayer.Paint(X, Y, AWidth, AHeight: Integer);
begin
  if (imageHDC > 0) then
    BitBlt(FWindowDC, FChildOffset.X + X, FChildOffset.Y + Y, AWidth, AHeight, imageHDC, 0, 0, SRCCOPY);
end;

destructor TMufasaLayer.Destroy;
begin
  if (Assigned(FThread)) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;

  if (imageHDC > 0) then
    DeleteObject(imageHDC);
  if (imageHDC > 0) then
    DeleteObject(imageHBitmap);
  if (FWindowDC > 0) then
    ReleaseDC(FWindow, FWindowDC);

  FProcess.Terminate(0);
  FProcess.Free;

  inherited Destroy;
end;

initialization
  Application.Initialize;

end.

