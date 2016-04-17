unit Main;

{$APPTYPE CONSOLE}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Windows,
  LMessages, ExtCtrls;

type

  { TTransparentForm }

  TTransparentForm = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  protected
    procedure CreateHandle; override;
  private
    FTarget: HWND;
    FHook: HHOOK;
    FBuffer: Graphics.TBitmap;
    FNeedBufferDraw: Boolean;

    procedure WMShowWindow(var message: TLMShowWindow); message LM_SHOWWINDOW;

    procedure AddHook;
    procedure RemoveHook;
  public
    property Target: HWND read FTarget;
    property Buffer: Graphics.TBitmap read FBuffer write FBuffer;
    property NeedBufferDraw: Boolean read FNeedBufferDraw write FNeedBufferDraw;
  end;

var
  TransparentForm: TTransparentForm;

implementation

{$R *.lfm}

uses
  JwaWindows;

procedure HookProc(hWinEventHook: HWINEVENTHOOK; event: DWORD; win: HWND; idObject: LONG;
                   idChild: LONG; idEventThread: DWORD; dwmsEventTime: DWORD); stdcall;

  procedure dbg;
  const Path = 'C:/Simba/events.txt';
  begin
    if (not FileExistsUTF8(Path)) then
      FileCreateUTF8(Path);
    with TStringList.Create do
    try
      LoadFromFile(Path);
      Add(IntToStr(event));
      SaveToFile(Path);
    finally
      Free;
    end;
  end;

var
  r: TRect;
begin
  if (win <> TransparentForm.Target) then
    Exit;
  //dbg;

  if (idObject = OBJID_WINDOW) and (event = EVENT_OBJECT_DESTROY) then
  begin
    TransparentForm.Close;
    TransparentForm.Free;
    Halt(0);
  end else
  if (event = EVENT_OBJECT_LOCATIONCHANGE) then
  begin
    if (GetWindowRect(win, r)) then
      SetWindowPos(TransparentForm.Handle, 0, r.Left, r.Top, 0, 0, SWP_NOSIZE or SWP_NOZORDER or SWP_NOACTIVATE);
  end;
end;

{ TTransparentForm }

procedure TTransparentForm.FormCreate(Sender: TObject);
begin
  FTarget := 0;
  FNeedBufferDraw := False;
  FBuffer := Graphics.TBitmap.Create;
  FTarget := StrToInt64Def(Trim(Application.Params[2]), 0);
  //FTarget := 394096;
  if (FTarget = 0) then
    raise Exception.Create('FTarget = 0');

  AddHook;
end;

procedure TTransparentForm.FormDestroy(Sender: TObject);
begin
  RemoveHook;
  FBuffer.Free;
end;

procedure TTransparentForm.FormPaint(Sender: TObject);
begin
  if (NeedBufferDraw) then
  begin
    Canvas.Draw(0, 0, FBuffer);
    NeedBufferDraw := False;
  end;
end;

procedure TTransparentForm.CreateHandle;
var
  r: TRect;
begin
  inherited CreateHandle;

  if (FTarget > 0) then
  begin
    SetWindowLongA(Handle, GWL_EXSTYLE, WS_EX_LAYERED or WS_EX_TRANSPARENT);
    SetLayeredWindowAttributes(Handle, 0, 0, 1);
    SetWindowLong(Handle, GWL_HWNDPARENT, FTarget);
    if (GetWindowRect(FTarget, r)) then
      SetBounds(r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top);

    Writeln('Handle=', Self.Handle);
  end;
end;

procedure TTransparentForm.WMShowWindow(var message: TLMShowWindow);
var
  dc: HDC;
begin
  if (Message.Show = False) then
  begin
    dc := GetWindowDC(Handle);
    FBuffer.LoadFromDevice(dc);
    ReleaseDC(0, dc);
    NeedBufferDraw := True;
  end;

  inherited;
end;

procedure TTransparentForm.AddHook;
var
  dwProcessId, dwThreadId: DWORD;
begin
  dwProcessId := 0;
  dwThreadId := GetWindowThreadProcessId(FTarget, @dwProcessId);
  FHook := SetWinEventHook(EVENT_MIN, EVENT_MAX, 0, @HookProc, dwProcessId, dwThreadId, WINEVENT_OUTOFCONTEXT);
  if (FHook = 0) then
    raise Exception.Create('Failed to set hook');
end;

procedure TTransparentForm.RemoveHook;
begin
  if (FHook > 0) then
    UnhookWinEvent(FHook);
end;

end.

