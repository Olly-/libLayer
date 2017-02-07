unit libLayer.PaintThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPaintProc = procedure of object;

  TPaintThread = class(TThread)
  private
    FInterval: Int32;
    FPaused: Boolean;
    FPaintProc: TPaintProc;
  protected
    procedure Execute; override;
  public
    property Paused: Boolean read FPaused write FPaused;

    constructor Create(Interval: Int32; PaintProc: TPaintProc);
  end;

implementation

procedure TPaintThread.Execute;
begin
  while (not Terminated) do
  begin
    while (FPaused) do
    begin
      if (Terminated) then
        Exit;

      Sleep(1);
    end;

    FPaintProc();
    Sleep(FInterval);
  end;
end;

constructor TPaintThread.Create(Interval: Int32; PaintProc: TPaintProc);
begin
  inherited Create(False);

  FInterval := Interval;
  FPaintProc := PaintProc;
  FPaused := False;
end;

end.

