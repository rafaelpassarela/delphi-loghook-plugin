unit uFormTest;

interface

uses
  System.Classes, uLogWrapper, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  System.SysUtils, Dialogs, uLogHook;

type
  TFormLogHookTest = class(TForm)
    ButtonDivZero: TButton;
    ButtonXYZ: TButton;
    ButtonSkip: TButton;
    ButtonRaise: TButton;
    Label1: TLabel;
    procedure ButtonDivZeroClick(Sender: TObject);
    procedure ButtonXYZClick(Sender: TObject);
    procedure ButtonRaiseClick(Sender: TObject);
    procedure ButtonSkipClick(Sender: TObject);
  private
    { Private declarations }
    procedure CheckLogActive;
  public
    { Public declarations }
  end;

var
  FormLogHookTest: TFormLogHookTest;

implementation

{$R *.dfm}

procedure TFormLogHookTest.ButtonXYZClick(Sender: TObject);
var
  x : Integer;
  a : string;
begin
  CheckLogActive;

  a := 'XYZ';
  x := StrToInt(a);
  ShowMessage( IntToHex(x) );
end;

procedure TFormLogHookTest.CheckLogActive;
begin
  if not LogHook.LogActive then
    ShowMessage('Open the "INI" config file, and set the Active value to 1');
end;

procedure TFormLogHookTest.ButtonSkipClick(Sender: TObject);
begin
  CheckLogActive;

  raise ExceptionNoInfo.Create('The error class ExceptionNoInfo is NEVER added to Log');
end;

procedure TFormLogHookTest.ButtonDivZeroClick(Sender: TObject);
var
  x : Double;
  zero: Integer;
begin
  CheckLogActive;

  zero := 0;
  x := 10 / Zero;
  ShowMessage( FloatToStr(x) );
end;

procedure TFormLogHookTest.ButtonRaiseClick(Sender: TObject);
begin
  CheckLogActive;

  raise EInOutError.Create('This error is add to Log, if LogHook is Active');
end;

initialization
  LogHook.Initialize( StringReplace(Application.ExeName, '.exe', 'Log.ini', [rfIgnoreCase]) );

finalization
  LogHook.Finalize;

end.
