unit uFormTest;

interface

uses
  System.Classes, uLogWrapper, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  System.SysUtils, Dialogs, LogHook;

type
  TFormLogHookTest = class(TForm)
    GroupBoxDirect: TGroupBox;
    ButtonStart: TButton;
    ButtonStop: TButton;
    ButtonDivZero: TButton;
    ButtonXYZ: TButton;
    ButtonSkip: TButton;
    ButtonRaise: TButton;
    GroupBoxDLL: TGroupBox;
    ButtonStartDLL: TButton;
    ButtonStopDLL: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonDivZeroClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure ButtonXYZClick(Sender: TObject);
    procedure ButtonRaiseClick(Sender: TObject);
    procedure ButtonSkipClick(Sender: TObject);
    procedure ButtonStartDLLClick(Sender: TObject);
    procedure ButtonStopDLLClick(Sender: TObject);
  private
    { Private declarations }
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
  a := 'XYZ';
  x := StrToInt(a);
end;

procedure TFormLogHookTest.ButtonSkipClick(Sender: TObject);
begin
  raise ExceptionNoInfo.Create('The error class ExceptionNoInfo is NEVER added to Log');
end;

procedure TFormLogHookTest.ButtonDivZeroClick(Sender: TObject);
var
  x : Double;
  zero: Integer;
begin
  zero := 0;
  x := 10 / Zero;
  ShowMessage( FloatToStr(x) );
end;

procedure TFormLogHookTest.ButtonRaiseClick(Sender: TObject);
begin
  raise EInOutError.Create('This error is add to Log, if LogHook is Active');
end;

procedure TFormLogHookTest.ButtonStartClick(Sender: TObject);
var
  lIniFile : string;
begin
  lIniFile := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);
  uLogWrapper.InitializeLogHook(lIniFile);
end;

procedure TFormLogHookTest.ButtonStartDLLClick(Sender: TObject);
var
  lIniFile : string;
begin
  lIniFile := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);
  LogHook.InitializeLogHook(PChar(lIniFile));
end;

procedure TFormLogHookTest.ButtonStopClick(Sender: TObject);
begin
  uLogWrapper.FinalizeLogHook;
end;

procedure TFormLogHookTest.ButtonStopDLLClick(Sender: TObject);
begin
  LogHook.FinalizeLogHook;
end;

end.
