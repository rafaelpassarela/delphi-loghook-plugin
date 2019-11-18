unit uFormTest;

interface

uses
  System.Classes, uLogWrapper, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  System.SysUtils, Dialogs;

type
  TForm1 = class(TForm)
    ButtonStart: TButton;
    ButtonDivZero: TButton;
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonDivZeroClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ButtonDivZeroClick(Sender: TObject);
var
  x : Double;
  zero: Integer;
begin
  zero := 0;
  x := 10 / Zero;
  ShowMessage( FloatToStr(x) );
end;

procedure TForm1.ButtonStartClick(Sender: TObject);
var
  lIniFile : string;
begin
  lIniFile := StringReplace(Application.ExeName, '.exe', '.ini', [rfIgnoreCase]);
  InitializeLogHook(lIniFile);
end;

end.
