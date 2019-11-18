program LogHookTestProj;

uses
  Vcl.Forms,
  uFormTest in 'uFormTest.pas' {Form1},
  uLogHook in '..\src\uLogHook.pas',
  uLogConfig in '..\src\uLogConfig.pas',
  uLogConsts in '..\src\uLogConsts.pas',
  uLogWinVersion in '..\src\uLogWinVersion.pas',
  uLogUserInfo in '..\src\uLogUserInfo.pas',
  uLogControl in '..\src\uLogControl.pas',
  uLogWrapper in '..\src\uLogWrapper.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
