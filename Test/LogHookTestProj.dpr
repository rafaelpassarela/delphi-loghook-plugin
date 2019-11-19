program LogHookTestProj;

uses
  ShareMem,
  Vcl.Forms,
  uFormTest in 'uFormTest.pas' {FormLogHookTest},
  uLogHook in '..\src\uLogHook.pas',
  uLogConfig in '..\src\uLogConfig.pas',
  uLogConsts in '..\src\uLogConsts.pas',
  uLogWinVersion in '..\src\uLogWinVersion.pas',
  uLogUserInfo in '..\src\uLogUserInfo.pas',
  uLogControl in '..\src\uLogControl.pas',
  uLogWrapper in '..\src\uLogWrapper.pas',
  LogHook in '..\plugin\LogHook.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormLogHookTest, FormLogHookTest);
  Application.Run;
end.
