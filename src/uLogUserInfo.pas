unit uLogUserInfo;

interface

uses
  System.SysUtils, uLogWinVersion, uRpJclSysInfo;

type
  TLogUserInfo = packed record
    UserName : string;
    IP : string;
    AppParams : string;
    ComputerName : string;
    SistemaOperacional : string;
    Versao: string;
    procedure Load;
    function GetInfo : string;
  end;

implementation

{ TLogUserInfo }

function TLogUserInfo.GetInfo: string;
begin
  Result := Format('Usu�rio....: %s' + sLineBreak
                 + 'Endere�o IP: %s' + sLineBreak
                 + 'OS.........: %s' + sLineBreak
                 + 'Par�metros.: %s' + sLineBreak
                 + 'Vers�o.....: %s' + sLineBreak
                 + 'Data e Hora: %s', [
                   UserName,
                   ComputerName,
                   SistemaOperacional,
                   AppParams,
                   Versao,
                   FormatDateTime('dd/mm/yyyy hh:nn:ss:zzz', Now) ]);

  Result := StringOfChar('-', 80) + sLineBreak + Result
          + sLineBreak + StringOfChar('-', 80);
end;

procedure TLogUserInfo.Load;
var
  lInd : Integer;
begin
  AppParams := '';
  for lInd := 0 to ParamCount do
    AppParams := AppParams + '"' + ParamStr(lInd) + '"  ';
  AppParams := Trim(AppParams);

  UserName := GetLocalUserName;
  ComputerName := GetLocalComputerName;
  IP := GetIPAddress(ComputerName);

  SistemaOperacional := TLogWinVersion.GetWindowsVersionName;

  Versao := TLogWinVersion.GetProgramVersion;
end;

end.
