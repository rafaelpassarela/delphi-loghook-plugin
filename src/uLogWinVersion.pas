unit uLogWinVersion;

interface

uses
  Winapi.Windows, System.SysUtils;

type
  TLogWinVersion = class
  public
    class function GetWindowsVersionName : string;
    class function GetProgramVersion : string;
  end;

implementation

{ TWindowsVersion }

class function TLogWinVersion.GetProgramVersion: string;
var
  v1, v2, v3, v4: Word;
  VerInfoSize, VerValueSize, Dummy: DWORD;
  VerInfo: Pointer;
  VerValue: PVSFixedFileInfo;
begin
  Result := EmptyStr;

  VerInfoSize := GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy);
  if VerInfoSize > 0 then
  begin
    GetMem(VerInfo, VerInfoSize);
    try
      if GetFileVersionInfo(PChar(ParamStr(0)), 0, VerInfoSize, VerInfo) then
      begin
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        with VerValue^ do
        begin
          V1 := dwFileVersionMS shr 16;
          V2 := dwFileVersionMS and $FFFF;
          V3 := dwFileVersionLS shr 16;
          V4 := dwFileVersionLS and $FFFF;
        end;
        Result := Format('%d.%d.%d.%d', [ v1, v2, v3, v4]);
      end;
    finally
      FreeMem(VerInfo, VerInfoSize);
    end;
  end;
end;

class function TLogWinVersion.GetWindowsVersionName: string;
var
  lVersion : TOSVersionInfo;
begin
  lVersion.dwOSVersionInfoSize := SizeOf(lVersion);
  GetVersionEx(lVersion);
  Result := '';
  case lVersion.dwPlatformId of
    1:
      case lVersion.dwMinorVersion of
         0: Result := 'Windows 95';
        10: Result := 'Windows 98';
        90: Result := 'Windows Me';
      end;
    2:
      case lVersion.dwMajorVersion of
        3: Result := 'Windows NT 3.51';
        4: Result := 'Windows NT 4.0';
        5: case lVersion.dwMinorVersion of
             0: Result := 'Windows 2000';
             1: Result := 'Windows XP';
             2: Result := 'Windows Server 2003';
           end;
        6: case lVersion.dwMinorVersion of
             0 : Result := 'Windows Vista';
             1 : Result := 'Windows 7';
             2 : Result := 'Windows 8';
             3 : Result := 'Windows 8.1';
           end;
        10: Result := 'Windows 10';
        else
          Result := 'Windows 10 ou Superior';
      end;
  end;

  if (Result = '') then
    Result := 'Sistema operacional desconhecido.'
  else
    Result := Result + ' ' + Trim(lVersion.szCSDVersion);
end;

end.
