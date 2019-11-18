unit uLogWrapper;

interface

uses
  uLogHook;

procedure InitializeLogHook(const pConfigIniFilePath : string);

implementation

procedure InitializeLogHook(const pConfigIniFilePath : string);
begin
  LogHook.Initialize(pConfigIniFilePath);
end;

end.
