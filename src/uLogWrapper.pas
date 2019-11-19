unit uLogWrapper;

interface

uses
  uLogHook;

procedure InitializeLogHook(const pConfigIniFilePath : string); cdecl;
procedure FinalizeLogHook; cdecl;
procedure EnterCriticalArea; stdcall;
procedure LeaveCriticalArea; stdcall;

implementation

procedure InitializeLogHook(const pConfigIniFilePath : string);
begin
  LogHook.Initialize(pConfigIniFilePath);
end;

procedure FinalizeLogHook;
begin
  LogHook.Finalize;
end;

procedure EnterCriticalArea;
begin
  LogHook.EnterCriticalArea;
end;

procedure LeaveCriticalArea;
begin
  LogHook.LeaveCriticalArea;
end;

end.
