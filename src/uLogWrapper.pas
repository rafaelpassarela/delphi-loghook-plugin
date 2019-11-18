unit uLogWrapper;

interface

uses
  uLogHook;

// start
procedure InitializeLogHook(const pConfigIniFilePath : string); stdcall;
// stop
procedure FinalizeLogHook; stdcall;
// pause the trace info
procedure EnterCriticalArea; stdcall;
// resume the trace info
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
