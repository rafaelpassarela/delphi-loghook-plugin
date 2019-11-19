unit uLogWrapper;

interface

uses
  uLogHook;

type
  PObject = ^TObject;

// DLL Map for external applications
procedure InitializeLogHook(pConfigIniFilePath : string); stdcall;
procedure FinalizeLogHook; stdcall;
procedure EnterCriticalArea; stdcall;
procedure LeaveCriticalArea; stdcall;
procedure ExternalLogEvent(pExceptObj: PObject); stdcall;

implementation

procedure InitializeLogHook(pConfigIniFilePath : string);
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

procedure ExternalLogEvent(pExceptObj: PObject);
begin
  LogExceptionHook(pExceptObj^, nil, False);
end;

end.
