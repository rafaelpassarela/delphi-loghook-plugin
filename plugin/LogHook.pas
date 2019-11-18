unit LogHook;

interface

uses
  System.SysUtils;

const
  DLL_NAME = 'RpLogHookLib.dll';
//  DLL_NAME = {$IFDEF WIN64} 'RpLogHookLibW64.dll' {$ELSE} 'RpLogHookLibW32.dll' {$ENDIF};

type
  // Exceptions of this type are automatically ignored (do not appear in the log)
  ExceptionNoInfo = class(Exception);

// start
procedure InitializeLogHook(const pConfigIniFilePath : PChar); stdcall; external DLL_NAME;
// stop
procedure FinalizeLogHook; stdcall; external DLL_NAME;
// pause the trace info
procedure EnterCriticalArea; stdcall; external DLL_NAME;
// resume the trace info
procedure LeaveCriticalArea; stdcall; external DLL_NAME;

implementation

end.
