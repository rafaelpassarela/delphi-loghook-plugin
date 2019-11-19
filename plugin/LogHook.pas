unit LogHook;

interface

uses
  System.SysUtils, Winapi.Windows, Vcl.Forms, System.SysConst;

const
  DLL_NAME =
    {$IFDEF DEBUG_MODE}
      'RpLogHookLib.dll'
    {$ELSE}
      {$IFDEF WIN64}
        'RpLogHookLibW64.dll'
      {$ELSE}
        'RpLogHookLibW32.dll'
      {$ENDIF}
    {$ENDIF};

type
  // Exceptions of this type are automatically ignored (do not appear in the log)
  ExceptionNoInfo = class(Exception);
  PObject = ^TObject;

  // dll mapping -> pointer signature must be the same as uLogWrapper signatures
  TDllInitializeLogHook = procedure(const pConfigIniFilePath : PChar); stdcall;
  TDllExternalLogEvent = procedure(pExceptObj: PObject); stdcall;

  TLogHook = class
  strict private
    class var _LogHook : TLogHook;
  private
    FDLLHandle : THandle;
    FInitHook : TDllInitializeLogHook;
    FFinalizeHook : TProcedure;
    FEnterCriticalArea: TProcedure;
    FLeaveCriticalArea: TProcedure;
    FExternalLogEvent: TDllExternalLogEvent;
    procedure MapFunctions;
    procedure ClearMapFunctions;
    procedure DoException(Sender: TObject; E: Exception);
    // dll callers
    procedure DoInitialize(const pConfigFile : string);
    procedure DoFinalize;
    procedure DoEnterCriticalArea;
    procedure DoLeaveCriticalArea;
    procedure DoExternalLog(pExceptObj : PObject);
  public
    constructor Create;
    destructor Destroy; override;
    // dll mapping
    class procedure InitLogHook(const pConfigIniFilePath : string);
    class procedure FinalizeLogHook;
    class procedure EnterCriticalArea;
    class procedure LeaveCriticalArea;
  end;

//// start
//procedure InitializeLogHook(const pConfigIniFilePath : PChar); stdcall; external DLL_NAME;
//// stop
//procedure FinalizeLogHook; cdecl; external DLL_NAME;
//// pause the trace info
//procedure EnterCriticalArea; stdcall; external DLL_NAME;
//// resume the trace info
//procedure LeaveCriticalArea; stdcall; external DLL_NAME;

implementation

{ TLogHook }

procedure TLogHook.ClearMapFunctions;
begin
  FInitHook := nil;
  FFinalizeHook := nil;
  FEnterCriticalArea := nil;
  FLeaveCriticalArea := nil;
  FExternalLogEvent := nil;
end;

constructor TLogHook.Create;
begin
  FDLLHandle := LoadLibrary(DLL_NAME);
  if FDLLHandle = 0 then
    raise ExceptionNoInfo.CreateFmt('Unable to load %s', [DLL_NAME]);

  MapFunctions;
end;

destructor TLogHook.Destroy;
begin
  DoFinalize;

  ClearMapFunctions;
  if FDLLHandle > 0 then
    FreeLibrary(FDLLHandle);

  inherited;
end;

procedure TLogHook.DoEnterCriticalArea;
begin
  if Assigned(FEnterCriticalArea) then
    FEnterCriticalArea;
end;

procedure TLogHook.DoException(Sender: TObject; E: Exception);
{$IFNDEF CONSOLE}
var
//  O : TObject;
  lTitle : array[0..63] of Char;
{$ENDIF}
begin
  {$IFDEF CONSOLE}
  Writeln(E.Message);
  {$ELSE}

  DoExternalLog(@E);

  LoadString(FindResourceHInstance(HInstance), PResStringRec(@SExceptTitle).Identifier,
    lTitle, Length(lTitle));

//  Display error message next to memory address
//  O := ExceptObject;
//  ShowException(O, ExceptAddr);

  MessageBox(0, PChar(E.Message), lTitle, MB_OK or MB_ICONSTOP or MB_TASKMODAL);
  {$ENDIF}
end;

procedure TLogHook.DoExternalLog(pExceptObj: PObject);
begin
  if Assigned(FExternalLogEvent) then
    FExternalLogEvent(pExceptObj);
end;

procedure TLogHook.DoFinalize;
begin
  if Assigned(FFinalizeHook) then
    FFinalizeHook;

  if FDLLHandle > 0 then
    Application.OnException := nil;
end;

procedure TLogHook.DoInitialize(const pConfigFile: string);
begin
  if Assigned(FInitHook) then
    FInitHook(PChar(pConfigFile));

  if FDLLHandle > 0 then
    Application.OnException := DoException;
end;

procedure TLogHook.DoLeaveCriticalArea;
begin
  if Assigned(FLeaveCriticalArea) then
    FLeaveCriticalArea;
end;

class procedure TLogHook.EnterCriticalArea;
begin
  if Assigned(_LogHook) then
    _LogHook.DoEnterCriticalArea;
end;

class procedure TLogHook.FinalizeLogHook;
begin
  if Assigned(_LogHook) then
    FreeAndNil(_LogHook);
end;

class procedure TLogHook.InitLogHook(const pConfigIniFilePath: string);
begin
  if not Assigned(_LogHook) then
    _LogHook := TLogHook.Create;

  _LogHook.DoInitialize(pConfigIniFilePath);
end;

class procedure TLogHook.LeaveCriticalArea;
begin
  if Assigned(_LogHook) then
    _LogHook.DoLeaveCriticalArea;
end;

procedure TLogHook.MapFunctions;
begin
  if FDLLHandle > 0 then
  begin
    FInitHook := GetProcAddress(FDLLHandle, 'InitializeLogHook');
    FFinalizeHook := GetProcAddress(FDLLHandle, 'FinalizeLogHook');
    FEnterCriticalArea := GetProcAddress(FDLLHandle, 'EnterCriticalArea');
    FLeaveCriticalArea := GetProcAddress(FDLLHandle, 'LeaveCriticalArea');
    FExternalLogEvent := GetProcAddress(FDLLHandle, 'ExternalLogEvent');
  end else
    ClearMapFunctions;
end;

end.
