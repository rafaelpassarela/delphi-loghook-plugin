//{$D-}
unit uLogHook;

interface

uses
  SysUtils, Forms, IniFiles, Windows, Classes, Graphics, Jpeg,
  ExtCtrls, JclDebug, JclHookExcept, SyncObjs, uLogConfig,
  uLogUserInfo, uLogControl;

type
  LogHook = class
  private
    class var _LogControl: TLogControl;
    class var _Initialized : Boolean;
  private
    class function GetLogControl : TLogControl;
    class procedure FreeLogControl;
  public
    // define se o log será capturado ou não, quando o parametro Active = true,
    // mas em códigos onde o erro é esperado
    class procedure EnterCriticalArea;
    class procedure LeaveCriticalArea;

    class function LogActive: Boolean;
    class function GetLogDir: string;

    class procedure Initialize(const pIniFile : string);
    class procedure Finalize;
  end;

procedure LogExceptionHook(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);

implementation

procedure LogExceptionHook(ExceptObj: TObject; ExceptAddr: Pointer; OSException: Boolean);
begin
  if (not LogHook.GetLogControl.Config.CheckingError)
  and (LogHook.GetLogControl.Config.Active)
  and LogHook.GetLogControl.CanLog(Exception(ExceptObj).ClassName, Exception(ExceptObj).Message) then
  begin
    try
      LogHook.GetLogControl.LastStackTrace.BeginUpdate;
      LogHook.GetLogControl.LastStackTrace.Clear;
      LogHook.GetLogControl.LastStackTrace.Add(Format('''%s'': %s.' + sLineBreak + '%s', [
        Exception(ExceptObj).ClassName,
        Exception(ExceptObj).Message,
        LogHook.GetLogControl.UserInfo.GetInfo]));
      JclLastExceptStackListToStrings(LogHook.GetLogControl.LastStackTrace, True);

      LogHook.GetLogControl.AddLog();
      LogHook.GetLogControl.Config.SetErrorDetected(True);
    finally
      LogHook.GetLogControl.LastStackTrace.EndUpdate;
    end;
  end;
end;

{ LogHook }

class procedure LogHook.EnterCriticalArea;
begin
  _LogControl.EnterCriticalArea;
end;

class procedure LogHook.FreeLogControl;
begin
  if Assigned(LogHook._LogControl) then
    FreeAndNil(LogHook._LogControl);
end;

class function LogHook.GetLogControl: TLogControl;
begin
  Result := LogHook._LogControl;
end;

class function LogHook.GetLogDir: string;
begin
  Result := IncludeTrailingPathDelimiter(_LogControl.Config.OutputDir);
end;

class procedure LogHook.Finalize;
begin
  if LogHook._Initialized then
  begin
    JclRemoveExceptNotifier(LogExceptionHook);
    LogHook.FreeLogControl;
  end;
  LogHook._Initialized := False;
end;

class procedure LogHook.Initialize(const pIniFile: string);
begin
  if not LogHook._Initialized then
  begin
    if not Assigned(LogHook._LogControl) then
      LogHook._LogControl := TLogControl.Create(pIniFile);

  //  JclStackTrackingOptions := [stStack, stExceptFrame, stRawMode, stAllModules, stStaticModuleList];
    JclStackTrackingOptions := [stStack, stRawMode];

    JclStartExceptionTracking;
    JclAddExceptNotifier(LogExceptionHook);

    LogHook._Initialized := True;
  end;
end;

class procedure LogHook.LeaveCriticalArea;
begin
  _LogControl.LeaveCriticalArea;
end;

class function LogHook.LogActive: Boolean;
begin
  Result := _LogControl.Config.Active;
end;

initialization
  LogHook._Initialized := False;

finalization
  LogHook.Finalize;

end.
