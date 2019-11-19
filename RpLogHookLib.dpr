library RpLogHookLib;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  System.SysUtils,
  System.Classes,
  uLogConfig in 'src\uLogConfig.pas',
  uLogConsts in 'src\uLogConsts.pas',
  uLogControl in 'src\uLogControl.pas',
  uLogHook in 'src\uLogHook.pas',
  uLogUserInfo in 'src\uLogUserInfo.pas',
  uLogWinVersion in 'src\uLogWinVersion.pas',
  uLogWrapper in 'src\uLogWrapper.pas';

{$R *.res}

exports
  InitializeLogHook,
  FinalizeLogHook,
  EnterCriticalArea,
  LeaveCriticalArea,
  ExternalLogEvent;

begin
end.
