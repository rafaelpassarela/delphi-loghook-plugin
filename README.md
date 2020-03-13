# delphi-loghook-plugin

## Remarks
The application must have **ShareMem** as first unit in  DPR uses clause.
Remember to Unload the Log Hook DLL on application closes. 
  Ex.: Call on Main formClose LogHook.FinalizeLogHook;

## Base IniFile Estruture

    [LogConfig]
    Active=1
    CanCreateJPEG=0
    JPEGQuality=50
    LogSize=1024
    LogLifeTime=7
    IgnoreLogTypes=;EResNotFound;EAbort;EDBClient;EExternalException;EIB_ISCError;EDatabaseError;EIdSocketError;EIdConnClosedGracefully;
    LogOutDir=<Application Folder>\Logs
    LogCount=9
    [Log]
    TratandoErro=0
    ErrorDetected=0

