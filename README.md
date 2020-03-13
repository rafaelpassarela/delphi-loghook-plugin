# delphi-loghook-plugin

## Remarks
How to Use:
1) Build and Install package "RpLogHookPackage"
2) Configure the search path to "$(BDSCOMMONDIR)\Dcu\"
3) Add to uses unit list of main .pas file "uLogHook"
4) On the "initialization" section, call the hoog initializer
5) On the "finalization" section, call the hoog finalizer


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

