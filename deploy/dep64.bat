rem @echo off
set OUTPUTPATH=%1
set OUTPUTDIR=%2

copy %OUTPUTPATH% %OUTPUTDIR%RpLogHookLibW64.dll

copy %OUTPUTPATH% %OUTPUTDIR%\..\Test\Win64\RpLogHookLibW64.dll