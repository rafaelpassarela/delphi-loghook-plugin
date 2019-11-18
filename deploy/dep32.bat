rem @echo off
set OUTPUTPATH=%1
set OUTPUTDIR=%2

copy %OUTPUTPATH% %OUTPUTDIR%RpLogHookLibW32.dll

copy %OUTPUTPATH% %OUTPUTDIR%\..\Test\Win32\RpLogHookLibW32.dll