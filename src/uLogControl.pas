unit uLogControl;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  uLogUserInfo, uLogConfig, System.Classes, System.SysUtils, Winapi.Windows,
  System.SyncObjs, Vcl.Forms, Vcl.Imaging.jpeg, Vcl.Graphics, Vcl.ExtCtrls,
  uLogConsts;

type
  TLogControl = class
  private class var
    _LogHookCS: TCriticalSection;
  private
    FUserInfo : TLogUserInfo;
    FConfig: TLogConfig;
    FLastStackTrace: TStringList;
    FLastLogValid: Boolean;
    FInCriticalArea: Boolean;
    function FileSize(const aFilename: String): Int64;
    function GetLogBkpFileName(const AFileName : string) : string;
    function GetLogFileName: string;
    procedure GetScreenShot(const pNumber : Integer);
  public
    constructor Create(const pIniFilePath : string);
    destructor Destroy; override;

    property UserInfo : TLogUserInfo read FUserInfo;
    property Config : TLogConfig read FConfig;
    property LastStackTrace : TStringList read FLastStackTrace;

    property LastLogValid : Boolean read FLastLogValid;

    function CanLog(const AErrorClassName, AErrorMessage : string) : Boolean;

    procedure WriteLog(const AMessage : string; const AFileName : string; const ALineBreak : Boolean = True);

    procedure EnterCriticalArea;
    procedure LeaveCriticalArea;
    procedure AddLog(pMessage: string = ''; const pFileName : string = '';
      const pAddExceptionNumber: Boolean = True; const pLineBreak : Boolean = True;
      const pTakeScreenShot : Boolean = True);
    procedure ClearErrorDetected;
  end;

implementation

{ TLogControl }

function TLogControl.CanLog(const AErrorClassName, AErrorMessage: string): Boolean;
var
  i: Integer;
  lMens: string;
begin
  // testa a classe do erro
  Result := (not FInCriticalArea)
        and (Pos(';' + AErrorClassName + ';', FConfig.IgnoredTypes) <= 0);

  // testa a mensagem do erro
  if Result then
  begin
    for i := 0 to FConfig.IgnoredMensList.Count - 1 do
    begin
      lMens := Trim(FConfig.IgnoredMensList[i]);
      if (lMens <> EmptyStr) and (Pos(lMens, AErrorMessage) > 0) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

  FLastLogValid := Result;
end;

procedure TLogControl.ClearErrorDetected;
var
  lLista : TStringList;

  procedure FindLogFiles(const AMask : string);
  var
    lRec: TSearchRec;
  begin
    if FindFirst(FConfig.OutputDir + AMask, faAnyFile, lRec) = 0 then
    begin
      repeat
        if FileExists(FConfig.OutputDir + lRec.Name) then
          lLista.Add(FConfig.OutputDir + lRec.Name );
      until FindNext(lRec) <> 0;
    end;
    System.SysUtils.FindClose(lRec);
  end;

  procedure ApagaArquivoLog;
  var
    DataArq : TDateTime;
    lFileAtr: TWin32FileAttributeData;
    SystemTime, LocalTime: TSystemTime;
    i : Integer;
  begin
    for i := 0 to lLista.Count - 1 do
    begin
      if GetFileAttributesEx(PChar(lLista.Strings[i]), GetFileExInfoStandard, @lFileAtr) then
      begin
        if FileTimeToSystemTime(lFileAtr.ftCreationTime, SystemTime)
        and SystemTimeToTzSpecificLocalTime(nil, SystemTime, LocalTime) then
        begin
          DataArq := SystemTimeToDateTime(LocalTime);
          if DataArq < (Now - FConfig.LifeTime) then
            DeleteFile(PChar(lLista.Strings[i]));
        end
        else
          DeleteFile( PChar(lLista.Strings[i]) )
      end
      else
        DeleteFile( PChar(lLista.Strings[i]) );
    end;
  end;

begin
  FConfig.SetCheckingError(False);
  FConfig.SetErrorDetected(False);

  if FConfig.LifeTime > 0 then
  try
    lLista := TStringList.Create;
    { logs de erros }
    FindLogFiles('*' + C_DEFAULT_LOG_EXT);
    FindLogFiles('*' + C_DEFAULT_BKP_EXT);
    FindLogFiles(C_DEFAULT_IMG_DIR + '*' + C_DEFAULT_IMG_EXT);
    ApagaArquivoLog;
  finally
    FreeAndNil(lLista);
  end;
end;

constructor TLogControl.Create(const pIniFilePath : string);
begin
  inherited Create;
  FLastLogValid := False;
  FLastStackTrace := TStringList.Create;
  FUserInfo.Load;
  FConfig := TLogConfig.Create(pIniFilePath);

  ClearErrorDetected;
end;

destructor TLogControl.Destroy;
begin
  FreeAndNil(FLastStackTrace);
  FreeAndNil(FConfig);
  inherited;
end;

procedure TLogControl.EnterCriticalArea;
begin
  FInCriticalArea := True;
end;

function TLogControl.FileSize(const aFilename: String): Int64;
var
  lInfo: TWin32FileAttributeData;
  lOk: Boolean;
begin
  Result := -1;
  {$IFDEF CONSOLE}
  lOk := GetFileAttributesEx(PAnsiChar(aFileName), GetFileExInfoStandard, @lInfo);
  {$ELSE}
    {$IFDEF DLLMODE}
    lOk := GetFileAttributesEx(PAnsiChar(aFileName), GetFileExInfoStandard, @lInfo);
    {$ELSE}
      {$IFNDEF UNICODE}
      lOk := GetFileAttributesEx(PAnsiChar(aFileName), GetFileExInfoStandard, @lInfo);
      {$ELSE}
      lOk := GetFileAttributesEx(PWideChar(aFileName), GetFileExInfoStandard, @lInfo);
      {$ENDIF}
    {$ENDIF}
  {$ENDIF}

  if not lOk then
    Exit;

  Result := lInfo.nFileSizeLow or (lInfo.nFileSizeHigh shl 32);
end;

function TLogControl.GetLogBkpFileName(const AFileName: string): string;
var
  lBaseName: string;
  lCount: Integer;
  lRec: TSearchRec;
begin
  lBaseName := StringReplace(AFileName, C_DEFAULT_LOG_EXT, '', [rfIgnoreCase]);
  lCount := 0;
  // renomeia o arquivo para o próximo arquivo de backup
  if FindFirst(lBaseName + C_DEFAULT_BKP_EXT + '*', faAnyFile, lRec) = 0 then
  begin
    repeat
      Inc(lCount);
    until FindNext(lRec) <> 0;
  end;
  Inc(lCount);

  Result := lBaseName + C_DEFAULT_BKP_EXT + IntToStr(lCount);
end;

function TLogControl.GetLogFileName: string;
begin
  Result := FConfig.OutputDir
          + StringReplace(ExtractFileName(Application.ExeName), '.exe', '', [rfIgnoreCase])
          + FormatDateTime('_YYYY-MM-DD_"E"rror', Date) + C_DEFAULT_LOG_EXT;
end;

procedure TLogControl.GetScreenShot(const pNumber: Integer);
var
  lJPG : TJPEGImage;
  lRect : TRect;
  lDC : HDC;
  lCanvas : TCanvas;
  lMyPictureFormat : Word;
  lImgData : NativeUInt;
  lPalette : HPALETTE;
  lImgName: String;

  { adiciona o cursor no paint screen no momento do erro }
  procedure DrawCursor(ScreenShotBitmap : TBitmap);
  var
    lMouseRect: TRect;
    lCursorInfo: TCursorInfo;
    lMouseIcon: TIcon;
    lIconInfo: TIconInfo;
  begin
    lMouseRect := ScreenShotBitmap.Canvas.ClipRect;
    lMouseIcon := TIcon.Create;
    try
      lCursorInfo.cbSize := SizeOf(lCursorInfo);
      if GetCursorInfo(lCursorInfo) then
        if lCursorInfo.Flags = CURSOR_SHOWING then
        begin
          lMouseIcon.Handle := CopyIcon(lCursorInfo.hCursor);
          if GetIconInfo(lMouseIcon.Handle, lIconInfo) then
          begin
            ScreenShotBitmap.Canvas.Draw(
                  lCursorInfo.ptScreenPos.x - Integer(lIconInfo.xHotspot) - lMouseRect.Left,
                  lCursorInfo.ptScreenPos.y - Integer(lIconInfo.yHotspot) - lMouseRect.Top,
                  lMouseIcon
                  );
          end;
        end;
    finally
      if Assigned(lMouseIcon) then
        FreeAndNil(lMouseIcon);
    end;
  end;

begin
  { Tira PaintScreen }
  if FConfig.TakeScreenShot then
  begin
    keybd_event(vk_snapshot, 0, 0, 0); // tecla do paint screen
    with TImage.Create(Application) do
    try
      lJPG := TJPEGImage.Create;
      // 2 or more screens
      Width  := Screen.Width;
      Height := Screen.Height;
      // just one screen
//      Width  := Screen.DesktopWidth;
//      Height := Screen.DesktopHeight;

      lRect := Rect( 0, 0, Width, Height );
      lDC := GetWindowDC( GetDeskTopWindow );
      lCanvas := TCanvas.Create;
      lCanvas.Handle := lDC;
      Canvas.CopyRect( lRect, lCanvas, lRect );
      ReleaseDC( GetDeskTopWindow, lDC );
      DrawCursor(Picture.Bitmap);
      Picture.SaveToClipboardFormat(lMyPictureFormat, lImgData, lPalette);
      lJPG.CompressionQuality := FConfig.ImageQuality;
      lJPG.Assign(Picture.Bitmap);
      lJPG.Compress;

      ForceDirectories(FConfig.OutputDir + C_DEFAULT_IMG_DIR);
      lImgName := FConfig.OutputDir + C_DEFAULT_IMG_DIR + 'Img_'
                + StringReplace(ExtractFileName(Application.ExeName), '.exe', '', [rfIgnoreCase])
                + FormatFloat('_#000000000', pNumber) + C_DEFAULT_IMG_EXT;

      lJPG.SaveToFile( lImgName );
    finally
      if Assigned(lJPG) then
        FreeAndNil(lJPG);
      if Assigned(lCanvas) then
        FreeAndNil(lCanvas);
      Free;
    end;
  end;
end;

procedure TLogControl.LeaveCriticalArea;
begin
  FInCriticalArea := False;
end;

procedure TLogControl.AddLog(pMessage: string; const pFileName : string;
  const pAddExceptionNumber: Boolean; const pLineBreak : Boolean;
  const pTakeScreenShot : Boolean);
var
  LogFile: Text;
  lFileOpen: Boolean;
  lControl: Integer;
  lNumber: Integer;
  lRealLogName: string;
  lFileName: string;
  lMessage: string;
begin
  _LogHookCS.Enter;
  try
    ForceDirectories(FConfig.OutputDir);

    lFileOpen := False;
    try
      // garante que os arquivos fiquem na mesma pasta, mesmo quando o nome é
      // informado incorretamente para outra pasta
      if pFileName = EmptyStr then
        lFileName := GetLogFileName
      else
        lFileName := pFileName;

      lRealLogName := FConfig.OutputDir + ExtractFileName(GetLogFileName);
      ForceDirectories(FConfig.OutputDir);

      if FileExists(lRealLogName) then
      begin
        AssignFile(LogFile, lRealLogName);
        if FileSize(lRealLogName) > (1024 * FConfig.MaxLogSize) then
        begin
          CopyFile(PChar(lRealLogName), PChar( GetLogBkpFileName(lRealLogName) ), False );
          DeleteFile(PChar(lRealLogName));
          ReWrite(LogFile);
          lFileOpen := True;
        end else
        begin
          // alguem pode estar utilizando o log neste momento
          lControl := 0;
          repeat
            try
              Inc(lControl);
              Append(LogFile);
              lFileOpen := True;
            except
              OutputDebugString(PChar('Arquivo de Log em Uso [' + lRealLogName + ']'));
              Sleep(500);
            end;
          until lFileOpen or (lControl > 120); //max 1min tentando liberar o arquivo
        end;
      end
      else
      begin
        AssignFile(LogFile, lRealLogName);
        ReWrite(LogFile);
        lFileOpen := True;
      end;

      if pAddExceptionNumber or pTakeScreenShot then
        lNumber := FConfig.GetNextExceptionNumber
      else
        lNumber := -1;

      if pAddExceptionNumber then
        Writeln(LogFile, Format('Exceção N°: %d)', [lNumber]));

      if pMessage = EmptyStr then
        lMessage := LastStackTrace.Text
      else
        lMessage := pMessage;

      if pLineBreak then
        WriteLn(LogFile, lMessage)
      else
        Write(LogFile, lMessage);

      if pTakeScreenShot then
        GetScreenShot(lNumber);
    finally
      if lFileOpen then
        CloseFile(LogFile);
    end;
  finally
    _LogHookCS.Leave;
  end;

  if DebugHook = 1 then
    OutputDebugString( PChar(lMessage) );
end;

procedure TLogControl.WriteLog(const AMessage, AFileName: string; const ALineBreak : Boolean);
begin
  AddLog(AMessage, AFileName, False, ALineBreak);
end;

initialization
  TLogControl._LogHookCS := TCriticalSection.Create;

finalization
  FreeAndNil(TLogControl._LogHookCS);

end.
