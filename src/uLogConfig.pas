unit uLogConfig;

interface

uses
  System.Classes, System.SysUtils, IniFiles, uLogConsts, uRpJclSysInfo;

type
  TLogConfig = class
  private
    FActive: Boolean;
    FIgnoredTypes: string;
    FIgnoredMessages: string;
    FIgnoredMensList: TStringList;
    FMaxLogSize: Integer;
    FLifeTime: Integer;
    FCheckingError : Boolean;
    FErrorDetected : Boolean;
    FConfigFilePath : string;
    FLogFilesDir: string;
    FOutputDir: string;
    FTakeScreenShot: Boolean;
    FImageQuality: Integer;
//    FServerURL: string;
    procedure Load(const AFirstLoading : Boolean = True);
  public
    constructor Create(const pIniFilePath : string);
    destructor Destroy; override;

    property Active : Boolean read FActive;
    property IgnoredTypes : string read FIgnoredTypes;
    property IgnoredMessages : string read FIgnoredMessages write FIgnoredMessages;
    property MaxLogSize : Integer read FMaxLogSize;
    property LifeTime : Integer read FLifeTime write FLifeTime;
    property CheckingError : Boolean read FCheckingError; // write FCheckingError;
    property ErrorDetected : Boolean read FErrorDetected; // write FErrorDetected;
    property IgnoredMensList : TStringList read FIgnoredMensList;
    property LogFilesDir : string read FLogFilesDir;
    property OutputDir : string read FOutputDir;
    property TakeScreenShot : Boolean read FTakeScreenShot;
    property ImageQuality : Integer read FImageQuality;

    function GetNextExceptionNumber : Integer;
    procedure ReLoadConfig;
    procedure SetErrorDetected(const Value : Boolean);
    procedure SetCheckingError(const Value : Boolean);
  end;

implementation

{ TLogConfig }

constructor TLogConfig.Create(const pIniFilePath : string);
begin
  FConfigFilePath := pIniFilePath;
  FIgnoredMensList := TStringList.Create;
  FIgnoredMensList.StrictDelimiter := True;
  FIgnoredMensList.Delimiter := ';';
  FLogFilesDir := EmptyStr;
  FOutputDir := EmptyStr;
  FTakeScreenShot := False;

  Load;

  FErrorDetected := False;
  FCheckingError := False;
end;

destructor TLogConfig.Destroy;
begin
  FreeAndNil(FIgnoredMensList);

  inherited;
end;

function TLogConfig.GetNextExceptionNumber: Integer;
var
  lIni : TIniFile;
begin
  lIni := TIniFile.Create(FConfigFilePath);
  try
    Result := lIni.ReadInteger(C_SECTION_CONFIG, C_IDENT_COUNT, 0) + 1;
    lIni.WriteInteger(C_SECTION_CONFIG, C_IDENT_COUNT, Result);
  finally
    FreeAndNil(lIni);
  end;
end;

procedure TLogConfig.Load(const AFirstLoading: Boolean);
var
  i: Integer;
begin
  FOutputDir := IncludeTrailingPathDelimiter(ExtractFilePath(FConfigFilePath))
              + C_DEFAULT_LOG_DIR;

  with TIniFile.Create( FConfigFilePath ) do
  try
    // grava os valores default de configuracao
    if AFirstLoading and (not FileExists(FConfigFilePath)) then
    begin
      WriteBool(C_SECTION_CONFIG,    C_IDENT_ACTIVE,        False);
      WriteBool(C_SECTION_CONFIG,    C_IDENT_GET_IMAGE,     True);
      WriteInteger(C_SECTION_CONFIG, C_IDENT_IMAGE_QUALITY, C_DEFAULT_IMAGE_QUALITY);
      WriteInteger(C_SECTION_CONFIG, C_IDENT_LOGSIZE,       C_DEFAULT_LOG_SIZE);
      WriteInteger(C_SECTION_CONFIG, C_IDENT_LOGLIFETIME,   C_DEFAULT_LIFETIME);
      WriteString(C_SECTION_CONFIG,  C_IDENT_IGNORELIST,    C_DEFAULT_IGNORE_LIST);
      WriteString(C_SECTION_CONFIG,  C_IDENT_LOG_FILES_DIR, FOutputDir);

      WriteBool(C_SECTION_LOG, C_IDENT_CHECKINGERROR,  False);
      WriteBool(C_SECTION_LOG, C_IDENT_ERRORDETECTED,  False);
    end;

    FActive          := ReadBool(C_SECTION_CONFIG,    C_IDENT_ACTIVE,         False);
    FTakeScreenShot  := ReadBool(C_SECTION_CONFIG,    C_IDENT_GET_IMAGE,      True);
    FIgnoredTypes    := ReadString(C_SECTION_CONFIG,  C_IDENT_IGNORELIST,     C_DEFAULT_IGNORE_LIST);
    FIgnoredMessages := ReadString(C_SECTION_CONFIG,  C_IDENT_IGNOREMESSAGES, '');
    FImageQuality    := ReadInteger(C_SECTION_CONFIG, C_IDENT_IMAGE_QUALITY,  C_DEFAULT_IMAGE_QUALITY);

    FIgnoredMensList.Clear;
    FIgnoredMensList.DelimitedText := FIgnoredMessages;

    for i := FIgnoredMensList.Count - 1 downto 0 do
      if FIgnoredMensList[i] = EmptyStr then
        FIgnoredMensList.Delete(i);

    if FIgnoredMensList.Count = 0 then
      FIgnoredMensList.DelimitedText := 'Unable to complete network request to host;<<>>Connection Closed Gracefully';

    // garante que não se exclua todas as Exceptions
    if AFirstLoading and (Pos(';Exception;', FIgnoredTypes) > 0) or (Pos('Exception; ', FIgnoredTypes) > 0) then
    begin
      WriteString(C_SECTION_CONFIG,  C_IDENT_IGNORELIST, C_DEFAULT_IGNORE_LIST);
      FIgnoredTypes := C_DEFAULT_IGNORE_LIST;
    end;
    FIgnoredTypes := FIgnoredTypes + ';ExceptionNoInfo;';

    FMaxLogSize    := ReadInteger(C_SECTION_CONFIG, C_IDENT_LOGSIZE,     C_DEFAULT_LOG_SIZE);
    FLifeTime      := ReadInteger(C_SECTION_CONFIG, C_IDENT_LOGLIFETIME, C_DEFAULT_LIFETIME);

    if (FMaxLogSize <= 0) or (FMaxLogSize > C_LOGSIZE_LIMIT) then // 10Mb
      FMaxLogSize := C_DEFAULT_LOG_SIZE;

    FOutputDir := ReadString(C_SECTION_CONFIG,  C_IDENT_LOG_FILES_DIR, FOutputDir);

    if AFirstLoading then
      ForceDirectories(FOutputDir);
  finally
    Free;
  end;
end;

procedure TLogConfig.ReLoadConfig;
begin
  Load(False);
end;

procedure TLogConfig.SetCheckingError(const Value: Boolean);
begin
  FCheckingError := Value;
end;

procedure TLogConfig.SetErrorDetected(const Value: Boolean);
begin
  FErrorDetected := Value;
end;

end.
