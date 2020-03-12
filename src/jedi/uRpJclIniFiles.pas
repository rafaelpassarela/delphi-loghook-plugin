unit uRpJclIniFiles;

{$I jcl.inc}

interface

uses
  {$IFDEF HAS_UNITSCOPE}
  System.SysUtils, System.Classes, System.IniFiles;
  {$ELSE ~HAS_UNITSCOPE}
  SysUtils, Classes, IniFiles;
  {$ENDIF ~HAS_UNITSCOPE}

// Initialization (ini) Files
function IniReadBool(const FileName, Section, Line: string): Boolean;              // John C Molyneux
function IniReadInteger(const FileName, Section, Line: string): Integer;           // John C Molyneux
function IniReadString(const FileName, Section, Line: string): string;             // John C Molyneux
procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);     // John C Molyneux
procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);  // John C Molyneux
procedure IniWriteString(const FileName, Section, Line, Value: string);            // John C Molyneux

// Initialization (ini) Files helper routines
procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);

implementation

// Initialization Files
function IniReadBool(const FileName, Section, Line: string): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadBool(Section, Line, False);
  finally
    Ini.Free;
  end;
end;

function IniReadInteger(const FileName, Section, Line: string): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadInteger(Section, Line, 0);
  finally
    Ini.Free;
  end;
end;

function IniReadString(const FileName, Section, Line: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadString(Section, Line, '');
  finally
    Ini.Free;
  end;
end;

procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteBool(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

procedure IniWriteString(const FileName, Section, Line, Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString(Section, Line, Value);
  finally
    Ini.Free;
  end;
end;

// Initialization (ini) Files helper routines
const
  ItemCountName = 'Count';

procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  Count, I: Integer;
begin
  with IniFile do
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      Count := ReadInteger(Section, ItemCountName, 0);
      for I := 0 to Count - 1 do
        Strings.Add(ReadString(Section, IntToStr(I), ''));
    finally
      Strings.EndUpdate;
    end;
  end;
end;

procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  with IniFile do
  begin
    EraseSection(Section);
    WriteInteger(Section, ItemCountName, Strings.Count);
    for I := 0 to Strings.Count - 1 do
      WriteString(Section, IntToStr(I), Strings[I]);
  end;
end;

end.
