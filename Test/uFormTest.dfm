object FormLogHookTest: TFormLogHookTest
  Left = 0
  Top = 0
  Caption = 'LogHook Test'
  ClientHeight = 247
  ClientWidth = 323
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 286
    Height = 91
    Caption = 
      'How to Use:'#13#10'1) Build and Install package "RpLogHookPackage"'#13#10'2)' +
      ' Configure the search path to "$(BDSCOMMONDIR)\Dcu\"'#13#10'3) Add to ' +
      'uses unit list of main .pas file "uLogHook"'#13#10'4) On the "initiali' +
      'zation" section, call the hoog initializer'#13#10'5) On the "finalizat' +
      'ion" section, call the hoog finalizer'#13#10
  end
  object ButtonDivZero: TButton
    Tag = 1
    Left = 8
    Top = 120
    Width = 150
    Height = 35
    Caption = 'Div by Zero'
    TabOrder = 0
    OnClick = ButtonDivZeroClick
  end
  object ButtonXYZ: TButton
    Tag = 1
    Left = 8
    Top = 161
    Width = 150
    Height = 35
    Caption = 'Convert "XYZ" to Int'
    TabOrder = 1
    OnClick = ButtonXYZClick
  end
  object ButtonSkip: TButton
    Tag = 1
    Left = 8
    Top = 202
    Width = 150
    Height = 35
    Caption = 'Skip - ExptionNoInfo'
    TabOrder = 2
    OnClick = ButtonSkipClick
  end
  object ButtonRaise: TButton
    Tag = 1
    Left = 164
    Top = 120
    Width = 150
    Height = 35
    Caption = 'Raise InOutError'
    TabOrder = 3
    OnClick = ButtonRaiseClick
  end
end
