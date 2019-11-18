object FormLogHookTest: TFormLogHookTest
  Left = 0
  Top = 0
  Caption = 'LogHook Test'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBoxDirect: TGroupBox
    Left = 8
    Top = 0
    Width = 169
    Height = 103
    Caption = ' Direct '
    TabOrder = 0
    object ButtonStart: TButton
      Left = 8
      Top = 16
      Width = 150
      Height = 35
      Caption = 'Start LogHook'
      TabOrder = 0
      OnClick = ButtonStartClick
    end
    object ButtonStop: TButton
      Left = 8
      Top = 57
      Width = 150
      Height = 35
      Caption = 'Stop LogHook'
      TabOrder = 1
      OnClick = ButtonStopClick
    end
  end
  object ButtonDivZero: TButton
    Tag = 1
    Left = 8
    Top = 176
    Width = 150
    Height = 35
    Caption = 'Div by Zero'
    TabOrder = 1
    OnClick = ButtonDivZeroClick
  end
  object ButtonXYZ: TButton
    Tag = 1
    Left = 8
    Top = 217
    Width = 150
    Height = 35
    Caption = 'Convert "XYZ" to Int'
    TabOrder = 2
    OnClick = ButtonXYZClick
  end
  object ButtonSkip: TButton
    Tag = 1
    Left = 8
    Top = 258
    Width = 150
    Height = 35
    Caption = 'Skip - ExptionNoInfo'
    TabOrder = 3
    OnClick = ButtonSkipClick
  end
  object ButtonRaise: TButton
    Tag = 1
    Left = 164
    Top = 176
    Width = 150
    Height = 35
    Caption = 'Raise InOutError'
    TabOrder = 4
    OnClick = ButtonRaiseClick
  end
  object GroupBoxDLL: TGroupBox
    Left = 183
    Top = 0
    Width = 169
    Height = 103
    Caption = ' DLL Call'
    TabOrder = 5
    object ButtonStartDLL: TButton
      Left = 8
      Top = 16
      Width = 150
      Height = 35
      Caption = 'Start LogHook'
      TabOrder = 0
      OnClick = ButtonStartDLLClick
    end
    object ButtonStopDLL: TButton
      Left = 8
      Top = 57
      Width = 150
      Height = 35
      Caption = 'Stop LogHook'
      TabOrder = 1
      OnClick = ButtonStopDLLClick
    end
  end
end
