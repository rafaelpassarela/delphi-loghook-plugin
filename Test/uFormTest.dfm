object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
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
  object ButtonStart: TButton
    Left = 8
    Top = 8
    Width = 150
    Height = 35
    Caption = 'Start LogHook'
    TabOrder = 0
    OnClick = ButtonStartClick
  end
  object ButtonDivZero: TButton
    Left = 8
    Top = 112
    Width = 150
    Height = 35
    Caption = 'Div by Zero'
    TabOrder = 1
    OnClick = ButtonDivZeroClick
  end
end
