object frmCalcList: TfrmCalcList
  Left = 224
  Top = 123
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = #1048#1089#1090#1086#1088#1080#1103' '#1074#1099#1095#1080#1089#1083#1077#1085#1080#1081
  ClientHeight = 433
  ClientWidth = 368
  Color = clBtnFace
  Font.Charset = RUSSIAN_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    368
    433)
  PixelsPerInch = 120
  TextHeight = 16
  object lblExpressions: TLabel
    Left = 8
    Top = 8
    Width = 77
    Height = 16
    Caption = #1042#1099#1088#1072#1078#1077#1085#1080#1103':'
    FocusControl = lbExpressions
  end
  object lbExpressions: TListBox
    Left = 8
    Top = 28
    Width = 349
    Height = 237
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 16
    TabOrder = 0
    OnClick = lbExpressionsClick
    OnDblClick = lbExpressionsDblClick
  end
  object memoSelExpr: TMemo
    Left = 8
    Top = 272
    Width = 349
    Height = 113
    Anchors = [akLeft, akRight, akBottom]
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object btnOK: TButton
    Left = 8
    Top = 396
    Width = 100
    Height = 30
    Anchors = [akLeft, akBottom]
    Caption = #1042#1099#1073#1088#1072#1090#1100
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btnClose: TButton
    Left = 120
    Top = 396
    Width = 100
    Height = 30
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = #1047#1072#1082#1088#1099#1090#1100
    ModalResult = 2
    TabOrder = 3
  end
end
