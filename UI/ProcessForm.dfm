object FormProcessInfo: TFormProcessInfo
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Process Information'
  ClientHeight = 305
  ClientWidth = 288
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 282
    Height = 299
    ActivePage = ThreadsTab
    Align = alClient
    TabOrder = 0
    object ThreadsTab: TTabSheet
      Caption = 'Threads'
      object lvThreads: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 268
        Height = 265
        Align = alClient
        Columns = <
          item
            Caption = 'Thread ID'
            Width = 120
          end
          item
            Caption = 'Flags'
            Width = 100
          end>
        DoubleBuffered = True
        GridLines = True
        MultiSelect = True
        ReadOnly = True
        RowSelect = True
        ParentDoubleBuffered = False
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
      end
    end
  end
end
