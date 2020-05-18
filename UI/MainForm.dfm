object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Things AppContainer Knows'
  ClientHeight = 453
  ClientWidth = 551
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 545
    Height = 447
    ActivePage = TabProcesses
    Align = alClient
    TabOrder = 0
    object TabProcesses: TTabSheet
      Caption = 'Processes'
      object lbProcesses: TLabel
        Left = 3
        Top = 398
        Width = 131
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Snapshotting in progress...'
      end
      object lvProcesses: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 531
        Height = 383
        Margins.Bottom = 33
        Align = alClient
        Columns = <
          item
            Caption = 'Process Name'
            Width = 200
          end
          item
            Alignment = taCenter
            Caption = 'PID'
            Width = 70
          end
          item
            Caption = 'Flags'
            Width = 220
          end>
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        ColoringItems = True
      end
    end
  end
  object AppEvents: TApplicationEvents
    OnMinimize = AppEventsMinimize
    OnRestore = AppEventsRestore
    Left = 455
    Top = 155
  end
end
