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
  Menu = MainMenu
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
    Height = 428
    ActivePage = TabProcesses
    Align = alClient
    TabOrder = 0
    object TabProcesses: TTabSheet
      Caption = 'Processes'
      object lvProcesses: TListViewEx
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 531
        Height = 394
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
        PopupMenu = PopupMenu
        TabOrder = 0
        ViewStyle = vsReport
        OnDblClick = lvProcessesDblClick
        ColoringItems = True
      end
    end
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 434
    Width = 551
    Height = 19
    Panels = <
      item
        Text = 'Unknown'
        Width = 100
      end
      item
        Text = 'Processes: Unknown'
        Width = 140
      end
      item
        Text = 'Threads: Unknown'
        Width = 140
      end
      item
        Text = 'Modules: Unknown'
        Width = 140
      end>
  end
  object AppEvents: TApplicationEvents
    OnException = AppEventsException
    OnMinimize = AppEventsMinimize
    OnRestore = AppEventsRestore
    Left = 455
    Top = 155
  end
  object MainMenu: TMainMenu
    Left = 375
    Top = 155
    object cmProgram: TMenuItem
      Caption = 'Program'
      object cmAC: TMenuItem
        Caption = 'Restart as AppContainer'
        OnClick = cmACClick
      end
      object cmLPAC: TMenuItem
        Caption = 'Restart as LPAC'
        OnClick = cmLPACClick
      end
    end
  end
  object PopupMenu: TPopupMenu
    Left = 375
    Top = 115
    object cmInspect: TMenuItem
      Caption = 'Inspect'
      Default = True
      ShortCut = 13
    end
  end
end
