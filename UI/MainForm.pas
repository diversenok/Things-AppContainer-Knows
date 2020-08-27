unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VclEx.ListView, Vcl.StdCtrls, DelphiUiLib.HysteresisList, PsSnapshot,
  Vcl.ExtCtrls, PsSnapshotThread, Vcl.AppEvnts, Vcl.Menus;

type
  TFormMain = class(TForm)
    Pages: TPageControl;
    TabProcesses: TTabSheet;
    lvProcesses: TListViewEx;
    AppEvents: TApplicationEvents;
    MainMenu: TMainMenu;
    cmProgram: TMenuItem;
    cmAC: TMenuItem;
    cmLPAC: TMenuItem;
    StatusBar: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AppEventsMinimize(Sender: TObject);
    procedure AppEventsRestore(Sender: TObject);
    procedure AppEventsException(Sender: TObject; E: Exception);
    procedure cmACClick(Sender: TObject);
    procedure cmLPACClick(Sender: TObject);
  private
    Processes: THysteresisList<TProcessData>;
    FirstUpdate: Boolean;
    SnapshottingThread: TPsSnapshotThread;
    procedure ColorItem(const Item: TProcessData; Index: Integer);
    procedure AtAddStart(const Item: TProcessData; Index: Integer);
    procedure AtAddFinish(const Item: TProcessData; Index: Integer);
    procedure AtRemoveStart(const Item: TProcessData; Index: Integer);
    procedure AtRemoveFinish(const Item: TProcessData; Index: Integer);
  public
    procedure ConsumeSnapshot(Snapshot: TPsSnapshot);
  end;

var
  FormMain: TFormMain;

implementation

uses
  NtUiLib.Icons, NtUtils.Files, NtUtils.Threads,NtUtils, DelphiUiLib.Strings,
  NtUiLib.Exceptions, NtUiLib.Exceptions.Dialog, MainForm.Logic;

{$R *.dfm}

function PsFlagsToString(Flags: TProcessFlags): String;
var
  Flag: TProcessFlag;
  Count: Integer;
  Names: TArray<String>;
begin
  Count := 0;
  for Flag in Flags do
    Inc(Count);

  SetLength(Names, Count);

  Count := 0;
  for Flag in Flags do
  begin
    case Flag of
      pfWoW64: Names[Count] := 'WoW64';
      pfdotNet: Names[Count] := '.NET';
      pfModernUI: Names[Count] := 'Modern UI';
      pfForeground: Names[Count] := 'Foreground';
      pfInJob: Names[Count] := 'In job';
      pfTerminated: Names[Count] := 'Terminated';
    else
      Continue;
    end;
    Inc(Count);
  end;

  Result := string.Join(', ', Names, 0, Count);
end;

function CompareProcesses(const A, B: TProcessData): Boolean;
begin
  Result := (A.PID = B.PID) and (A.NtFileName = B.NtFileName);
end;

procedure TFormMain.AppEventsException(Sender: TObject; E: Exception);
begin
  ShowNtxException(Handle, E);
end;

procedure TFormMain.AppEventsMinimize(Sender: TObject);
begin
  // We do not want to consume CPU while minimized
  NtxSuspendThread(SnapshottingThread.Handle);
end;

procedure TFormMain.AppEventsRestore(Sender: TObject);
begin
  // Continue snapshotting
  NtxResumeThread(SnapshottingThread.Handle);
end;

procedure TFormMain.AtAddFinish(const Item: TProcessData; Index: Integer);
begin
  ColorItem(Item, Index);
end;

procedure TFormMain.AtAddStart(const Item: TProcessData; Index: Integer);
begin
  with lvProcesses.Items.Add do
  begin
    Cell[0] := Item.ImageName;
    Cell[1] := IntToStr(Item.PID);
    Hint := BuildHint('NT Filename', Item.NtFileName);

    if not FirstUpdate then
      Color := clLime
    else
      ColorItem(Item, Index);

    ImageIndex := TProcessIcons.GetIcon(RtlxNtPathToDosPathUnsafe(
      Item.NtFileName));
  end;
end;

procedure TFormMain.AtRemoveFinish(const Item: TProcessData; Index: Integer);
begin
  lvProcesses.Items.Delete(Index);
end;

procedure TFormMain.AtRemoveStart(const Item: TProcessData; Index: Integer);
begin
  lvProcesses.Items[Index].Color := clRed;
end;

procedure TFormMain.cmACClick(Sender: TObject);
begin
  RestartAsAppContainer(False).RaiseOnError;;
  Close;
end;

procedure TFormMain.cmLPACClick(Sender: TObject);
begin
  RestartAsAppContainer(True).RaiseOnError;
  Close;
end;

procedure TFormMain.ColorItem(const Item: TProcessData; Index: Integer);
begin
  with lvProcesses.Items[Index] do
    if pfTerminated in Item.Flags then
      Color := $AAAAAA
    else if pfdotNet in Item.Flags then
      Color := $00FFDE
    else if pfModernUI in Item.Flags then
      Color := $DBD0FF
    else if pfWoW64 in Item.Flags then
      Color := $A0A8DC
    else if pfInJob in Item.Flags then
      Color := $68ACD9
    else if pfForeground in Item.Flags then
      Color := $AAFFFF
    else
      ColorEnabled := False;
end;

procedure TFormMain.ConsumeSnapshot(Snapshot: TPsSnapshot);
var
  i: Integer;
begin
  Processes.Update(Snapshot.Processes);

  lvProcesses.Items.BeginUpdate;

  for i := 0 to Processes.Count - 1 do
  if Processes[i].State <> hisDeleted then
  begin
    // Flags might change
    lvProcesses.Items[i].Cell[2] := PsFlagsToString(
      Processes.Items[i].Data.Flags);

    // Update colors
    if Processes[i].State = hisExisting then
      ColorItem(Processes.Items[i].Data, i);
  end;

  StatusBar.Panels[1].Text := Format('Processes: %d/%d', [Length(
    Snapshot.Processes), Snapshot.ProcessInfo.Other.TotalNumberOfObjects]);

  FirstUpdate := False;
  lvProcesses.Items.EndUpdate;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NtxQueueApcThread(SnapshottingThread.Handle, RequestShutdown);
  NtxResumeThread(SnapshottingThread.Handle);
  SnapshottingThread.WaitFor;
  SnapshottingThread.Free;
  Processes.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  State: TRunningAs;
begin
  NtxSetNameThread(NtCurrentThread, 'UI');
  lvProcesses.SmallImages := TProcessIcons.ImageList;

  Processes := THysteresisList<TProcessData>.Create(CompareProcesses, 4);
  Processes.OnAddStart := AtAddStart;
  Processes.OnAddFinish := AtAddFinish;
  Processes.OnRemoveStart := AtRemoveStart;
  Processes.OnRemoveFinish := AtRemoveFinish;

  FirstUpdate := True;
  SnapshottingThread := TPsSnapshotThread.Create;

  State := DetermineRunningState;
  StatusBar.Panels[0].Text := RunningStateToString(State);

  // Disable some menu items
  if State >= raAppContainer then
    cmAC.Enabled := False;
  if State >= raLPAC then
    cmLPAC.Enabled := False;
end;

end.
