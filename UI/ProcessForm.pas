unit ProcessForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  VclEx.ListView, Winapi.WinNt, TdSnapshot, WorkerThreads,
  DelphiUiLib.HysteresisList, PsSnapshot;

type
  TFormProcessInfo = class(TForm)
    PageControl: TPageControl;
    ThreadsTab: TTabSheet;
    lvThreads: TListViewEx;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PerformClose(const Sender: TObject);
  private
    FPID: TProcessId;
    Threads: THysteresisList<TThreadData>;
    FirstUpdate: Boolean;
    SnapshottingThread: TTdSnapshotThread;
    procedure ColorItem(const Item: TThreadData; Index: Integer);
    procedure AtAddStart(const Item: TThreadData; Index: Integer);
    procedure AtAddFinish(const Item: TThreadData; Index: Integer);
    procedure AtRemoveStart(const Item: TThreadData; Index: Integer);
    procedure AtRemoveFinish(const Item: TThreadData; Index: Integer);
  public
    property PID: TProcessId read FPID;
    procedure ConsumeSnapshot(Snapshot: TArray<TThreadData>);
    constructor CreateForProcess(AOwner: TComponent; const Data: TProcessData);
  end;

implementation

uses
  NtUtils.Threads, NtUiLib.Icons, MainForm;

{$R *.dfm}

function TdFlagsToString(Flags: TThreadFlags): String;
begin
  if tfGUI in Flags then
    Result := 'GUI'
  else
    Result := '';
end;

function CompareThreads(const A, B: TThreadData): Boolean;
begin
  Result := A.TID = B.TID;
end;

{ TFormProcessInfo }

procedure TFormProcessInfo.AtAddFinish(const Item: TThreadData; Index: Integer);
begin
  ColorItem(Item, Index);
end;

procedure TFormProcessInfo.AtAddStart(const Item: TThreadData; Index: Integer);
begin
  with lvThreads.Items.Add do
  begin
    Cell[0] := IntToStr(Item.TID);
    Cell[1] := TdFlagsToString(Item.Flags);

    if not FirstUpdate then
      Color := clLime
    else
      ColorItem(Item, Index);
  end;
end;

procedure TFormProcessInfo.AtRemoveFinish(const Item: TThreadData;
  Index: Integer);
begin
  lvThreads.Items.Delete(Index);
end;

procedure TFormProcessInfo.AtRemoveStart(const Item: TThreadData;
  Index: Integer);
begin
  lvThreads.Items[Index].Color := clRed;
end;

procedure TFormProcessInfo.ColorItem(const Item: TThreadData; Index: Integer);
begin
  with lvThreads.Items[Index] do
    if tfGUI in Item.Flags then
      Color := $77FFFF
    else
      ColorEnabled := False;
end;

procedure TFormProcessInfo.ConsumeSnapshot(Snapshot: TArray<TThreadData>);
var
  i: Integer;
begin
  Threads.Update(Snapshot);

  lvThreads.Items.BeginUpdate;

  for i := 0 to Threads.Count - 1 do
  if Threads[i].State <> hisDeleted then
  begin
    // Flags might change
    lvThreads.Items[i].Cell[2] := TdFlagsToString(Threads.Items[i].Data.Flags);

    // Update colors
    if Threads[i].State = hisExisting then
      ColorItem(Threads.Items[i].Data, i);
  end;

  FirstUpdate := False;
  lvThreads.Items.EndUpdate;
end;

constructor TFormProcessInfo.CreateForProcess(AOwner: TComponent;
  const Data: TProcessData);
begin
  inherited Create(AOwner);
  Caption := Data.ImageName + ' [' + IntToStr(Data.PID) + ']';
  TProcessIcons.ImageList.GetIcon(TProcessIcons.GetIconByPid(Data.PID), Icon);
  FPID := Data.PID;
end;

procedure TFormProcessInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormMain.OnMainFormClosing.Unsubscribe(PerformClose);
  NtxQueueApcThread(SnapshottingThread.Handle, RequestShutdown);
  NtxResumeThread(SnapshottingThread.Handle);
  SnapshottingThread.WaitFor;
  SnapshottingThread.Free;
  Threads.Free;
  Action := caFree;
end;

procedure TFormProcessInfo.FormCreate(Sender: TObject);
begin
  NtxSetNameThread(NtCurrentThread, 'Threads of ' + Caption);

  Threads := THysteresisList<TThreadData>.Create(CompareThreads, 4);
  Threads.OnAddStart := AtAddStart;
  Threads.OnAddFinish := AtAddFinish;
  Threads.OnRemoveStart := AtRemoveStart;
  Threads.OnRemoveFinish := AtRemoveFinish;

  FirstUpdate := True;
  SnapshottingThread := TTdSnapshotThread.CreateOwned(Self);
  FormMain.OnMainFormClosing.Subscribe(PerformClose);
end;

procedure TFormProcessInfo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #27 then
    Close;
end;

procedure TFormProcessInfo.PerformClose(const Sender: TObject);
begin
  Close;
end;

end.
