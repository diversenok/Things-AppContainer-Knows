unit WorkerThreads;

interface

uses
  System.Classes, Winapi.WinNt;

type
  TPsSnapshotThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TTdSnapshotThread = class(TThread)
    FOwner: TObject; // should be TFormProcessInfo
  protected
    procedure Execute; override;
  public
    constructor CreateOwned(Owner: TObject);
  end;

// The main thread needs to post us an APC when it's time to terminate
procedure RequestShutdown(ApcArgument1, ApcArgument2, ApcArgument3: Pointer);
  stdcall;

implementation

uses
  PsSnapshot, TdSnapshot, MainForm, NtUtils.Threads, ProcessForm;

procedure RequestShutdown(ApcArgument1, ApcArgument2, ApcArgument3: Pointer);
  stdcall;
begin
  TThread.Current.Terminate;
end;

{ TPsSnapshotThread }

procedure TPsSnapshotThread.Execute;
var
  Processes: TPsSnapshot;
begin
  Priority := tpLower;
  NtxSetNameThread(NtCurrentThread, 'Process Snapshotting');

  repeat
    // Perform snapshotting (might take some time)
    Processes := TPsSnapshot.Create;
    Processes.Snapshot;

    // Notify the UI to update the process list
    Synchronize(
      procedure ()
      begin
        FormMain.ConsumeSnapshot(Processes);
      end
    );

    Processes.Free;

    // Sleep until it's time to snapshot again. We wait in an alertable state,
    // therefore the main thread can wake us up to request termination.
    NtxSleep(1000 * MILLISEC, True);
  until Terminated;
end;

{ TTdSnapshotThread }

constructor TTdSnapshotThread.CreateOwned(Owner: TObject);
begin
  inherited Create;
  FOwner := Owner;
end;

procedure TTdSnapshotThread.Execute;
var
  Owner: TFormProcessInfo;
  Threads: TArray<TThreadData>;
begin
  Priority := tpLower;
  Owner := TFormProcessInfo(FOwner);
  NtxSetNameThread(NtCurrentThread, 'Thread snapshotting for ' + Owner.Caption);

  repeat
    Threads := SnapshotThreads(Owner.PID);

    // Notify the UI to update the process list
    Synchronize(
      procedure ()
      begin
        Owner.ConsumeSnapshot(Threads);
      end
    );

    // Sleep until it's time to snapshot again. We wait in an alertable state,
    // therefore the main thread can wake us up to request termination.
    NtxSleep(1500 * MILLISEC, True);
  until Terminated;
end;

end.
