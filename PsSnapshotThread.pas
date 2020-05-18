unit PsSnapshotThread;

interface

uses
  System.Classes;

type
  TPsSnapshotThread = class(TThread)
  protected
    procedure Execute; override;
  end;

// The main thread needs to post us an APC when it's time to terminate
procedure RequestShutdown(ApcArgument1, ApcArgument2, ApcArgument3: Pointer);
  stdcall;

implementation

uses
  PsSnapshot, MainForm, Winapi.WinNt, NtUtils.Threads;

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

end.
