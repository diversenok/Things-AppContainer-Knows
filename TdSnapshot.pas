unit TdSnapshot;

interface

uses
  Winapi.WinNt;

type
  TThreadFlag = (tfGUI);
  TThreadFlags = set of TThreadFlag;

  TThreadData = record
    TID: TThreadID;
    Flags: TThreadFlags;
  end;

// Find all threads that belong to this process
function SnapshotThreads(PID: TProcessId): TArray<TThreadData>;

implementation

uses
  Ntapi.ntdef, Ntapi.ntpsapi, Ntapi.ntobapi, Ntapi.ntstatus,
  NtUtils.Objects, NtUtils.WinUser;

function SnapshotThreads(PID: TProcessId): TArray<TThreadData>;
var
  ProcessInfo, ThreadInfo: TObjectTypeInfo;
  i: Integer;
  CID: TClientId;
  hThread: THandle;
  ObjAttr: TObjectAttributes;
  Status: NTSTATUS;
begin
  Result := nil;

  // Determine the total amount of processes and threads
  if not NtxQueryTypeObject(NtCurrentProcess, ProcessInfo).IsSuccess then
    ProcessInfo.Other.HighWaterNumberOfObjects := 300;

  // Fallback to reasonable limits on failure
  if not NtxQueryTypeObject(NtCurrentThread, ThreadInfo).IsSuccess then
    ThreadInfo.Other.HighWaterNumberOfObjects := 2000;

  InitializeObjectAttributes(ObjAttr);
  CID.UniqueProcess := PID;

  for i := 3 to ProcessInfo.Other.HighWaterNumberOfObjects +
    ThreadInfo.Other.HighWaterNumberOfObjects do
    begin
      // Thread IDs are always divisable by 4
      CID.UniqueThread := i shl 2;

      // Thy this pair of PID + TID
      Status := NtOpenThread(hThread, 0, ObjAttr, CID);

      // NtOpenThread performs a lookup first, and since we supplied both
      // process and thread IDs, the status indicates whether the thread
      // belongs to the process or not.
      if Status <> STATUS_INVALID_CID then
      begin
        // Save it
        SetLength(Result, Succ(Length(Result)));
        Result[High(Result)].TID := CID.UniqueThread;
      end;

      if NT_SUCCESS(Status) then
        NtClose(hThread);
    end;

  // Mark all GUI threads
  for i := 0 to High(Result) do
    if UsrxIsGuiThread(Result[i].TID) then
      Include(Result[i].Flags, tfGUI);
end;

end.
