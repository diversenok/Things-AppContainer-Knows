unit PsSnapshot;

interface

uses
  Winapi.WinNt, NtUtils.Objects, System.Generics.Collections,
  DelphiUtils.Arrays;

type
  TProcessFlag = (pfWoW64, pfdotNet, pfModernUI, pfForeground, pfInJob,
    pfTerminated);

  TProcessFlags = set of TProcessFlag;

  TProcessData = record
    PID: TProcessId;
    ImageName: String;
    NtFileName: String;
    Flags: TProcessFlags;
  end;

  TThreadData = record
    PID: TProcessId;
  end;

  TPsSnapshot = class
  private
    PidToIndex: TDictionary<TProcessId, Integer>;
    function AddProcess(PID: TProcessId; Callback: TItemCallback<TProcessData>):
      Integer;
    procedure AddFile(hFile: THandle; Callback: TItemCallback<TProcessData>);
  public
    Processes: TArray<TProcessData>;
    ProcessInfo, ThreadInfo: TObjectTypeInfo;
    constructor Create;
    destructor Destroy; override;
    procedure Snapshot;
  end;

implementation

uses
  Ntapi.ntpsapi, LdrSnapshot, NtUtils, NtUtils.Files, NtUtils.Job,
  NtUtils.Processes.Query, System.SysUtils;

const
  SYSTEM_IDLE_PID = 0;
  SYSTEM_PID = 4;

{ Process callbacks }

procedure SetWoW64Flag(var Process: TProcessData);
begin
  Include(Process.Flags, pfWoW64);
end;

procedure SetDotNetFlag(var Process: TProcessData);
begin
  Include(Process.Flags, pfdotNet);
end;

procedure SetModernUIFlag(var Process: TProcessData);
begin
  Include(Process.Flags, pfModernUI);
end;

procedure SetForegroundFlag(var Process: TProcessData);
begin
  Include(Process.Flags, pfForeground);
end;

procedure SetJobFlag(var Process: TProcessData);
begin
  Include(Process.Flags, pfInJob);
end;

procedure SetTerminatedFlag(var Process: TProcessData);
begin
  Include(Process.Flags, pfTerminated);
end;

procedure SetImageName(var Process: TProcessData);
begin
  with Process do
    case PID of
      SYSTEM_IDLE_PID: ImageName := 'System Idle Process';
      SYSTEM_PID: ImageName := 'System';
    else
      if NtxQueryImageNameProcessId(PID, NtFileName).IsSuccess then
        ImageName := ExtractFileName(NtFileName)
      else
        ImageName := 'Unknown';
    end;
end;

{ TPsSnapshot }

procedure TPsSnapshot.AddFile(hFile: THandle;
  Callback: TItemCallback<TProcessData>);
var
  PIDs: TArray<TProcessId>;
  i: Integer;
begin
  if NtxEnumerateUsingProcessesFile(hFile, PIDs).IsSuccess then
    for i := 0 to High(PIDs) do
      AddProcess(PIDs[i], Callback);
end;

function TPsSnapshot.AddProcess(PID: TProcessId;
  Callback: TItemCallback<TProcessData>): Integer;
begin
  // Try to locate existing entry
  if not PidToIndex.TryGetValue(PID, Result) then
  begin
    // Allocate a new entry
    SetLength(Processes, Length(Processes) + 1);
    Result := High(Processes);
    PidToIndex.Add(PID, Result);

    // Fill the default fields
    Processes[Result].PID := PID;
  end;

  // Invoke item update
  if Assigned(Callback) then
    Callback(Processes[Result]);
end;

constructor TPsSnapshot.Create;
begin
  PidToIndex := TDictionary<TProcessId, Integer>.Create;
end;

destructor TPsSnapshot.Destroy;
begin
  PidToIndex.Free;
  inherited;
end;

procedure TPsSnapshot.Snapshot;
var
  b: TBaseModule;
  Callback: TItemCallback<TProcessData>;
  IsInJob: Boolean;
  PIDs: TArray<TProcessId>;
  PID: TProcessId;
  i, j: Integer;
  NtFileName: String;
begin
  AddProcess(SYSTEM_IDLE_PID, nil);
  AddProcess(SYSTEM_PID, nil);

  // Enumerate all processes that have ntdll, mscoree, etc.
  for b := Low(TBaseModule) to High(TBaseModule) do
    if Assigned(TBaseModules.Handles[b]) then
    begin
      case b of
        kmNtdll32:              Callback := SetWoW64Flag;
        kmMsCoree, kmMsCoree32: Callback := SetDotNetFlag;
        kmWinUI, kmWinUI32:     Callback := SetModernUIFlag;
      else
        Callback := nil;
      end;

      AddFile(TBaseModules.Handles[b].Handle, Callback);
    end;

  // Mark active (foreground) process
  AddProcess(USER_SHARED_DATA.ConsoleSessionForegroundProcessID,
    SetForegroundFlag);

  // Mark other processes from the same job
  if NtxIsProcessInJob(IsInJob, NtCurrentProcess).IsSuccess and
    IsInJob and NtxEnumerateProcessesInJob(0, PIDs).IsSuccess then
    for i := 0 to High(PIDs) do
      AddProcess(PIDs[i], SetJobFlag);

  // Determine the total amount of processes and threads
  if not NtxQueryTypeObject(NtCurrentProcess, ProcessInfo).IsSuccess then
    ProcessInfo.Other.HighWaterNumberOfObjects := 300;

  // Fallback to reasonable limits on failure
  if not NtxQueryTypeObject(NtCurrentThread, ThreadInfo).IsSuccess then
    ThreadInfo.Other.HighWaterNumberOfObjects := 2000;

  // Determine image names for found processes
  for i := 0 to High(Processes) do
    SetImageName(Processes[i]);

  // Bruteforce PIDs to find terminated processes
  for i := 3 to ProcessInfo.Other.HighWaterNumberOfObjects +
    ThreadInfo.Other.HighWaterNumberOfObjects do
    begin
      // Process IDs are always divisable by 4
      PID := i shl 2;

      // Include anything that we can query image name of
      if not PidToIndex.ContainsKey(PID) and
        NtxQueryImageNameProcessId(PID, NtFileName).IsSuccess then
      begin
        // TODO: Pico processes tend to fall into this category as well

        j := AddProcess(PID, SetTerminatedFlag);
        Processes[j].NtFileName := NtFileName;
        Processes[j].ImageName := ExtractFileName(NtFileName);

        // Stop if we found all the processes already
        if Length(Processes) >= ProcessInfo.Other.TotalNumberOfObjects then
          Break;
      end;
    end;

  // Fix terminated status for minimal processes
  for i := 0 to High(Processes) do
    if (pfTerminated in Processes[i].Flags) and
      not Processes[i].ImageName.EndsWith('.exe') then
        Exclude(Processes[i].Flags, pfTerminated);
end;

end.
