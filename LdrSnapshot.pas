unit LdrSnapshot;

interface

uses
  NtUtils;

type
  TBaseModule = (
    kmNtdll, kmNtdll32,
    kmMsCoree, kmMsCoree32,
    kmWinUI, kmWinUI32
  );

  // The modules that are used to perform process snapshotting
  TBaseModules = class abstract
    class var Handles: array [TBaseModule] of IHandle;
    class var Initialized: Boolean;
    class procedure Initialize;
  end;

  TModuleData = record
    hxFile: IHandle;
    NtFileName: String;
  end;

  TKnownDllData = record
    FileName: String;
    ImageBase: UIntPtr;
    ImageSize: NativeUInt;
  end;

  TModuleSnapshot = record
    Modules: TArray<TModuleData>;
    KnownDlls, KnownDlls32: TArray<TKnownDllData>;
  end;

implementation

uses
  Winapi.WinNt, Ntapi.ntioapi, NtUtils.Files;

{ TBaseModules }

class procedure TBaseModules.Initialize;
const
  BaseNames: array [TBaseModule] of String = (
    '\SystemRoot\System32\ntdll.dll',
    '\SystemRoot\SysWoW64\ntdll.dll',
    '\SystemRoot\System32\mscoree.dll',
    '\SystemRoot\SysWoW64\mscoree.dll',
    '\SystemRoot\System32\Windows.UI.dll',
    '\SystemRoot\SysWoW64\Windows.UI.dll'
  );
var
  i: TBaseModule;
begin
  if Initialized then
    Exit;

  for i := Low(TBaseModule) to High(TBaseModule) do
    if not NtxOpenFile(Handles[i], FILE_READ_DATA or FILE_READ_ATTRIBUTES,
      BaseNames[i]).IsSuccess then
      Handles[i] := nil;

  Initialized := True;
end;


end.
