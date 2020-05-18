program AppContainerKnows;

uses
  Vcl.Forms,
  DelphiApi.Reflection in 'NtUtils\Headers\DelphiApi.Reflection.pas',
  Ntapi.ntdbg in 'NtUtils\Headers\Ntapi.ntdbg.pas',
  Ntapi.ntdef in 'NtUtils\Headers\Ntapi.ntdef.pas',
  Ntapi.ntexapi in 'NtUtils\Headers\Ntapi.ntexapi.pas',
  Ntapi.ntioapi in 'NtUtils\Headers\Ntapi.ntioapi.pas',
  Ntapi.ntkeapi in 'NtUtils\Headers\Ntapi.ntkeapi.pas',
  Ntapi.ntldr in 'NtUtils\Headers\Ntapi.ntldr.pas',
  Ntapi.ntlpcapi in 'NtUtils\Headers\Ntapi.ntlpcapi.pas',
  Ntapi.ntmmapi in 'NtUtils\Headers\Ntapi.ntmmapi.pas',
  Ntapi.ntobapi in 'NtUtils\Headers\Ntapi.ntobapi.pas',
  Ntapi.ntpebteb in 'NtUtils\Headers\Ntapi.ntpebteb.pas',
  Ntapi.ntpsapi in 'NtUtils\Headers\Ntapi.ntpsapi.pas',
  Ntapi.ntregapi in 'NtUtils\Headers\Ntapi.ntregapi.pas',
  Ntapi.ntrtl in 'NtUtils\Headers\Ntapi.ntrtl.pas',
  Ntapi.ntsam in 'NtUtils\Headers\Ntapi.ntsam.pas',
  Ntapi.ntseapi in 'NtUtils\Headers\Ntapi.ntseapi.pas',
  Ntapi.ntstatus in 'NtUtils\Headers\Ntapi.ntstatus.pas',
  Ntapi.nttmapi in 'NtUtils\Headers\Ntapi.nttmapi.pas',
  Ntapi.ntwow64 in 'NtUtils\Headers\Ntapi.ntwow64.pas',
  Winapi.ConsoleApi in 'NtUtils\Headers\Winapi.ConsoleApi.pas',
  Winapi.ntlsa in 'NtUtils\Headers\Winapi.ntlsa.pas',
  Winapi.NtSecApi in 'NtUtils\Headers\Winapi.NtSecApi.pas',
  Winapi.ProcessThreadsApi in 'NtUtils\Headers\Winapi.ProcessThreadsApi.pas',
  Winapi.Sddl in 'NtUtils\Headers\Winapi.Sddl.pas',
  Winapi.securitybaseapi in 'NtUtils\Headers\Winapi.securitybaseapi.pas',
  Winapi.Shell in 'NtUtils\Headers\Winapi.Shell.pas',
  Winapi.Shlwapi in 'NtUtils\Headers\Winapi.Shlwapi.pas',
  Winapi.Svc in 'NtUtils\Headers\Winapi.Svc.pas',
  Winapi.UserEnv in 'NtUtils\Headers\Winapi.UserEnv.pas',
  Winapi.Wdc in 'NtUtils\Headers\Winapi.Wdc.pas',
  Winapi.WinBase in 'NtUtils\Headers\Winapi.WinBase.pas',
  Winapi.WinError in 'NtUtils\Headers\Winapi.WinError.pas',
  Winapi.WinNt in 'NtUtils\Headers\Winapi.WinNt.pas',
  Winapi.WinSafer in 'NtUtils\Headers\Winapi.WinSafer.pas',
  Winapi.winsta in 'NtUtils\Headers\Winapi.winsta.pas',
  Winapi.WinUser in 'NtUtils\Headers\Winapi.WinUser.pas',
  DelphiUtils.Arrays in 'NtUtils\DelphiUtils.Arrays.pas',
  DelphiUtils.AutoObject in 'NtUtils\DelphiUtils.AutoObject.pas',
  NtUtils in 'NtUtils\NtUtils.pas',
  NtUtils.Access.Expected in 'NtUtils\NtUtils.Access.Expected.pas',
  NtUtils.Files in 'NtUtils\NtUtils.Files.pas',
  NtUtils.Job in 'NtUtils\NtUtils.Job.pas',
  NtUtils.Objects in 'NtUtils\NtUtils.Objects.pas',
  NtUtils.Processes in 'NtUtils\NtUtils.Processes.pas',
  NtUtils.Processes.Memory in 'NtUtils\NtUtils.Processes.Memory.pas',
  NtUtils.Processes.Query in 'NtUtils\NtUtils.Processes.Query.pas',
  NtUtils.Security.Sid in 'NtUtils\NtUtils.Security.Sid.pas',
  NtUtils.System in 'NtUtils\NtUtils.System.pas',
  NtUtils.SysUtils in 'NtUtils\NtUtils.SysUtils.pas',
  NtUtils.Threads in 'NtUtils\NtUtils.Threads.pas',
  NtUtils.Version in 'NtUtils\Headers\NtUtils.Version.pas',
  DelphiUiLib.HysteresisList in 'NtUtils\NtUiLib\DelphiUiLib.HysteresisList.pas',
  DelphiUiLib.Strings in 'NtUtils\NtUiLib\DelphiUiLib.Strings.pas',
  NtUiLib.Icons in 'NtUtils\NtUiLib\NtUiLib.Icons.pas',
  LdrSnapshot in 'LdrSnapshot.pas',
  PsSnapshot in 'PsSnapshot.pas',
  PsSnapshotThread in 'PsSnapshotThread.pas',
  VclEx.ListView in 'VclEx\VclEx.ListView.pas',
  MainForm in 'UI\MainForm.pas' {FormMain};

{$R *.res}

begin
  {$IFDEF Debug}ReportMemoryLeaksOnShutdown := True;{$ENDIF}
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
