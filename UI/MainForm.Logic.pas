unit MainForm.Logic;

interface

uses
  NtUtils;

const
  PARAM_DROP_IMPERSONATION = '/drop-impersonation';

type
  TRunningAs = (
    raUnknown,
    raElevated,
    raLimited,
    raAppContainer,
    raLPAC,
    raChromeRenderer
  );

// Start a new instance is an AppContainer sandbox
function RestartAsAppContainer(LPAC: Boolean): TNtxStatus;

// Start a new instance in a Chrome Renderer sandbox (with bootstrapping)
function RestertAsChromeRendered: TNtxStatus;

// Determine the avaliable rights
function DetermineRunningState: TRunningAs;
function RunningStateToString(State: TRunningAs): String;

implementation

uses
  Winapi.Winnt, Ntapi.ntseapi, NtUtils.Processes.Create.Win32, NtUtils.Profiles,
  NtUtils.Tokens.Query, NtUtils.Version, NtUtils.Security.Sid,
  NtUtils.Tokens, NtUtils.Tokens.Impersonate, DelphiUtils.Arrays,
  Winapi.ProcessThreadsApi, Ntapi.ntpsapi, Ntapi.ntstatus, NtUtils.Threads;

const
  APPCONT_NAME = 'Things-AppContainer-Knows';
  APPCONT_DISPNAME = APPCONT_NAME;
  APPCONT_DESCR = 'A demonstration of retrieving system information from an ' +
    'AppContainer sandbox.';

function RestartAsAppContainer(LPAC: Boolean): TNtxStatus;
var
  Options: TCreateProcessOptions;
  Info: TProcessInfo;
begin
  Options := Default(TCreateProcessOptions);

  // Prepare AppContainer profile/SID
  Result := UnvxCreateDeriveAppContainer(Options.Attributes.AppContainer,
    APPCONT_NAME, APPCONT_DISPNAME, APPCONT_DESCR);

  if not Result.IsSuccess then
    Exit;

  Options.Attributes.LPAC := LPAC;

  // Create the process
  Result := AdvxCreateProcess(ParamStr(0), '', Options, Info);
end;

function GroupToSid(const Group: TGroup): ISid;
begin
  Result := Group.Sid;
end;

function PrivilegeToLuid(const Privilege: TPrivilege): TLuid;
begin
  Result := Privilege.Luid;
end;

function PrepareRendererTokens(out hxProcessToken: IHandle;
  out hxBootstrapToken: IHandle): TNtxStatus;
var
  AllGroups: TArray<TGroup>;
  AllPrivileges: TArray<TPrivilege>;
  User: TGroup;
  NullSid: ISid;
  hxImpToken: IHandle;
begin
  // Convert out current token to impersonation one
  Result := NtxDuplicateToken(hxImpToken, NtCurrentProcessToken,
    TOKEN_ALL_ACCESS, TokenImpersonation, SecurityImpersonation);

  if not Result.IsSuccess then
    Exit;

  // Collect all groups
  Result := NtxQueryGroupsToken(NtCurrentProcessToken, TokenGroups, AllGroups);

  if not Result.IsSuccess then
    Exit;

  // Collect the user's SID
  Result := NtxQueryGroupToken(NtCurrentProcessToken, TokenUser, User);

  if not Result.IsSuccess then
    Exit;

  // We want to disable the user as well
  AllGroups := AllGroups + [User];

  // Prepare the bootrapping token (which is supposed to be restricted,
  // but not to much)
  Result := NtxFilterToken(hxBootstrapToken, hxImpToken.Handle, 0, nil,
    nil, TArray.Map<TGroup, ISid>(AllGroups, GroupToSid));

  if not Result.IsSuccess then
    Exit;

  // Collect all privileges
  Result := NtxQueryPrivilegesToken(NtCurrentProcessToken, AllPrivileges);

  if not Result.IsSuccess then
    Exit;

  // Prepare NULL SID
  Result := RtlxNewSid(NullSid, SECURITY_NULL_SID_AUTHORITY,
    [SECURITY_NULL_RID]);

  if not Result.IsSuccess then
    Exit;

  // Create a fully restricted token for the process, but don't drop its
  // integrity yet.
  Result := NtxFilterToken(
    hxProcessToken,
    NtCurrentProcessToken,
    0,
    TArray.Map<TGroup, ISid>(AllGroups, GroupToSid),
    TArray.Map<TPrivilege, TLuid>(AllPrivileges, PrivilegeToLuid),
    [NullSid]
  );
end;

function RestertAsChromeRendered: TNtxStatus;
var
  Options: TCreateProcessOptions;
  hxBootrapToken: IHandle;
  Info: TProcessInfo;
begin
  Options := Default(TCreateProcessOptions);

  Result := PrepareRendererTokens(Options.hxToken, hxBootrapToken);

  if not Result.IsSuccess then
    Exit;

  // Because of the limited security context, the process needs some
  // bootstrapping (intial impersonation)
  Options.CreationFlags := CREATE_SUSPENDED;

  // Create the process
  Result := AdvxCreateProcess(ParamStr(0), '"' + ParamStr(0) + '" ' +
    PARAM_DROP_IMPERSONATION, Options, Info);

  if not Result.IsSuccess then
    Exit;

  // Let it temporary impersonate a normal token on the main thread to allow it
  // to start normally.
  Result := NtxSafeSetThreadToken(Info.hxThread.Handle, hxBootrapToken.Handle);

  // Reopen the process token
  if Result.IsSuccess then
    Result := NtxOpenProcessToken(Options.hxToken, Info.hxProcess.Handle,
      TOKEN_ADJUST_DEFAULT);

  // Finally, drop its integrity to untrusted
  if Result.IsSuccess then
    Result := NtxSetIntegrityToken(Options.hxToken.Handle,
      SECURITY_MANDATORY_UNTRUSTED_RID);

  // Let it run or cancel everything
  if Result.IsSuccess then
    NtxResumeThread(Info.hxThread.Handle)
  else
    NtxTerminateThread(Info.hxThread.Handle, STATUS_CANCELLED);
end;

function DetermineRunningState: TRunningAs;
var
  Flags: Cardinal;
  IsLPAC: Boolean;
  LevelSid: ISid;
begin
  Result := raUnknown;

  // Running as Chrome Renderer we won't even be able to open our token
  if ParamStr(1) = PARAM_DROP_IMPERSONATION then
    Exit(raChromeRenderer);

  // Check AppContainer and LPAC
  if RtlOsVersionAtLeast(OsWin8) then
  begin
    if not NtxQueryFlagsToken(NtCurrentEffectiveToken, Flags).IsSuccess then
      Exit;

    // Is AppContainer
    if (Flags and TOKEN_LOWBOX <> 0) then
    begin
      if not NtxQueryLpacToken(NtCurrentEffectiveToken, IsLPAC).IsSuccess then
        Exit;

      if IsLPAC then
        Exit(raLPAC)
      else
        Exit(raAppContainer);
    end;
  end;

  // Use integrity to determine
  if not NtxQuerySidToken(NtCurrentEffectiveToken, TokenIntegrityLevel,
    LevelSid).IsSuccess then
    Exit;

  if RtlxRidSid(LevelSid.Data) > SECURITY_MANDATORY_MEDIUM_RID then
    Result := raElevated
  else
    Result := raLimited;
end;

function RunningStateToString(State: TRunningAs): String;
const
  NAMES: array [TRunningAs] of String = ('Unknown', 'Elevated', 'Non-elevated',
    'AppContainer', 'LPAC', 'Chrome Renderer');
begin
  if (Low(TRunningAs) <= State) and (State <= High(TRunningAs)) then
    Result := NAMES[State]
  else
    Result := '';
end;

end.
