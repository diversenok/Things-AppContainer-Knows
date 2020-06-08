unit MainForm.Logic;

interface

uses
  NtUtils;

type
  TRunningAs = (
    raUnknown,
    raElevated,
    raLimited,
    raAppContainer,
    raLPAC
  );

// Start a new instance is an AppContainer sandbox
function RestartAsAppContainer(LPAC: Boolean): TNtxStatus;

// Determine the avaliable rights
function DetermineRunningState: TRunningAs;
function RunningStateToString(State: TRunningAs): String;

implementation

uses
  Winapi.Winnt, Ntapi.ntseapi, NtUtils.Processes.Create.Win32, NtUtils.Profiles,
  NtUtils.Tokens.Query, NtUtils.Version, NtUtils.Security.Sid;

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

function DetermineRunningState: TRunningAs;
var
  Flags: Cardinal;
  IsLPAC: Boolean;
  LevelSid: ISid;
begin
  Result := raUnknown;

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
    'AppContainer', 'LPAC');
begin
  if (Low(TRunningAs) <= State) and (State <= High(TRunningAs)) then
    Result := NAMES[State]
  else
    Result := '';
end;

end.
