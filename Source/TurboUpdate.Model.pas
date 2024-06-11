{******************************************************************************}
{                           ErrorSoft TurboUpdate                              }
{                          ErrorSoft(c)  2016-2017                             }
{                                                                              }
{                     More beautiful things: errorsoft.org                     }
{                                                                              }
{           errorsoft@mail.ru | vk.com/errorsoft | github.com/errorcalc        }
{              errorsoft@protonmail.ch | habrahabr.ru/user/error1024           }
{                                                                              }
{             Open this on github: github.com/errorcalc/TurboUpdate            }
{                                                                              }
{ You can order developing vcl/fmx components, please submit requests to mail. }
{ Âû ìîæåòå çàêàçàòü ðàçðàáîòêó VCL/FMX êîìïîíåíòà íà çàêàç.                   }
{******************************************************************************}
{                                                                              }
{Modidicado por Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate}
{Modified by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate   }
{******************************************************************************}
unit TurboUpdate.Model;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.IniFiles,
  System.SyncObjs,
  System.SysUtils,
  System.Zip,

  TurboUpdate.Model.Consts,
  TurboUpdate.Model.Internet,
  TurboUpdate.Model.Interfaces,
  TurboUpdate.Model.Language.Interfaces,
  TurboUpdate.Model.Types,
  TurboUpdate.Model.Utils;

type
  TUpdater = class(TInterfacedPersistent, IUpdateModel)
  protected type
    TUpdateResult = (Success, Fail, TryAgain, Abort);
  private
    FLaunchUpdateApp : TFileName; // Adicionado por renato trevisan dia 10/06/2024 16:42:20
    FDownloadPath    : string;
    FUrls            : TStringArray;
    FExeNames        : TStringArray;
    FKeyName         : string;
    FIsAbort         : Boolean;
    FRootPath        : string;
    FUpdateFile      : string;
  protected
    FView  : IUpdateView;
    FConsts: IFactoryConsts;
    // Internal routlines
    procedure SyncView(Proc: TThreadProcedure);          // perfect
    procedure SyncShowView;                              // perfect
    procedure SyncCloseView;                             // perfect
    function SyncErrorMessage(Text: string)   : Boolean; // perfect
    function GetUpdateFileName                : string;  // perfect
    function ExtractAll(AStrins: TStringArray): Boolean;
    // ... routlines
    function DoUpdate                       : TUpdateResult; virtual; // perfect
    function GetDownloadInfo                : Boolean; virtual; // perfect
    function Download                       : Boolean; virtual; // perfect
    function Unpacking                      : Boolean; virtual; // perfect
    procedure DeleteFiles;   // perfect
    procedure Done; virtual; // perfect
    { IUpdateModel }
    procedure Cancel; virtual; // perfect
  public
    constructor Create(View: IUpdateView; UpdateInfo: TUpdateInfo); virtual;
    destructor Destroy; override;
    procedure Update;
    procedure UpdateFromFile(FileName: string);
  end;

implementation

uses
  TurboUpdate.Model.Update.Thread; // added by renato trevisan

{$HINTS OFF}
function FileToOld(FileName: string): Boolean;
const
  Suffics = '.old';
begin
  if FileExists(FileName + Suffics) then
   if not DeleteFile(FileName + Suffics) then
     Exit(False);

  if FileExists(FileName) then
    if not RenameFile(FileName, FileName + Suffics) then
      Exit(False);

  Result := True;
end;
{$HINTS ON}

{ TUpdateModel }
procedure TUpdater.Cancel;
begin
  FIsAbort := True;
end;

procedure TUpdater.SyncCloseView;
begin
  SyncView(
   procedure
    begin
      FView.Close;
    end);
end;

constructor TUpdater.Create(View: IUpdateView; UpdateInfo: TUpdateInfo);
begin
  FLaunchUpdateApp := ExtractFileName(ParamStr(0)); //adicionado por renato trevisan dia 10/06/2024 16:30:00
  FConsts          := TFactoryConsts.New;
  // Info
  FUrls     := UpdateInfo.Urls;
  FKeyName  := UpdateInfo.KeyName;
  FExeNames := UpdateInfo.ExeNames + [ExtractFileName(ParamStr(0))];
  if UpdateInfo.RootPath.IsEmpty then //adicionado por renato trevisan dia 11/06/2024 14:00:30
   FRootPath := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)) + PathDelim + UpdateInfo.RootPath) else
   FRootPath := IncludeTrailingPathDelimiter(ExtractFileDir(ParamStr(0)) + PathDelim);

  // set View
  Self.FView := View;

  SyncView(
    procedure
    begin
      // set Model
      View.Model := Self;

      // State
      View.State := TUpdateState.Waiting;
      // Description
      if UpdateInfo.Description <> '' then
        View.Description := UpdateInfo.Description
      else
        View.Description := UpdateInfo.KeyName;
      // PngRes
      if UpdateInfo.PngRes <> '' then
        View.PngRes := UpdateInfo.PngRes;
      // Progress
      View.Progress(0, 1);
      // Status
      View.Status := FConsts.Consts.WaitingStatus;
      // Version
      View.Version := '';
    end);
end;

procedure TUpdater.DeleteFiles;
begin
  if FileExists(GetUpdateFileName) then
    DeleteFile(GetUpdateFileName);
end;

destructor TUpdater.Destroy;
begin
  FView.Model := nil;
  inherited;
end;

procedure TUpdater.Done;
begin
  // View
  SyncView(
    procedure
    begin
      FView.State   := TUpdateState.Done;
      FView.Status  := FConsts.Consts.DoneStatus;
      FView.ShowMessage(FConsts.Consts.DoneMessage);
      // open new update by application or app in inno setup //add by Francisco Aurino in 17/12/2022 16:25:43
      TurboUpdate.Model.Utils.LaunchUpdateApp(FLaunchUpdateApp, True);
      // Adicionado por Renato Trevisan dia 11/06/2024 15:48:57
      TurboUpdate.Model.Utils.Killtask(FLaunchUpdateApp);
    end);
end;

function TUpdater.Download: Boolean;
begin
  // View
  SyncView(
    procedure
    begin
      FView.State := TUpdateState.Downloading;
      FView.Status := FConsts.Consts.DownloadingStatus;
    end);

  try
    Result := DowloadFile(FDownloadPath, GetUpdateFileName,
     procedure(Length, Progress: Int64; var Abort: Boolean)
      begin
        Abort := FIsAbort;
        // View
        SyncView(
          procedure
          begin
            FView.Progress(Progress, Length);
          end);
      end);
  except
    Result := False;
  end;
end;

function TUpdater.ExtractAll(AStrins: TStringArray): Boolean;
const
  CExtractParameter = '&ALL';
var
  LStringList : TStringList;
  LFindIndex  : integer;
begin
 // Adicionado por Renato Trevisan dia 11/06/2024 14:30:35
 LStringList := TStringList.Create;
  try
   LStringList.AddStrings(AStrins);
   Result := LStringList.Find(CExtractParameter, LFindIndex);
  finally
   FreeAndNil(LStringList);
  end;
end;

function TUpdater.GetDownloadInfo: Boolean;
var
  FileVersion: TFileVersion;
begin
  // View
  SyncView(
    procedure
    begin
      FView.State := TUpdateState.Waiting;
      FView.Status := FConsts.Consts.WaitingStatus;
    end);

  if GetUpdateVersion(FUrls, FKeyName, FileVersion) then
    SyncView(
      procedure
      begin
        FView.Version := Format(FConsts.Consts.Version, [FileVersion.ToString]);
      end);

  FDownloadPath := GetUpdateUrl(FUrls, FKeyName);
  Result        := FDownloadPath <> '';
end;

function TUpdater.SyncErrorMessage(Text: string): Boolean;
var
  R: Boolean;
begin
  SyncView(
    procedure
    begin
      R := FView.ShowErrorMessage(Text);
    end);

  Result := R;
end;

procedure TUpdater.SyncShowView;
begin
  SyncView(
    procedure
    begin
      FView.Show;
    end);
end;

procedure TUpdater.SyncView(Proc: TThreadProcedure);
begin
  if FView <> nil then
    TThread.Synchronize(nil, Proc);
end;

function TUpdater.Unpacking: Boolean;
var
  LZipFile     : TZipFile;
  LForIndex    : Integer;
  LFullFileName: string;
  LFileName    : string;
  LExeName     : string;
begin
  Result := True;
  // View
  SyncView(
   procedure
    begin
      FView.State  := TUpdateState.Unpacking;
      FView.Status := FConsts.Consts.UnpackingStatus;
    end);
 // Unpacking
 LZipFile := TZipFile.Create;
  try
    try
     LZipFile.Open(GetUpdateFileName, TZipMode.zmRead);
     {Verifica se vai extratir tudo ou não
      Adicionado e modificado por Renato Trevisan dia 11/06/2024 14:42:10}
     if not ExtractAll(FExeNames) then
      begin
       for LForIndex := 0 to LZipFile.FileCount - 1 do
        begin
         for LExeName in FExeNames do
          begin
           LFileName := ExtractFileName(NormalizeFileName(LZipFile.FileName[LforIndex]));
            if LExeName.ToUpper = LFileName.ToUpper then
             begin
              LFullFileName := FRootPath + NormalizeFileName(LZipFile.FileName[LforIndex]);
              if not FileToOld(LFullFileName) then
                Exit(False);

              LZipFile.Extract(LFileName, FRootPath);
              Break;
             end;
          end;
        // View
        SyncView(
         procedure
          begin
            FView.Progress(LforIndex, LZipFile.FileCount - 1);
          end);
        end;
      end
     else
      begin
       for LForIndex := 0 to LZipFile.FileCount - 1 do
        begin
         LFullFileName := FRootPath + NormalizeFileName(LZipFile.FileName[LforIndex]);

         if not FileToOld(LFullFileName) then
            Exit(False);
        // View
         SyncView(procedure
          begin
           FView.Progress(LforIndex, LZipFile.FileCount - 1);
          end);
        end;
       LZipFile.ExtractAll(FRootPath);
      end;
    except
      on EZipException do Exit(False);
      on EFOpenError   do Exit(False);
    end;
  finally
    LZipFile.Free;
  end;
end;

function TUpdater.DoUpdate: TUpdateResult;
begin
  // GetInfo
  if not GetDownloadInfo then
    if FIsAbort then
      Exit(TUpdateResult.Abort)
    else if SyncErrorMessage(FConsts.Consts.ConnectionError) then
      Exit(TUpdateResult.TryAgain)
    else
      Exit(TUpdateResult.Fail);

  // Download
  if not Download then
    if FIsAbort then
      Exit(TUpdateResult.Abort)
    else if SyncErrorMessage(FConsts.Consts.DownloadError) then
      Exit(TUpdateResult.TryAgain)
    else
      Exit(TUpdateResult.Fail);

  //Unpacking
  if not Unpacking then
    if SyncErrorMessage(FConsts.Consts.CorruptedFilesError) then
      Exit(TUpdateResult.TryAgain)
    else
      Exit(TUpdateResult.Fail);
  // Deleta arquivo .ZIP de atualização
  DeleteFiles;

  Done;

  Result := TUpdateResult.Success;
end;

procedure TUpdater.Update;
var
  FResult: TUpdateResult;
begin
  FIsAbort := False;
  SyncShowView;
  try
    repeat
      FResult := DoUpdate;
    until FResult <> TUpdateResult.TryAgain;
  finally
    SyncCloseView;
  end;
end;

function TUpdater.GetUpdateFileName: string;
const
  ArchiveName = 'Update.zip';
begin
  if FUpdateFile = '' then
    Result := ExtractFileDir(ParamStr(0)) + PathDelim + ArchiveName
  else
    Result := FUpdateFile;
end;

procedure TUpdater.UpdateFromFile(FileName: string);
var
  TryAgain: Boolean;
begin
  FUpdateFile := FileName;
  try
   SyncShowView;
    try
     TryAgain := False;
      repeat
        // Unpacking
       if Unpacking then
        begin
          Done;
        end
        else
          TryAgain := SyncErrorMessage(FConsts.Consts.CorruptedFilesError);
      until not TryAgain;
    finally
      SyncCloseView;
    end;
  finally
    FUpdateFile := '';
  end;
end;

end.
