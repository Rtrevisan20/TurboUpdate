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
{ �� ������ �������� ���������� VCL/FMX ���������� �� �����.                   }
{******************************************************************************}
{                                                                              }
{Adicionado por Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate}
{added by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate      }
{******************************************************************************}
unit TurboUpdate;

interface

uses
  HDMessageDlg,
  HDMessageDlg.Interfaces,
  System.Classes,
  System.SysUtils,
  TurboUpdate.FMX.Utils,
  TurboUpdate.Interfaces,
  TurboUpdate.Model.Check,
  TurboUpdate.Model.Consts,
  TurboUpdate.Model.Interfaces,
  TurboUpdate.Model.Language.Interfaces,
  TurboUpdate.Model.Types,
  TurboUpdate.Model.Utils;

type
  TTurboUpdate = class(TInterfacedObject, iTurboUpdate)
  private
    FConsts         : IMessageConsts;
    MSG             : iHDMessageDlg;
    FExeNames       : TStringArray;
    FUrls           : TStringArray;
    FKeyName        : string;
    FRootPath       : string;
    FDescription    : string;
    FPngRes         : string;
    FVersion        : TFileVersion;
    FExecUpdateApp  : string;
    FKillTaskApp    : TFileName;
    FModelCheck     : IModelCheck;
    procedure CheckFMX(UpdateAviable: Boolean; Version: TFileVersion);
    procedure CheckStandalone(UpdateAviable: Boolean; Version: TFileVersion);
  public
    constructor Create;
    destructor Destroy; override;
    class function New                                    : iTurboUpdate;
    function ExeNames(aValue: TStringArray)               : iTurboUpdate;
    function Urls(aValue: TStringArray)                   : iTurboUpdate;
    function KeyName(aValue: string)                      : iTurboUpdate;
    function RootPath(aValue: string)                     : iTurboUpdate;
    function Description(aValue: string)                  : iTurboUpdate;
    function PngRes(aValue: string)                       : iTurboUpdate;
    function Version(aValue: TFileVersion)                : iTurboUpdate;
    function ExecUpdateApp(aValue: string = 'Update.exe') : iTurboUpdate;
    function KillTaskApp(aValue: TFileName)               : iTurboUpdate;
    function ChekUpdate                                   : Boolean;
    function GetVersion                                   : TFileVersion;
    procedure UpdateThreadFMX;
    procedure Standalone;
    procedure UpdateFMX;
  end;

var
  GlobalUpdate : iTurboUpdate;

implementation

procedure TTurboUpdate.CheckFMX(UpdateAviable: Boolean; Version: TFileVersion);
var
  FUpdateInfo: TUpdateInfo;
begin
  MSG     := THDMessageDlg.New;
  FConsts := TFactoryConsts.New.Consts;
 if UpdateAviable then
  begin
   if
    MSG.MsgTitle(FConsts.MsgTitle)
     .MsgQuestion(FConsts.MsgQuestion)
     .MsgBody(Format(FConsts.MsgBodyUpdateVersion + FConsts.Version, [Version.ToString]))
     .MsgIcon(TiQuestion)
     .MsgType(TyQuestion)
     .DisplayQuestion
   then
    begin
     FUpdateInfo             := Default(TUpdateInfo);
     FUpdateInfo.Urls        := FUrls;
     FUpdateInfo.ExeNames    := FExeNames;
     FUpdateInfo.KeyName     := FKeyName;
     FUpdateInfo.Description := FDescription;
     FUpdateInfo.RootPath    := FRootPath;
     FUpdateInfo.PngRes      := FPngRes;
     TurboUpdate.FMX.Utils.FMXUpdate(FUpdateInfo);
    end;
  end else
   MSG.MsgTitle(FConsts.MsgTitle)
    .MsgQuestion('')
    .MsgBody(Format(FConsts.MsgBodyLastVersion + FConsts.Version, [Version.ToString]))
    .MsgIcon(TiMessage)
    .MsgType(TyOK)
    .DisplayMessage;
end;

procedure TTurboUpdate.CheckStandalone(UpdateAviable: Boolean; Version: TFileVersion);
begin
  MSG     := THDMessageDlg.New;
  FConsts := TFactoryConsts.New.Consts;
 if UpdateAviable then
  begin
   if
    MSG.MsgTitle(FConsts.MsgTitle)
     .MsgQuestion(FConsts.MsgQuestion)
     .MsgBody(Format(FConsts.MsgBodyUpdateVersion + FConsts.Version, [Version.ToString]))
     .MsgIcon(TiQuestion)
     .MsgType(TyQuestion)
     .DisplayQuestion
   then
    begin
     LaunchUpdateApp(FExecUpdateApp);
     Killtask(FKillTaskApp);
    end;
   end else
    MSG.MsgTitle(FConsts.MsgTitle)
     .MsgQuestion('')
     .MsgBody(Format(FConsts.MsgBodyLastVersion + FConsts.Version, [Version.ToString]))
     .MsgIcon(TiMessage)
     .MsgType(TyOK)
     .DisplayMessage;
end;

function TTurboUpdate.ChekUpdate: boolean;
begin
  Result := FModelCheck.CheckUpdate(FUrls, FKeyName, FVersion);
end;

constructor TTurboUpdate.Create;
begin
  FModelCheck := TModelCheck.New;
end;

function TTurboUpdate.Description(aValue: string): iTurboUpdate;
begin
  Result := Self;
  FDescription := aValue;
end;

destructor TTurboUpdate.Destroy;
begin

  inherited;
end;

function TTurboUpdate.ExeNames(aValue: TStringArray): iTurboUpdate;
begin
  Result := self;
  FExeNames := aValue;
end;

function TTurboUpdate.GetVersion: TFileVersion;
begin
  Result := FModelCheck.GetVersionUpdate(FUrls, FKeyName);
end;

function TTurboUpdate.KillTaskApp(aValue: TFileName): iTurboUpdate;
begin
  Result       := Self;
  FKillTaskApp := aValue;
end;

function TTurboUpdate.ExecUpdateApp(aValue: string): iTurboUpdate;
begin
  Result := Self;
  FExecUpdateApp := aValue;
end;

function TTurboUpdate.KeyName(aValue: string): iTurboUpdate;
begin
  Result := Self;
  FKeyName := aValue;
end;

class function TTurboUpdate.New: iTurboUpdate;
begin
  Result := Self.Create;
end;

function TTurboUpdate.PngRes(aValue: string): iTurboUpdate;
begin
  Result := Self;
  FPngRes := aValue;
end;

function TTurboUpdate.RootPath(aValue: string): iTurboUpdate;
begin
  Result := Self;
  FRootPath := aValue;
end;

procedure TTurboUpdate.Standalone;
begin
  FModelCheck.CheckUpdate(FUrls, FKeyName, FVersion, CheckStandalone);
end;

procedure TTurboUpdate.UpdateFMX;
begin
  FModelCheck.CheckUpdate(FUrls, FKeyName, FVersion, CheckFMX);
end;

procedure TTurboUpdate.UpdateThreadFMX;
var
  FUpdateInfo: TUpdateInfo;
begin
  FUpdateInfo.Urls        := FUrls;
  FUpdateInfo.ExeNames    := FExeNames;
  FUpdateInfo.KeyName     := FKeyName;
  FUpdateInfo.Description := FDescription;
  FUpdateInfo.RootPath    := FRootPath;
  FUpdateInfo.PngRes      := FPngRes;
  FMXUpdate(FUpdateInfo);
end;

function TTurboUpdate.Urls(aValue: TStringArray): iTurboUpdate;
begin
  Result := Self;
  FUrls := aValue;
end;

function TTurboUpdate.Version(aValue: TFileVersion): iTurboUpdate;
begin
  Result := Self;
  FVersion := aValue;
end;

initialization
 GlobalUpdate := TTurboUpdate.New;

end.
