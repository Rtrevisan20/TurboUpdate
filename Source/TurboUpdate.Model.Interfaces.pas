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
{ Вы можете заказать разработку VCL/FMX компонента на заказ.                   }
{******************************************************************************}
{                                                                              }
{Adicionado por Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate}
{added by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate      }
{******************************************************************************}
unit TurboUpdate.Model.Interfaces;

interface

uses
  IdComponent,
  System.Net.HttpClient,
  System.SysUtils,
  TurboUpdate.Model.Types;

type
  TReceiveDataEventRef   = reference to procedure(ALength: Int64; AProgress: Int64; var AAbort: Boolean);
  TUpdateCheckResultProc = reference to procedure(UpdateAviable: Boolean; AVersion: TFileVersion);

  IUpdateModel = interface
    ['{CEAD1A55-AF8B-4003-B1C2-84D7371D2CE1}']
    procedure Cancel;
  end;

  IUpdateView = interface
    ['{D7D57022-217A-4D79-944F-6D3112D674D9}']
    procedure SetVersion(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetStatus(const Value: string);
    procedure SetPngRes(const Value: string);
    procedure SetModel(Model: IUpdateModel);
    procedure SetUpdateState(Value: TUpdateState);
    // routliness
    procedure ShowMessage(Message: string);
    function ShowErrorMessage(Message: string): Boolean;
    procedure Progress(Progress, Length: Integer);
    procedure Close;
    procedure Show;
    // properties
    property Version: string write SetVersion;
    property Status: string write SetStatus;
    property Description: string write SetDescription;
    property PngRes: string write SetPngRes;
    property Model: IUpdateModel write SetModel;
    property State: TUpdateState write SetUpdateState;
  end;

  {Add by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate 6-5-25}
  IModelInternet = interface
    ['{58FA0D07-20F1-4EBD-BC53-1D0489F061BD}']
    function GetUpdateUrl(AIniFileUrl: string; AKeyName: string): string; overload;
    function GetUpdateUrl(AUrls: TStringArray; AKeyName: string): string; overload;
    function GetUpdateVersion(AIniFileUrl: string; AKeyName: string; out AVersion: TFileVersion): Boolean; overload;
    function GetUpdateVersion(AUrls: TStringArray; AKeyName: string; out AVersion: TFileVersion): Boolean; overload;
    function GetUpdateVersion(AUrls: TStringArray; AKeyName: string): TFileVersion; overload;
    function DowloadFile(AUrl: string; APath: string; ADownloadProgress: TReceiveDataEventRef): Boolean;
  end;
  {Add by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate 6-5-25}
  IHttpClientHook = interface
    ['{AD88BADC-A5E8-4E36-AEA5-190DB44A3703}']
    function ResiveDataProc: TReceiveDataEvent; overload;
    function ResiveDataProc(OnResiveData: TReceiveDataEventRef): IHttpClientHook; overload;
    function ResiveWorkEventId: TWorkEvent; overload;
    function ResiveWorkEventId(OnResiveData: TReceiveDataEventRef): IHttpClientHook; overload;
    function IsAbort(AAbort : boolean): IHttpClientHook;
  end;
  {Add by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate 6-5-25}
  IModelCheck = interface
    ['{FD602A83-2C2B-4D0A-BDEE-F3440EDDD4D6}']
    function GetVersionUpdate(AUrls: TStringArray; KeyName: string): TFileVersion;
    function CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion): boolean; overload;
    procedure CheckUpdate(AUrls: TStringArray; KeyName: string; AUpdateCheckResultProc:
      TUpdateCheckResultProc); overload;
    procedure CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion;
      AUpdateCheckResultProc: TUpdateCheckResultProc); overload;
  end;

implementation

end.

