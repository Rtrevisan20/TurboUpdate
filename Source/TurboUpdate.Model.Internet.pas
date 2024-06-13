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
{Modidicado por Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate}
{Modified by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate   }
{******************************************************************************}
unit TurboUpdate.Model.Internet;

interface

uses
  System.Classes,
  System.IniFiles,
  System.Net.HttpClient,
  System.Net.UrlClient,
  System.SysUtils,

  TurboUpdate.Model.Types;

function GetUpdateUrl(AIniFileUrl: string; AKeyName: string): string; overload;
function GetUpdateUrl(AUrls: TStringArray; AKeyName: string): string; overload;
function GetUpdateVersion(AIniFileUrl: string; AKeyName: string;out AVersion: TFileVersion): Boolean; overload;
function GetUpdateVersion(AUrls: TStringArray; AKeyName: string;out AVersion: TFileVersion): Boolean; overload;
function GetUpdateVersion(AUrls: TStringArray; AKeyName: string): TFileVersion; overload;

type
  TReceiveDataEventRef = reference to procedure(ALength: Int64;
    AProgress: Int64; var AAbort: Boolean);

function DowloadFile(AUrl: string; APath: string;
  ADownloadProgress: TReceiveDataEventRef): Boolean;

var
  ProxySettings: TProxySettings;
  FVersion: TFileVersion;

implementation

function GetStream(AUrl: string): TStream;
var
  Http: THttpClient;
  Stream: TMemoryStream;
begin
  Result := nil;
  Stream := TMemoryStream.Create;
  try
    Http := THttpClient.Create;
    try
      Http.ProxySettings := ProxySettings;
      // Http.ConnectTimeout := 10 * 1000;// 10 sec
      if Http.Get(AUrl, Stream).StatusCode >= 300 then
        FreeAndNil(Stream);
      Result := Stream;
    finally
      Http.Free;
    end;
  except
    FreeAndNil(Stream);
  end;
end;

function GetIniFile(AUrl: string): TMemIniFile;
var
  Stream: TStream;
  Strings: TStringList;
begin
  Result := nil;
  Stream := GetStream(AUrl);
  if Stream = nil then
    Exit;
  try
    Strings := TStringList.Create;
    try
      Strings.LoadFromStream(Stream);
      Result := TMemIniFile.Create('');
      Result.SetStrings(Strings);
    finally
      Strings.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function GetUpdateUrl(AIniFileUrl: string; AKeyName: string): string;
var
  Ini: TMemIniFile;
begin
  Result := '';
  Ini := GetIniFile(AIniFileUrl);
  if Ini = nil then
    Exit;
  try
    Result := Ini.ReadString(AKeyName, 'Download', '');
  finally
    Ini.Free;
  end;
end;

function GetUpdateVersion(AUrls: TStringArray; AKeyName: string): TFileVersion;
var
  AUrl: string;
begin
 if FVersion.ToString = '0.0.0.0' then
  begin
    for AUrl in AUrls do
    begin
      if AUrl.IsEmpty then
        Exit;
      Result := TFileVersion.Create(GetIniFile(AUrl).ReadString(AKeyName,
        'Version', ''));
      break;
    end;
  end
  else Result := FVersion;
end;

function GetUpdateVersion(AIniFileUrl: string; AKeyName: string;
  out AVersion: TFileVersion): Boolean;
var
  Ini: TMemIniFile;
begin
  Result := False;
  Ini := GetIniFile(AIniFileUrl);
  if Ini = nil then
    Exit;
  try
    AVersion := TFileVersion.Create(Ini.ReadString(AKeyName, 'Version', ''));
    FVersion := TFileVersion.Create(Ini.ReadString(AKeyName, 'Version', ''));
    Result := True;
  finally
    Ini.Free;
  end;
end;

function GetUpdateUrl(AUrls: TStringArray; AKeyName: string): string;
var
  AUrl: string;
begin
  Result := '';
  for AUrl in AUrls do
  begin
    Result := GetUpdateUrl(AUrl, AKeyName);
    if Result <> '' then
      break;
  end;
end;

function GetUpdateVersion(AUrls: TStringArray; AKeyName: string;
  out AVersion: TFileVersion): Boolean;
var
  AUrl: string;
begin
  for AUrl in AUrls do
  begin
    if GetUpdateVersion(AUrl, AKeyName, AVersion) then
    begin
      FVersion := AVersion;
      Exit(True);
    end;
  end;
  Exit(False)
end;

type
  IHttpClientHook = interface
    procedure SetOnResiveData(OnResiveData: TReceiveDataEventRef);
    function GetResiveDataProc: TReceiveDataEvent;
    // ---
    property OnResiveData: TReceiveDataEventRef write SetOnResiveData;
    property ResiveDataProc: TReceiveDataEvent read GetResiveDataProc;
  end;

  THttpClientHook = class(TInterfacedObject, IHttpClientHook)
  private
    FOnResiveData: TReceiveDataEventRef;
    procedure ReceiveDataProc(const Sender: TObject; AContentLength: Int64;
      AReadCount: Int64; var AAbort: Boolean);
  public
    destructor Destroy; override;
    { IHttpClientHook }
    procedure SetOnResiveData(OnResiveData: TReceiveDataEventRef);
    function GetResiveDataProc: TReceiveDataEvent;
  end;

  { THttpClientHook }
destructor THttpClientHook.Destroy;
begin
  inherited;
end;

function THttpClientHook.GetResiveDataProc: TReceiveDataEvent;
begin
  Result := ReceiveDataProc;
end;

procedure THttpClientHook.ReceiveDataProc(const Sender: TObject;
  AContentLength, AReadCount: Int64; var AAbort: Boolean);
begin
  if Assigned(FOnResiveData) then
    FOnResiveData(AContentLength, AReadCount, AAbort);
end;

procedure THttpClientHook.SetOnResiveData(OnResiveData: TReceiveDataEventRef);
begin
  FOnResiveData := OnResiveData;
end;
{$HINTS OFF}

function DowloadFile(AUrl: string; APath: string;
  ADownloadProgress: TReceiveDataEventRef): Boolean;
var
  Http: THttpClient;
  Stream: TStream;
  Hook: IHttpClientHook;
  Response: IHTTPResponse;
  Time: Cardinal;
begin
  Result := False;
  Time := 0;
  Hook := THttpClientHook.Create;
  Hook.OnResiveData := procedure(ALength, AProgress: Int64; var AAbort: Boolean)
    begin
      if (Time < TThread.GetTickCount) or (ALength = AProgress) then
      begin
        ADownloadProgress(ALength, AProgress, AAbort);
        Time := TThread.GetTickCount + 1000 div 30; // 30 per second
      end;
      // if AAbort then
      // raise ENetHTTPException.Create('Aborted');
    end;
  Stream := TFileStream.Create(APath, fmCreate);
  try
    Http := THttpClient.Create;
    Http.ProxySettings := ProxySettings;
    Http.OnReceiveData := Hook.ResiveDataProc;
    try
      // Http.DownloadProgressProc := ADownloadProgress;
      // Http.ConnectTimeout := 10 * 1000;// 10 sec
      try
        Response := Http.Get(AUrl, Stream);
        Result := (Response.StatusCode <= 299) and
          (Response.ContentLength = Stream.Size);
      except
        on ENetHTTPException do
          Result := False;
      end;
    finally
      Http.Free;
    end;
  finally
    Stream.Free;
  end;
end;
{$HINTS ON}

end.
