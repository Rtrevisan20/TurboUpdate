unit TurboUpdate.Model.Internet.INDY;

interface

uses
  IdComponent,
  System.Classes,
  System.IniFiles,
  System.SysUtils,
  TurboUpdate.Model.Interfaces,
  TurboUpdate.Model.Types;

type
  TModelInternetINDY = class(TInterfacedObject, IModelInternet)
  private
    function GetIniFile(AUrl: string): TMemIniFile;
    function GetStream(AUrl: string): TMemoryStream;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IModelInternet;
    function GetUpdateUrl(AIniFileUrl: string; AKeyName: string): string; overload;
    function GetUpdateUrl(AUrls: TStringArray; AKeyName: string): string; overload;
    function GetUpdateVersion(AIniFileUrl: string; AKeyName: string; out AVersion: TFileVersion): Boolean; overload;
    function GetUpdateVersion(AUrls: TStringArray; AKeyName: string; out AVersion: TFileVersion): Boolean; overload;
    function GetUpdateVersion(AUrls: TStringArray; AKeyName: string): TFileVersion; overload;
    function DowloadFile(AUrl: string; APath: string; ADownloadProgress: TReceiveDataEventRef): Boolean;
  end;

var
  FVersion: TFileVersion;

implementation

uses
  IdException,
  IdHTTP,
  IdSSLOpenSSL,
  TurboUpdate.Model.HTTPClientHook;

constructor TModelInternetINDY.Create;
begin

end;

destructor TModelInternetINDY.Destroy;
begin

  inherited;
end;

{$HINTS OFF}
function TModelInternetINDY.DowloadFile(AUrl, APath: string; ADownloadProgress: TReceiveDataEventRef): Boolean;
var
  FHttp     : TIdHTTP;
  FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  Stream    : TStream;
  FHook     : IHttpClientHook;
  FTime     : Cardinal;
begin
  Result := False;
  FTime := 0;
  FHook := THttpClientHook.New;
  FHook.ResiveWorkEventId(
      procedure(ALength, AProgress: Int64; var AAbort: Boolean)
      begin
        if (FTime < TThread.GetTickCount) or (ALength = AProgress) then begin
          FHook.IsAbort(AAbort);
          ADownloadProgress(ALength, AProgress, AAbort);
          FTime := TThread.GetTickCount + 1000 div 30; // 30 per second
        end;
      end
  );
  Stream := TFileStream.Create(APath, fmCreate);
  try
    FHttp      := TIdHTTP.Create(nil);
    FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);
    FIOHandler.SSLOptions.Method  := sslvSSLv23;
    FIOHandler.SSLOptions.Mode    := sslmClient;
    FHttp.IOHandler               := FIOHandler;
    FHttp.OnWork := FHook.ResiveWorkEventId;
    try
      try
        FHttp.Get(AUrl, Stream);
        Result := (FHttp.ResponseCode <= 299) and (FHttp.Response.ContentLength = Stream.Size);
      except
        on EIdException do
          Result := False;
      end;
    finally
      FHttp.Free;
    end;
  finally
    Stream.Free;
  end;
end;
{$HINTS ON}

function TModelInternetINDY.GetIniFile(AUrl: string): TMemIniFile;
var
  Stream : TMemoryStream;
  Strings: TStringList;
begin
  Result := nil;
  Stream := GetStream(AUrl);
  if Stream = nil then
    Exit;
  try
    Strings := TStringList.Create;
    try
      Stream.Position := 0;
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

function TModelInternetINDY.GetStream(AUrl: string): TMemoryStream;
var
  FHttp     : TIdHTTP;
  FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
  Stream    : TMemoryStream;
begin
  Result := nil;
  Stream := TMemoryStream.Create;
  try
    FHttp      := TIdHTTP.Create(nil);
    FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FHttp);
    FIOHandler.SSLOptions.Method  := sslvSSLv23;
    FIOHandler.SSLOptions.Mode    := sslmClient;
    FHttp.IOHandler               := FIOHandler;
    Stream.Position := 0;
    try
      FHttp.Get(AUrl, Stream);
      if FHttp.ResponseCode >= 300 then
        FreeAndNil(Stream);
      Result := Stream;
    finally
      FHttp.Free;
    end;
  except
    FreeAndNil(Stream);
  end;
end;

function TModelInternetINDY.GetUpdateUrl(AUrls: TStringArray; AKeyName: string): string;
var
  AUrl: string;
begin
  Result := '';
  for AUrl in AUrls do begin
    Result := GetUpdateUrl(AUrl, AKeyName);
    if Result <> '' then
      break;
  end;
end;

function TModelInternetINDY.GetUpdateUrl(AIniFileUrl, AKeyName: string): string;
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

function TModelInternetINDY.GetUpdateVersion(AIniFileUrl, AKeyName: string; out AVersion: TFileVersion): Boolean;
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

function TModelInternetINDY.GetUpdateVersion(
    AUrls: TStringArray;
    AKeyName: string;
    out AVersion: TFileVersion
): Boolean;
var
  AUrl: string;
begin
  for AUrl in AUrls do begin
    if GetUpdateVersion(AUrl, AKeyName, AVersion) then begin
      FVersion := AVersion;
      Exit(True);
    end;
  end;
  Exit(False);
end;

function TModelInternetINDY.GetUpdateVersion(AUrls: TStringArray; AKeyName: string): TFileVersion;
var
  AUrl: string;
begin
  if FVersion.ToString = '0.0.0.0' then begin
    for AUrl in AUrls do begin
      if AUrl.IsEmpty then
        Exit;
      Result := TFileVersion.Create(GetIniFile(AUrl).ReadString(AKeyName, 'Version', ''));
      break;
    end;
  end
  else
    Result := FVersion;
end;

class function TModelInternetINDY.New: IModelInternet;
begin
  Result := Self.Create;
end;

end.
