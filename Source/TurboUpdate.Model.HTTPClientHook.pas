unit TurboUpdate.Model.HTTPClientHook;

interface

uses
  System.Net.HttpClient,
  System.SysUtils,
  TurboUpdate.Model.Interfaces;

type
  THttpClientHook = class(TInterfacedObject, IHttpClientHook)
  private
    FOnResiveData: TReceiveDataEventRef;
    procedure ReceiveDataProc(const Sender: TObject; AContentLength: Int64; AReadCount:
      Int64; var AAbort: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IHttpClientHook;
    function ResiveDataProc: TReceiveDataEvent; overload;
    function ResiveDataProc(OnResiveData: TReceiveDataEventRef): IHttpClientHook; overload;
  end;

implementation

constructor THttpClientHook.Create;
begin
end;

destructor THttpClientHook.Destroy;
begin
  inherited;
end;

class function THttpClientHook.New: IHttpClientHook;
begin
  Result := Self.Create;
end;

procedure THttpClientHook.ReceiveDataProc(const Sender: TObject; AContentLength,
  AReadCount: Int64; var AAbort: Boolean);
begin
  if Assigned(FOnResiveData) then
    FOnResiveData(AContentLength, AReadCount, AAbort);
end;

function THttpClientHook.ResiveDataProc(OnResiveData: TReceiveDataEventRef): IHttpClientHook;
begin
  Result := Self;
  FOnResiveData := OnResiveData;
end;

function THttpClientHook.ResiveDataProc: TReceiveDataEvent;
begin
  Result := ReceiveDataProc;
end;

end.

