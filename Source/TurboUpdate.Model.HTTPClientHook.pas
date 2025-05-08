unit TurboUpdate.Model.HTTPClientHook;

interface

uses
  IdComponent,
  System.Net.HttpClient,
  System.SysUtils,
  TurboUpdate.Model.Interfaces;

type
  THttpClientHook = class(TInterfacedObject, IHttpClientHook)
  private
    FOnResiveData : TReceiveDataEventRef;
    FIsAbort : boolean;
    procedure ReceiveDataProc(const Sender: TObject; AContentLength: Int64; AReadCount:
      Int64; var AAbort: Boolean);
    procedure ReceiveWorkEventID(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IHttpClientHook;
    function ResiveDataProc: TReceiveDataEvent; overload;
    function ResiveDataProc(OnResiveData: TReceiveDataEventRef): IHttpClientHook; overload;
    function ResiveWorkEventId: TWorkEvent; overload;
    function ResiveWorkEventId(OnResiveData: TReceiveDataEventRef): IHttpClientHook; overload;
    function IsAbort(AAbort : boolean): IHttpClientHook;
  end;

implementation

constructor THttpClientHook.Create;
begin
end;

destructor THttpClientHook.Destroy;
begin
  inherited;
end;

function THttpClientHook.IsAbort(AAbort: boolean): IHttpClientHook;
begin
  Result   := Self;
  FIsAbort := AAbort;
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

procedure THttpClientHook.ReceiveWorkEventID(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
begin
  if Assigned(FOnResiveData) then
    FOnResiveData(AWorkCount, AWorkCount, FIsAbort);
end;

function THttpClientHook.ResiveDataProc(OnResiveData: TReceiveDataEventRef): IHttpClientHook;
begin
  Result        := Self;
  FOnResiveData := OnResiveData;
end;

function THttpClientHook.ResiveWorkEventId(OnResiveData: TReceiveDataEventRef): IHttpClientHook;
begin
  Result        := Self;
  FOnResiveData := OnResiveData;
end;

function THttpClientHook.ResiveWorkEventId: TWorkEvent;
begin
  Result := ReceiveWorkEventID;
end;

function THttpClientHook.ResiveDataProc: TReceiveDataEvent;
begin
  Result := ReceiveDataProc;
end;

end.

