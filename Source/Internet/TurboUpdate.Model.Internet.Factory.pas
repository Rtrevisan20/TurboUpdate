unit TurboUpdate.Model.Internet.Factory;

interface

uses
  System.SysUtils,
  TurboUpdate.Model.Interfaces;

type
  TModelIternetFactory = class(TInterfacedObject, IModelIternetFactory)
  private
    FInternetSystem : IModelInternet;
    FInternetINDY   : IModelInternet;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IModelIternetFactory;
    function InternetSystem : IModelInternet;
    function InternetINDY   : IModelInternet;
  end;

implementation

uses
  TurboUpdate.Model.Internet.INDY, TurboUpdate.Model.Internet;

constructor TModelIternetFactory.Create;
begin

end;

destructor TModelIternetFactory.Destroy;
begin

  inherited;
end;

function TModelIternetFactory.InternetINDY: IModelInternet;
begin
  if not Assigned(FInternetINDY) then
    FInternetINDY := TModelInternetINDY.New;

  Result := FInternetINDY;
end;

function TModelIternetFactory.InternetSystem: IModelInternet;
begin
  if not Assigned(FInternetSystem) then
    FInternetSystem := TModelInternet.New;

  Result := FInternetSystem;
end;

class function TModelIternetFactory.New: IModelIternetFactory;
begin
  Result := Self.Create;
end;

end.
