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
