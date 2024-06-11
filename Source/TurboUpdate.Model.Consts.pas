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
{Modidicado por Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate}
{Modified by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate   }
{******************************************************************************}
unit TurboUpdate.Model.Consts;
{$I \Language.inc}

interface

uses
  System.Classes,
  System.SysUtils,

  TurboUpdate.Model.LanguagePTbr,
  TurboUpdate.Model.LanguageUS,

  TurboUpdate.Model.Language.Interfaces;

type
  TFactoryConsts = class (TInterfacedObject, IFactoryConsts)
    private
      FConsts : IMessageConsts;
    public
      constructor Create;
      destructor Destroy; override;
      class function New : IFactoryConsts;
      function Consts    : IMessageConsts;
  end;

implementation

{ TFactoryConsts }

function TFactoryConsts.Consts: IMessageConsts;
begin
{$IFDEF EN-Us}
  FConsts := TMessageConstsUS.New;
  Result  := FConsts;
{$ENDIF}

{$IFDEF PT-Br}
  FConsts := TMessageConstsPTbr.New;
  Result  := FConsts;
{$ENDIF}
end;

constructor TFactoryConsts.Create;
begin

end;

destructor TFactoryConsts.Destroy;
begin

  inherited;
end;

class function TFactoryConsts.New: IFactoryConsts;
begin
  Result := Self.Create;
end;

end.
