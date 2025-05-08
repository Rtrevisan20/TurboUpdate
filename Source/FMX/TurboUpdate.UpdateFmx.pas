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
unit TurboUpdate.UpdateFmx;

interface

uses
  Fmx.Forms,

  System.Classes,
  System.SysUtils,

  TurboUpdate.Model.Interfaces,
  TurboUpdate.Model,
  TurboUpdate.Model.Types,
  TurboUpdate.Model.Update.Thread;

type
  TFmxUpdateThread = class(TUpdateThread)
  protected
    function CreateView: TCustomForm; virtual;
    procedure Work; override;
  end;

implementation

uses
  TurboUpdate.FormUpdateFmx;

function TFmxUpdateThread.CreateView: TCustomForm;
begin
  Result := TFormUpdateFmx.Create(Application);
end;

procedure TFmxUpdateThread.Work;
var
  FModel: TUpdater;
  FView: TCustomForm;
begin
  // need waiting start mainloop
  while ApplicationState = TApplicationState.None do Sleep(0);
  Sync(procedure
  begin
    FView := CreateView;
  end);
  if Application.MainForm = nil then
    Application.MainForm := FView;
  FModel := nil;
  try
    FModel := CreateModel(FView as IUpdateView);
    if IsUpdateFromFile then
      FModel.UpdateFromFile(FileName)
    else
      FModel.Update;
  finally
    FModel.Free;
    if FView <> Application.MainForm then
      Sync(procedure
      begin
        FView.Free;
      end)
    else
    begin
      IsUpdating := False;
      Sync(procedure
      begin
        FView.Close;
      end)
    end;
  end;
end;
end.

