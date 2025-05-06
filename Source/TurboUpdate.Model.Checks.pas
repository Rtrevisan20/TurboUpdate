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
unit TurboUpdate.Model.Checks;

interface

uses
  TurboUpdate.Model.Types;

type
  TUpdateCheckResultProc = reference to procedure(UpdateAviable: Boolean; AVersion: TFileVersion);

function GetVersionUpdate(AUrls: TStringArray; KeyName: string): TFileVersion;
function CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion): boolean; overload;
procedure CheckUpdate(AUrls: TStringArray; KeyName: string; AUpdateCheckResultProc: TUpdateCheckResultProc); overload;
procedure CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion; AUpdateCheckResultProc: TUpdateCheckResultProc); overload;

implementation

uses
  System.Classes, TurboUpdate.Model.Internet;

var
  IsChecking: Boolean = False;

function GetVersionUpdate(AUrls: TStringArray; KeyName: string): TFileVersion;
begin
  Result := TModelInternet.New.GetUpdateVersion(AUrls, KeyName);
end;

function CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion): boolean;
var
  LUrl          : string;
  LUpdateVersion: TFileVersion;
begin
 Result := False;
 if IsChecking then
    Exit(False);
 IsChecking := True;
  try
   for LUrl in AUrls do
    begin
     TModelInternet.New.GetUpdateVersion(LUrl, KeyName, LUpdateVersion);
     if AVersion < LUpdateVersion then
      Result := True else
      Result := False;
     break;
    end;
  finally
    IsChecking := False;
  end;
end;

procedure CheckUpdate(AUrls: TStringArray; KeyName: string;
  AVersion: TFileVersion; AUpdateCheckResultProc: TUpdateCheckResultProc);
begin
  if IsChecking then Exit;

  TThread.CreateAnonymousThread(
    procedure
    var
      Url: string;
      UpdateVersion: TFileVersion;
    begin
     IsChecking := True;
      try
       for Url in AUrls do
        begin
         if TModelInternet.New.GetUpdateVersion(Url, KeyName, UpdateVersion) then
          begin
            TThread.Synchronize(nil,
             procedure
              begin
                if UpdateVersion > AVersion then
                  AUpdateCheckResultProc(True, UpdateVersion)
                else
                  AUpdateCheckResultProc(False, UpdateVersion);
              end);
            break;
          end;
        end;
      finally
        IsChecking := False;
      end;
    end).Start;
end;

procedure CheckUpdate(AUrls: TStringArray; KeyName: string;
AUpdateCheckResultProc: TUpdateCheckResultProc);
begin
  CheckUpdate(AUrls, KeyName, TFileVersion.CreateForFile(ParamStr(0)),
    AUpdateCheckResultProc);
end;

end.
