unit TurboUpdate.Model.Check;

interface

uses
  System.SysUtils,
  TurboUpdate.Model.Interfaces,
  TurboUpdate.Model.Types;

type
  TModelCheck = class(TInterfacedObject, IModelCheck)
  private
    FModelInternet: IModelInternet;
  public
    constructor Create;
    destructor Destroy; override;
    class function New: IModelCheck;
    function GetVersionUpdate(AUrls: TStringArray; KeyName: string): TFileVersion;
    function CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion): boolean; overload;
    procedure CheckUpdate(AUrls: TStringArray; KeyName: string; AUpdateCheckResultProc:
      TUpdateCheckResultProc); overload;
    procedure CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion;
      AUpdateCheckResultProc: TUpdateCheckResultProc); overload;
  end;

var
 FIsChecking: Boolean = False;

implementation

uses
  System.Classes,
  TurboUpdate.Model.Internet;

function TModelCheck.CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion: TFileVersion): boolean;
var
  LUrl          : string;
  LUpdateVersion: TFileVersion;
begin
 Result := False;
 if FIsChecking then
    Exit(False);
 FIsChecking := True;
  try
   for LUrl in AUrls do
    begin
     FModelInternet.GetUpdateVersion(LUrl, KeyName, LUpdateVersion);
     if AVersion < LUpdateVersion then
      Result := True else
      Result := False;
     break;
    end;
  finally
    FIsChecking := False;
  end;
end;

procedure TModelCheck.CheckUpdate(AUrls: TStringArray; KeyName: string;
  AUpdateCheckResultProc: TUpdateCheckResultProc);
begin
  CheckUpdate(AUrls, KeyName, TFileVersion.CreateForFile(ParamStr(0)),
    AUpdateCheckResultProc);
end;

procedure TModelCheck.CheckUpdate(AUrls: TStringArray; KeyName: string; AVersion:
  TFileVersion; AUpdateCheckResultProc: TUpdateCheckResultProc);
begin
  if FIsChecking then Exit;
  TThread.CreateAnonymousThread(
    procedure
    var
      Url: string;
      UpdateVersion: TFileVersion;
    begin
     FIsChecking := True;
      try
       for Url in AUrls do
        begin
         if FModelInternet.GetUpdateVersion(Url, KeyName, UpdateVersion) then
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
        FIsChecking := False;
      end;
    end).Start;
end;

constructor TModelCheck.Create;
begin
  FModelInternet := TModelInternet.New;
end;

destructor TModelCheck.Destroy;
begin

  inherited;
end;

function TModelCheck.GetVersionUpdate(AUrls: TStringArray; KeyName: string): TFileVersion;
begin
  Result := FModelInternet.GetUpdateVersion(AUrls, KeyName);
end;

class function TModelCheck.New: IModelCheck;
begin
  Result := Self.Create;
end;

end.

