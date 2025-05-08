unit Main;
interface

uses
  TurboUpdate,

  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.Dialogs,
  FMX.Forms,
  FMX.Graphics,
  FMX.StdCtrls,
  FMX.Types,

  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,

  TurboUpdate.Model.Types;

type
  TFormMain = class(TForm)
    ButtonCheckUpdateOld: TButton;
    ButtonCheckUpdateNew: TButton;
    ButtonCheckUpdateCur: TButton;
    Label1: TLabel;
    procedure ButtonCheckUpdateCurClick(Sender: TObject);
    procedure ButtonCheckUpdateNewClick(Sender: TObject);
    procedure ButtonCheckUpdateOldClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
  end;
var
  FormMain: TFormMain;

implementation
{$R *.fmx}
procedure TFormMain.ButtonCheckUpdateCurClick(Sender: TObject);
begin
  GlobalUpdate
   .ExeNames(['FmxApplication.exe'])
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/refs/heads/master/Update.ini'])
   .KeyName('TurboUpdate.Fmx.Standalone')
   .Description('TurboUpdate/Fmx/Standalone')
   .Version(TFileVersion.CreateForFile(ParamStr(0)))
   .UpdateFMX;
end;
procedure TFormMain.ButtonCheckUpdateNewClick(Sender: TObject);
begin
  GlobalUpdate
   .ExeNames(['FmxApplication.exe'])
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/refs/heads/master/Update.ini'])
   .KeyName('TurboUpdate.Fmx.Standalone')
   .Description('TurboUpdate/Fmx/Standalone')
   .Version(TFileVersion.Create('2.0.0.0'))
   .UpdateFMX;
end;
procedure TFormMain.ButtonCheckUpdateOldClick(Sender: TObject);
begin
  GlobalUpdate
   .ExeNames(['&ALL']) //&ALL
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/refs/heads/master/Update.ini'])
   .KeyName('TurboUpdate.Fmx.Standalone')
   .Description('TurboUpdate/Fmx/Standalone')
   .Version(TFileVersion.Create('1.9.3.0'))
   .UpdateFMX;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  Label1.Text := 'On GitHub - Version: ' +
   GlobalUpdate
    .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/refs/heads/master/Update.ini'])
    .KeyName('TurboUpdate.Fmx.Standalone')
    .GetVersion.ToString;
end;

end.
