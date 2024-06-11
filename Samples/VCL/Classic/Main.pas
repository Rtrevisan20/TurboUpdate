unit Main;

interface

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,
  System.Variants,

  TurboUpdate,
  TurboUpdate.Model.Types,

  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,

  Winapi.Messages,
  Winapi.Windows;

type
  TFormMain = class(TForm)
    ButtonCheckUpdateOld: TButton;
    ButtonCheckUpdateCur: TButton;
    Label1: TLabel;
    ButtonCheckUpdateNew: TButton;
    procedure ButtonCheckUpdateOldClick(Sender: TObject);
    procedure ButtonCheckUpdateCurClick(Sender: TObject);
    procedure ButtonCheckUpdateNewClick(Sender: TObject);
  private
  public
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.ButtonCheckUpdateCurClick(Sender: TObject);
begin
  GlobalUpdate
   .ExeNames(['VclApplication.exe', 'VclUpdate.exe'])
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Version(TFileVersion.CreateForFile(ParamStr(0)))
   .Description('TurboUpdate/Vcl/Classic')
   .UpdateVCL;
end;

procedure TFormMain.ButtonCheckUpdateNewClick(Sender: TObject);
begin
  GlobalUpdate
   .ExeNames(['VclApplication.exe', 'VclUpdate.exe'])
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Version(TFileVersion.CreateForFile('2.0.0.0'))
   .Description('TurboUpdate/Vcl/Classic')
   .UpdateVCL;
end;

procedure TFormMain.ButtonCheckUpdateOldClick(Sender: TObject);
begin
  GlobalUpdate
   .ExeNames(['&ALL']) // VclApplication.exe', 'VclUpdate.exe
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Version(TFileVersion.CreateForFile('1.9.3.0'))
   .Description('TurboUpdate/Vcl/Classic')
   .UpdateVCL;
end;

end.
