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
{Modificado por Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate}
{Modified by Renato Trevisan Fork=https://github.com/Rtrevisan20/TurboUpdate   }
{******************************************************************************}
unit TurboUpdate.FormUpdate;

interface

uses
  HDMessageDlg,

  ES.BaseControls,
  ES.Images,
  ES.Indicators,
  ES.Layouts,

  HDMessageDlg.Interfaces,

  System.Classes,
  System.SysUtils,
  System.Variants,

  TurboUpdate.Model.Consts,
  TurboUpdate.Model.Interfaces,
  TurboUpdate.Model.Language.Interfaces,
  TurboUpdate.Model.Types,

  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.Imaging.pngimage,
  Vcl.StdCtrls,

  Winapi.Messages,
  Winapi.Windows;

type
  TFormUpdate = class(TForm, IUpdateView)
    Image                 : TEsImageControl;
    ProgressBar           : TEsActivityBar;
    LayoutFotter          : TEsLayout;
    LayoutMain            : TEsLayout;
    LayoutProgress        : TEsLayout;
    LayoutFotterSeparator : TEsLayout;
    LabelDescription      : TLabel;
    ButtonCancel          : TButton;
    LabelStatus           : TLabel;
    LabelVersion          : TLabel;
    LinkLabelTurboUpdate  : TLinkLabel;
    procedure ButtonCancelClick(Sender: TObject);
    procedure LinkLabelTurboUpdateLinkClick(Sender: TObject;
              const Link: string; LinkType: TSysLinkType);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    Model   : IUpdateModel;
    Msg     : iHDMessageDlg;
    FConsts : IMessageConsts;
  public
    { IUpdateView }
    procedure SetVersion(const Value: string);
    procedure SetDescription(const Value: string);
    procedure SetPngRes(const Value: string);
    procedure SetModel(Model: IUpdateModel);
    procedure SetStatus(const Value: string);
    procedure SetUpdateState(Value: TUpdateState);
    procedure ShowMessage(Message: string);
    function ShowErrorMessage(Message: string): Boolean;
    procedure IUpdateView.Close = ViewClose;
    procedure ViewClose;
    procedure IUpdateView.Show = ViewShow;
    procedure ViewShow;
    procedure Progress(Progress, Length: Integer);
  end;
implementation

uses
  System.UITypes,

  Winapi.ShellApi;

{$R *.dfm}

procedure TFormUpdate.ButtonCancelClick(Sender: TObject);
begin
  Model.Cancel;
end;

procedure TFormUpdate.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Model.Cancel;
  Action := caNone;
end;

procedure TFormUpdate.LinkLabelTurboUpdateLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), nil, nil, 0);
end;

procedure TFormUpdate.Progress(Progress, Length: Integer);
begin
  ProgressBar.AnimationType := TActivityAnimationType.Progress;
  ProgressBar.Max           := Length;
  ProgressBar.Position      := Progress;
end;

procedure TFormUpdate.SetDescription(const Value: string);
begin
  LabelDescription.Caption := Value;
end;

procedure TFormUpdate.SetModel(Model: IUpdateModel);
begin
  Self.Model := Model;
end;

procedure TFormUpdate.SetPngRes(const Value: string);
var
  Png: TPngImage;
begin
  Png := TPngImage.Create;
  try
    Png.LoadFromResourceName(hInstance, PChar(Value));
    Image.Picture.Assign(Png);
  finally
    Png.Free;
  end;
end;

procedure TFormUpdate.SetStatus(const Value: string);
begin
  LabelStatus.Caption := Value;
end;

procedure TFormUpdate.SetUpdateState(Value: TUpdateState);
begin
  case Value of
    TUpdateState.Waiting:
      begin
        ProgressBar.Activate;
        ProgressBar.AnimationType := TActivityAnimationType.WindowsX;
        ButtonCancel.Enabled := False;
      end;
    TUpdateState.Downloading:
      begin
        ProgressBar.AnimationType := TActivityAnimationType.Progress;
        ButtonCancel.Enabled := True;
      end;
    TUpdateState.Unpacking:
      begin
        ProgressBar.AnimationType := TActivityAnimationType.Progress;
        ButtonCancel.Enabled := False;
      end;
    TUpdateState.Done:
      begin
        ProgressBar.Deactivate;
      end;
  end;
end;

procedure TFormUpdate.SetVersion(const Value: string);
begin
  LabelVersion.Caption := Value;
end;

procedure TFormUpdate.ViewShow;
begin
  if Application.MainForm <> Self then
   begin
    Self.Position  := poScreenCenter;  // Add by Renato Trevisan
    Self.FormStyle := fsStayOnTop;
   end;
  inherited Show;
end;

function TFormUpdate.ShowErrorMessage(Message: string): Boolean;
var
 Msg : iHDMessageDlg;
begin
  FConsts := TFactoryConsts.New.Consts;
  Result  := Msg
    .MsgTitle(FConsts.MsgTitle)
    .MsgQuestion('')
    .MsgBody(Message)
    .MsgIcon(TiError)
    .MsgType(TyQuestion)
    .DisplayQuestion;  // add by Renato Trevisan 12-1-24
end;

procedure TFormUpdate.ShowMessage(Message: string);
begin
  FConsts := TFactoryConsts.New.Consts;
  Msg := THDMessageDlg.New;
  Msg
   .MsgTitle(FConsts.MsgTitle)
   .MsgQuestion('')
   .MsgBody(Message)
   .MsgIcon(TiMessage)
   .MsgType(TyOK)
   .DisplayMessage; // add by Renato Trevisan 12-1-24
end;

procedure TFormUpdate.ViewClose;
begin
  OnClose := nil;
  inherited Close;
end;

end.
