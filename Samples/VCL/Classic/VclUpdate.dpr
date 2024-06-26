Program VclUpdate;

uses
  Vcl.Forms,
  System.SysUtils,
  TurboUpdate;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  GlobalUpdate
   .ExeNames(['VclApplication.exe', 'VclUpdate.exe'])
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Description('TurboUpdate/Vcl/Classic')
   .UpdateThreadVCL;

  Application.Run;
end.

