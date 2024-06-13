# TurboUpdate

TurboUpdate foi projetado para ser uma forma simples e elegante de fazer atualizações automáticas.

## ⚙️ Instalação

#### Para instalar em seu projeto usando [boss](https://github.com/HashLoad/boss):
```sh
$ boss install github.com/Rtrevisan20/TurboUpdate
```
### Dependências
[HDmessageDlg](https://github.com/Rtrevisan20/HDMessageDlg)
[FreeEsVclComponents](https://github.com/errorcalc/FreeEsVclComponents)

#### Instalação Manual
  * Adicione as seguintes pastas ao seu projeto, em *Project > Options > Delphi Compiler > Search path*
```
../TurboUpdate/Source
../TurboUpdate/Source/VCL
../TurboUpdate/Source/FMX
../TurboUpdate/Source/Language
```

## Exemplos
  * Veja alguns exemplos: [samples](https://github.com/Rtrevisan20/TurboUpdate/tree/master/Samples)
  * Uses necessárias para a maioria dos usos

```delphi
uses 
  TurboUpdate.Model.Types, 
  TurboUpdate;
``` 
## Parâmetros obrigatórios
```delphi
begin
  GlobalUpdate
   .ExeNames(['executáveis a serem extraídos']) //StringArray
   .ExeNames(['&ALL']) // Ou pode passar o parâmetro &ALL para extrair tudo //StringArray 
   .Urls(['urls para buscar atualização']) //StringArray
   .KeyName('nome da chave do aquivo .ini que esta no GitHub') // String
   .Version(TFileVersion.CreateForFile(ParamStr(0))) // TFileVersion
   .Version(TFileVersion.Create('2.0.0.0')) // TFileVersion
   .ExecUpdateApp('Obrigatório no Modo Standalone: App a ser executado para atualizar o sistema. Default: Update.exe') // String
   .KillTaskApp('Obrigatório no Modo Standalone: App a ser fechado antes de continuar com a atualização. Ex: Update.exe'); //TFileName
end;   
```
## Parâmetros Opcionais
  * Para usar o parâmetro PngRes é necessário adicionar o recurso em: *Project > Resources and Images*, e passar o nome do recurso como parâmetro.
```delphi
begin
   GlobalUpdate
    .Description('Descrição que vai aparecer na tela de atualização. Default: ficará em branco')
    .RootPath('Nome da pasta onde deseja descompactar o executável atualizado. Default: Pasta Raiz do sistema')
    .PngRes('name.png');
end;   
```
## Funções e Procedures
```delphi
begin
  GlobalUpdate
   .UpdateThreadVCL // Usar quando for um app de linha de comando - VCL
   .UpdateThreadFMX // Usar quando for um app de linha de comando - FMX
   .Standalone      // Usar quando for chamar um app externo para fazer a atualização
   .UpdateVCL       // Usar quando não chamar um app externo, nesse caso a tela de atualização será VCL 
   .UpdateFMX       // Usar quando não chamar um app externo, nesse caso a tela de atualização será FMX
   .ChekUpdate      // Retorna um valor (boolean) para verificação de Update
   .GetVersion;     // Retorna um valor (TFileVersion) com a versão que esta no GitHub
end;   
```
## Como usar
  * Existem algumas formas diferentes de usar o TurboUpdate. Você pode configurar para VCL ou FMX e executar em Standalone.

#### Standalone
```delphi
begin
  GlobalUpdate
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Version(TFileVersion.CreateForFile(ParamStr(0)))
   .ExecUpdateApp('Default: Update.exe') //App a ser executado para atualizar
   .KillTaskApp(ExtractFileName(ParamStr(0))) //App a ser fechado antes da atualização
   .Standalone;
end;
```
#### Usando FMX passando o executável como parâmetro no Version 
```delphi
begin
  GlobalUpdate
   .ExeNames(['FmxApplication.exe'])
   .Urls(['https://raw.githubusercontent.com/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Description('TurboUpdate Atualizações...')
   .Version(TFileVersion.CreateForFile(ParamStr(0)))
   .RootPath('') //Pasta para descompactar - Opcional 
   .PngRes('') //Nome do Resource - Opcional
   .UpdateFMX;
end;
```
#### Usando VCL passando '2.0.0.0' como parâmetro no Version  
```delphi
begin
  GlobalUpdate
   .ExeNames(['VclApplication.exe'])
   .Urls(['https://raw.githubusercontent.com/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Description('TurboUpdate Atualizações...')
   .Version(TFileVersion.Create('2.0.0.0'))
   .RootPath('') //Opcional
   .PngRes('') //Opcional
   .UpdateVCL;
end;
```
#### Apenas um executável sem tela
  * Abrir *Project > View Source* e adicionar:
```delphi  
uses 
  TurboUpdate;
begin    
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  GlobalUpdate
   .ExeNames(['VclApplication.exe'])
   .Urls(['https://raw.githubusercontent.com/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Description('TurboUpdate Atualizações...')
   .UpdateThreadVCL;
  Application.Run;
end.  
```
#### Verificar se tem update no GitHub
  * Essa função retorna um valor (Boolean) True ou False.
```delphi  
begin
  GlobalUpdate
   .ExeNames(['VclApplication.exe'])
   .Urls(['https://raw.githubusercontent.com/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Vcl.Classic')
   .Description('TurboUpdate Atualizações...')
   .Version(TFileVersion.Create('2.0.0.0'))
   .ChekUpdate;    
end;
```
#### Buscar versão no Github 
```delphi  
begin
  Label1.Text := 'On GitHub - Version: ' + 
  GlobalUpdate
   .Urls(['https://raw.githubusercontent.com/Rtrevisan20/TurboUpdate/master/Update.ini'])
   .KeyName('TurboUpdate.Fmx.Standalone')
   .GetVersion.ToString;
end;
```