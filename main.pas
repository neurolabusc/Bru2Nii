unit main;

{$ifdef fpc}{$mode objfpc}{$H+} {$endif}

interface

uses
 {$ifndef fpc} ShellAPI,messages, strutils, {$endif}
  SysUtils, FileUtil, Forms, Controls, Dialogs, Buttons,
  StdCtrls, ExtCtrls,
  Classes;
   //Classes,  Graphics,  ComCtrls,  FileCtrl,
type
  { TForm1 }
  TForm1 = class(TForm)
    ConvertBtn: TSpeedButton;
    ConvertLabel: TLabel;
    FOVx10Check: TCheckBox;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure ConvertBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
  private
    { private declarations }
    {$ifndef fpc} procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES; {$endif}
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation
{$ifdef fpc}
 {$R *.lfm}
{$else}
 {$R *.DFM}
{$endif}
{ TForm1 }

procedure Showmsg(lStr: string);
begin
   Form1.Memo1.lines.Add(lStr);
end;

{$include common.inc}

procedure TForm1.ConvertBtnClick(Sender: TObject);
begin
     opendialog1.Filter := '"subject" or "acqp"|subject;acqp';
     opendialog1.Title:='Select  Bruker format file file';
     if not opendialog1.Execute then exit;
     Form1.Memo1.lines.Clear;
     BrConvertBatch (opendialog1.FileName,'', FOVx10Check.checked, true);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
     decimalseparator := '.';
     Showmsg(kVers);
     {$ifndef fpc} DragAcceptFiles(Handle, True); {$endif}
     //BrConvertBatch ('/Users/rorden/Downloads/100603/10/acqp','', FOVx10Check.checked, true);
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of String);
var
   lI: integer;
begin
     Form1.Memo1.lines.Clear;
     for lI := 0 to (length(FileNames)-1) do
         BrConvertBatch(Filenames[lI],'', FOVx10Check.checked, true);
end;

{$ifndef fpc}
procedure TForm1.WMDropFiles(var Msg: TWMDropFiles);  //implement drag and drop
const
  MAX_PATH = 1024;
var  CFileName: array[0..MAX_PATH] of Char;
begin
  try
    if DragQueryFile(Msg.Drop, 0, CFileName, MAX_PATH) > 0 then begin
      BrConvertBatch(CFilename,'', FOVx10Check.checked, true);
      Msg.Result := 0;
    end;
  finally
    DragFinish(Msg.Drop);
  end;
end; //WMDropFiles
{$endif}

end.

