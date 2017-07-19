unit console;
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}
interface

implementation

uses
 strutils,Classes,SysUtils;

procedure Showmsg(lStr: string);
begin
   Writeln(lStr);
end;

{$include common.inc}

procedure WriteHelp;
var
  E: string;
begin
  E := extractfilename(ParamStr(0));
  writeln('Usage: ',E,'[options] brukerfilename');
  writeln(kVers);
  writeln(' Converts Bruker "subject" or "acqp" MRI images');
  writeln('Options:');
  writeln(' -a actual size (otherwise x10 scale so animals match human)');
  writeln(' -f force conversion of localizer images (multiple slice orientations)');
  writeln(' -h show these help instructions');
  writeln(' -o output filename');
  writeln(' -p append protocol name to output filename');
  writeln(' -s append series type ID to output filename');
  writeln(' -v verbose conversion comments');
  writeln('Examples:');
{$IFDEF UNIX}
  writeln(' '+E+' /Users/bruker/mri/1/acqp');
  writeln(' '+E+' -v -a /Users/bruker/mri/subject');
  writeln(' '+E+' -o /Users/cr/mynifti /Users/bruker/mri/subject');
  writeln(' '+E+' "/Users/spaces in filename/subject"');
{$ELSE}
  writeln(' '+E+' c:\bruker\mri\1\acqp');
  writeln(' '+E+' -v -a c:\bruker\mri\1\acqp');
  writeln(' '+E+' -o c:\newnifti c:\bruker\mri\1\acqp');
  writeln(' '+E+' "c:\spaces in filename\mri\1\acqp"');
{$ENDIF}
end;

procedure ProcessParamStr;
var
  inFname, outFname, cmd: string;
  i: integer;
  FOVx10, verbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID: boolean;
begin
    FOVx10 := true;
    Verbose := false;
    AppendProtocolName := false;
    AppendSeriesTypeID := false;
    OnlyConvert3D := true;
    outFname := '';
    i := 1;
    while i <= (ParamCount) do begin
          cmd := ParamStr(i);
          i := i + 1;
          if AnsiPos('-', cmd) = 1 then begin
            if AnsiPos('-a', cmd) = 1 then
               FOVx10 := false
            else if AnsiPos('-f', cmd) = 1 then
               OnlyConvert3D := false
            else if AnsiPos('-h', cmd) = 1 then
               WriteHelp
            else if AnsiPos('-p', cmd) = 1 then
               AppendProtocolName := true
            else if AnsiPos('-s', cmd) = 1 then
                 AppendSeriesTypeID := true
            else if (AnsiPos('-o', cmd) = 1) and (i <= (ParamCount-1)) then begin
               outFname := ParamStr(i);
               i := i + 1;
            end else if AnsiPos('-v', cmd) = 1 then
               Verbose := true
            else
                showmsg('Unknown command (is this software obsolete?) '+cmd);
          end else
              inFname := cmd;
    end;
    writeln(kVers);
    BrConvertBatch (inFname, outFname, FOVx10, Verbose, OnlyConvert3D, AppendProtocolName, AppendSeriesTypeID);
end;

begin
  DefaultFormatSettings.DecimalSeparator := '.'; //Bruker will write real numbers as 1.23 not 1,23
  if (ParamCount = 0)  then
     WriteHelp
  else
      ProcessParamStr;
end.

