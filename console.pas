unit console;
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}
interface



implementation



uses
 {$ifndef fpc} strutils, {$endif}
  Classes,SysUtils;

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
  writeln(' -o output filename');
  writeln(' -h show these help instructions');
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
  FOVx10, verbose: boolean;
begin
     FOVx10 := true;
     verbose := false;
     outFname := '';
     if ParamCount > 1 then begin
        i := 1;
        while i <= (ParamCount-1) do begin
              cmd := ParamStr(i);
              i := i + 1;
              if AnsiPos('-a', cmd) = 1 then FOVx10 := false;
              if AnsiPos('-h', cmd) = 1 then WriteHelp;
              if (AnsiPos('-o', cmd) = 1) and (i <= (ParamCount-1)) then begin
                 outFname := ParamStr(i);
                 i := i + 1;
              end;
              if AnsiPos('-v', cmd) = 1 then verbose := true;
        end;
     end;
     inFname := ParamStr(ParamCount);
     BrConvertBatch (inFname, outFname, FOVx10, verbose);
end;

begin
  decimalseparator := '.'; //Bruker will write real numbers as 1.23 not 1,23
  if (ParamCount = 0)  then
     WriteHelp
  else
      ProcessParamStr;
end.
 
