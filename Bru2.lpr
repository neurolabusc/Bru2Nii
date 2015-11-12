program Bru2;
{$ifdef windows}
 {$APPTYPE CONSOLE}
{$endif}
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
 {$ifndef fpc} strutils, {$endif}
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, console;
begin
  //let console.pas do the work
end.


