program Bru2;
{$APPTYPE CONSOLE}
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


