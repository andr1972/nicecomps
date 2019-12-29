unit niceRegister;

interface

procedure Register;

implementation

uses
  Classes,
  LResources,
  nicegrid, noteComps;

procedure Register;
begin
  RegisterComponents('Nice', [TNicePages, TNiceGrid]);
end;


initialization
{$ifdef FPC}
  {$I nice.lrs}
{$endif}
end.
