unit niceRegister;

interface

procedure Register;

implementation

uses
  Classes,
  LResources,
  NiceGrid, NicePages;

procedure Register;
begin
  RegisterComponents('Nice', [TNicePages, TNiceGrid]);
end;


initialization
{$ifdef FPC}
  {$I nice.lrs}
{$endif}
end.
