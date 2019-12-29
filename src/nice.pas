{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit nice;

{$warn 5023 off : no warning about unused units}
interface

uses
  nicegrid, noteComps, niceRegister, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('niceRegister', @niceRegister.Register);
end;

initialization
  RegisterPackage('nice', @Register);
end.
