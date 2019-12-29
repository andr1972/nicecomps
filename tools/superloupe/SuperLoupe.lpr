program SuperLoupe;

uses
  Forms,
  Interfaces,
  frmLoupe,
  ColorDesc,
  frmClip,
  dlgAbout;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TLoupeForm, LoupeForm);
  Application.CreateForm(TClipForm, ClipForm);
  Application.CreateForm(TAboutBox, AboutBox);
  Application.Run;
end.
