unit dlgAbout;

interface

uses
  SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAboutBox = class(TForm)
    Panel1: TPanel;
    lblProductName: TLabel;
    lblVersion: TLabel;
    OKButton: TBitBtn;
    lblDate: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutBox: TAboutBox;

implementation
uses
  LCLType;

{$R *.lfm}

procedure TAboutBox.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key=char(VK_ESCAPE) then Close;
end;

end.
 
