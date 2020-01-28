unit nicePopupFrom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm3 }

  TForm3 = class(TForm)
    ListBox1: TListBox;
    Timer1: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    beingActivated: boolean;
  end;

var
  Form3: TForm3;

implementation
uses NiceFileForm;
{$R *.lfm}

{ TForm3 }

procedure TForm3.FormShow(Sender: TObject);
begin

end;

procedure TForm3.FormActivate(Sender: TObject);
begin
  Timer1.Enabled:=true;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=false;
  Form2.Edit1.SetFocus;
  beingActivated:=false;
end;

end.

