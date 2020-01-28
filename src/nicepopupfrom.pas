unit nicePopupFrom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, VirtualTrees;

type

  { TForm3 }

  TForm3 = class(TForm)
    Timer1: TTimer;
    VirtualStringTree1: TVirtualStringTree;
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
var
  XNode: PVirtualNode;
  XData: PTreeData;
  i: integer;
begin
  VirtualStringTree1.Clear;
  for i:=0 to Form2.CurrentItems.Count-1 do
  begin
    XNode := VirtualStringTree1.AddChild(nil);
    XData := VirtualStringTree1.GetNodeData(XNode);
    //XData^:= Form2.CurrentItems[0];
  end;
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

