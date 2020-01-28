unit nicePopupFrom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, VirtualTrees;

type

  { TForm3 }

  TForm3 = class(TForm)
    Edit1: TEdit;
    VirtualStringTree1: TVirtualStringTree;
    procedure FormActivate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public
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
  for i:=0 to 10 do ;//Form2.CurrentItems.Count-1 do
  begin
    XNode := VirtualStringTree1.AddChild(nil);
    XData := VirtualStringTree1.GetNodeData(XNode);
    //XData^:= Form2.CurrentItems[0];
  end;
end;

procedure TForm3.FormActivate(Sender: TObject);
begin

end;

end.

