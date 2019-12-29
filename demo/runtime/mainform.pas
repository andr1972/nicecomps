unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ActnList,
  NicePages, NiceGrid;

type

  { TForm1 }

  TForm1 = class(TForm)
    actFileClosePage: TAction;
    actFileNew: TAction;
    actFileOpen: TAction;
    actlFile: TActionList;
    imglFile16: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem3: TMenuItem;
    miFileClosePage: TMenuItem;
    miFileNew: TMenuItem;
    NicePages1: TNicePages;
    NiceGrid1: TNiceGrid;
    procedure actFileClosePageExecute(Sender: TObject);
    procedure actFileNewExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private

  public

  end;

var
  Form1: TForm1;

implementation
uses LCLType;

{$R *.lfm}

{ TForm1 }
procedure TForm1.actFileNewExecute(Sender: TObject);
var
  sheet: TNiceSheet;
  grid: TNiceGrid;
begin
  sheet := NicePages1.AddTabSheet();
  grid := TNiceGrid.Create(sheet);
  sheet.InsertControl(grid);
  sheet.FocusedControl := grid;
  grid.Align:=alClient;
  sheet.Caption:='Grid'+IntToStr(NicePages1.PageCount);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NicePages1:=TNicePages.Create(Self);
  InsertControl(NicePages1);
  NicePages1.Height:=280;
  NicePages1.Align:=alTop;
  NiceGrid1:=TNiceGrid.Create(Self);
  InsertControl(NiceGrid1);
  NiceGrid1.Align:=alClient;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Number: integer;
begin
  if Key in [ord('0')..Ord('9')] then
  begin
    if Shift<>[ssAlt] then exit;
    if Key>ord('0') then Number := Key-ord('1')
    else Number := 9;
    if Number<NicePages1.PageCount then
      NicePages1.PageIndex:=Number;
    Key:=0;//to avoid beep
  end else if (Key=VK_TAB) and (ssCtrl in Shift) then
  begin
    if ssShift in Shift then
      NicePages1.ActivePrev()
    else
      NicePages1.ActiveNext();
  end;
end;

procedure TForm1.actFileClosePageExecute(Sender: TObject);
var
  CanClose: TCloseEnum;
begin
  CanClose:=clClose;
  NicePages1.TryCloseSheet(NicePages1.ActiveSheet, CanClose);
end;

end.

