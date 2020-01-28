unit NiceFileForm;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,fgl,
  VirtualTrees, Messages;

type
  PTreeData = ^TTreeData;

  { TTreeData }

  TTreeData = record
    FileName: string;
    FileTime: longint;
    FileSize: int64;
    IsDirectory: boolean;
    function Compare(other: PTreeData; n: integer): integer;
    class operator = (X,Y:TTreeData): boolean;
  end;

  TFileItemList = specialize TFPGList<TTreeData>;

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ImageList: TImageList;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    VirtualStringTree1: TVirtualStringTree;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure VirtualStringTree1DblClick(Sender: TObject);
    procedure VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  private
    ComboItems: TFileItemList;
  public
    CurrentItems: TFileItemList;
  end;

var
  Form2: TForm2;

implementation
uses LCLType, nicePopupFrom;
{$R *.lfm}

function CompareInt(n1, n2: int64): integer;
begin
  if n2 > n1 then
    Result := 1
  else if n2 < n1 then
    Result := -1
  else
    Result := 0;
end;

function CompareBool(b1, b2: boolean): integer;
begin
  if b1 then
  begin
    if b2 then
      Result := 0
    else
      Result := -1;
  end
  else
  begin
    if b1 then
      Result := 1
    else
      Result := 0;
  end;
end;

function CompareName(name1, name2: string): integer;
begin
  Result := AnsiCompareText(name1, name2);
  if Result = 0 then
    Result := AnsiCompareStr(name1, name2);
end;

function IntToFormattedStr(number: int64; sep: char=''''; groupSize: integer=3): string;
var
  temp: string;
  i,sepcount: integer;
  posIn, posOut: integer;
begin
  if groupSize < 1 then
    raise Exception.Create('bad group size');
  temp := IntToStr(number);
  sepcount := (Length(temp)-1) div groupSize;
  posIn := Length(temp);
  posOut := posIn + sepcount;
  SetLength(result, posOut);
  while posIn >= 1 do
  begin
    for i := 1 to groupSize do
    begin
      if posOut<=0 then break;
      Result[posOut] := temp[posIn];
      dec(posIn);
      dec(posOut);
    end;
    if posOut<=0 then break;
    Result[posOut] := sep;
    dec(posOut);
  end;
  Assert(posIn=0);
  Assert(posOut=0);
end;

{ TForm2 }

procedure FillItems(path: string; items: TFileItemList);
var
  Data: TTreeData;
  sr: TSearchRec;
begin
  items.Clear;
  if FindFirst('*', faAnyFile, sr) = 0 then
    repeat
      if (sr.Name = '.') or (sr.Name = '..') then
        continue;
      Data.FileName := sr.Name;
      Data.FileTime := sr.Time;
      Data.IsDirectory := (sr.Attr and faDirectory) = faDirectory;
      if Data.IsDirectory then
        Data.FileSize := 0
      else
        Data.FileSize := sr.Size;
      items.Add(Data);
    until FindNext(sr) <> 0;
  FindClose(sr);
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  XNode: PVirtualNode;
  XData: PTreeData;
  i: integer;
begin
  VirtualStringTree1.Clear;
  FillItems('*',CurrentItems);
  for i:=0 to CurrentItems.Count-1 do
  begin
    XNode := VirtualStringTree1.AddChild(nil);
    XData := VirtualStringTree1.GetNodeData(XNode);
    XData^:= CurrentItems[i];
  end;
  VirtualStringTree1.SortTree(0, sdAscending);
end;

procedure TForm2.Edit1Change(Sender: TObject);
var
  p: TPoint;
begin
  Form3.Show;
{  if Length(Edit1.Text)>0 then
  begin
    p.x:=Edit1.Left;
    p.y:=Edit1.Top+Edit1.Height;
    p:=ClientToScreen(p);
    if (p.y+Form3.Height+40>=Screen.Height) then
    begin
      p.x:=Edit1.Left;
      p.y:=Edit1.Top-Form3.Height;
      p:=ClientToScreen(p);
    end;
    Form3.Left := p.x;
    Form3.Top := p.y;
    Form3.Width := Edit1.Width;
    if not Form3.Visible then
    begin
      Form3.Show;
    end;
  end else Form3.Hide;}
end;

procedure TForm2.FormActivate(Sender: TObject);
begin
  FormChangeBounds(Sender);
end;

procedure TForm2.FormChangeBounds(Sender: TObject);
var
  p: TPoint;
begin
  p.x:=Label1.Left+Label1.Width+10;
  p.y:=Label1.Top+Label1.Height;
  p:=ClientToScreen(p);
  Form3.Left := p.x;
  Form3.Top := p.y-Form3.Edit1.Height;
  if not Form3.Visible then
  begin
    Form3.Show;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  CurrentItems:=TFileItemList.Create;
  ComboItems:=TFileItemList.Create;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  ComboItems.Free;
  CurrentItems.Free;
end;

procedure TForm2.FormShow(Sender: TObject);
begin

end;

procedure TForm2.VirtualStringTree1CompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  Data1, Data2: PTreeData;
begin
  Data1 := Sender.GetNodeData(Node1);
  Data2 := Sender.GetNodeData(Node2);
  Result := Data1^.Compare(Data2, Column);
end;

procedure TForm2.VirtualStringTree1DblClick(Sender: TObject);
var
  Data: PTreeData;
begin
  Data := (Sender as TBaseVirtualTree).GetNodeData(VirtualStringTree1.FocusedNode);
  if Data^.IsDirectory then
    begin
      Chdir(Data^.FileName);
      Button3Click(nil);
    end else ShowMessage(Data^.FileName);
end;

procedure TForm2.VirtualStringTree1FreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PTreeData;
begin
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    Data^.FileName := '';
  end;
end;

procedure TForm2.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PTreeData;
begin
  if Column <> 0 then
    exit;
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if Data^.IsDirectory then
      ImageIndex := 1
    else
      ImageIndex := 0;
  end;
end;

procedure TForm2.VirtualStringTree1GetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
  NodeDataSize := SizeOf(TTreeData);
end;

procedure TForm2.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  Data: PTreeData;
  dt: TDateTime;
  st: TSystemTime;
begin
  Data := Sender.GetNodeData(Node);
  dt := FileDateToDateTime(Data^.FileTime);
  DateTimeToSystemTime(dt, st);
  case Column of
    0: CellText := Data^.FileName;
    1: CellText := Format('%.4d-%.2d-%.2d %.2d:%.2d', [st.Year, st.Month,
        st.Day, st.Hour, st.Minute]);
    2: if Data^.IsDirectory then
           CellText :=''
       else
           CellText :=ExtractFileExt(Data^.FileName);
    3: if Data^.IsDirectory then
           CellText := ''
       else
           CellText := IntToFormattedStr(Data^.FileSize);
  end;
end;

procedure TForm2.Button1Click(Sender: TObject);
begin
  OpenDialog1.Execute;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  SelectDirectoryDialog1.Execute;
end;

function TTreeData.Compare(other: PTreeData; n: integer): integer;
begin
  case n of
    0:
    begin
      Result := CompareBool(IsDirectory, other^.IsDirectory);
    end;
    1:
    begin
      Result := CompareInt(FileTime, other^.FileTime);
    end;
    2:
    begin
      Result := AnsiCompareText(ExtractFileExt(FileName),
        ExtractFileExt(other^.FileName));
    end;
    3:
    begin
      Result := -CompareBool(IsDirectory, other^.IsDirectory);
      if Result = 0 then
        Result := CompareInt(FileSize, other^.FileSize);
    end;
  end;
  if Result = 0 then
    Result := CompareName(FileName, other^.FileName);
end;

class operator TTreeData.=(X, Y: TTreeData): boolean;
begin
  result:=(X.FileName=Y.FileName) and (X.FileSize=Y.FileSize) and (X.FileTime=Y.FileTime)
                                  and (X.IsDirectory=Y.IsDirectory);
end;

end.
