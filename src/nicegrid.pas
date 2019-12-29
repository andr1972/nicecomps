//MIT License

{$mode objfpc}{$H+}
unit NiceGrid;

interface

uses
  Types,messages,SysUtils, Classes, LMessages,
  Graphics, Controls, StdCtrls,ImgList,Forms,contnrs;

type
  TnGridRow = class;
  TnBaseGrid = class;

  TItemCompare = function (Item1,Item2:Pointer; Column:integer): Integer;
  TnResizeFieldArea = (rfaNone, rfaHeader, rfaWhole, rfaWholeAllow);//rfaWholeAllow - allow other actions, for example select
  TnDistingOnInplace = (doiMouseDown,doiKeybMove,doiSetFocused,doiPaint);//source of event
  TGridDrawState = set of (gdSelected, gdFocused, gdFixed);
  TGridMoveSource = (gmsKeyboard, gmsMouse, gmsCallFocusGo, gmsSetFocused);
  TnGridMoveEvent = procedure(Sender:TnBaseGrid; fromCol,fromRow: integer;
      var toCol,toRow: integer; GridMoveSource: TGridMoveSource) of object;
  TnGridInplace = procedure(Sender:TnBaseGrid; ACol,ARow: integer; Disting:TnDistingOnInplace) of object;
  TnResizeCellsEvent = procedure(Sender:TnBaseGrid;
      index,prevSize: integer; var newSize: integer) of object;
  TnCustomDrawCellEvent = procedure(Sender:TnBaseGrid; Canvas:TCanvas; R:TRect;
    ACol,ARow: Longint; State:TGridDrawState; var DefaultDraw:boolean) of object;
  TnResizeTogether = (rtSeparately, rtAll, rtAllWithHeader);

  TnCell = class
  private
    FOwnerRow: TnGridRow;
    FText: UTF8String;
    FImageIndex: TImageIndex;
    FData: pointer;
    FAlignment: TAlignment;
    procedure SetText(const Value: UTF8String);
    procedure SetData(const Value: pointer);
    procedure SetAlignment(const Value: TAlignment);
  protected
    FIndex: integer;
    procedure Changed(AllItems: boolean; Data:integer);
  public
    constructor Create(AOwnerRow: TnGridRow);
    procedure Clear;
    property Index: integer read FIndex;
    property OwnerRow: TnGridRow read FOwnerRow;
    property Data: pointer read FData write SetData;
    property Text: UTF8String read FText write SetText;
    property ImageIndex: TImageIndex read FImageIndex write FImageIndex;
    property Alignment: TAlignment read FAlignment write SetAlignment;
  end;

  TnColl = class;

  TnCollItem = class(TPersistent)
  private
    FCollection: TnColl;
    FIndex: integer;
  protected
    procedure Changed(AllItems: boolean; SubItem:TObject; Data:integer);
  public
    constructor Create(Collect: TnColl); virtual;
    constructor CreateWithIndex(Collect:TnColl; Index:integer);
    function Next: TnCollItem;
    function Prev: TnCollItem;
    property Collect: TnColl read FCollection;
    property Index: integer read FIndex;
  end;

  TnCollItemClass = class of TnCollItem;
  TnColl = class(TPersistent)
  private
    FUpdateCount: Cardinal;
    procedure ReverseList;
    procedure RestoreIndex(start:integer = 0);
  protected
    FItems: TObjectList;
    FItemClass: TnCollItemClass;
    function GetCount: integer; virtual;
    procedure SetCount(const Value: integer); virtual;
    function GetItem(Index: integer): TnCollItem; virtual;
    procedure Update(Item: TnCollItem; SubItem:TObject; Data:integer); virtual;
    procedure Changed;
    function Add: TnCollItem;
  public
    constructor Create(ItemClass: TnCollItemClass);
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function Insert(index: integer): TnCollItem;
    procedure Delete(index: integer);
    procedure Clear; virtual;
    procedure CustomSort(Compare:TItemCompare; Column:integer=0; Reverse:boolean = false); virtual;
    property UpdateCount: Cardinal read FUpdateCount;
    property Count: integer read GetCount write SetCount;
    property Items: TObjectList read FItems;
    property DefItems[Index: integer]: TnCollItem read GetItem; default;
  end;

  TnGridRow = class(TnCollItem)
  private
    FCellList: TObjectList;
    function Get(AIndex: integer): TnCell;
    procedure Put(AIndex: integer; const Value: TnCell);
    function GetCount: integer;
  protected
    procedure RestoreIndex(start:integer = 0);
  public
    constructor Create(Collection: TnColl); override;
    destructor Destroy; override;
    function AddCell: TnCell;
    function InsertCell(AIndex: integer): TnCell;
    procedure DeleteCell(AIndex: integer);
    property CellList: TObjectList read FCellList;
    property Cells[AIndex: integer]: TnCell read Get write Put; default;
    property Count: integer read GetCount;
  end;

  TnRowList = class(TnColl)
  private
    FOwnerGrid: TnBaseGrid;
    function Get(index: integer): TnGridRow;
    procedure Put(index: integer; const Value: TnGridRow);
  protected
    procedure Update(Item: TnCollItem; SubItem:TObject; Data:integer); override;
  public
    constructor Create(GridControl: TnBaseGrid);
    function Add: TnGridRow;
    function Insert(index: integer): TnGridRow;
    property Rows[index: Integer]: TnGridRow read Get write Put; default;
    property OwnerGrid: TnBaseGrid read FOwnerGrid;
  end;

  TnBaseGrid = class(TCustomControl)
  protected
    FColorFocusShadow: TColor;
    FColorFixedBkg: TColor;
    FColorTableFace: TColor;
    FColorMeshLine: TColor;
    FColorFixesMeshLine: TColor;
    FInplaceControl: TControl;
    FOnInplaceEnter: TnGridInplace;
    FOnInplaceLeave: TnGridInplace;
    FOnMove: TnGridMoveEvent;
    FFocusedChanged: boolean;
    FFocusedRow: Longint;
    FFocusedCol: Longint;
    procedure SetFocusedCol(const Value: integer);
    procedure SetFocusedRow(const Value: integer);
    procedure SetFocusedColRow(const ACol,ARow: integer);
    procedure InplaceMove(fromCol,fromRow,toCol,toRow:integer; Disting:TnDistingOnInplace);
    property InplaceControl: TControl read FInplaceControl write FInplaceControl;
  public
    function IsCellFocused(Cell: TnCell): boolean;
    procedure UpdateForItems(Items:TnRowList; Row:TnGridRow; Cell:TnCell; Data:integer); virtual;
    property FocusedCol: Longint read FFocusedCol write SetFocusedCol;
    property FocusedRow: Longint read FFocusedRow write SetFocusedRow;
    property ColorFocusShadow: TColor read FColorFocusShadow write FColorFocusShadow;
    property ColorFixedBkg: TColor read FColorFixedBkg write FColorFixedBkg;
    property ColorTableFace: TColor read FColorTableFace write FColorTableFace;
    property ColorMeshLine: TColor read FColorMeshLine write FColorMeshLine;
    property ColorFixesMeshLine: TColor read FColorFixesMeshLine write FColorFixesMeshLine;
    property OnInplaceEnter: TnGridInplace read FOnInplaceEnter write FOnInplaceEnter;
    property OnInplaceLeave: TnGridInplace read FOnInplaceLeave write FOnInplaceLeave;
  end;

 {
  todo: TiSparseGrid = class(TnBaseGrid)
  efficient management memory for theoretically huge tables.
  Like Excel could have 256 cols and 65536 rows (or even more)
  but not all of this cells really in memory
  only part of table that is really filled
 }
  TNiceGrid = class(TnBaseGrid)
  private
    FRowList: TnRowList;
    FRowCount: Longint;
    FColCount: Longint;
    FFixedCols: integer;
    FFixedRows: integer;
    FColWidths: TList;
    FRowHeights: TList;
    FDefaultColWidth: integer;
    FDefaultRowHeight: integer;
    FSumColWidths: int64;
    FSumRowHeights: int64;
    FViewRect: TRect;
    FReadyToResizeH: integer;
    FReadyToResizeV: integer;
    FResizedH: integer;
    FResizedV: integer;
    FStartResizeX: integer;
    FStartResizeY: integer;
    FStartResizeWidth: integer;
    FStartResizeHeight: integer;
    FResizeColArea: TnResizeFieldArea;
    FResizeRowArea: TnResizeFieldArea;
    FResizeColsTogether: TnResizeTogether;
    FResizeRowsTogether: TnResizeTogether;
    FOnScroll: TNotifyEvent;
    FOnResizeH: TnResizeCellsEvent;
    FOnResizeV: TnResizeCellsEvent;
    FShowRowNumbers:boolean;
    FShowColNumbers:boolean;
    FHeaderWidth: integer;
    FHeaderHeight: integer;
    procedure SetShowColNumbers(Value: boolean);
    procedure SetShowRowNumbers(Value: boolean);
    procedure SetHeaderHeight(Value: integer);
    procedure SetHeaderWidth(Value: integer);
{$ifdef VisualCLX}
    function WidgetFlags: integer; override;
{$else}
    procedure WMEraseBkgnd(var Msg: TLMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSetFocus(var Msg: TLMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg : TLMKillFocus); message WM_KILLFOCUS;
{$endif}
    function SumFixedX: integer;
    function SumFixedY: integer;
    procedure ScrollChange(Sender: TObject);
    procedure SetColCount(Value: Longint);
    procedure SetRowCount(Value: Longint);
    procedure SetFixedCols(Value: integer);
    procedure SetFixedRows(Value: integer);
    function GetColWidths(index: Integer): integer;
    procedure SetColWidths(index: integer; Value: integer);
    function GetRowHeights(index: integer): integer;
    procedure SetRowHeights(index: integer; Value: integer);
    function GetFirstCol: Longint;
    procedure SetFirstCol(const Value: Longint);
    function GetFirstRow: Longint;
    procedure SetFirstRow(const Value: Longint);
    function GetCellStr(ACol,ARow: integer): UTF8String;
    procedure SetCellStr(ACol,ARow: integer; const Value: UTF8String);
    function GetCells(ACol,ARow: integer): TnCell;
  protected
    HScroll,VScroll: TScrollBar;
    FOnCustomDrawCell: TnCustomDrawCellEvent;
    procedure DrawFocus;
    function GetResizeAreaH(X: integer): integer;
    function GetResizeAreaV(Y: integer): integer;
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState);override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      {$ifdef VisualCLX}const{$endif} MousePos: TPoint): Boolean; override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
{$ifndef VisualCLX}
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
{$endif}
{$ifdef VisualCLX}
    function NeedKey(Key: Integer; Shift: TShiftState; const KeyText: UnicodeString): Boolean; override;
{$else}
    procedure CMWantSpecialKey(var Msg: {$ifdef FPC}TWMKey{$else}TCMWantSpecialKey{$endif}); message CM_WANTSPECIALKEY;
{$endif}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear(removeCells: boolean);
    //todo: insert and delete row and col
    procedure CalcViewRect;
    procedure AppearFocused;
    procedure CorrectProperties;
    procedure AddRow;
    procedure InsertRow(index: integer);
    procedure DeleteRow(index: integer);
    procedure AddCol;
    procedure InsertCol(index: integer);
    procedure DeleteCol(index: integer);
    procedure SetTotalAlignment(Value: TAlignment);
    procedure SetColAlignment(ACol:integer; Value: TAlignment);
    procedure SetRowAlignment(ARow:integer; Value: TAlignment);
    function FindCellRect(out R:TRect; ACol,ARow: integer):boolean;
    function FindCellAt(out Col,Row: integer; X,Y: integer):boolean;
    procedure FocusGoUp(KeyBoadCall:boolean=false);
    procedure FocusGoDown(KeyBoadCall:boolean=false);
    procedure FocusGoPageUp(KeyBoadCall:boolean=false);
    procedure FocusGoPageDown(KeyBoadCall:boolean=false);
    procedure FocusGoFirstRow(KeyBoadCall:boolean=false);
    procedure FocusGoLastRow(KeyBoadCall:boolean=false);
    procedure FocusGoLeft(KeyBoadCall:boolean=false);
    procedure FocusGoRight(KeyBoadCall:boolean=false);
    procedure SetEveryColWidth(AWidth: integer);
    procedure SetEveryRowHeight(AHeight: integer);
    property FirstCol: Longint read GetFirstCol write SetFirstCol;
    property FirstRow: Longint read GetFirstRow write SetFirstRow;
    property ColWidths[index: Longint]: integer read GetColWidths write SetColWidths;
    property RowHeights[index: Longint]: integer read GetRowHeights write SetRowHeights;
    property CellStr[ACol,ARow: integer]: UTF8String read GetCellStr write SetCellStr;
    property Cells[ACol,ARow: integer]: TnCell read GetCells; default;
    property RowList: TnRowList read FRowList;
  published
    property ColCount: Longint read FColCount write SetColCount default 5;
    property RowCount: Longint read FRowCount write SetRowCount default 5;
    property FixedCols: integer read FFixedCols write SetFixedCols default 1;
    property FixedRows: integer read FFixedRows write SetFixedRows default 1;
    property DefaultColWidth: integer read FDefaultColWidth write FDefaultColWidth default 64;
    property DefaultRowHeight: integer read FDefaultRowHeight write FDefaultRowHeight default 19;
    property ResizeColArea: TnResizeFieldArea read FResizeColArea write FResizeColArea default rfaHeader;
    property ResizeRowArea: TnResizeFieldArea read FResizeRowArea write FResizeRowArea default rfaHeader;
    property ResizeColsTogether: TnResizeTogether read FResizeColsTogether write FResizeColsTogether default rtSeparately;
    property ResizeRowsTogether: TnResizeTogether read FResizeRowsTogether write FResizeRowsTogether default rtAll;
    property OnMove: TnGridMoveEvent read FOnMove write FOnMove;
    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
    property OnResizeH: TnResizeCellsEvent read FOnResizeH write FOnResizeH;
    property OnResizeV: TnResizeCellsEvent read FOnResizeV write FOnResizeV;
    property OnCustomDrawCell: TnCustomDrawCellEvent read FOnCustomDrawCell write FOnCustomDrawCell;
    property ShowRowNumbers: boolean read FShowRowNumbers write SetShowRowNumbers default true;
    property ShowColNumbers: boolean read FShowColNumbers write SetShowColNumbers default true;
    property HeaderWidth: integer read FHeaderWidth write SetHeaderWidth default 32;
    property HeaderHeight: integer read FHeaderHeight write SetHeaderHeight default 19;
    property InplaceControl;
    property OnInplaceEnter;
    property OnInplaceLeave;
    property Align;
    property Anchors;
    property Font;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnMouseMove;
    property TabOrder;
    property TabStop;
    property PopupMenu;
    property Visible;
  end;

implementation

uses
  Math, LCLType, Dialogs;

const
  MinColWidth = 0;
  FrameWidth = 1;
  SpaceToResize = 40;

{ TnBaseGrid }

procedure TnBaseGrid.InplaceMove(fromCol, fromRow, toCol, toRow: integer;
  Disting: TnDistingOnInplace);
begin
  if InplaceControl<>nil then
  begin
    if Assigned(FOnInplaceLeave) then FOnInplaceLeave(self,fromCol,fromRow,Disting);
    if Assigned(FOnInplaceEnter) then FOnInplaceEnter(self,toCol,toRow,Disting);
  end;
end;

function TnBaseGrid.IsCellFocused(Cell: TnCell): boolean;
begin
  if Cell=nil then
  begin
    result:=false;
    exit;
  end;
  result:=(Cell.Index=FocusedCol)and(Cell.OwnerRow.Index=FocusedRow);
end;

procedure TnBaseGrid.SetFocusedCol(const Value: integer);
var
  prevFocusedCol: integer;
begin
  if Value<>FFocusedCol then
  begin
    prevFocusedCol := FFocusedCol;
    FFocusedCol := Value;
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,FFocusedRow,FFocusedCol,FFocusedRow,gmsSetFocused);
    InplaceMove(FFocusedCol,FFocusedRow,Value,FFocusedRow,doiSetFocused);
    Invalidate;
  end
end;

procedure TnBaseGrid.SetFocusedRow(const Value: integer);
var
  prevFocusedRow: integer;
begin
  if Value<>FFocusedRow then
  begin
    prevFocusedRow := FFocusedRow;
    FFocusedRow := Value;
    if Assigned(FOnMove) then
      FOnMove(self, FFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,gmsSetFocused);
    InplaceMove(FFocusedCol,FFocusedRow,FFocusedCol,Value,doiSetFocused);
    Invalidate;
  end;
end;

procedure TnBaseGrid.SetFocusedColRow(const ACol,ARow: integer);
var
  prevFocusedCol: integer;
  prevFocusedRow: integer;
begin
  if (ACol<>FFocusedCol)or(ARow<>FFocusedRow) then
  begin
    prevFocusedCol := FFocusedCol;
    prevFocusedRow := FFocusedRow;
    FFocusedCol := ACol;
    FFocusedRow := ARow;
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,gmsSetFocused);
    InplaceMove(prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,doiSetFocused);
    Invalidate;
  end;
end;

procedure TnBaseGrid.UpdateForItems(Items:TnRowList; Row:TnGridRow; Cell:TnCell; Data:integer);
begin
  if (data=1) and IsCellFocused(Cell) then FFocusedChanged:=true;
  Invalidate;
end;

{ TnCell }

procedure TnCell.Changed(AllItems: boolean; Data:integer);
begin
  if OwnerRow<>nil then OwnerRow.Changed(AllItems,self,Data);
end;

procedure TnCell.Clear;
begin
  FText:='';
  FImageIndex:=-1;
  FData:=nil;
end;

constructor TnCell.Create(AOwnerRow: TnGridRow);
begin
  FOwnerRow:=AOwnerRow;
  FImageIndex:=-1;
end;

procedure TnCell.SetAlignment(const Value: TAlignment);
begin
  if Value<>FAlignment then
  begin
    FAlignment := Value;
    OwnerRow.Changed(false,self,0);
  end;
end;

procedure TnCell.SetData(const Value: pointer);
begin
  FData := Value;
end;

procedure TnCell.SetText(const Value: UTF8String);
begin
  FText := Value;
  Assert(OwnerRow.Collect is TnRowList);
  if TnRowList(OwnerRow.Collect).UpdateCount=0 then
    OwnerRow.Changed(false,self,1);
end;

{ TNiceGrid }
procedure TNiceGrid.AddCol;
begin
  InsertCol(ColCount);
end;

procedure TNiceGrid.AddRow;
begin
  InsertRow(RowCount);
end;

procedure TNiceGrid.AlignControls(AControl: TControl; var ARect: TRect);
begin
//I don't call inherited, cause CLX paint in it
  if (csFramed in ControlStyle)or(VScroll.Kind=sbHorizontal) then exit;
  if HScroll.Visible or (csDesigning in ComponentState) then
  begin
    HScroll.Left:=FrameWidth;
    HScroll.Top:=Height-HScroll.Height-FrameWidth;
    HScroll.Width:=Width-2*FrameWidth;
  end;
  if VScroll.Visible or (csDesigning in ComponentState) then
  begin
    VScroll.Left:=Width-VScroll.Width-FrameWidth;
    VScroll.Top:=FrameWidth;
    if HScroll.Visible then
      VScroll.Height:=Height-2*FrameWidth-HScroll.Height
    else
      VScroll.Height:=Height-2*FrameWidth;
  end;
end;

procedure TNiceGrid.AppearFocused;
var
  i,Sum:integer;
begin
  if FFocusedRow<FirstRow then FirstRow:=FFocusedRow
  else
  begin
    Sum:=FViewRect.Bottom-FViewRect.Top-SumFixedY;
    if ShowColNumbers then dec(Sum,HeaderHeight);
    for i:=FFocusedRow downto FirstRow do
    begin
      dec(Sum,RowHeights[i]);
      if Sum<0 then
      begin
        FirstRow:=min(i+1,FocusedRow);
        break;
      end;
    end;
  end;
  if FFocusedCol<FirstCol then FirstCol:=FFocusedCol
  else
  begin
    Sum:=FViewRect.Right-FViewRect.Left-SumFixedX;
    if ShowRowNumbers then dec(Sum,HeaderWidth);
    for i:=FFocusedCol downto FirstCol do
    begin
      dec(Sum,ColWidths[i]);
      if Sum<0 then
      begin
        FirstCol:=min(i+1,FocusedCol);
        break;
      end;
    end;
  end;
end;

procedure TNiceGrid.CalcViewRect;
var
  VirtualWidth, VirtualHeight: integer;
begin
  VirtualWidth:=FSumColWidths+max(SpaceToResize,FViewRect.Right-FViewRect.Left-SumFixedX);
  VirtualHeight:=FSumRowHeights+max(SpaceToResize,FViewRect.Bottom-FViewRect.Top-SumFixedY);
  FViewRect.Left:=0;
  FViewRect.Top:=0;
  FViewRect.Right:=Width;
  FViewRect.Bottom:=Height;
  VScroll.Min:=FFixedRows;
  HScroll.Min:=FFixedCols;
  if (FSumColWidths+SpaceToResize>=FViewRect.Right-FViewRect.Left)or
     (FSumRowHeights+SpaceToResize>=FViewRect.Bottom-FViewRect.Top) then
  begin
    if FSumColWidths+SpaceToResize>=FViewRect.Right-FViewRect.Left then
    begin
      HScroll.Visible:=true;
      FViewRect.Bottom:=FViewRect.Bottom-HScroll.Height;
      if FSumRowHeights+SpaceToResize>=FViewRect.Bottom-FViewRect.Top then
      begin
        VScroll.Visible:=true;
        FViewRect.Right:=FViewRect.Right-VScroll.Width;
      end else VScroll.Visible:=false;
    end else
    begin
      VScroll.Visible:=true;
      FViewRect.Right:=FViewRect.Right-VScroll.Width;
      if FSumColWidths+SpaceToResize>=FViewRect.Right-FViewRect.Left then
      begin
        HScroll.Visible:=true;
        FViewRect.Bottom:=FViewRect.Bottom-HScroll.Height;
      end else HScroll.Visible:=false;
    end;
    HScroll.Max:=ColCount-1; {FindMaxFirstCol}
    VScroll.Max:=RowCount-1;  {FindMaxFirstRow;}
    HScroll.PageSize:=HScroll.Max*Width div VirtualWidth;
    VScroll.PageSize:=VScroll.Max*Height div VirtualHeight;
  end else
  begin
    VScroll.Visible:=false;
    HScroll.Visible:=false;
    VScroll.Max:=FFixedRows;
    HScroll.Max:=FFixedCols;
    VScroll.Position:=FFixedRows;
    HScroll.Position:=0;
  end;
  if not HScroll.Visible and (HScroll.Position>HScroll.Min)
  then HScroll.Position:=HScroll.Min;
  if not VScroll.Visible and (VScroll.Position>VScroll.Min)
  then VScroll.Position:=VScroll.Min;
  InflateRect(FViewRect,-FrameWidth,-FrameWidth);//my inner frame
end;

procedure TNiceGrid.Clear(removeCells: boolean);
var
  GridRow: TnGridRow;
  i,j: integer;
begin
  if removeCells then
  begin
    RowCount:=0;
    ColCount:=0;
  end else
  for i:=0 to RowCount-1 do
  begin
    GridRow:=RowList[i];
    for j:=0 to ColCount-1 do
      GridRow[j].Clear;
  end;
  Invalidate;
end;

procedure TNiceGrid.CMWantSpecialKey(var Msg: TWMKey);
begin
  if Msg.CharCode in [VK_TAB,VK_ESCAPE] then
    Msg.result := 0
  else
    Msg.result := 1;
end;

procedure TNiceGrid.CorrectProperties;
begin
  if FFixedCols>FColCount then FFixedCols:=FColCount;
  if FFixedCols<0 then FFixedCols:=0;
  if FFixedRows>FRowCount then FFixedRows:=FRowCount;
  if FFixedRows<0 then FFixedRows:=0;
  if FFocusedCol>=FColCount then FFocusedCol:=FColCount-1;
  if FFocusedCol<0 then FFocusedCol:=0;
  if FFocusedRow>=FRowCount then FFocusedRow:=FRowCount-1;
  if FFocusedRow<0 then FFocusedRow:=0;
end;

constructor TNiceGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ColorFocusShadow := TColor($80d0ff);
  ColorFixedBkg := clBtnFace;
  ColorTableFace := clWindow;
  FColorMeshLine := clSilver;
  FColorFixesMeshLine := clGray;
  TabStop:=true;
  Width:=320;
  Height:=120;
  FReadyToResizeH := -2;
  FReadyToResizeV := -2;
  FResizedH := -2;
  FResizedV := -2;
  HScroll := TScrollBar.Create(self);
  VScroll := TScrollBar.Create(self);
  HScroll.Parent := self;
  VScroll.Parent := self;
  VScroll.Kind := sbVertical;
  HScroll.Align := alBottom;
  VScroll.Align := alRight;
  FDefaultColWidth:=64;
  FDefaultRowHeight:=19;
  FResizeColArea:=rfaHeader;
  FResizeRowArea:=rfaHeader;
  FResizeColsTogether:=rtSeparately;
  FResizeRowsTogether:=rtAll;
  FRowList := TnRowList.Create(self);
  FColWidths := TList.Create;
  FRowHeights:= TList.Create;
  ColCount:=5;
  RowCount:=5;
  FixedCols := 1;
  FixedRows := 1;
  FirstCol :=1;
  FirstRow :=1;
  FocusedCol :=1;
  FocusedRow :=1;
  FShowRowNumbers:=true;
  FShowColNumbers:=true;
  FHeaderWidth:=32;
  FHeaderHeight:=19;
  HScroll.OnChange := @ScrollChange;
  VScroll.OnChange := @ScrollChange;
  Font.Name := 'Default';
end;

procedure TNiceGrid.DeleteCol(index: integer);
var
  i: integer;
begin
  for i:=0 to FRowCount-1 do
    RowList[i].DeleteCell(index);
  dec(FSumColWidths,ColWidths[index]);
  FColWidths.Delete(index);
  dec(FColCount);
  CorrectProperties;
  if RowList.UpdateCount=0 then
    Invalidate;
end;

procedure TNiceGrid.DeleteRow(index: integer);
var
  Row: TnGridRow;
  j: integer;
begin
  Row:=FRowList[index];
  for j:=FColCount-1 downto 0 do Row.DeleteCell(j);
  FRowList.Delete(index);
  dec(FSumRowHeights,RowHeights[index]);
  FRowHeights.Delete(index);
  dec(FRowCount);
  CorrectProperties;
  if RowList.UpdateCount=0 then
    Invalidate;
end;

destructor TNiceGrid.Destroy;
begin
  Clear(true);
  FRowHeights.Free;
  FColWidths.Free;
  FRowList.Free;
  HScroll.Free;
  VScroll.Free;
  inherited;
end;

function TNiceGrid.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      {$ifdef VisualCLX}const{$endif} MousePos: TPoint): Boolean;
begin
  if WheelDelta<0 then
    FirstRow:=min(FirstRow+3,RowCount-1)
  else if WheelDelta>0
  then FirstRow:=max(FirstRow-3,FFixedRows);
  result:=true;
  Invalidate;
end;

function TNiceGrid.FindCellAt(out Col,Row: integer; X,Y: integer):boolean;
var
  sum,i: integer;
  boundX,boundY: integer;
begin
  boundX:=SumFixedX;
  if ShowRowNumbers then inc(boundX,HeaderWidth);
  Col:=0;
  if X<boundX then
  begin
    sum:=0;
    if ShowRowNumbers then inc(sum,HeaderWidth);
    Col:=-1;
    if FixedCols<1 then Col:=0 else
    for i:=0 to FixedCols-1 do
    begin
      inc(sum, ColWidths[i]);
      if sum>=X then
      begin
        Col:=i;
        break;
      end;
    end;
    result:=Col>=0;
  end else
  begin
    sum:=boundX;
    Col:=-1;
    if FColCount-1<FirstCol then Col:=FirstCol else
    for i:=FirstCol to FColCount-1 do
    begin
      inc(sum, ColWidths[i]);
      if sum>=X then
      begin
        Col:=i;
        break;
      end;
    end;
    result:=Col>=0;
  end;
  if not result then exit;
  Row:=0;
  boundY:=SumFixedY;
  if ShowColNumbers then inc(boundY,HeaderHeight);
  if Y<boundY then
  begin
    sum:=0;
    if ShowColNumbers then inc(sum,HeaderHeight);
    Row:=-1;
    if FixedRows<1 then Row:=0 else
    for i:=0 to FixedRows-1 do
    begin
      inc(sum, RowHeights[i]);
      if sum>=Y then
      begin
        Row:=i;
        break;
      end;
    end;
    result:=Row>=0;
  end else
  begin
    sum:=boundY;
    Row:=-1;
    if FRowCount-1<FirstRow then Row:=FirstRow else
    for i:=FirstRow to FRowCount-1 do
    begin
      inc(sum, RowHeights[i]);
      if sum>=Y then
      begin
        Row:=i;
        break;
      end;
    end;
    result:=Row>=0;
  end;
end;

function TNiceGrid.FindCellRect(out R:TRect; ACol,ARow: integer):boolean;
var
  i: integer;
begin
  R.Left:=FViewRect.Left;
  R.Top:=FViewRect.Top;
  if ShowRowNumbers then inc(R.Left,HeaderWidth);
  if ShowColNumbers then inc(R.Top,HeaderHeight);
  result:=false;
  if (RowCount<1)or(ColCount<1) then exit;
  if ACol<FixedCols then
  begin
    for i:=0 to ACol-1 do
    begin
      inc(R.Left,ColWidths[i]);
      if R.Left>=FViewRect.Right then exit;
    end;
  end else
  begin
    if ACol<FirstCol then exit;
    inc(R.Left,SumFixedX);
    for i:=FirstCol to ACol-1 do
    begin
      inc(R.Left,ColWidths[i]);
      if R.Left>=FViewRect.Right then exit;
    end;
  end;
  R.Right:=min(R.Left+ColWidths[ACol],FViewRect.Right);
  if ARow<FixedRows then
  begin
    for i:=0 to ARow-1 do
    begin
      inc(R.Top,RowHeights[i]);
      if R.Top>=FViewRect.Bottom then exit;
    end;
  end else
  begin
    if ARow<FirstRow then exit;
    inc(R.Top,SumFixedY);
    for i:=FirstRow to ARow-1 do
    begin
      inc(R.Top,RowHeights[i]);
      if R.Top>=FViewRect.Bottom then exit;
    end;
  end;
  R.Bottom:=min(R.Top+RowHeights[ARow],FViewRect.Bottom);
  result:=true;
end;

procedure TNiceGrid.FocusGoUp(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  if FFocusedRow>0 then dec(FFocusedRow);
  if FFocusedRow<FirstRow then
    FirstRow:=FFocusedRow;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoDown(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  if FFocusedRow<RowCount-1 then inc(FFocusedRow);
  AppearFocused;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoPageUp(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  ypos,tempFirstRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  if FFocusedRow<0 then exit;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  if FFocusedRow>FirstRow then
    FFocusedRow:=FirstRow
  else if FFocusedRow=FirstRow then
  begin
    ypos:=FViewRect.Top+SumFixedY+RowHeights[FFocusedRow];
    tempFirstRow:=FirstRow;
    if ypos>FViewRect.Bottom then
    begin //visible only one (and not whole) Row
      if tempFirstRow>FixedRows then dec(tempFirstRow);
    end else
    while (ypos<=FViewRect.Bottom)and(tempFirstRow>FixedRows) do
    begin
      dec(tempFirstRow);
      inc(ypos, RowHeights[tempFirstRow]);
    end;
    FirstRow:=tempFirstRow;
    FFocusedRow:=FirstRow;
  end else FirstRow:=FFocusedRow;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoPageDown(KeyBoadCall:boolean=false);
var
  R: TRect;
  function gotoLastVisible:integer;
  var
    n: integer;
  begin
    result:=R.Top;
    for n:=FFocusedRow to RowCount-1 do
    begin
      inc(result,RowHeights[n]);
      if result>FViewRect.Bottom then
      begin
        FFocusedRow:=n-1;
        break;
      end;
    end;
    if n=RowCount then FFocusedRow:=RowCount-1;
  end;
var
  prevFocusedCol,prevFocusedRow: integer;
  fixedY,tempFirstRow,ypos: integer;
  prevFirstRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  if FFocusedRow>=RowCount-1 then exit;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  if not FindCellRect(R,FocusedCol,FocusedRow) then
  begin
    AppearFocused;
    exit;
  end;
  if R.Top+RowHeights[FFocusedRow]+RowHeights[FFocusedRow+1]<=FViewRect.Bottom then
    gotoLastVisible
  else
  begin
    prevFirstRow:=FirstRow;
    fixedY:=SumFixedY;
    if ShowColNumbers then inc(fixedY,HeaderHeight);
    for tempFirstRow:=FirstRow to FFocusedRow+1 do
    begin
      dec(R.Top, RowHeights[tempFirstRow]);
      if R.Top<fixedY then break;
    end;
    tempFirstRow:=FFocusedRow+1;
    ypos:=gotoLastVisible;
    if FFocusedRow=RowCount-1 then //focused on screen bottom
    begin
      while ypos<FViewRect.Bottom do
      begin
        dec(tempFirstRow);
        inc(ypos,RowHeights[tempFirstRow]);
      end;
      tempFirstRow:=min(max(tempFirstRow+1,prevFirstRow),RowCount-1);
    end;
    FirstRow:=tempFirstRow;
  end;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoFirstRow(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  FirstRow:=FFixedRows;
  FocusedRow:=0;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoLastRow(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  FocusedRow:=RowCount-1;
  AppearFocused;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoLeft(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  if FFocusedCol>0 then dec(FFocusedCol);
  if (FFocusedCol<FirstCol) then
    FirstCol:=FFocusedCol;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.FocusGoRight(KeyBoadCall:boolean=false);
var
  prevFocusedCol,prevFocusedRow: integer;
  GridMoveSource: TGridMoveSource;
begin
  if KeyBoadCall then GridMoveSource:=gmsKeyboard
  else GridMoveSource:=gmsCallFocusGo;
  prevFocusedCol:=FocusedCol;
  prevFocusedRow:=FocusedRow;
  if FFocusedCol<ColCount-1 then inc(FFocusedCol);
  AppearFocused;
  if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
  begin
    if Assigned(FOnMove) then
      FOnMove(self, prevFocusedCol,prevFocusedRow,FFocusedCol,FFocusedRow,GridMoveSource);
    //FOnMove can deny move
    if (prevFocusedCol<>FocusedCol)or(prevFocusedRow<>FocusedRow) then
    begin
      InplaceMove(prevFocusedCol,prevFocusedRow,FocusedCol,FocusedRow,doiKeybMove);
      Invalidate;
    end;
  end;
end;

procedure TNiceGrid.DrawFocus;
var
  R: TRect;
begin
  if Focused then
    Canvas.Pen.Color:=clBlack
  else
    Canvas.Pen.Color:=clGray;
  if FindCellRect(R, FocusedCol,FocusedRow) then
  begin
    if (InplaceControl<>nil)and InplaceControl.Visible then
    begin
      InplaceControl.Left:=R.Left;
      InplaceControl.Top:=R.Top;
      InplaceControl.Width:=R.Right-R.Left;
      InplaceControl.Height:=R.Bottom-R.Top;
      if FFocusedChanged then
      begin
        if Assigned(FOnInplaceEnter) then FOnInplaceEnter(self,FocusedCol,FocusedRow,doiPaint);
        FFocusedChanged:=false;
      end;
    end else
    begin
      Canvas.Pen.Width:=3;
      Canvas.Pen.Mode:=pmNotXor;
      Canvas.Brush.Style:=bsClear;
      Canvas.Rectangle(R);
    end;
  end else
end;

function TNiceGrid.GetCells(ACol,ARow: integer): TnCell;
begin
  result := RowList[ARow][ACol];
end;

function TNiceGrid.GetCellStr(ACol,ARow: integer): UTF8String;
begin
  if (ACol<0)or(ACol>=ColCount)or(ARow<0)or(ARow>=RowCount) then
  begin
    result:='';
    exit;
  end;
  result := RowList[ARow][ACol].Text;
end;

function TNiceGrid.GetColWidths(index: Integer): integer;
begin
  result := integer(FColWidths[index]);
end;

function TNiceGrid.GetFirstCol: Longint;
begin
  result:=HScroll.Position;
end;

function TNiceGrid.GetResizeAreaH(X: integer): integer;
const ResizeWidth = 6;
var
  n,SumWidths,d: integer;
  RW: integer;
begin
  if ShowRowNumbers then
  if Abs(X-HeaderWidth)<=ResizeWidth then
  if (ColCount=0)or(X<HeaderWidth+ColWidths[0]) then//enable expand shrinken col 0
  begin
    result:=-1;
    exit;
  end;
  if ColCount=0 then
  begin
    result:=-2;
    exit;
  end;
  //fixed part
  SumWidths:=FViewRect.Left;
  if ShowRowNumbers then inc(SumWidths,HeaderWidth);
  for n:=0 to FFixedCols-1 do
  begin
    inc(SumWidths, ColWidths[n]);
    if ResizeColsTogether=rtSeparately then
    begin
      if SumWidths>X then break
    end else
      if SumWidths+ResizeWidth>X then break;
  end;
  d:=SumWidths-X;
  if n<FFixedCols then
  begin
    RW:=(ColWidths[n]-3*ResizeWidth div 2) div 2;
    RW:=min(max(RW,0),ResizeWidth);
    if ColWidths[n]-d<=RW then
    begin
      if n>0 then result:=n-1 //-1 - resize header, -2 none
      else result:=-2;
    end
    else
    if d<=RW then
         result:=n
    else result:=-2;
  end else result:=-2;
  if result>-2 then exit;
  if SumWidths>X+ResizeWidth then exit;
  //scrolled part
  for n:=FirstCol to ColCount-1 do
  begin
    inc(SumWidths, ColWidths[n]);
    if ResizeColsTogether=rtSeparately then
    begin
      if SumWidths>X then break //enable expand shrunk col
    end else
      if SumWidths+ResizeWidth>X then break; //when all zero - don't catch last
  end;
  d:=SumWidths-X;
  if n<ColCount then
  begin
    RW:=(ColWidths[n]-3*ResizeWidth div 2) div 2;
    RW:=min(max(RW,0),ResizeWidth);
    if ColWidths[n]-d<=RW then
    begin
      if n>0 then result:=n-1 //-1 - resize header, -2 none
      else result:=-2;
    end else
    if d<=RW then
         result:=n
    else result:=-2;
  end else
  begin
    RW:=ResizeWidth;
    if -d<=RW then result:=n-1 else result:=-2;
  end;
end;

function TNiceGrid.GetResizeAreaV(Y: integer): integer;
const ResizeHeight = 4;
var
  n,SumHeights,d: integer;
  RW: integer;
begin
  if ShowColNumbers then
  if Abs(Y-HeaderHeight)<=ResizeHeight then
  if (RowCount=0)or(Y<HeaderHeight+RowHeights[0]) then//enable expand shrinken row 0
  begin
    result:=-1;
    exit;
  end;
  if RowCount=0 then
  begin
    result:=-2;
    exit;
  end;
  //fixed part
  SumHeights:=FViewRect.Top;
  if ShowColNumbers then inc(SumHeights,HeaderHeight);
  for n:=0 to FFixedRows-1 do
  begin
    inc(SumHeights, RowHeights[n]);
    if ResizeRowsTogether=rtSeparately then
    begin
      if SumHeights>Y then break;
    end else
      if SumHeights+ResizeHeight>Y then break;
  end;
  d:=SumHeights-Y;
  if n<FFixedRows then
  begin
    RW:=(RowHeights[n]-3*ResizeHeight div 2) div 2;
    RW:=min(max(RW,0),ResizeHeight);
    if RowHeights[n]-d<=RW then
    begin
      if n>0 then result:=n-1 //-1 - resize header, -2 none
      else result:=-2;
    end else
    if d<=RW then
         result:=n
    else result:=-2;
  end else result:=-2;
  if result>-2 then exit;
  if SumHeights>Y+ResizeHeight then exit;
  //scrolled part
  for n:=FirstRow to RowCount-1 do
  begin
    inc(SumHeights, RowHeights[n]);
    if ResizeRowsTogether=rtSeparately then
    begin
      if SumHeights>Y then break; //enable expand shrunk row
    end else
      if SumHeights+ResizeHeight>Y then break; //when all zero - don't catch last
  end;
  d:=SumHeights-Y;
  if n<RowCount then
  begin
    RW:=(RowHeights[n]-3*ResizeHeight div 2) div 2;
    RW:=min(max(RW,0),ResizeHeight);
    if RowHeights[n]-d<=RW then
    begin
      if n>0 then result:=n-1 //-1 - resize header, -2 none
      else result:=-2;
    end else
    if d<=RW then
         result:=n
    else result:=-2;
  end else
  begin
    RW:=ResizeHeight;
    if -d<=RW then result:=n-1 else result:=-2;
  end;
end;

function TNiceGrid.GetRowHeights(index: integer): integer;
begin
  result := integer(FRowHeights[index]);
end;

function TNiceGrid.GetFirstRow: Longint;
begin
  result:=VScroll.Position;
end;

procedure TNiceGrid.InsertCol(index: integer);
 procedure SetDefW;
 begin
   FColWidths.Insert(index, pointer(FDefaultColWidth));
   inc(FSumColWidths,FDefaultColWidth);
 end;
 procedure SetUsedW;
 var
   n,w: integer;
 begin
   if index<FColCount then n:=index //from next
   else if FColCount>0 then n:=index-1 //index=FColCount <> 0; for add at end - from prev
   else n:=-1;//previous FColCount
   if n>=0 then w:=integer(FColWidths[n])
   else w:=FDefaultColWidth;
   FColWidths.Insert(index, pointer(w));
   inc(FSumColWidths,w);
 end;
var
  i: integer;
begin
  for i:=0 to FRowCount-1 do
    RowList[i].InsertCell(index);
  case ResizeColsTogether of
    rtSeparately: SetDefW;
    rtAll,rtAllWithHeader: SetUsedW;
  end;
  inc(FColCount);
  if RowList.UpdateCount=0 then
    Invalidate;
end;

procedure TNiceGrid.InsertRow(index: integer);
 procedure SetDefH;
 begin
   FRowHeights.Insert(index, pointer(FDefaultRowHeight));
   inc(FSumRowHeights,FDefaultRowHeight);
 end;
 procedure SetUsedH;
 var
   n,w: integer;
 begin
   if index<FRowCount then n:=index //from next
   else if FRowCount>0 then n:=index-1 //index=FRowCount <> 0; for add at end - from prev
   else n:=-1;//previous FRowCount
   if n>=0 then w:=integer(FRowHeights[n])
   else w:=FDefaultRowHeight;
   FRowHeights.Insert(index, pointer(w));
   inc(FSumRowHeights,w);
 end;
var
  Row: TnGridRow;
  j: integer;
  CellList: TObjectList;
  Cell: TnCell;
begin
  if index>0 then //copy Align from prev or first
    CellList:=RowList[index-1].FCellList
  else if RowCount>0 then
    CellList:=RowList[0].FCellList
  else
    CellList:=nil;
  Row:=RowList.Insert(index);
  for j:=0 to FColCount-1 do
  begin
    Cell:=Row.AddCell;
    if CellList<>nil then Cell.Alignment:=TnCell(CellList[j]).Alignment;
  end;
  case ResizeRowsTogether of
    rtSeparately: SetDefH;
    rtAll,rtAllWithHeader: SetUsedH;
  end;
  inc(FRowCount);
  Assert(FRowCount=RowList.Count);
  if RowList.UpdateCount=0 then
    Invalidate;
end;

procedure TNiceGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited; //user's handling OnKeyDown
  if Key = 0 then exit;
  case Key of
    VK_UP: FocusGoUp(true);
    VK_DOWN: FocusGoDown(true);
    VK_PRIOR: FocusGoPageUp(true);
    VK_NEXT: FocusGoPageDown(true);
    VK_HOME: FocusGoFirstRow(true);
    VK_END: FocusGoLastRow(true);
    VK_LEFT: FocusGoLeft(true);
    VK_RIGHT: FocusGoRight(true);
  end;
end;

procedure TNiceGrid.MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
var
  Col,Row: integer;
begin
  SetFocus; //can going from inplace edit
  if FReadyToResizeH>=-1 then
  begin
    FResizedH := FReadyToResizeH;
    FReadyToResizeH := -2;
    FStartResizeX := X;
    if FResizedH>=0 then
      FStartResizeWidth := ColWidths[FResizedH]
    else
      FStartResizeWidth := HeaderWidth;
    if ResizeColArea<>rfaWholeAllow then exit;
  end else
  if FReadyToResizeV>=-1 then
  begin
    FResizedV := FReadyToResizeV;
    FReadyToResizeV := -2;
    FStartResizeY := Y;
    if FResizedV>=0 then
      FStartResizeHeight := RowHeights[FResizedV]
    else
      FStartResizeHeight := HeaderHeight;
    if ResizeRowArea<>rfaWholeAllow then exit;
  end;
  if FindCellAt(Col,Row, X,Y) then
    SetFocusedColRow(Col,Row);
end;

procedure TNiceGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  prevSize,newSize: integer;
  slowfactor: integer;
  boundX,boundY: integer;
begin
  FReadyToResizeH := -1;
  FReadyToResizeV := -1;
  if FResizedH>=0 then
  begin
    prevSize := ColWidths[FResizedH];
    if ResizeColsTogether in [rtAll,rtAllWithHeader] then
    begin
      slowfactor:=max(FResizedH+1-FirstCol+FFixedCols,1);
      if ResizeRowsTogether=rtAllWithHeader then inc(slowfactor);
      newSize := FStartResizeWidth+(X-FStartResizeX) div slowfactor;
      if Assigned(FOnResizeH) then
        FOnResizeH(self,-2,prevSize,newSize);
      SetEveryColWidth(newSize);
      if ResizeColsTogether=rtAllWithHeader then HeaderWidth:=newSize;
    end else
    begin
      ColWidths[FResizedH] := FStartResizeWidth+X-FStartResizeX;
      newSize := ColWidths[FResizedH];
      if Assigned(FOnResizeH) then
        FOnResizeH(self,FResizedH,prevSize,newSize);
      ColWidths[FResizedH]:=newSize;
    end;
  end else if FResizedV>=0 then
  begin
    prevSize := RowHeights[FResizedV];
    if ResizeRowsTogether in [rtAll,rtAllWithHeader] then
    begin
      slowfactor:=max(FResizedV+1-FirstRow+FFixedRows,1);
      if ResizeRowsTogether=rtAllWithHeader then inc(slowfactor);
      newSize := FStartResizeHeight+(Y-FStartResizeY) div slowfactor;
      if Assigned(FOnResizeV) then
        FOnResizeV(self,-2,prevSize,newSize);
      SetEveryRowHeight(newSize);
      if ResizeRowsTogether=rtAllWithHeader then HeaderHeight:=newSize;
    end else
    begin
      RowHeights[FResizedV] := FStartResizeHeight+Y-FStartResizeY;
      newSize := RowHeights[FResizedV];
      if Assigned(FOnResizeV) then
        FOnResizeV(self,FResizedV,prevSize,newSize);
      RowHeights[FResizedV]:= newSize;
    end;
  end else
  if FResizedH=-1 then
  begin
    prevSize:=HeaderWidth;
    newSize:=max(FStartResizeWidth+X-FStartResizeX,0);
    if Assigned(FOnResizeH) then
        FOnResizeH(self,-1,prevSize,newSize);
    HeaderWidth:=newSize;
  end else
  if FResizedV=-1 then
  begin
    prevSize:=HeaderHeight;
    newSize:=max(FStartResizeHeight+Y-FStartResizeY,0);
    if Assigned(FOnResizeV) then
        FOnResizeV(self,-1,prevSize,newSize);
    HeaderHeight:=newSize;
  end else
  case ResizeColArea of
    rfaNone:
    begin
      FReadyToResizeH:=-2;
      Cursor := crDefault;
    end;
    rfaHeader:
    begin
      boundY:=FViewRect.Top+SumFixedY;
      if ShowColNumbers then inc(boundY,HeaderHeight);
      if Y<boundY then
      begin
        FReadyToResizeH := GetResizeAreaH(X);
        if FReadyToResizeH>=-1 then Cursor := crHSplit
        else Cursor := crDefault;
      end else
      begin
        FReadyToResizeH:=-2;
        Cursor := crDefault;
      end;
    end;
    rfaWhole,rfaWholeAllow:
    begin
      FReadyToResizeH := GetResizeAreaH(X);
      if FReadyToResizeH>=-1 then Cursor := crHSplit
      else Cursor := crDefault;
    end;
  end;
  if FReadyToResizeH<-1 then
  case ResizeRowArea of
    rfaNone:
    begin
      FReadyToResizeV:=-2;
      Cursor := crDefault;
    end;
    rfaHeader:
    begin
      boundX:=FViewRect.Left+SumFixedX;
      if ShowRowNumbers then inc(boundX,HeaderWidth);
      if X<boundX then
      begin
        FReadyToResizeV := GetResizeAreaV(Y);
        if FReadyToResizeV>=-1 then Cursor := crVSplit
        else Cursor := crDefault;
      end else
      begin
        FReadyToResizeV:=-2;
        Cursor := crDefault;
      end;
    end;
    rfaWhole,rfaWholeAllow:
    begin
      FReadyToResizeV := GetResizeAreaV(Y);
      if FReadyToResizeV>=0 then Cursor := crVSplit
      else Cursor := crDefault;
    end;
  end;
end;

procedure TNiceGrid.MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
begin
  FResizedH := -2;
  FResizedV := -2;
end;

{$ifndef VisualCLX}
procedure TNiceGrid.CMMouseLeave(var Message: TMessage);
begin
  Cursor := crDefault;
end;
{$endif}

{$ifdef VisualCLX}
function TiGrid.NeedKey(Key: Integer; Shift: TShiftState;
  const KeyText: UnicodeString): Boolean;
begin
  result := not((Key=iKEY_TAB) or (Key=iKEY_ESCAPE));
end;
{$endif}

procedure DrawBorder(ACanvas:TCanvas; R:TRect; APressed,AMouseOver:boolean; AWidth:integer);
var
  i: integer;
begin
  with ACanvas do
  begin
    if APressed then
    begin
      if AMouseOver then Pen.Color:=clBlack
      else Pen.Color:=clBtnShadow
    end else Pen.Color:=clBtnHighlight;
    for i:=1 to AWidth do
    begin
      MoveTo(R.Right-i, R.Top-1+i);
      LineTo(R.Left-1+i, R.Top-1+i);
      LineTo(R.Left-1+i, R.Bottom-i);
    end;
    //Bottom,Right
    if APressed then
       Pen.Color:=clBtnHighlight
    else
    begin
      if AMouseOver then Pen.Color:=clBlack
      else Pen.Color:=cl3DDkShadow;
    end;
    for i:=1 to AWidth do
    begin
      MoveTo(R.Left-1+i, R.Bottom-i);
      LineTo(R.Right-i, R.Bottom-i);
      LineTo(R.Right-i, R.Top+i-1);
    end;
  end;
end;


procedure TNiceGrid.Paint;
var
  CellList: TObjectList;
  R: TRect;
const
  wasRaise: boolean=false;
  countBad: integer=0;

  function DrawCell(ncol,nrow: integer): boolean;
  const MaxStackLen=128;
  var
    alignedPosX,stringWidth: integer;
    WideSize: TSize;
    DefaultDraw: boolean;
    State: TGridDrawState;
    Cell: TnCell;
    WidePtr: PWideChar;
//    WideBuf: array[0..MaxStackLen] of WideChar;
    LenWide: integer;
  begin
    if (ncol>0)and(R.Left=FViewRect.Left)and(ColWidths[ncol]>0)
      and(ColWidths[ncol-1]=0) then
    begin //Draw first zero-width cell as red
      Canvas.Pen.Color:=clRed;
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Left, R.Bottom);
      inc(R.Left);
    end;
    if (nrow>0)and(R.Top=FViewRect.Top)and(RowHeights[nrow]>0)
      and (RowHeights[nrow-1]=0) then
    begin //Draw first zero-height cell as red
      Canvas.Pen.Color:=clRed;
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);
      inc(R.Top);
    end;
    if (nrow<FixedRows)or(ncol<FixedCols) then
    begin
      if (RowHeights[nrow]<3)or(ColWidths[ncol]<3) then
      begin
        Canvas.Brush.Color:=clRed;
        Canvas.Pen.Color:=clRed;
      end else
      begin
        Canvas.Brush.Color:=ColorFixedBkg;
        Canvas.Pen.Color:=ColorFixesMeshLine;
      end;
    end else
    begin
      Canvas.Brush.Color:=ColorTableFace;
      Canvas.Pen.Color:=ColorMeshLine;
    end;
    Cell:=CellList[ncol] as TnCell;
{@todo delto    LenWide:=UTF8To16(Cell.FText,nil,0);
    if LenWide<=MaxStackLen then WidePtr:=@WideBuf
    else GetMem(WidePtr,(LenWide+1)*2);
    UTF8To16(Cell.FText,WidePtr,LenWide+1);}
    R.Right:=min(R.Left+ColWidths[ncol],FViewRect.Right);
    if (ncol<FFixedCols)or(nrow<FFixedRows) then State:=[gdFixed]
    else State:=[];
    if (ncol=FFocusedCol)and(nrow=FFocusedRow) then State:=State+[gdFocused];
    if (ncol=FFocusedCol)and(nrow=FFocusedRow) and (InplaceControl<>nil)
      and InplaceControl.Visible then
      DefaultDraw := false
    else
    begin
      DefaultDraw := true;
      if Assigned(FOnCustomDrawCell) then
        FOnCustomDrawCell(self, Canvas, R, ncol,nrow, State, DefaultDraw);
    end;
    if DefaultDraw then
    begin
      WideSize:=Canvas.TextExtent(Cell.FText);
{      GetTextExtentExPointW(Canvas.Handle, WidePtr, LenWide,
         ColWidths[ncol], nil, nil, WideSize);}
      stringWidth:=WideSize.cx;
      case Cell.Alignment of
        taCenter: alignedPosX:=max((ColWidths[ncol]-stringWidth) div 2,3);
        taRightJustify: alignedPosX:=max(ColWidths[ncol]-stringWidth-4,3);
        else alignedPosX:=3
      end;
      Canvas.TextOut(R.Left+alignedPosX,R.Top+3,Cell.FText);
{@todo delto      if ExtTextOutW(Canvas.Handle, R.Left+alignedPosX,R.Top+3,
            ETO_CLIPPED or ETO_OPAQUE, @R,WidePtr,LenWide,nil)=false then
      begin
        inc(countBad);
        if GetLastError()=2 then
        if not wasRaise then
        begin
           wasRaise:=true;
           raise Exception.Create('No/bad font file '+Canvas.Font.Name);
        end;
      end; }
    end;
    Canvas.MoveTo(R.Left,R.Bottom-1);
    Canvas.LineTo(R.Right-1,R.Bottom-1);
    Canvas.LineTo(R.Right-1,R.Top);
    R.Left:=R.Right;
    result:=R.Right>=FViewRect.Right;
//    if WidePtr<>@WideBuf then FreeMem(WidePtr);
  end;

  function DrawHeaderCell(ncol,nrow: integer): boolean;
  var
    Text: UTF8String;
    stringWidth,alignedPosX: integer;
    WideSize: TSize;
  begin
    if (nrow=FocusedRow)and(ncol<=FocusedCol)
     or(ncol=FocusedCol)and(nrow<=FocusedRow)
    then
      Canvas.Brush.Color:=ColorFocusShadow
    else
      Canvas.Brush.Color:=ColorFixedBkg;
    Canvas.Pen.Color:=ColorFixesMeshLine;
    if ncol<0 then
      R.Right:=min(R.Left+HeaderWidth,FViewRect.Right)
    else
      R.Right:=min(R.Left+ColWidths[ncol],FViewRect.Right);
    if nrow<0 then
    begin
      if ncol>=0 then Text:=IntToStr(ncol) else Text:='';
    end else Text:=IntToStr(nrow);
    WideSize:=Canvas.TextExtent(Text);
    stringWidth:=WideSize.cx; //@todo delto TextWidthUTF8(Canvas,Text);
    if ncol<0 then
      alignedPosX:=max((HeaderWidth-stringWidth) div 2,3)
    else
      alignedPosX:=max((ColWidths[ncol]-stringWidth) div 2,3);
    Canvas.TextRect(R,R.Left+alignedPosX,R.Top+3,Text);
    Canvas.MoveTo(R.Left,R.Bottom-1);
    Canvas.LineTo(R.Right-1,R.Bottom-1);
    Canvas.LineTo(R.Right-1,R.Top);
    R.Left:=R.Right;
    result:=R.Right>=FViewRect.Right;
  end;

  ///true if no more lines
  function DrawRow(nrow: integer):boolean;
  var
    j:integer;
  begin
    Assert(nrow>=0);
    CellList:=FRowList[nrow].CellList;
    Assert(CellList.Count=ColCount);
    R.Bottom:=min(R.Top+RowHeights[nrow],FViewRect.Bottom);
    R.Left:=FViewRect.Left;
    if ShowRowNumbers then DrawHeaderCell(-1,nrow);
    for j:=0 to FixedCols-1 do
      if DrawCell(j,nrow) then break;
    for j:=FirstCol to ColCount-1 do
      if DrawCell(j,nrow) then break;
    Canvas.Brush.Color:=ColorTableFace;
    Canvas.FillRect(Rect(R.Right,R.Top,FViewRect.Right,R.Bottom));
    R.Top:=R.Bottom;
    result:=R.Bottom>=FViewRect.Bottom;
  end;
  procedure DrawColNumbersRow;
  var
    j:integer;
  begin
    R.Bottom:=min(R.Top+HeaderHeight,FViewRect.Bottom);
    R.Left:=FViewRect.Left;
    if ShowRowNumbers then DrawHeaderCell(-1,-1);
    for j:=0 to FixedCols-1 do
      if DrawHeaderCell(j,-1) then break;
    for j:=FirstCol to ColCount-1 do
      if DrawHeaderCell(j,-1) then break;
    Canvas.Brush.Color:=ColorTableFace;
    Canvas.FillRect(Rect(R.Right,R.Top,FViewRect.Right,R.Bottom));
    R.Top:=R.Bottom;
  end;
var
  i: integer;
begin
  countBad:=0;
  CorrectProperties;
  CalcViewRect;
  Canvas.Font:=Font;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Mode:=pmCopy;
  Canvas.Pen.Width:=1;
  R.Top:=FViewRect.Top;
  R.Bottom:=R.Top;
  if ShowColNumbers then
    DrawColNumbersRow;
  if RowCount>0 then
  begin
    for i:=0 to FixedRows-1 do
      if DrawRow(i) then break;
    for i:=FirstRow to RowCount-1 do
      if DrawRow(i) then break;
  end;
  Canvas.Brush.Color:=ColorTableFace;
  Canvas.FillRect(Rect(FViewRect.Left,R.Bottom,FViewRect.Right,FViewRect.Bottom));
  DrawBorder(Canvas,ClientRect,true,false,1);
  DrawFocus;
  if countBad=0 then wasRaise:=false;
end;

procedure TNiceGrid.SetColAlignment(ACol:integer; Value: TAlignment);
var
  i: integer;
  CellList: TObjectList;
begin
  for i:=0 to FRowCount-1 do
  begin
    CellList:=RowList[i].FCellList;
    TnCell(CellList[ACol]).Alignment:=Value;
  end;
end;

procedure TNiceGrid.SetRowAlignment(ARow:integer; Value: TAlignment);
var
  i: integer;
  CellList: TObjectList;
begin
  CellList:=RowList[ARow].FCellList;
  for i:=0 to FColCount-1 do
    TnCell(CellList[i]).Alignment:=Value;
end;

procedure TNiceGrid.SetShowColNumbers(Value: boolean);
begin
  if Value<>FShowColNumbers then
  begin
    FShowColNumbers:=Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetShowRowNumbers(Value: boolean);
begin
  if Value<>FShowRowNumbers then
  begin
    FShowRowNumbers:=Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetHeaderHeight(Value: integer);
begin
  if Value<>FHeaderHeight then
  begin
    FHeaderHeight:=Value;
    Invalidate;
  end
end;

procedure TNiceGrid.SetHeaderWidth(Value: integer);
begin
  if Value<>FHeaderWidth then
  begin
    FHeaderWidth:=Value;
    Invalidate;
  end
end;

procedure TNiceGrid.SetTotalAlignment(Value: TAlignment);
var
  i,j: integer;
  CellList: TObjectList;
begin
  for i:=0 to FRowCount-1 do
  begin
    CellList:=RowList[i].FCellList;
    for j:=0 to CellList.Count-1 do
      TnCell(CellList[j]).Alignment:=Value;
  end;
end;

procedure TNiceGrid.ScrollChange(Sender: TObject);
begin
  if Assigned(FOnScroll) then FOnScroll(self);
  Invalidate;
end;

procedure TNiceGrid.SetCellStr(ACol,ARow: integer; const Value: UTF8String);
begin
  if (ACol<0)or(ACol>=ColCount)or(ARow<0)or(ARow>=RowCount) then
    raise Exception.Create('TiGrid.SetCells out of range');
  FRowList[ARow][ACol].Text := Value;
  if (ARow=FFocusedRow)and(ACol=FFocusedCol) then FFocusedChanged:=true;
end;

procedure TNiceGrid.SetColCount(Value: Longint);
var
  Row: TnGridRow;
  i,j: integer;
begin
  if Value<0 then Value:=0;
  if Value=FColCount then exit;
  for i:=0 to FRowCount-1 do
  begin
    Row:=FRowList[i];
    if Value>FColCount then
    begin
      for j:=FColCount to Value-1 do Row.AddCell;
    end else
    begin
      for j:=FColCount-1 downto Value do Row.DeleteCell(j)
    end;
  end;
  if Value>FColCount then
    for j:=FColCount to Value-1 do
    begin
      FColWidths.Add(pointer(FDefaultColWidth));
      inc(FSumColWidths,FDefaultColWidth);
    end
  else
    for j:=FColCount-1 downto Value do
    begin
      dec(FSumColWidths,ColWidths[j]);
      FColWidths.Delete(j);
    end;
  FColCount:=Value;
  Assert(FColWidths.Count=Value);
  Assert(FSumColWidths>=0);
  Invalidate;
end;

procedure TNiceGrid.SetColWidths(index: Integer; Value: integer);
var
  delta: integer;
begin
  if Value<MinColWidth then Value:=MinColWidth;
  delta:=Value-integer(FColWidths[index]);
  if delta<>0 then
  begin
    FColWidths[index] := pointer(Value);
    inc(FSumColWidths,delta);
    Invalidate;
  end;
end;

procedure TNiceGrid.SetEveryColWidth(AWidth: integer);
var
  i: integer;
begin
  if AWidth<0 then AWidth:=0;
  for i:=0 to FColCount-1 do
    FColWidths[i]:=pointer(AWidth);
  FSumColWidths:=FColCount*AWidth;
  Invalidate;
end;

procedure TNiceGrid.SetEveryRowHeight(AHeight: integer);
var
  i: integer;
begin
  if AHeight<0 then AHeight:=0;
  for i:=0 to FRowCount-1 do
    FRowHeights[i]:=pointer(AHeight);//can be a lot rows
  FSumRowHeights:=FRowCount*AHeight;
  Invalidate;
end;

procedure TNiceGrid.SetFirstCol(const Value: Longint);
begin
  if Value<>HScroll.Position then
  begin
    HScroll.Position:=Value;
    if Assigned(FOnScroll) then FOnScroll(self);
    Invalidate;
  end;
end;

procedure TNiceGrid.SetFirstRow(const Value: Longint);
begin
  if Value<>VScroll.Position then
  begin
    VScroll.Position:=Value;
    if Assigned(FOnScroll) then FOnScroll(self);
    Invalidate;
  end;
end;

procedure TNiceGrid.SetFixedCols(Value: integer);
begin
  if Value<>FFixedCols then
  begin
    FFixedCols:=Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetFixedRows(Value: integer);
begin
  if Value<>FFixedRows then
  begin
    FFixedRows:=Value;
    Invalidate;
  end;
end;

procedure TNiceGrid.SetRowCount(Value: Longint);
var
  i: integer;
begin
  if Value<0 then Value:=0;
  if Value=FRowCount then exit;
  if Value>FRowCount then
    for i:=FRowCount to Value-1 do AddRow
  else
    for i:=FRowCount-1 downto Value do DeleteRow(i);
  Assert(FRowCount=Value);
  Assert(FRowHeights.Count=Value);
  Assert(FSumRowHeights>=0);
  if RowList.UpdateCount=0 then
    Invalidate;
end;

procedure TNiceGrid.SetRowHeights(index: Integer; Value: integer);
var
  delta: integer;
begin
  if Value<0 then Value:=0;
  delta:=Value-integer(FRowHeights[index]);
  if delta<>0 then
  begin
    FRowHeights[index] := pointer(Value);
    inc(FSumRowHeights,delta);
    Invalidate;
  end;
end;

function TNiceGrid.SumFixedX: integer;
var
  i:integer;
begin
  result:=0;
  for i:=0 to FFixedCols-1 do
    inc(result,ColWidths[i]);
end;

function TNiceGrid.SumFixedY: integer;
var
  i:integer;
begin
  result:=0;
  for i:=0 to FFixedRows-1 do
    inc(result,RowHeights[i]);
end;

{$ifdef VisualCLX}
function TiGrid.WidgetFlags: integer;
begin
  result := integer(WidgetFlags_WResizeNoErase)
         or integer(WidgetFlags_WRepaintNoErase);
end;
{$endif}

{$ifndef VisualCLX}
procedure TNiceGrid.WMEraseBkgnd(var Msg: TLMEraseBkgnd);
begin
  Msg.result := 1;
end;

procedure TNiceGrid.WMKillFocus(var Msg : TLMKillFocus);
begin
  Invalidate;
end;

procedure TNiceGrid.WMSetFocus(var Msg: TLMSetFocus);
begin
  Invalidate;
end;
{$endif}


{ TnCollItem }

procedure TnCollItem.Changed(AllItems: boolean; SubItem:TObject; Data:integer);
var
  Item: TnCollItem;
begin
  if (FCollection <> nil) and (FCollection.FUpdateCount = 0) then
  begin
    if AllItems then Item := nil else Item := self;
    FCollection.Update(Item,SubItem,Data);
  end;
end;

constructor TnCollItem.Create(Collect: TnColl);
begin
  FCollection := Collect;
  FIndex := Collect.Count;
end;

constructor TnCollItem.CreateWithIndex(Collect: TnColl; Index: integer);
begin
  Create(Collect);
  FIndex := Index;
end;

function TnCollItem.Next: TnCollItem;
begin
  if Index<Collect.Count-1 then
    result:=TnCollItem(Collect.Items[Index+1])
  else result:=nil;
end;

function TnCollItem.Prev: TnCollItem;
begin
  if Index>0 then// this imply also Collect.Count>0
    result:=TnCollItem(Collect.Items[Index-1])
  else result:=nil
end;

{ TnColl }

function TnColl.Add: TnCollItem;
begin
  result := FItemClass.Create(self);
  FItems.Add(result);
  Changed;
end;

procedure TnColl.BeginUpdate;
begin
  if FUpdateCount=$FFFFFFFF then
    raise Exception.Create('Too many BeginUpdate');
  inc(FUpdateCount);
end;

procedure TnColl.Changed;
begin
  if FUpdateCount = 0 then Update(nil,nil,0);
end;

procedure TnColl.Clear;
var
  i:integer;
begin
  for i:=0 to FItems.Count-1 do
    TnCollItem(FItems[i]).Free;
  FItems.Clear;
  Changed;
end;

constructor TnColl.Create(ItemClass: TnCollItemClass);
begin
  FItemClass := ItemClass;
  FItems := TObjectList.Create(false);
end;

procedure TnColl.CustomSort(Compare:TItemCompare; Column:integer=0; Reverse:boolean = false);
var
  Left, Right, SubArray, SubLeft, SubRight: LongInt;
  Stack: array[1..32] of record First, Last: LongInt; end;
  Pivot, Temp: TObject;
begin
  if Count > 1 then
  begin
    SubArray := 1;
    Stack[SubArray].First := 0;
    Stack[SubArray].Last := Count - 1;
    repeat
      Left := Stack[SubArray].First;
      Right := Stack[SubArray].Last;
      Dec(SubArray);
      repeat
        SubLeft := Left;
        SubRight := Right;
        Pivot := FItems[(Left + Right) shr 1];
        Assert(Pivot<>nil);
        repeat
          while Compare(FItems[SubLeft], Pivot, Column) <0 do Inc(SubLeft);
          while Compare(FItems[SubRight], Pivot, Column) >0 do Dec(SubRight);
          IF SubLeft <= SubRight then
          begin
            Temp := FItems[SubLeft];
            FItems[SubLeft] := FItems[SubRight];
            FItems[SubRight] := Temp;
            Inc(SubLeft);
            Dec(SubRight);
          end;
        until SubLeft > SubRight;
        IF SubLeft < Right then
        begin
          Inc(SubArray);
          Stack[SubArray].First := SubLeft;
          Stack[SubArray].Last := Right;
        end;
        Right := SubRight;
      until Left >= Right;
    until SubArray = 0;
  end;
  if Reverse then ReverseList;
  RestoreIndex;
end;

procedure TnColl.Delete(index: integer);
begin
  Assert(TObject(FItems[index]) is TnCollItem);
  TnCollItem(FItems[index]).Free;
  FItems.Delete(index);
  if index<=Count-1 then RestoreIndex(index);
  Changed;
end;

destructor TnColl.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

procedure TnColl.EndUpdate;
begin
  if FUpdateCount=0 then
    raise Exception.Create('called EndUpdate without BeginUpdate');
  dec(FUpdateCount);
  Changed;
end;

function TnColl.GetCount: integer;
begin
  result:=FItems.Count;
end;

function TnColl.GetItem(Index: integer): TnCollItem;
begin
  result:=TnCollItem(FItems[Index]);
  Assert(result.Index=Index);
end;

function TnColl.Insert(index: integer): TnCollItem;
begin
  result := FItemClass.CreateWithIndex(self,index);
  FItems.Insert(index,result);
  if index<Count-1 then RestoreIndex(index+1);
  Changed;
end;

procedure TnColl.RestoreIndex(start:integer = 0);
var
  i:integer;
begin
// FItems[i] or FItems.List[i] both are very slow (FItems.List[i] - 200 times slower writing than reading),
// reading item is fast but modification slow - how it optimize?
  for i:=start to FItems.Count-1 do
  if FItems[i]<>nil then
    TnCollItem(FItems[i]).FIndex:=i;
end;

procedure TnColl.ReverseList;
var
  i,n:integer;
  aux:TObject;
begin
  n:=FItems.Count-1;
  for i:=0 to FItems.Count div 2 - 1 do
  begin//what about exchange?
    aux:=FItems[i];
    FItems[i]:=FItems[n];
    FItems[n]:=aux;
    dec(n)
  end;
end;

procedure TnColl.SetCount(const Value: integer);
begin
  FItems.Count:=Value;
end;

procedure TnColl.Update(Item: TnCollItem; SubItem:TObject; Data:integer);
begin
end;

{ TnGridRow }

function TnGridRow.AddCell: TnCell;
begin
  result:=InsertCell(FCellList.Count);
end;

constructor TnGridRow.Create(Collection: TnColl);
begin
  inherited Create(Collection);
  FCellList:=TObjectList.Create(false);
end;

procedure TnGridRow.DeleteCell(AIndex: integer);
begin
  Assert(TObject(FCellList[AIndex]) is TnCell);
  TnCell(FCellList[AIndex]).Free;
  FCellList.Delete(AIndex);
  if AIndex<=Count-1 then RestoreIndex(AIndex);
end;

destructor TnGridRow.Destroy;
begin
  FCellList.Free;
  inherited;
end;

function TnGridRow.Get(AIndex: integer): TnCell;
begin
  result:=FCellList[AIndex] as TnCell;
end;

function TnGridRow.GetCount: integer;
begin
  result:=FCellList.Count;
end;

function TnGridRow.InsertCell(AIndex: integer): TnCell;
begin
  result:=TnCell.Create(self);
  FCellList.Insert(AIndex,result);
  RestoreIndex(AIndex);
end;

procedure TnGridRow.Put(AIndex: Integer; const Value: TnCell);
begin
  FCellList[AIndex]:=Value;
end;

procedure TnGridRow.RestoreIndex(start: integer);
var
  i:integer;
begin
  for i:=start to FCellList.Count-1 do
  if FCellList[i]<>nil then
    TnCell(FCellList[i]).FIndex:=i;
end;

{ TnRowList }

function TnRowList.Add: TnGridRow;
begin
  result := TnGridRow(inherited Add);
end;

constructor TnRowList.Create(GridControl: TnBaseGrid);
begin
  inherited Create(TnGridRow);
  FOwnerGrid:=GridControl;
end;

function TnRowList.Get(index: integer): TnGridRow;
begin
  result:=FItems[index] as TnGridRow;
end;

function TnRowList.Insert(index: integer): TnGridRow;
begin
  result := TnGridRow(inherited Insert(index));
end;

procedure TnRowList.Update(Item: TnCollItem; SubItem:TObject; Data:integer);
begin
  OwnerGrid.UpdateForItems(self, TnGridRow(Item), TnCell(SubItem), Data);
end;

procedure TnRowList.Put(index: integer; const Value: TnGridRow);
begin
  FItems[index]:=Value;
end;

end.
