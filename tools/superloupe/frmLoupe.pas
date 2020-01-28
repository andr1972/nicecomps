unit frmLoupe;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, Spin, Buttons,
  CheckLst, Menus, ExtDlgs, frmClip, Dialogs;

type
  TMouseMode = (mmNormal, mmZoom, mmHand);
  TDrawOrErase = (deDraw, deErase);
  TLoupeForm = class(TForm)
    PaintBox: TPaintBox;
    StatusBar: TStatusBar;
    HScroll: TScrollBar;
    VScroll: TScrollBar;
    Panel: TPanel;
    cbGrid: TCheckBox;
    SpinEdit: TSpinEdit;
    ScaleScroll: TScrollBar;
    btnCalc: TBitBtn;
    btnNotepa: TBitBtn;
    cbTimer: TCheckBox;
    Timer: TTimer;
    MainMenu1: TMainMenu;
    Edit1: TMenuItem;
    Paste1: TMenuItem;
    File1: TMenuItem;
    Open1: TMenuItem;
    BmpClose: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    OpenPictureDialog: TOpenPictureDialog;
    SaveClip: TMenuItem;
    CopyClip: TMenuItem;
    cbMouseFrame: TCheckBox;
    HElp1: TMenuItem;
    Clip1: TMenuItem;
    cbDist: TCheckBox;
    miAbout: TMenuItem;
    pmnuStatus: TPopupMenu;
    miHtmlNET: TMenuItem;
    miDelphi: TMenuItem;
    cbFollowMouse: TCheckBox;
    cbAlwaysOnTop: TCheckBox;
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpinEditChange(Sender: TObject);
    procedure ScaleScrollChange(Sender: TObject);
    procedure cbGridClick(Sender: TObject);
    procedure VScrollChange(Sender: TObject);
    procedure HScrollChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var AAction: TCloseAction);
    procedure btnCalcClick(Sender: TObject);
    procedure btnNotepaClick(Sender: TObject);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TimerTimer(Sender: TObject);
    procedure cbTimerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure BmpCloseClick(Sender: TObject);
    procedure SaveClipClick(Sender: TObject);
    procedure CopyClipClick(Sender: TObject);
    procedure cbMouseFrameClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Clip1Click(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure pmnuStatusPopup(Sender: TObject);
    procedure miHtmlNETClick(Sender: TObject);
    procedure miDelphiClick(Sender: TObject);
    procedure cbAlwaysOnTopClick(Sender: TObject);
  private
    FScrDrawRect: TRect;
    FCrossPos: TPoint; //Drawed Cross
    FSrcPos: TPoint;//outside PaintBox for Screen
    FLastMousePressedPos: TPoint;
    FStartDist,FLastDist: TPoint;
    FStartHand: TPoint;
    FScale: integer;
    FDrawedRect: boolean;
    FDrawedDist: boolean;
    FButtonsPressed: TMouseButtonSet;
    FMouseMode: TMouseMode;
    FDataCaption: string;
    function ColorInPos(Pos:TPoint): TColor;
    procedure DrawPoint(Pos:TPoint; Erase:TDrawOrErase);
    procedure DrawCenterPoint(Erase: TDrawOrErase);
    procedure DrawMouseFrame;
    procedure DrawDist;
    procedure SetScale(Value: integer);
    procedure Zoom(NewScale:integer; X, Y: integer);
  public
    Bitmap: TBitmap;
    function GetSrcMax:TPoint;
    procedure ShowGrid;
    procedure UpdateStatusInfo(Pos: TPoint);
    procedure LoadFromSource(DataPlace:TDataPlace; const FileName:string);
    procedure ZeroSrcPos;
    procedure ReadIniSettings;
    procedure WriteIniSettings;
  end;

var
  LoupeForm: TLoupeForm;

implementation

uses
  Math, Types, ColorDesc, Clipbrd, dlgAbout, IniFiles, LCLIntf, LCLType;

{$R *.lfm}
{$R Cursors.res}
type
  TRGBQUAD = record
    rgbBlue : BYTE;
    rgbGreen : BYTE;
    rgbRed : BYTE;
    rgbReserved : BYTE;
  end;

const
  crZoom     = TCursor(1);
  crZoomIn   = TCursor(2);
  crZoomOut  = TCursor(3);
  crHand     = TCursor(4);
  crHandHold = TCursor(5);

procedure LoadCustomCursors;
begin
  Screen.Cursors[crZoom] := LoadCursor(HInstance, 'ZOOM');
  Screen.Cursors[crZoomIn] := LoadCursor(HInstance, 'ZOOM_IN');
  Screen.Cursors[crZoomOut] := LoadCursor(HInstance, 'ZOOM_OUT');
  Screen.Cursors[crHand] := LoadCursor(HInstance, 'HAND');
  Screen.Cursors[crHandHold] := LoadCursor(HInstance, 'HAND_HOLD');
end;

function ColorToStr(AColor: TColor):string;
begin
  with TRGBQuad(AColor) do
  result:=IntToHex(rgbBlue,2)+'.'+IntToHex(rgbGreen,2)+'.'+IntToHex(rgbRed,2)
end;

function DifferColor(AColor: TColor):TColor;
var
 resRgb:TRGBQuad;
 colRgb:TRGBQuad absolute AColor;
begin
  if colRgb.rgbBlue>$80 then resRgb.rgbBlue:=0 else resRgb.rgbBlue:=$FF;
  if colRgb.rgbGreen>$80 then resRgb.rgbGreen:=0 else resRgb.rgbGreen:=$FF;
  if colRgb.rgbRed>$80 then resRgb.rgbRed:=0 else resRgb.rgbRed:=$FF;
  resRgb.rgbReserved:=0;
  result:=TColor(resRgb);
end;


function TLoupeForm.ColorInPos(Pos:TPoint): TColor;
begin
  result:=PaintBox.Canvas.Pixels[Pos.X*FScale+3 ,Pos.Y*FScale+1];
end;

//not xormode because is not visible in gray surface
procedure TLoupeForm.DrawPoint(Pos:TPoint; Erase:TDrawOrErase);
begin
  if FScale<3 then exit;
  if cbDist.Checked then exit;
  PaintBox.Canvas.Pen.Mode:=pmCopy;
  if Erase=deErase then
    PaintBox.Canvas.Pen.Color:=ColorInPos(Pos)
  else
    PaintBox.Canvas.Pen.Color:=DifferColor(ColorInPos(Pos));
  PaintBox.Canvas.MoveTo(Pos.X*FScale+2,  Pos.Y*FScale+2);
  PaintBox.Canvas.LineTo(Pos.X*FScale+FScale-1,  Pos.Y*FScale+FScale-1);
  PaintBox.Canvas.MoveTo(Pos.X*FScale+2,  Pos.Y*FScale+FScale-1);
  PaintBox.Canvas.LineTo(Pos.X*FScale+FScale-1,  Pos.Y*FScale+2);
end;

procedure TLoupeForm.DrawCenterPoint(Erase: TDrawOrErase);
var
  Pos:TPoint;
begin
  Pos.X:=PaintBox.Width div 2 div FScale;
  Pos.Y:=PaintBox.Height div 2 div FScale;
  if Erase=deDraw then
    UpdateStatusInfo(Pos);
  DrawPoint(Pos, Erase);
end;

procedure TLoupeForm.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Pos,Src: TPoint;
begin
  Src:=GetSrcMax;
  with PaintBox.ClientRect do
  if (X>=Left)and(X<Right)and(Y>=Top)and(Y<Bottom) then
  begin
    DrawCenterPoint(deErase);
    Pos.X := X div FScale;
    Pos.Y := Y div FScale;
    if (FButtonsPressed<>[]) and (FMouseMode=mmNormal) then
    begin
      if int64(Pos)<>int64(FCrossPos) then DrawPoint(FCrossPos, deErase); //erase in old position
      if (Pos.X<Src.X)and(Pos.Y<Src.Y) then
      begin
        DrawPoint(Pos, deDraw);
        FCrossPos:=Pos;
      end;
      Caption:=FDataCaption+Format('   %d:%d',[Abs(Pos.X-FStartDist.X)+1,Abs(Pos.Y-FStartDist.Y)+1]);
      if FDrawedDist then DrawDist;
      FLastDist:=Pos;
      DrawDist;
      FDrawedDist:=true;
    end;
    with ClipForm do if Visible then
      AddPoint(Point(FCrossPos.X+FScrDrawRect.Left,FCrossPos.Y+FScrDrawRect.Top), FButtonsPressed);
    UpdateStatusInfo(Pos)
  end else
  if (Bitmap=nil)and not((FMouseMode=mmHand) and (FButtonsPressed<>[])) then
  begin
    Pos.X:=X; Pos.Y:=Y;
    FSrcPos:=PaintBox.ClientToScreen(Pos);
    FLastMousePressedPos:=FSrcPos;
    PaintBoxPaint(Sender);
    DrawCenterPoint(deDraw);
  end;
  if (FMouseMode=mmHand) and (FButtonsPressed<>[]) then
  begin
    FSrcPos.X:=FSrcPos.X+FStartHand.X-X div FScale;
    FSrcPos.Y:=FSrcPos.Y+FStartHand.Y-Y div FScale;
    FStartHand.X:=X div FScale;
    FStartHand.Y:=Y div FScale;
    Point(X,Y);
    Invalidate;
  end;
end;

procedure TLoupeForm.FormCreate(Sender: TObject);
begin
  DoubleBuffered:=true;
  HScroll.Max:=Screen.Width-1;
  VScroll.Max:=Screen.Height-1;
  LoadCustomCursors;
  FMouseMode:=mmNormal;
  SetScale(16);
  Caption:=Format('Screen %dx%d',[Screen.Width, Screen.Height]);
  PaintBox.ControlStyle:=PaintBox.ControlStyle+[csCaptureMouse];
  ReadIniSettings;
end;

procedure TLoupeForm.FormDestroy(Sender: TObject);
begin
  Bitmap.Free;
  WriteIniSettings;
end;

procedure TLoupeForm.DrawMouseFrame;
var
  savDC,ScreenDC: HDC;
begin
  savDC:=Canvas.Handle;
  ScreenDC:=GetDC(0);
  Canvas.Handle:=ScreenDC;
  Canvas.Brush.Style:=bsClear;
  Canvas.Pen.Color:=clBlack;
  Canvas.Pen.Mode:=pmNotXor;
  Canvas.Rectangle(FScrDrawRect);
  Canvas.Handle:=savDC;
  ReleaseDC(0, ScreenDC);
end;

function TLoupeForm.GetSrcMax:TPoint;
begin
  if Bitmap<>nil then
  begin
    result.X:=Bitmap.Width;
    result.Y:=Bitmap.Height;
  end else
  begin
    result.X:=Screen.Width;
    result.Y:=Screen.Height;
  end;
end;

procedure TLoupeForm.PaintBoxPaint(Sender: TObject);
var
  Src: TPoint;
  SrcClipW,SrcClipH: integer;
  ScrCorner: TPoint;
  ScreenDC: HDC;
 procedure SetScrDrawRect;
 begin
   FScrDrawRect.Left:=ScrCorner.X;
   FScrDrawRect.Top:=ScrCorner.Y;
   FScrDrawRect.Right:=ScrCorner.X+SrcClipW;
   FScrDrawRect.Bottom:=ScrCorner.Y+SrcClipH;
 end;
begin
  SrcClipW:=PaintBox.Width div FScale;
  SrcClipH:=PaintBox.Height div FScale;
  Src:=GetSrcMax;
  ScrCorner.X:=min(max(FSrcPos.X-SrcClipW div 2,0),max(Src.X-SrcClipW,0));
  ScrCorner.Y:=min(max(FSrcPos.Y-SrcClipH div 2,0),max(Src.Y-SrcClipH,0));

//Stretch: SrcClipW+1,SrcClipH+1 - stretch additional pixel as parti visible
  if Bitmap<>nil then
  begin
    StretchBlt(PaintBox.Canvas.Handle, 0,0,(SrcClipW+1)*FScale,(SrcClipH+1)*FScale,
      Bitmap.Canvas.Handle,ScrCorner.X,ScrCorner.Y,SrcClipW+1,SrcClipH+1, SRCCOPY);
    SetScrDrawRect;
  end else
  begin
    ScreenDC:=GetDC(0);
    if FDrawedRect then DrawMouseFrame;
    StretchBlt(PaintBox.Canvas.Handle, 0,0,(SrcClipW+1)*FScale,(SrcClipH+1)*FScale,
             ScreenDC,ScrCorner.X,ScrCorner.Y,SrcClipW+1,SrcClipH+1, SRCCOPY);
    ReleaseDC(0, ScreenDC);
    SetScrDrawRect;//between DrawMouseFrame
    if cbMouseFrame.Checked then
    begin
      DrawMouseFrame;
      FDrawedRect:=true;
    end;
  end;
  HScroll.Max:=Src.X-SrcClipW-1;
  VScroll.Max:=Src.Y-SrcClipH-1;
  HScroll.Position:=FSrcPos.X-SrcClipW div 2;
  VScroll.Position:=FSrcPos.Y-SrcClipH div 2;
  HScroll.LargeChange:=SrcClipW-1;
  VScroll.LargeChange:=SrcClipH-1;
  if cbGrid.Checked then ShowGrid;
end;

//To avoid accidentaly closing by Alt-F4 instead of other program
procedure TLoupeForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
//  CanClose := MessageDlg('Really exit Super Loupe?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
//  if not CanClose then Application.Minimize;
end;

procedure TLoupeForm.SpinEditChange(Sender: TObject);
begin
  SetScale(SpinEdit.Value);
end;

procedure TLoupeForm.ScaleScrollChange(Sender: TObject);
begin
  SetScale(ScaleScroll.Position);
end;

procedure TLoupeForm.UpdateStatusInfo(Pos: TPoint);
var
  imgX,imgY:integer;
  AColor,NearestColor: TColor;
  Names: TStringList;
  str: string;
  i: integer;
  dR,dG,dB:integer;
begin
  imgX := Pos.X+FScrDrawRect.Left;
  imgY := Pos.Y+FScrDrawRect.Top;
  if (Bitmap<>nil)and((imgX>=Bitmap.Width)or(imgY>=Bitmap.Height)) then
  begin
    for i:=0 to 3 do StatusBar.Panels[i].Text:='';
    exit;
  end;
  StatusBar.Panels[0].Text:='X: '+IntToStr(imgX);
  StatusBar.Panels[1].Text:='Y: '+IntToStr(imgY);
  //avoid grid
  AColor:=ColorInPos(Pos);
  if AColor=-1 then
  begin
    StatusBar.Panels[2].Text:='';
    StatusBar.Panels[3].Text:='';
  end else
  begin
    StatusBar.Panels[2].Text:='RGB: '+ColorToStr(AColor);
    Names:=TStringList.Create;
    NearestColor:=DescribeColor(AColor, Names);
    str:='';
    if Names.Count>0 then str:=Names[0];
    if NearestColor<>AColor then
    begin
      DiffRGB(AColor,NearestColor, dR,dG,dB);
      str:=str+Format(' (%d,%d,%d)',[dR,dG,dB]);
    end;
    for i:=1 to Names.Count-1 do str:=str+','+Names[i];
    Names.Free;
    StatusBar.Panels[3].Text:=str;
  end;
end;

procedure TLoupeForm.ShowGrid;
var
  i:integer;
  AColor: TColor;
  Src: TPoint;
  W,H:integer;
begin
  AColor := cl3DDkShadow;
  if ColorToRgb(AColor)=clBlack then AColor:=clDkGray;
  PaintBox.Canvas.Pen.Mode := pmXor;
  PaintBox.Canvas.Pen.Color := AColor;
  Src:=GetSrcMax;
  //if Src smaller than PaintBox, FSrcPos can be omited
  W:=min(PaintBox.Width div FScale,Src.X);
  H:=min(PaintBox.Height div FScale,Src.Y);
  for i:=1 to H do
  begin
    PaintBox.Canvas.MoveTo(0, i*FScale);
    PaintBox.Canvas.LineTo(W*FScale, i*FScale);
  end;
  for i:=1 to W do
  begin
    PaintBox.Canvas.MoveTo(i*FScale, 0);
    PaintBox.Canvas.LineTo(i*FScale, H*FScale);
  end;
end;

procedure TLoupeForm.ZeroSrcPos;
begin
  FSrcPos:=Point(0,0);
end;

procedure TLoupeForm.cbGridClick(Sender: TObject);
begin
  ShowGrid;
end;

procedure TLoupeForm.HScrollChange(Sender: TObject);
begin
  if Bitmap<>nil then
    FSrcPos.X:=HScroll.Position+PaintBox.Width div FScale div 2
  else
    FSrcPos.X:=HScroll.Position+PaintBox.Width div FScale div 2;
  Invalidate;
end;

procedure TLoupeForm.VScrollChange(Sender: TObject);
begin
  if Bitmap<>nil then
    FSrcPos.Y:=VScroll.Position+PaintBox.Height div FScale div 2
  else
    FSrcPos.Y:=VScroll.Position+PaintBox.Height div FScale div 2;
  Invalidate;
end;

procedure TLoupeForm.FormClose(Sender: TObject; var AAction: TCloseAction);
begin
  if FDrawedRect then DrawMouseFrame;
  FDrawedRect:=false;
end;

procedure TLoupeForm.btnCalcClick(Sender: TObject);
begin
  //ShellExecuteA(0,PAnsiChar('open'),'calc','','',SW_SHOW);
end;

procedure TLoupeForm.btnNotepaClick(Sender: TObject);
begin
  //ShellExecuteA(0,PAnsiChar('open'),'notepad','','',SW_SHOW);
end;


procedure TLoupeForm.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Src: TPoint;
begin
  Src:=GetSrcMax;
  FCrossPos.X := X div FScale;
  FCrossPos.Y := Y div FScale;
  if (FMouseMode=mmNormal)and
    (FCrossPos.X<Src.X)and(FCrossPos.Y<Src.Y) then DrawPoint(FCrossPos, deDraw);
  Include(FButtonsPressed,Button);
  with ClipForm do if Visible then
    AddPoint(Point(FCrossPos.X+FScrDrawRect.Left,FCrossPos.Y+FScrDrawRect.Top), FButtonsPressed);
  case FMouseMode of
    mmNormal:
    begin
      FStartDist.X:=X div FScale;
      FStartDist.Y:=Y div FScale;
    end;
    mmZoom:
      if Button=mbLeft then
      begin
        Screen.Cursor:=crZoomIn;
        Zoom(FScale+1, X, Y);
      end else
      begin
        Screen.Cursor:=crZoomOut;
        Zoom(FScale-1, X, Y);
      end;
    mmHand:
    begin
      Screen.Cursor:=crHandHold;
      FStartHand.X:=X div FScale;
      FStartHand.Y:=Y div FScale;
    end;
  end;
end;

procedure TLoupeForm.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Pos: TPoint;
begin
  Pos.X := X div FScale;
  Pos.Y := Y div FScale;
  DrawPoint(Pos, deErase);
  if FDrawedDist then
  begin
    DrawDist;
    FDrawedDist:=false;
  end;
  Exclude(FButtonsPressed,Button);
  Caption:=FDataCaption;
end;        

procedure TLoupeForm.TimerTimer(Sender: TObject);
var
  Pos:TPoint;
begin
  if Bitmap<>nil then exit;
  Pos:=PaintBox.ScreenToClient(Mouse.CursorPos);
  with Pos, PaintBox.ClientRect do
  if (X>=Left)and(X<Right)and(Y>=Top)and(Y<Bottom) then exit;
  if cbFollowMouse.Checked then
    FSrcPos:=Mouse.CursorPos
  else
    FSrcPos:=FLastMousePressedPos;
  PaintBoxPaint(Sender);
  DrawCenterPoint(deDraw);
end;

procedure TLoupeForm.cbTimerClick(Sender: TObject);
begin
  Timer.Enabled:=cbTimer.Checked;
end;

procedure TLoupeForm.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TLoupeForm.Open1Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
    LoadFromSource(dpFile, OpenPictureDialog.FileName);
end;

procedure TLoupeForm.SaveClipClick(Sender: TObject);
begin
  ClipForm.ShowAs(dpFile);
end;

procedure TLoupeForm.CopyClipClick(Sender: TObject);
begin
  ClipForm.ShowAs(dpClipboard);
end;

procedure TLoupeForm.Clip1Click(Sender: TObject);
begin
  ClipForm.ShowAs(dpBitmap);
end;

procedure TLoupeForm.LoadFromSource(DataPlace:TDataPlace; const FileName:string);
var
  Picture: TPicture;
begin
  Picture:=TPicture.Create;
  if DataPlace=dpClipboard then
  try
    Picture.Assign(Clipboard);
  except
    MessageDlg('First press Print Screen or Copy to Clipboard', mtWarning, [mbOK], 0);
    exit;
  end
  else
    Picture.LoadFromFile(FileName);
  if Bitmap=nil then Bitmap:=TBitmap.Create;
  Bitmap.Width:=Picture.Width;
  Bitmap.Height:=Picture.Height;
  Bitmap.Canvas.Draw(0,0,Picture.Graphic);
  Picture.Free;
  ZeroSrcPos;
  ClipForm.Empty;
  BmpClose.Enabled:=true;
  if FDrawedRect then DrawMouseFrame;
  FDrawedRect:=false;
  Invalidate;
  case DataPlace of
    dpFile: FDataCaption:=Format('%s %dx%d',[ExtractFileName(FileName),Bitmap.Width, Bitmap.Height]);
    dpClipboard: FDataCaption:=Format('Clipboard %dx%d',[Bitmap.Width, Bitmap.Height]);
    else FDataCaption:='';
  end;
  caption:=FDataCaption;
end;

procedure TLoupeForm.Paste1Click(Sender: TObject);
begin
  LoadFromSource(dpClipboard, '');
end;

procedure TLoupeForm.BmpCloseClick(Sender: TObject);
begin
  Bitmap.Free;
  Bitmap:=nil;
  BmpClose.Enabled:=false;
  Invalidate;
  Caption:=Format('Screen %dx%d',[Screen.Width, Screen.Height]);
end;


procedure TLoupeForm.cbMouseFrameClick(Sender: TObject);
begin
  if not cbMouseFrame.Checked then
  begin
    if FDrawedRect then DrawMouseFrame;
  end else
  begin
    if not FDrawedRect then DrawMouseFrame;
  end;
  FDrawedRect:=cbMouseFrame.Checked;
end;

procedure TLoupeForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F3:
      begin
        if (FMouseMode<>mmZoom)or (FButtonsPressed=[]) then
          Screen.Cursor:=crZoom;
        FMouseMode:=mmZoom;
      end;
    VK_F4:
      begin
        if (FMouseMode<>mmHand)or not (FButtonsPressed=[]) then
          Screen.Cursor:=crHand;
        FMouseMode:=mmHand;
      end;
    VK_F5:PaintBoxPaint(Sender);
    VK_F7:cbTimer.Checked:=not cbTimer.Checked;
  end;
end;

procedure TLoupeForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Screen.Cursor:=crDefault;
  FMouseMode:=mmNormal;
end;

procedure TLoupeForm.SetScale(Value: integer);
begin
  FScale:=Value;
  ScaleScroll.Position:=FScale;
  SpinEdit.Value:=FScale;
  Invalidate;
end;

procedure TLoupeForm.Zoom(NewScale:integer; X, Y: integer);
var
  FromCenter: TPoint;
begin
  if NewScale<=0 then exit;
  FromCenter.X := PaintBox.Width div 2 - X;
  FromCenter.Y := PaintBox.Height div 2 - Y;
  if Bitmap<>nil then
  begin
  end else
  begin
    FSrcPos.X:=FSrcPos.X-FromCenter.X div FScale+FromCenter.X div NewScale;
    FSrcPos.Y:=FSrcPos.Y-FromCenter.Y div FScale+FromCenter.Y div NewScale;//-Y div FScale div 2;
  end;
  SetScale(NewScale);
end;

procedure TLoupeForm.DrawDist;
begin
  if not cbDist.Checked then exit;
  PaintBox.Canvas.Pen.Mode:=pmXor;
  PaintBox.Canvas.Pen.Color:=clGray;
  PaintBox.Canvas.MoveTo(FStartDist.X*FScale+FScale div 2, FStartDist.Y*FScale+FScale div 2);
  PaintBox.Canvas.LineTo(FLastDist.X*FScale+FScale div 2, FLastDist.Y*FScale+FScale div 2);
end;

procedure TLoupeForm.miAboutClick(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TLoupeForm.pmnuStatusPopup(Sender: TObject);
begin
  miHtmlNET.Checked:=PaletteType=ptHtmlNET;
  miDelphi.Checked:=PaletteType=ptDelphi;
end;

procedure TLoupeForm.miHtmlNETClick(Sender: TObject);
begin
  miHtmlNET.Checked:=True;
  miDelphi.Checked:=False;
  PaletteType:=ptHtmlNET;
end;

procedure TLoupeForm.miDelphiClick(Sender: TObject);
begin
  miDelphi.Checked:=True;
  miHtmlNET.Checked:=False;
  PaletteType:=ptDelphi;
end;

procedure TLoupeForm.ReadIniSettings;
var
  iniFile: TIniFile;
  S: AnsiString;
begin
  iniFile := TIniFile.Create(ExtractFileDir(ParamStr(0))+'/SuperLoupe.ini');
  try
    S:=iniFile.ReadString('Main','RecognizePalette','Html');
    if S='Delphi' then PaletteType:=ptDelphi
    else PaletteType:=ptHtmlNET;
  finally
    iniFile.Free;
  end;
end;

procedure TLoupeForm. WriteIniSettings;
var
  iniFile: TIniFile;
  S: AnsiString;
begin
  iniFile := TIniFile.Create(ExtractFileDir(ParamStr(0))+'/SuperLoupe.ini');
  try
    if PaletteType=ptDelphi then S:='Delphi'
    else S:='HtmlNET';
    iniFile.WriteString('Main','RecognizePalette', S);
  finally
    iniFile.Free;
  end;
end;


procedure TLoupeForm.cbAlwaysOnTopClick(Sender: TObject);
begin
  if cbAlwaysOnTop.Checked then
     FormStyle :=  fsStayOnTop
  else
    FormStyle :=  fsNormal;
end;

end.
