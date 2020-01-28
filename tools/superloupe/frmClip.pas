unit frmClip;

interface

uses
  Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtDlgs, ImgList, ExtCtrls;

type
  TDataPlace = (dpFile, dpClipboard, dpBitmap);
  TFileType = (ftNone,ftBmp,ftJpeg);
  TMouseButtonSet = set of TMouseButton;
  TClipForm = class(TForm)
    edLeft: TEdit;
    edTop: TEdit;
    edRight: TEdit;
    edBottom: TEdit;
    btnClose: TBitBtn;
    btnCopySave: TButton;
    btnWhole: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SavePictureDialog: TSavePictureDialog;
    ImageList: TImageList;
    rgPF: TRadioGroup;
    procedure btnCopySaveClick(Sender: TObject);
    procedure btnWholeClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
    FOutDataPlace: TDataPlace;
  public
    ClipRange: TRect;
    procedure WriteToDest(FileName:string);
    function ValidRange(ShowWarnings: boolean):boolean;
    procedure ShowAs(DataPlace: TDataPlace);
    procedure FillToWhole;
    procedure Empty;
    procedure AddPoint(Point:TPoint; Buttons:TMouseButtonSet); 
  end;

var
  ClipForm: TClipForm;

implementation

uses
  frmLoupe, Types, Clipbrd, LCLIntf, LCLType;

{$R *.lfm}

procedure TClipForm.FillToWhole;
begin
  edLeft.Text:='0';
  edTop.Text:='0';
  if LoupeForm.Bitmap<>nil then
  begin
    edRight.Text:=IntToStr(LoupeForm.Bitmap.Width-1);
    edBottom.Text:=IntToStr(LoupeForm.Bitmap.Height-1);
  end else
  begin
    edRight.Text:=IntToStr(Screen.Width-1);
    edBottom.Text:=IntToStr(Screen.Height-1);
  end;
end;

procedure TClipForm.Empty;
begin
  edLeft.Text:='';
  edTop.Text:='';
  edRight.Text:='';
  edBottom.Text:='';
end;

procedure TClipForm.btnCopySaveClick(Sender: TObject);
begin
  if not ValidRange(true) then exit;
  if FOutDataPlace=dpFile then
  begin
    SavePictureDialog.Filter:='Bitmaps (*.bmp)|*.bmp|JPEG Image File (*.jpg)|*.jpg';
    if SavePictureDialog.Execute then
      WriteToDest(SavePictureDialog.FileName)
  end else WriteToDest('');
end;

procedure TClipForm.WriteToDest(FileName:string);
var
  Picture: TPicture;
  Bmp: TBitmap;
  ScreenDC: HDC;
  Jpeg: TJpegImage;
  ext: string;
  FileType: TFileType;
begin
  Bmp:=TBitmap.Create;
  Bmp.Width:=ClipRange.Right-ClipRange.Left+1;
  Bmp.Height:=ClipRange.Bottom-ClipRange.Top+1;
  if LoupeForm.Bitmap<>nil then
    BitBlt(Bmp.Canvas.Handle,0,0,Bmp.Width,Bmp.Height,
          LoupeForm.Bitmap.Canvas.Handle,ClipRange.Left,ClipRange.Top,SRCCOPY)
  else
  begin
    ScreenDC:=GetDC(0);
    BitBlt(Bmp.Canvas.Handle,0,0,Bmp.Width,Bmp.Height,
          ScreenDC,ClipRange.Left,ClipRange.Top,SRCCOPY);
    ReleaseDC(0, ScreenDC);
  end;
  case rgPF.ItemIndex of
    0: Bmp.PixelFormat:=pf1bit;
    1: Bmp.PixelFormat:=pf4bit;
    2: Bmp.PixelFormat:=pf8bit;
    else Bmp.PixelFormat:=pf24bit;
  end;
  if FOutDataPlace=dpBitmap then
  begin //Clip
    LoupeForm.Bitmap.Free;
    LoupeForm.Bitmap:=Bmp;
    LoupeForm.ZeroSrcPos;
    LoupeForm.PaintBox.Invalidate;
    FillToWhole;
    LoupeForm.BmpClose.Enabled:=true;
    LoupeForm.Caption:=Format('Clip %dx%d',[LoupeForm.Bitmap.Width, LoupeForm.Bitmap.Height]);
  end else
  begin
    Jpeg := TJpegImage.Create;
    Picture:=TPicture.Create;
    Picture.Assign(Bmp);
    if FOutDataPlace=dpClipboard then
      Clipboard.Assign(Picture)
    else
    begin
      ext:=ExtractFileExt(FileName);
      FileType:=ftNone;
      if ext='' then
      case SavePictureDialog.FilterIndex of
        1:
        begin
          FileName:=ChangeFileExt(FileName,'.bmp');
          FileType:=ftBmp;
        end;
        2:
        begin
          FileName:=ChangeFileExt(FileName,'.jpg');
          FileType:=ftJpeg;
        end;
      end
      else if SameFileName(ext,'.bmp') then
        FileType:=ftBmp
      else if SameFileName(ext,'.jpg') or SameFileName(ext,'.jpeg') then
        FileType:=ftJpeg;
      case FileType of
        ftNone:
          ShowMessage('Not supported file extenasion: '+ext);
        ftBmp:
          Picture.SaveToFile(FileName);
        ftJpeg:
        begin
          Jpeg.Assign(Bmp);
          Jpeg.SaveToFile(FileName);
        end;
      end;
    end;
    Picture.Free;
    Jpeg.Free
  end;
  //no Bmp.Free because LoupeForm.Bitmap:=Bmp;
end;

procedure TClipForm.btnWholeClick(Sender: TObject);
begin
  FillToWhole;
end;

function TClipForm.ValidRange(ShowWarnings: boolean):boolean;
var
  Bound:TPoint;
begin
  result:=false;
  with ClipRange do
  begin
    Left:=StrToIntDef(edLeft.Text,-1);
    Top:=StrToIntDef(edTop.Text,-1);
    Right:=StrToIntDef(edRight.Text,-1);
    Bottom:=StrToIntDef(edBottom.Text,-1);
    if (Left<0)or(Top<0)or(Right<0)or(Bottom<0) then
    begin
      FillChar(ClipRange,sizeof(ClipRange),0);
      if ShowWarnings then ShowMessage('Bad clip range');
      exit;
    end;
  end;
  if LoupeForm.Bitmap<>nil then
  begin
    Bound.X:=LoupeForm.Bitmap.Width;
    Bound.Y:=LoupeForm.Bitmap.Height;
  end else
  begin
    Bound.X:=Screen.Width;
    Bound.Y:=Screen.Height;
  end;
  with ClipRange do
  if (Left>Right)or(Top>Bottom) then
  begin
    if ShowWarnings then
      MessageDlg('Must be: Left<=Right and Top<=Bottom', mtWarning, [mbOK], 0);
    exit;
  end;
  with ClipRange do
  if (Left<0)or(Right>=Bound.X)or(Top<0)or(Bottom>=Bound.Y) then
  begin
    if ShowWarnings then
      MessageDlg('Values not in range', mtWarning, [mbOK], 0);
    exit;
  end;
  result:=true;
end;

procedure TClipForm.ShowAs(DataPlace: TDataPlace);
var
  index:integer;
begin
  FOutDataPlace := DataPlace;
  case DataPlace of
    dpFile: begin index:=0; Caption:='Save to file';end;
    dpClipboard: begin index:=1; Caption:='Copy to clipboard';end;
    dpBitmap: begin index:=2; Caption:='Clip';end;
    else index:=0;
  end;
  //ImageList.GetBitmap(index,btnCopySave.Glyph);
  Show;
end;         

procedure TClipForm.btnCloseClick(Sender: TObject);
begin
  Hide;
end;


procedure TClipForm.AddPoint(Point: TPoint; Buttons: TMouseButtonSet);
begin
  if Buttons=[mbLeft] then
  begin
    edLeft.Text:=IntToStr(Point.X);
    edTop.Text:=IntToStr(Point.Y);
  end
  else if Buttons=[mbRight] then
  begin
    edRight.Text:=IntToStr(Point.X);
    edBottom.Text:=IntToStr(Point.Y);
  end;
  if ValidRange(false) then
  with ClipRange do
  begin
    if ((Right-Left+1=16)and(Bottom-Top+1=16)) or
       ((Right-Left+1=32)and(Bottom-Top+1=32)) then
      Caption:=Format('Clip [%dx%d]',[Right-Left+1,Bottom-Top+1]) 
    else
      Caption:=Format('Clip %dx%d',[Right-Left+1,Bottom-Top+1])
  end else
    Caption:='Clip';
end;

end.
