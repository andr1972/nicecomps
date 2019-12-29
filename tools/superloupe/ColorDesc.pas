unit ColorDesc;

interface

uses
  Classes, Graphics;

type
  TPaletteType = (ptDelphi, ptHtmlNET);
  TEntryArray = array[0..999] of TIdentMapEntry;
  PEntryArray = ^TEntryArray;

function DescribeColor(Color:TColor; Names:TStringList): TColor;
procedure DiffRGB(Value1,Value2: TColor; var dR,dG,dB:integer);
var
  PaletteType: TPaletteType;

implementation
uses
  Math;

var //from clBtn to colors
  DelphiColors: array[0..46] of TIdentMapEntry = (
    (Value: clAqua; Name: 'clAqua'),
    (Value: clBlack; Name: 'clBlack'),
    (Value: clBlue; Name: 'clBlue'),
    (Value: clCream; Name: 'clCream'),
    (Value: clFuchsia; Name: 'clFuchsia'),
    (Value: clGray; Name: 'clGray'),
    (Value: clGreen; Name: 'clGreen'),
    (Value: clLime; Name: 'clLime'),
    (Value: clMaroon; Name: 'clMaroon'),
    (Value: clMedGray; Name: 'clMedGray'),
    (Value: clMoneyGreen; Name: 'clMoneyGreen'),
    (Value: clNavy; Name: 'clNavy'),
    (Value: clOlive; Name: 'clOlive'),
    (Value: clPurple; Name: 'clPurple'),
    (Value: clRed; Name: 'clRed'),
    (Value: clSilver; Name: 'clSilver'),
    (Value: clSkyBlue; Name: 'clSkyBlue'),
    (Value: clTeal; Name: 'clTeal'),
    (Value: clWhite; Name: 'clWhite'),
    (Value: clYellow; Name: 'clYellow'),
    (Value: clBtnFace; Name: 'clBtnFace'),
    (Value: clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: clBtnShadow; Name: 'clBtnShadow'),
    (Value: clBtnText; Name: 'clBtnText'),
    (Value: cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: cl3DLight; Name: 'cl3DLight'),
    (Value: clScrollBar; Name: 'clScrollBar'),
    (Value: clWindow; Name: 'clWindow'),
    (Value: clWindowText; Name: 'clWindowText'),
    (Value: clWindowFrame; Name: 'clWindowFrame'),
    (Value: clMenu; Name: 'clMenu'),
    (Value: clMenuText; Name: 'clMenuText'),
    (Value: clHighlight; Name: 'clHighlight'),
    (Value: clHighlightText; Name: 'clHighlightText'),
    (Value: clGrayText; Name: 'clGrayText'),
    (Value: clAppWorkSpace; Name: 'clAppWorkSpace'),
    (Value: clBackground; Name: 'clBackground'),
    (Value: clInfoText; Name: 'clInfoText'),
    (Value: clInfoBk; Name: 'clInfoBk'),
    (Value: clActiveBorder; Name: 'clActiveBorder'),
    (Value: clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: clActiveCaption; Name: 'clActiveCaption'),
    (Value: clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: clCaptionText; Name: 'clCaptionText'),
    (Value: clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: clGradientActiveCaption; Name: 'clGradientActiveCaption'),
    (Value: clGradientInactiveCaption; Name: 'clGradientInactiveCaption')
    );
  HtmlNETColors: array[0..139] of TIdentMapEntry = (
// from http://www.w3.org/TR/css3-color/
// and NET KnownColor
  //main colors
    (Value: $000000 ; Name: 'Black'),
    (Value: $FFFFFF ; Name: 'White'),
    (Value: $FF0000 ; Name: 'Red'),
    (Value: $00FF00 ; Name: 'Lime'),
    (Value: $0000FF ; Name: 'Blue'),
    (Value: $00FFFF ; Name: 'Aqua'),
    (Value: $FF00FF ; Name: 'Fuchsia'),
    (Value: $FFFF00 ; Name: 'Yellow'),
    (Value: $808080 ; Name: 'Gray'),
    (Value: $008000 ; Name: 'Green'),
    (Value: $800000 ; Name: 'Maroon'),
    (Value: $000080 ; Name: 'Navy'),
    (Value: $808000 ; Name: 'Olive'),
    (Value: $800080 ; Name: 'Purple'),
    (Value: $008080 ; Name: 'Teal'),
    (Value: $C0C0C0 ; Name: 'Silver'),            
  //other
    (Value: $F0F8FF ; Name: 'AliceBlue'),
    (Value: $FAEBD7 ; Name: 'AntiqueWhite'),
    (Value: $7FFFD4 ; Name: 'Aquamarine'),
    (Value: $F0FFFF ; Name: 'Azure'),
    (Value: $F5F5DC ; Name: 'Beige'),
    (Value: $FFE4C4 ; Name: 'Bisque'),
    (Value: $FFEBCD ; Name: 'BlanchedAlmond'),
    (Value: $8A2BE2 ; Name: 'BlueViolet'),
    (Value: $A52A2A ; Name: 'Brown'),
    (Value: $DEB887 ; Name: 'BurlyWood'),
    (Value: $5F9EA0 ; Name: 'CadetBlue'),
    (Value: $7FFF00 ; Name: 'Chartreuse'),
    (Value: $D2691E ; Name: 'Chocolate'),
    (Value: $FF7F50 ; Name: 'Coral'),
    (Value: $6495ED ; Name: 'CornflowerBlue'),
    (Value: $FFF8DC ; Name: 'Cornsilk'),
    (Value: $DC143C ; Name: 'Crimson'),
    (Value: $00FFFF ; Name: 'Cyan'),
    (Value: $00008B ; Name: 'DarkBlue'),
    (Value: $008B8B ; Name: 'DarkCyan'),
    (Value: $B8860B ; Name: 'DarkGoldenrod'),
    (Value: $A9A9A9 ; Name: 'DarkGray'),
    (Value: $006400 ; Name: 'DarkGreen'),
    (Value: $BDB76B ; Name: 'DarkKhaki'),
    (Value: $8B008B ; Name: 'DarkMagenta'),
    (Value: $556B2F ; Name: 'DarkOliveGreen'),
    (Value: $FF8C00 ; Name: 'DarkOrange'),
    (Value: $9932CC ; Name: 'DarkOrchid'),
    (Value: $8B0000 ; Name: 'DarkRed'),
    (Value: $E9967A ; Name: 'DarkSalmon'),
    (Value: $8FBC8F ; Name: 'DarkSeaGreen'),
    (Value: $483D8B ; Name: 'DarkSlateBlue'),
    (Value: $2F4F4F ; Name: 'DarkSlateGray'),
    (Value: $00CED1 ; Name: 'DarkTurquoise'),
    (Value: $9400D3 ; Name: 'DarkViolet'),
    (Value: $FF1493 ; Name: 'DeepPink'),
    (Value: $00BFFF ; Name: 'DeepSkyBlue'),
    (Value: $696969 ; Name: 'DimGray'),
    (Value: $1E90FF ; Name: 'DodgerBlue'),
    (Value: $B22222 ; Name: 'Firebrick'),
    (Value: $FFFAF0 ; Name: 'FloralWhite'),
    (Value: $228B22 ; Name: 'ForestGreen'),
    (Value: $DCDCDC ; Name: 'Gainsboro'),
    (Value: $F8F8FF ; Name: 'GhostWhite'),
    (Value: $FFD700 ; Name: 'Gold'),
    (Value: $DAA520 ; Name: 'Goldenrod'),
    (Value: $ADFF2F ; Name: 'GreenYellow'),
    (Value: $F0FFF0 ; Name: 'Honeydew'),
    (Value: $FF69B4 ; Name: 'HotPink'),
    (Value: $CD5C5C ; Name: 'IndianRed'),
    (Value: $4B0082 ; Name: 'Indigo'),
    (Value: $FFFFF0 ; Name: 'Ivory'),
    (Value: $F0E68C ; Name: 'Khaki'),
    (Value: $E6E6FA ; Name: 'Lavender'),
    (Value: $FFF0F5 ; Name: 'LavenderBlush'),
    (Value: $7CFC00 ; Name: 'LawnGreen'),
    (Value: $FFFACD ; Name: 'LemonChiffon'),
    (Value: $ADD8E6 ; Name: 'LightBlue'),
    (Value: $F08080 ; Name: 'LightCoral'),
    (Value: $E0FFFF ; Name: 'LightCyan'),
    (Value: $FAFAD2 ; Name: 'LightGoldenrodYellow'),
    (Value: $90EE90 ; Name: 'LightGreen'),
    (Value: $D3D3D3 ; Name: 'LightGray'),
    (Value: $FFB6C1 ; Name: 'LightPink'),
    (Value: $FFA07A ; Name: 'LightSalmon'),
    (Value: $20B2AA ; Name: 'LightSeaGreen'),
    (Value: $87CEFA ; Name: 'LightSkyBlue'),
    (Value: $778899 ; Name: 'LightSlateGray'),
    (Value: $B0C4DE ; Name: 'LightSteelBlue'),
    (Value: $FFFFE0 ; Name: 'LightYellow'),
    (Value: $32CD32 ; Name: 'LimeGreen'),
    (Value: $FAF0E6 ; Name: 'Linen'),
    (Value: $FF00FF ; Name: 'Magenta'),
    (Value: $66CDAA ; Name: 'MediumAquamarine'),
    (Value: $0000CD ; Name: 'MediumBlue'),
    (Value: $BA55D3 ; Name: 'MediumOrchid'),
    (Value: $9370DB ; Name: 'MediumPurple'),
    (Value: $3CB371 ; Name: 'MediumSeaGreen'),
    (Value: $7B68EE ; Name: 'MediumSlateBlue'),
    (Value: $00FA9A ; Name: 'MediumSpringGreen'),
    (Value: $48D1CC ; Name: 'MediumTurquoise'),
    (Value: $C71585 ; Name: 'MediumVioletRed'),
    (Value: $191970 ; Name: 'MidnightBlue'),
    (Value: $F5FFFA ; Name: 'MintCream'),
    (Value: $FFE4E1 ; Name: 'MistyRose'),
    (Value: $FFE4B5 ; Name: 'Moccasin'),
    (Value: $FFDEAD ; Name: 'NavajoWhite'),
    (Value: $FDF5E6 ; Name: 'OldLace'),
    (Value: $6B8E23 ; Name: 'OliveDrab'),
    (Value: $FFA500 ; Name: 'Orange'),
    (Value: $FF4500 ; Name: 'OrangeRed'),
    (Value: $DA70D6 ; Name: 'Orchid'),
    (Value: $EEE8AA ; Name: 'PaleGoldenrod'),
    (Value: $98FB98 ; Name: 'PaleGreen'),
    (Value: $AFEEEE ; Name: 'PaleTurquoise'),
    (Value: $DB7093 ; Name: 'PaleVioletRed'),
    (Value: $FFEFD5 ; Name: 'PapayaWhip'),
    (Value: $FFDAB9 ; Name: 'PeachPuff'),
    (Value: $CD853F ; Name: 'Peru'),
    (Value: $FFC0CB ; Name: 'Pink'),
    (Value: $DDA0DD ; Name: 'Plum'),
    (Value: $B0E0E6 ; Name: 'PowderBlue'),
    (Value: $BC8F8F ; Name: 'RosyBrown'),
    (Value: $4169E1 ; Name: 'RoyalBlue'),
    (Value: $8B4513 ; Name: 'SaddleBrown'),
    (Value: $FA8072 ; Name: 'Salmon'),
    (Value: $F4A460 ; Name: 'SandyBrown'),
    (Value: $2E8B57 ; Name: 'SeaGreen'),
    (Value: $FFF5EE ; Name: 'SeaShell'),
    (Value: $A0522D ; Name: 'Sienna'),
    (Value: $87CEEB ; Name: 'SkyBlue'),
    (Value: $6A5ACD ; Name: 'SlateBlue'),
    (Value: $708090 ; Name: 'SlateGray'),
    (Value: $FFFAFA ; Name: 'Snow'),
    (Value: $00FF7F ; Name: 'SpringGreen'),
    (Value: $4682B4 ; Name: 'SteelBlue'),
    (Value: $D2B48C ; Name: 'Tan'),
    (Value: $D8BFD8 ; Name: 'Thistle'),
    (Value: $FF6347 ; Name: 'Tomato'),
    (Value: $40E0D0 ; Name: 'Turquoise'),
    (Value: $EE82EE ; Name: 'Violet'),
    (Value: $F5DEB3 ; Name: 'Wheat'),
    (Value: $F5F5F5 ; Name: 'WhiteSmoke'),
    (Value: $9ACD32 ; Name: 'YellowGreen')
  );

function ReverseRGB(Value: integer):integer;
type
  RGBrec = packed record r,g,b,a: byte; end;
begin
  with RGBrec(Value) do
  begin
    RGBrec(result).r:=b;
    RGBrec(result).g:=g;
    RGBrec(result).b:=r;
    RGBrec(result).a:=a;
  end;
end;

function DiffColors(Value1,Value2: TColor): double;
type
  RGBrec = packed record r,g,b,a: byte; end;
var
  Value1Rec:RGBrec absolute Value1;
  Value2Rec:RGBrec absolute Value2;
begin
  Assert(Value1Rec.a=0);
  Assert(Value2Rec.a=0);
  result:=sqrt(sqr(Value1Rec.r-Value2Rec.r)+sqr(Value1Rec.g-Value2Rec.g)+
               sqr(Value1Rec.b-Value2Rec.b));
end;


procedure DiffRGB(Value1,Value2: TColor; var dR,dG,dB:integer);
type
  RGBrec = packed record r,g,b,a: byte; end;
var
  Value1Rec:RGBrec absolute Value1;
  Value2Rec:RGBrec absolute Value2;
begin
  Assert(Value1Rec.a=0);
  Assert(Value2Rec.a=0);
  dR:=Value1Rec.r-Value2Rec.r;
  dG:=Value1Rec.g-Value2Rec.g;
  dB:=Value1Rec.b-Value2Rec.b;
end;


//return nearest color
function DescribeColor(Color:TColor; Names:TStringList): TColor;
var
  i:integer;
  ColorsTab: PEntryArray;
  ColorValue: TColor;
  NearestColor: TColor;
  HTab: integer;
  d,dmin: double;
begin
  if PaletteType=ptDelphi then
  begin
    ColorsTab:=@DelphiColors;
    HTab:=High(DelphiColors)
  end else if PaletteType=ptHtmlNET then
  begin
    ColorsTab:=@HtmlNETColors;
    HTab:=High(HtmlNETColors)
  end;
  Names.Clear;
  for i:=0 to HTab do
  with ColorsTab^[i] do
  if ColorsTab=@HtmlNETColors then
  begin
    if ReverseRGB(Value)=Color then Names.Add(Name);
  end
  else if ColorsTab=@DelphiColors then
  begin
    if ColorToRGB(Value)=Color then Names.Add(Name);
  end;
  if Names.Count=0 then
  begin
    dmin:=Infinity;
    for i:=0 to HTab do
    begin
      with ColorsTab^[i] do
      if ColorsTab=@HtmlNETColors then
      begin
        ColorValue:=ReverseRGB(Value);
      end
      else if ColorsTab=@DelphiColors then
      begin
        ColorValue:=ColorToRGB(Value);
      end;
      d:=DiffColors(ColorValue, Color);
      if d<dmin then
      begin
        dmin:=d;
        NearestColor:=ColorValue;
      end;
    end;
    for i:=0 to HTab do
    with ColorsTab^[i] do
    begin
      if ColorsTab=@HtmlNETColors then
      begin
        ColorValue:=ReverseRGB(Value);
      end
      else if ColorsTab=@DelphiColors then
      begin
        ColorValue:=ColorToRGB(Value);
      end;
      if ColorValue=NearestColor then Names.Add(Name);
    end;
    result:=NearestColor;
  end else result:=Color;
end;

initialization
  PaletteType:=ptHtmlNET;
end.
