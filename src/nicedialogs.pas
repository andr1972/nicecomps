unit NiceDialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLClasses;

type

  { TNiceFileDialog }

  TNiceFileDialog = class(TLCLComponent)
  private
    FDefaultExt: string;
    FFileName: String;
    FFilter: String;
    FInitialDir: string;
    FOnHelpClicked: TNotifyEvent;
    FOnTypeChange: TNotifyEvent;
    FTitle: string;
    function GetFilterIndex: Integer;
    procedure SetDefaultExt(AValue: string);
    procedure SetFileName(AValue: String);
    procedure SetFilter(AValue: String);
    procedure SetFilterIndex(AValue: Integer);
  public
    function Execute: boolean; virtual;
  published
    property Title: string read FTitle;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
    property FileName: String read FFileName write SetFileName;
    property Filter: String read FFilter write SetFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read FInitialDir write FInitialDir;
    property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
  end;

  TNiceOpenDialog = class(TNiceFileDialog)

  end;

  TNiceSaveDialog = class(TNiceOpenDialog)

  end;

  TNiceSelectDirectoryDialog = class(TNiceOpenDialog)

  end;

implementation

{ TNiceFileDialog }

function TNiceFileDialog.GetFilterIndex: Integer;
begin

end;

procedure TNiceFileDialog.SetDefaultExt(AValue: string);
begin
  if FDefaultExt=AValue then Exit;
  FDefaultExt:=AValue;
end;

procedure TNiceFileDialog.SetFileName(AValue: String);
begin
  if FFileName=AValue then Exit;
  FFileName:=AValue;
end;

procedure TNiceFileDialog.SetFilter(AValue: String);
begin
  if FFilter=AValue then Exit;
  FFilter:=AValue;
end;

procedure TNiceFileDialog.SetFilterIndex(AValue: Integer);
begin

end;

function TNiceFileDialog.Execute: boolean;
begin

end;

end.

