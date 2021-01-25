unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  CastleControl, CastleUIState, CastleControls, CastleUIControls;

type

  { TForm1 }

  TForm1 = class(TForm)
    CastleControlBase1: TCastleControlBase;
    CastleControlBase2: TCastleControlBase;
    Splitter1: TSplitter;
    procedure CastleControlBase1Open(Sender: TObject);
  private

  public

  end;

  TCastleApp = class(TUIState)
    procedure Resize; override; // TCastleUserInterface
  private
    AnImage: TCastleImageControl;
  public
    procedure Start; override; // TUIState
    procedure LoadView;
  end;

var
  Form1: TForm1;
  CastleApp: TCastleApp;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CastleControlBase1Open(Sender: TObject);
begin
  TCastleControlBase.MainControl := CastleControlBase1;
  CastleApp := TCastleApp.Create(Application);
  TUIState.Current := CastleApp;
  CastleControlBase1.Container.UIScaling := usDpiScale;
end;

procedure TCastleApp.LoadView;
begin
  AnImage := TCastleImageControl.Create(Application);
  AnImage.Width := StateContainer.Width;
  AnImage.Height := StateContainer.Height;
  AnImage.Stretch := True;
  AnImage.ProportionalScaling := psFit;
  InsertFront(AnImage);
  AnImage.URL := 'castle-data:/previewImage.png';
end;

procedure TCastleApp.Start;
begin
  inherited;
  LoadView;
end;

procedure TCastleApp.Resize;
begin
  inherited;
  AnImage.Width := StateContainer.Width;
  AnImage.Height := StateContainer.Height;
  AnImage.Left := (AnImage.Width - AnImage.EffectiveWidth) / 2;
  AnImage.Bottom := (AnImage.Height - AnImage.EffectiveHeight) / 2;
end;



end.

