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
    procedure CastleControlBase2Open(Sender: TObject);
  private

  public

  end;

  { TCastleApp }

  TCastleApp = class(TUIState)
    procedure Resize; override; // TCastleUserInterface
  private
    AnImage: TCastleImageControl;
  public
    procedure Start; override; // TUIState
    procedure LoadView;
  end;

  { TCardPreview }

  TCardPreview = class(TCastleUserInterface)
    procedure Resize; override;
  private
    PreviewImage: TCastleImageControl;
  public
    procedure Start;
    procedure LoadView;
    procedure LoadPreview(filename: String);
  end;

var
  Form1: TForm1;
  CastleApp: TCastleApp;
  CardPreview: TCardPreview;

const
  DefaultImage: String = 'castle-data:/badImage.png';
  DefaultPreviewImage: String = 'castle-data:/previewImage.png';

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.CastleControlBase2Open(Sender: TObject);
begin
  CardPreview := TCardPreview.Create(Application);
  CardPreview.FullSize := True;
  CardPreview.Start;
  CastleControlBase2.Controls.InsertFront(CardPreview);
  CastleControlBase2.Container.UIScaling := usDpiScale;
end;

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
  AnImage.URL := DefaultImage;
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

procedure TCardPreview.LoadView;
begin
  PreviewImage := TCastleImageControl.Create(Application);
  PreviewImage.Width := Width;
  PreviewImage.Height := Height;
  PreviewImage.Stretch := True;
  PreviewImage.ProportionalScaling := psFit;
  InsertFront(PreviewImage);
end;

procedure TCardPreview.LoadPreview(filename: String);
begin
  try
    PreviewImage.URL := filename;
  except
    on E : Exception do
      begin
        ShowMessage('TCardPreview.LoadFrame' + LineEnding + E.ClassName + LineEnding + E.Message);
       end;
  end;
end;

procedure TCardPreview.Start;
begin
  inherited;
  PreviewImage := nil;
  LoadView;
  LoadPreview(DefaultPreviewImage);
end;

procedure TCardPreview.Resize;
begin
  inherited;
  PreviewImage.Width := Container.Width;
  PreviewImage.Height := Container.Height;
  PreviewImage.Left := (PreviewImage.Width - PreviewImage.EffectiveWidth) / 2;
  PreviewImage.Bottom := (PreviewImage.Height - PreviewImage.EffectiveHeight) / 2;
end;



end.

