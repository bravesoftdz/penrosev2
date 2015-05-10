unit penroseunit;

{$inline on}
{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ucomplex, BGRABitmap, BGRABitmapTypes, BGRACanvas2D,
  Math, LCLIntf, LCLtype;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBoxKites: TCheckBox;
    ColorDialog1: TColorDialog;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PaintTriangles;
    procedure subdivide;
    procedure Createwheel;
    procedure TrackBar2Change(Sender: TObject);
    procedure TrackBar3Change(Sender: TObject);
    function translatex(x: single): smallint;
    function translatey(y: single): smallint;
  private
    { private declarations }
  public
    { public declarations }
  end;

  Triangle = packed record
    color: byte;
    A, B, C: complex;
  end;

const
  GoldenRatio = (1 + sqrt(5)) / 2;

var
  Form1: TForm1;
  TrianglesArray: array [0..65535 * 2] of Triangle;
  numerodetriangulos: integer;
  subdivisions: shortint;
  xres: smallint = 1000;
  yres: smallint = 1000;
  bitmap: TBGRABitmap;
  ctx: TBGRACanvas2D;
  color1: TColor = clYellow;
  color2: Tcolor = clRed;


implementation

{$R *.lfm}

{ TForm1 }

function TForm1.translatex(x: single): smallint; inline;

begin
  Result := trunc((x + 1) * (xres - 1) * 0.5);
end;

function TForm1.translatey(y: single): smallint; inline;

begin
  Result := trunc((yres - 1) - (y + 1) * (yres - 1) * 0.5);
end;

procedure TForm1.PaintTriangles;
var
  k: integer;
  delay: smallint;
  tiempo: longint;
  grosor: single;
  coordinatesarray: array [0..65535 * 2, 0..2] of TPointF;
begin
  tiempo := GetTickCount64;
  delay := numerodetriangulos div (trunc(10 * (1 / TrackBar1.Position)));
  if subdivisions = 0 then
    subdivisions := 1;
  grosor := 0.25 + (1 / (subdivisions / 15));
  ctx.lineJoin := 'round';
  ctx.lineCap := 'round';
  for k := 0 to numerodetriangulos - 1 do
  begin
    coordinatesarray[k, 0] := PointF(translatex(TrianglesArray[k].A.re), translatey(TrianglesArray[k].A.im));
    coordinatesarray[k, 1] := PointF(translatex(TrianglesArray[k].B.re), translatey(TrianglesArray[k].B.im));
    coordinatesarray[k, 2] := PointF(translatex(TrianglesArray[k].C.re), translatey(TrianglesArray[k].C.im));
  end;

  for k := 0 to numerodetriangulos - 1 do
  begin
    if ((k mod delay) = 0) then
    begin
      bitmap.Draw(Image1.Canvas, 0, 0);
      Image1.Invalidate;
      Application.ProcessMessages;
    end;
    if TrianglesArray[k].color = 0 then
    begin
      ctx.fillStyle(color1);
      ctx.strokeStyle(color1);
    end
    else
    begin
      ctx.fillStyle(color2);
      ctx.strokeStyle(color2);
    end;
    ctx.antialiasing := False;
    ctx.beginPath;
    ctx.lineWidth := 1;
    ctx.MoveTo(coordinatesarray[k, 0]);
    ctx.LineTo(coordinatesarray[k, 1]);
    ctx.LineTo(coordinatesarray[k, 2]);
    ctx.closePath;
    ctx.filloverstroke;

    if CheckBox1.Checked then
    begin
      ctx.antialiasing := True;
      ctx.beginPath;
      ctx.closePath;
      ctx.lineWidth := grosor;
      ctx.strokeStyle('#000000');
      ctx.MoveTo(coordinatesarray[k, 2]);
      ctx.LineTo(coordinatesarray[k, 0]);
      ctx.LineTo(coordinatesarray[k, 1]);
      ctx.stroke;

    end;

  end;

  bitmap.Draw(Image1.Canvas, 0, 0);
  Image1.Invalidate;
  ShowMessage(IntToStr(GetTickCount64 - tiempo));
  Application.ProcessMessages;
end;



function FromPolarCoordinates(magnitude, phase: single): complex; inline;
begin
  Result.re := (magnitude * cos(phase));
  Result.im := (magnitude * sin(phase));
end;

procedure TForm1.CreateWheel;
var
  j: integer;
  T: Complex;
begin
  //Create wheel of red triangles around the origin
  for j := 0 to numerodetriangulos - 1 do
  begin
    TrianglesArray[j].color := 0;
    TrianglesArray[j].A := 0;
    TrianglesArray[j].B := FromPolarCoordinates(1, (((2 * j - 1) * PI) / numerodetriangulos));
    TrianglesArray[j].C := FromPolarCoordinates(1, (((2 * j + 1) * PI) / numerodetriangulos));
    if j mod 2 = 0 then
    begin
      with TrianglesArray[j] do
      begin
        T := B;
        B := C;
        C := T;
      end;
    end;
    if CheckBoxKites.Checked then
    begin
      TrianglesArray[j].A := TrianglesArray[j].B;
      TrianglesArray[j].B := 0;
    end;
  end;
end;

procedure TForm1.subdivide;
var
  i, counter: integer;
  P, Q, R, A1, B1, C1: complex;
  TrianglesFinal: array [0..65535 * 2] of Triangle;
begin
  begin
    counter := 0;
    i := 0;
    if CheckBoxKites.Checked then
      repeat
        begin
          A1 := TrianglesArray[i].A;
          B1 := TrianglesArray[i].B;
          C1 := TrianglesArray[i].C;
          if TrianglesArray[i].color <> 0 then
          begin
            // Subdivide red triangle
            P := C1 + ((A1 - C1) / GoldenRatio);
            TrianglesFinal[counter].color := 1;
            TrianglesFinal[counter].A := B1;
            TrianglesFinal[counter].B := P;
            TrianglesFinal[counter].C := A1;
            Inc(counter);
            TrianglesFinal[counter].color := 0;
            TrianglesFinal[counter].A := P;
            TrianglesFinal[counter].B := C1;
            TrianglesFinal[counter].C := B1;
            Inc(counter);
          end
          else
            //Subdivide blue triangle
          begin
            Q := A1 + ((B1 - A1) / GoldenRatio);
            R := B1 + ((C1 - B1) / GoldenRatio);
            TrianglesFinal[counter].color := 1;
            TrianglesFinal[counter].A := R;
            TrianglesFinal[counter].B := Q;
            TrianglesFinal[counter].C := B1;
            Inc(counter);
            TrianglesFinal[counter].color := 0;
            TrianglesFinal[counter].A := Q;
            TrianglesFinal[counter].B := A1;
            TrianglesFinal[counter].C := R;
            Inc(counter);
            TrianglesFinal[counter].color := 0;
            TrianglesFinal[counter].A := C1;
            TrianglesFinal[counter].B := A1;
            TrianglesFinal[counter].C := R;
            Inc(counter);
          end;
          Inc(i);
        end;
      until i = numerodetriangulos
    else
      repeat
        begin
          A1 := TrianglesArray[i].A;
          B1 := TrianglesArray[i].B;
          C1 := TrianglesArray[i].C;
          if TrianglesArray[i].color = 0 then
          begin
            // Subdivide red triangle
            P := (A1 + ((B1 - A1) / GoldenRatio));
            TrianglesFinal[counter].color := 0;
            TrianglesFinal[counter].A := C1;
            TrianglesFinal[counter].B := P;
            TrianglesFinal[counter].C := B1;
            Inc(counter);
            TrianglesFinal[counter].color := 1;
            TrianglesFinal[counter].A := P;
            TrianglesFinal[counter].B := C1;
            TrianglesFinal[counter].C := A1;
            Inc(counter);
          end
          else
            //Subdivide blue triangle
          begin
            Q := B1 + ((A1 - B1)) / GoldenRatio;
            R := B1 + ((C1 - B1)) / GoldenRatio;
            TrianglesFinal[counter].color := 1;
            TrianglesFinal[counter].A := R;
            TrianglesFinal[counter].B := C1;
            TrianglesFinal[counter].C := A1;
            Inc(counter);
            TrianglesFinal[counter].color := 1;
            TrianglesFinal[counter].A := Q;
            TrianglesFinal[counter].B := R;
            TrianglesFinal[counter].C := B1;
            Inc(counter);
            TrianglesFinal[counter].color := 0;
            TrianglesFinal[counter].A := R;
            TrianglesFinal[counter].B := Q;
            TrianglesFinal[counter].C := A1;
            Inc(counter);
          end;
          Inc(i);
        end;
      until i = numerodetriangulos;
    numerodetriangulos := counter;
  end;
  for i := 0 to numerodetriangulos - 1 do
  begin
    TrianglesArray[i] := TrianglesFinal[i];
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  l: integer;
  //tiempo:int64;
  //tempbitmap:TBGRABitmap ;
  scale: single;
begin
  {IFDEF WINDOWS}
  Self.DoubleBuffered := True;
  {ENDIF}
  Image1.Picture.Bitmap.SetSize(xres, yres);
  Image1.Canvas.Clear;
  //tiempo:=GetTickCount64;
  numerodetriangulos := TrackBar2.Position;
  subdivisions := TrackBar3.Position;
  Bitmap := TBGRABitmap.Create(xres, yres, BGRAWhite);
  //tempbitmap:=TBGRABitmap.Create(xres, yres, BGRAWhite);;
  ctx := Bitmap.Canvas2D;
  scale := (1.2 * sqrt(((xres / 2) ** 2) + ((yres / 2) ** 2)));
  //ctx.translate(xres/2,yres/2);
  //ctx.scale(1.2,1.2);
  CreateWheel;
  l := 0;
  if subdivisions <> 0 then
    repeat
      begin
        subdivide;
        Inc(l);
      end;
    until l = subdivisions;
  Label5.Caption := 'Triangles:' + IntToStr(numerodetriangulos);
  PaintTriangles;
  {if CheckBox1.Checked then
    PaintLines;  }
  {l:=1;
  repeat
  begin
  Application.ProcessMessages;
  tempbitmap.PutImageAngle(tempbitmap.Width/2,tempbitmap.Height/2,bitmap,l,bitmap.Width/2,bitmap.Height/2);
  tempbitmap.Draw(Image1.Canvas, 0, 0);
  Form1.Invalidate;
  Image1.Refresh;
  Application.ProcessMessages;
  l:=l+1;
  end;
  until l=180;  }
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  PNGImage: TPortableNetworkGraphic;
begin
  if SaveDialog1.Execute then
  begin
    PNGImage := TPortableNetworkGraphic.Create;
    try
      PNGImage.Assign(Bitmap);
      PNGImage.SaveToFile(SaveDialog1.Filename);
    finally
      PNGImage.Free;
    end;

  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if colordialog1.Execute then
  begin
    color1 := (colordialog1.Color);
    button3.color := color1;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if colordialog1.Execute then
  begin
    color2 := (colordialog1.Color);
    button4.color := color2;
  end;

end;

procedure TForm1.Edit3Change(Sender: TObject);
begin
  if Length(Edit3.Text) > 0 then
    xres := StrToInt(Edit3.Text);
end;

procedure TForm1.Edit4Change(Sender: TObject);
begin
  if Length(Edit4.Text) > 0 then
    yres := StrToInt(Edit4.Text);
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  bitmap.Free;
  Application.Terminate;
end;

procedure TForm1.TrackBar2Change(Sender: TObject);
begin
  numerodetriangulos := TrackBar2.Position;
end;

procedure TForm1.TrackBar3Change(Sender: TObject);
begin
  subdivisions := TrackBar3.Position;
end;

end.
