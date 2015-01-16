unit penroseunit;

{$inline on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ucomplex, IntfGraphics, LCLType, FPimage, BGRABitmap, BGRABitmapTypes, BGRACanvas2D;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure PaintLines;
    procedure PaintTriangles2;
    procedure subdivide;
    function translatex(x: double): integer;
    function translatey(y: double): integer;
  private
    { private declarations }
  public
    { public declarations }
  end;

  Triangle = record
    color: byte;
    A, B, C: complex;
  end;

const
  GoldenRatio = (1 + sqrt(5)) / 2;

var
  Form1: TForm1;
  TrianglesArray: array [0..65535 * 2] of Triangle;
  numerodetriangulos: integer = 10;
  subdivisions: integer = 1;
  xres: integer = 1000;
  yres: integer = 1000;
  bitmap: TBGRABitmap;
  ctx: TBGRACanvas2D;

implementation

{$R *.lfm}

{ TForm1 }

function TForm1.translatex(x: double): integer; inline;

begin
  Result := round((x + 1) * (xres - 1) / 2);
end;

function TForm1.translatey(y: double): integer; inline;

begin
  Result := round((yres - 1) - (y + 1) * (yres - 1) / 2);
end;

procedure TForm1.PaintTriangles2;
var
  k: integer;
  x, y, x1, y1, x2, y2: double;
begin
  //writeln('***EN PAINTTRIANGLES2***');
  for k := 0 to numerodetriangulos - 1 do
  begin
    if TrianglesArray[k].color = 0 then
    begin
      ctx.fillStyle('#660000');
      ctx.strokeStyle('#660000');
    end
    else
    begin
      ctx.fillStyle('#03508B');
      ctx.strokeStyle('#03508B');
    end;
    ctx.beginPath;
    x := (TrianglesArray[k].A.re);
    y := (TrianglesArray[k].A.im);
    ctx.MoveTo(translatex(x), translatey(y));
    x1 := (TrianglesArray[k].B.re);
    y1 := (TrianglesArray[k].B.im);
    ctx.LineTo(translatex(x1), translatey(y1));
    x2 := (TrianglesArray[k].C.re);
    y2 := (TrianglesArray[k].C.im);
    ctx.LineTo(translatex(x2), translatey(y2));
    ctx.closePath;
    ctx.fill;
    if (k mod 256) = 0 then
    begin
      Form1.Invalidate;
      Image1.Refresh;
      Application.ProcessMessages;
      bitmap.Draw(Image1.Canvas, 0, 0);
    end;
  end;
  Form1.Invalidate;
  Image1.Refresh;
  Application.ProcessMessages;
  bitmap.Draw(Image1.Canvas, 0, 0);
end;

procedure TForm1.PaintLines;
var
  k: integer;
  x, y, x1, y1, x2, y2: double;
begin
  //writeln('***EN PAINTLINES***');
  ctx.strokeStyle('#0000cd');
  ctx.lineJoin := 'round';
  ctx.lineWidth := 18 - (subdivisions * 2 + 1);
  for k := 0 to numerodetriangulos - 1 do
  begin
    ctx.beginPath;
    ctx.closePath();
    x := (TrianglesArray[k].C.re);
    y := (TrianglesArray[k].C.im);
    ctx.MoveTo(translatex(x), translatey(y));
    x1 := (TrianglesArray[k].A.re);
    y1 := (TrianglesArray[k].A.im);
    ctx.LineTo(translatex(x1), translatey(y1));
    x2 := (TrianglesArray[k].B.re);
    y2 := (TrianglesArray[k].B.im);
    ctx.LineTo(translatex(x2), translatey(y2));
    ctx.stroke;
    if (k mod 256) = 0 then
    begin
      Form1.Invalidate;
      Image1.Refresh;
      Application.ProcessMessages;
      bitmap.Draw(Image1.Canvas, 0, 0);
    end;
  end;
end;

function FromPolarCoordinates(magnitude, phase: double): complex;
begin
  Result.re := (magnitude * cos(phase));
  Result.im := (magnitude * sin(phase));
end;

procedure CreateWheel;
var
  j: integer;
  T: Complex;
begin
  //Create wheel of red triangles around the origin
  //writeln('***EN CREATEWHEEL***');
  for j := 0 to numerodetriangulos - 1 do
  begin
    TrianglesArray[j].color := 0;
    TrianglesArray[j].A := 0;
    TrianglesArray[j].B := FromPolarCoordinates(1, (((2 * j + 1) * PI) / numerodetriangulos));
    TrianglesArray[j].C := FromPolarCoordinates(1, (((2 * j - 1) * PI) / numerodetriangulos));
    if j mod 2 = 0 then
    begin
      with TrianglesArray[j] do
      begin
        T := B;
        B := C;
        C := T;
      end;
    end;
  end;

end;

procedure TForm1.subdivide;
var
  i, counter: integer;
  P, Q, R, A1, B1, C1: complex;
  TrianglesFinal: array [0..65535 * 2] of Triangle;
begin
  //writeln('***EN SUBDIVIDE***');
  begin
    counter := 0;
    i := 0;
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
    TrianglesArray[i] := TrianglesFinal[i];
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  if Length(Edit1.Text) > 0 then
    numerodetriangulos := StrToInt(Edit1.Text);
end;

procedure TForm1.Edit2Change(Sender: TObject);
begin
  if Length(Edit2.Text) > 0 then
    subdivisions := StrToInt(Edit2.Text);
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

procedure TForm1.Button1Click(Sender: TObject);
var
  l: integer;
begin
  numerodetriangulos := StrToInt(Edit1.Text);
  Bitmap := TBGRABitmap.Create(xres, yres, BGRABlack);
  ctx := Bitmap.Canvas2D;
  CreateWheel;
  l := 0;
  repeat
    begin
      Subdivide;
      Inc(l);
    end;
  until l = subdivisions;
  PaintTriangles2;
  PaintLines;
  Bitmap.Draw(Image1.Canvas, 0, 0);
  Application.ProcessMessages;
  Form1.Invalidate;
  Image1.Refresh;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  JpegImage: TPortableNetworkGraphic;
begin
  if SaveDialog1.Execute then
  begin
    JpegImage := TPortableNetworkGraphic.Create;
    try
      JpegImage.Assign(Bitmap);
      JpegImage.SaveToFile(SaveDialog1.Filename);
    finally
      JpegImage.Free;
    end;

  end;
end;

end.
