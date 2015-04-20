unit penroseunit;

{$inline on}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, ucomplex, BGRABitmap, BGRABitmapTypes, BGRACanvas2D;

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
    Edit1: TEdit;
    Edit2: TEdit;
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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure Edit3Change(Sender: TObject);
    procedure Edit4Change(Sender: TObject);
    procedure PaintLines;
    procedure PaintTriangles2;
    procedure subdivide;
    procedure subdividekites;
    procedure Createwheel;
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
  TrianglesArray: array [0..65535*2] of Triangle;
  numerodetriangulos: integer = 10;
  subdivisions: integer = 8;
  xres: integer = 800;
  yres: integer = 800;
  bitmap: TBGRABitmap;
  ctx: TBGRACanvas2D;
  color1:TColor = clYellow;
  color2:Tcolor = clRed;


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
  x, y: double;
begin
  //writeln('***EN PAINTTRIANGLES2***');
  for k := 0 to numerodetriangulos - 1 do
  begin
    if TrianglesArray[k].color = 0 then
    begin
      ctx.fillStyle (color1);
      //ctx.strokeStyle (color1);
    end
    else
    begin
      ctx.fillStyle (color2);
      //ctx.strokeStyle (color2);
    end;
    ctx.beginPath;
    x := (TrianglesArray[k].A.re);
    y := (TrianglesArray[k].A.im);
    ctx.MoveTo(translatex(x), translatey(y));
    x := (TrianglesArray[k].B.re);
    y := (TrianglesArray[k].B.im);
    ctx.LineTo(translatex(x), translatey(y));
    x := (TrianglesArray[k].C.re);
    y := (TrianglesArray[k].C.im);
    ctx.LineTo(translatex(x), translatey(y));
    ctx.closePath;
    ctx.fill;
    if (k mod (TrackBar1.Position*100)) = 0 then
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
  x, y{, x1, y1, x2, y2}: double;
begin
  //writeln('***EN PAINTLINES***');
  ctx.strokeStyle('#000000');
  ctx.lineJoin := 'round';
  ctx.antialiasing:=True;
  ctx.lineWidth := 18 - (subdivisions * 2 + 1);
  for k := 0 to numerodetriangulos - 1 do
  begin
    ctx.beginPath;
    ctx.closePath;
    x := (TrianglesArray[k].C.re);
    y := (TrianglesArray[k].C.im);
    ctx.MoveTo(translatex(x), translatey(y));
    x := (TrianglesArray[k].A.re);
    y := (TrianglesArray[k].A.im);
    ctx.LineTo(translatex(x), translatey(y));
    x:= (TrianglesArray[k].B.re);
    y := (TrianglesArray[k].B.im);
    ctx.LineTo(translatex(x), translatey(y));
    ctx.stroke;
    if (k mod (TrackBar1.Position*10)) = 0 then
    begin
      Form1.Invalidate;
      Image1.Refresh;
      Application.ProcessMessages;
      bitmap.Draw(Image1.Canvas, 0, 0);
    end;
    Application.ProcessMessages;
  end;
end;

function FromPolarCoordinates(magnitude, phase: double): complex;
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
  //writeln('***EN CREATEWHEEL***');
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
        TrianglesArray[j].A:=TrianglesArray[j].B;
        TrianglesArray[j].B:=0;
    end;
  end;
end;

procedure TForm1.subdivide;
var
  i, counter: integer;
  P, Q, R, A1, B1, C1: complex;
  TrianglesFinal: array [0..65535*2] of Triangle;
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
  begin
    TrianglesArray[i] := TrianglesFinal[i];
  end;
end;

procedure TForm1.subdividekites;
var
  i, counter: integer;
  P, Q, R, A1, B1, C1: complex;
  TrianglesFinal: array [0..65535*2] of Triangle;
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
    until i = numerodetriangulos;
    numerodetriangulos := counter;
  end;
  for i := 0 to numerodetriangulos - 1 do
  begin
    TrianglesArray[i] := TrianglesFinal[i];
  end;
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
  //tiempo:int64;
  //tempbitmap:TBGRABitmap ;
begin
  Self.DoubleBuffered := True;
  Image1.Picture.Bitmap.SetSize(xres,yres);
  //tiempo:=GetTickCount64;
  numerodetriangulos := StrToInt(Edit1.Text);
  Bitmap := TBGRABitmap.Create(xres, yres, BGRAWhite);
  //tempbitmap:=TBGRABitmap.Create(xres, yres, BGRAWhite);;
  ctx := Bitmap.Canvas2D;
  CreateWheel;
  l := 0;
  repeat
    begin
      if CheckBoxKites.Checked then subdividekites else subdivide;
      Inc(l);
    end;
  until l = subdivisions;
  Label5.Caption:='Triangles:'+inttostr(numerodetriangulos);
  //writeln('Antes de PaintTriangles2:',inttostr(GetTickCount64-tiempo));
  PaintTriangles2;
  //writeln('Antes de Paintlines:',inttostr(GetTickCount64-tiempo));
  if CheckBox1.Checked then PaintLines;
  //writeln('Antes de Bitmap.Draw',inttostr(GetTickCount64-tiempo));
  Bitmap.Draw(Image1.Canvas, 0, 0);
  Application.ProcessMessages;
  Form1.Invalidate;
  Image1.Refresh;
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

procedure TForm1.Button3Click(Sender: TObject);
begin
   if colordialog1.Execute then
      color1:=(colordialog1.Color);
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
      if colordialog1.Execute then
      color2:=(colordialog1.Color);
end;

end.
