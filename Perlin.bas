
/' coherent noise function over 1, 2 or 3 dimensions '/
/' (copyright Ken Perlin) '/

'> Modified to not polute the global namespace with
'  short defines and global variable / function names

'#define B &H100
'#define BM &Hff

'#define N &H1000
'#define NP 12   /' 2^N '/
'#define NM &Hfff

#Include "Perlin.bi"

Namespace Perlin

Dim Shared p() As Integer
Dim Shared g3() As Vec3S
Dim Shared g2() As Vec2S
Dim Shared g1() As Single
Dim Shared start As Integer = 1
Dim Shared BM As Integer

#define PERLIN_s_curve(t) ( t * t * (3.0 - 2.0 * t) )
'#define PERLIN_s_curve(t) ( t * t * t * (10.0 - (15.0 - 6.0 * t) * t) )

#define PERLIN_lerp(t, a, b) ( a + t * (b - a) )

#define PERLIN_setup(vec,b0,b1,r0,r1) _
   t = vec:_
   b0 = CInt(Int(t)) And BM:_
   b1 = (b0+1) And BM:_
   r0 = t - CInt(Int(t)):_
   r1 = r0 - 1.0

Function Noise1(arg As Double) As Double Export
   Dim As Integer bx0, bx1
   Dim As Single rx0, rx1, sx, t, u, v

   If start <> 0 Then init()

   PERLIN_setup(arg, bx0,bx1, rx0,rx1)

   sx = PERLIN_s_curve(rx0)

   u = rx0 * g1( p( bx0 ) )
   v = rx1 * g1( p( bx1 ) )

   Return PERLIN_lerp(sx, u, v)
End Function

Function Noise2(vec As Vec2S) As Single Export
   Dim As Integer bx0, bx1, by0, by1, b00, b10, b01, b11
   Dim As Single rx0, rx1, ry0, ry1, sx, sy, a, b, t, u, v
   Dim As Single Ptr q
   Dim As Integer i, j 'register

   If start <> 0 Then init()

   PERLIN_setup(vec.V(0), bx0,bx1, rx0,rx1)
   PERLIN_setup(vec.V(1), by0,by1, ry0,ry1)

   i = p( bx0 )
   j = p( bx1 )

   b00 = p( i + by0 )
   b10 = p( j + by0 )
   b01 = p( i + by1 )
   b11 = p( j + by1 )

   sx = PERLIN_s_curve(rx0)
   sy = PERLIN_s_curve(ry0)

#define PERLIN_at2(rx,ry) ( rx * q[0] + ry * q[1] )

   q = @g2( b00 ).V(0) : u = PERLIN_at2(rx0,ry0)
   q = @g2( b10 ).V(0) : v = PERLIN_at2(rx1,ry0)
   a = PERLIN_lerp(sx, u, v)

   q = @g2( b01 ).V(0) : u = PERLIN_at2(rx0,ry1)
   q = @g2( b11 ).V(0) : v = PERLIN_at2(rx1,ry1)
   b = PERLIN_lerp(sx, u, v)

   Return PERLIN_lerp(sy, a, b)
End Function

Function Noise3(vec As Vec3S) As Single Export
   Dim As Integer bx0, bx1, by0, by1, bz0, bz1, b00, b10, b01, b11
   Dim As Single rx0, rx1, ry0, ry1, rz0, rz1, sy, sz, a, b, c, d, t, u, v
   Dim As Single Ptr q
   Dim As Integer i, j 'register

   If start <> 0 Then Init()

   PERLIN_setup(vec.V(0), bx0,bx1, rx0,rx1)
   PERLIN_setup(vec.V(1), by0,by1, ry0,ry1)
   PERLIN_setup(vec.V(2), bz0,bz1, rz0,rz1)

   i = p( bx0 )
   j = p( bx1 )

   b00 = p( i + by0 )
   b10 = p( j + by0 )
   b01 = p( i + by1 )
   b11 = p( j + by1 )

   t  = PERLIN_s_curve(rx0)
   sy = PERLIN_s_curve(ry0)
   sz = PERLIN_s_curve(rz0)

#define PERLIN_at3(rx,ry,rz) ( rx * q[0] + ry * q[1] + rz * q[2] )

   q = @g3( b00 + bz0 ).V(0) : u = PERLIN_at3(rx0,ry0,rz0)
   q = @g3( b10 + bz0 ).V(0) : v = PERLIN_at3(rx1,ry0,rz0)
   a = PERLIN_lerp(t, u, v)

   q = @g3( b01 + bz0 ).V(0) : u = PERLIN_at3(rx0,ry1,rz0)
   q = @g3( b11 + bz0 ).V(0) : v = PERLIN_at3(rx1,ry1,rz0)
   b = PERLIN_lerp(t, u, v)

   c = PERLIN_lerp(sy, a, b)

   q = @g3( b00 + bz1 ).V(0) : u = PERLIN_at3(rx0,ry0,rz1)
   q = @g3( b10 + bz1 ).V(0) : v = PERLIN_at3(rx1,ry0,rz1)
   a = PERLIN_lerp(t, u, v)

   q = @g3( b01 + bz1 ).V(0) : u = PERLIN_at3(rx0,ry1,rz1)
   q = @g3( b11 + bz1 ).V(0) : v = PERLIN_at3(rx1,ry1,rz1)
   b = PERLIN_lerp(t, u, v)

   d = PERLIN_lerp(sy, a, b)

   Return PERLIN_lerp(sz, c, d)
End Function

Sub normalize2(v As Vec2S)
   Dim s As Single

   s = Sqr(v.V(0) * v.V(0) + v.V(1) * v.V(1))
   v.V(0) /= s
   v.V(1) /= s
End Sub

Sub normalize3(v As Vec3S)
   Dim s As Single

   s = Sqr(v.V(0) * v.V(0) + v.V(1) * v.V(1) + v.V(2) * v.V(2))
   v.V(0) /= s
   v.V(1) /= s
   v.V(2) /= s
End Sub

Sub Init(B As Integer = &h100) Export
   Dim As Integer i, j
   
   ReDim p(B + B + 1) As Integer
   ReDim g3(B + B + 1) As Vec3S
   ReDim g2(B + B + 1) As Vec2S
   ReDim g1(B + B + 1) As Single
   
   BM = B - 1
   Assert((B And BM) = 0) 'B must be a power of 2, so mod B can be computed by masking with BM
   start = 0
   
   For i = 0 To B-1
      p(i) = i

      g1(i) = Rnd*2-1

      For j = 0 To 1
         g2(i).V(j) = Rnd*2-1
      Next j
      normalize2(g2(i))

      For j = 0 To 2
         g3(i).V(j) = Rnd*2-1
      Next j
      normalize3(g3(i))
   Next i

   i -= 1
   Do While i <> 0
      j = Int(Rnd * B)
      Swap p(i), p(j)
      i -= 1
   Loop

   For i = 0 To B + 1
      p(B + i) = p(i)
      g1(B + i) = g1(i)
      For j = 0 To 1
         g2(B + i).V(j) = g2(i).V(j)
      Next j
      For j = 0 To 2
         g3(B + i).V(j) = g3(i).V(j)
      Next j
   Next i
End Sub

End Namespace 'Perlin