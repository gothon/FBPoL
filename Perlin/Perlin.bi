
/' coherent noise function over 1, 2 or 3 dimensions '/
/' (copyright Ken Perlin) '/

'> Modified to not polute the global namespace with
'  short defines and global variable / function names

#Inclib "Perlin"
#Pragma Once

#IfNDef Vec2S
    Type Vec2S
        Union
            Type
                As Single X, Y
            End Type
            V(1) As Single
        End Union
    End Type
#EndIf

#IfNDef Vec3S
    Type Vec3S
        Union
            Type
                As Single X, Y, Z
            End Type
            V(2) As Single
        End Union
    End Type
#EndIf

Namespace Perlin

Declare Sub Init(B As Integer = &h100)
Declare Function Noise1(arg As Double) As Double
Declare Function Noise2(vec As Vec2S) As Single
Declare Function Noise3(vec As Vec3S) As Single

End Namespace 'Perlin