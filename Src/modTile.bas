
'#Include "Src/PoL.bi"
#Include "Src/modTile.bi"

#Include "Inc/Perlin.bi"


Sub VoxCubeEdges(A As Vec3I, B As Vec3I)
    VoxLine A, Vec3I(A.X, A.Y, B.Z)
    VoxLine A, Vec3I(A.X, B.Y, A.Z)
    VoxLine A, Vec3I(B.X, A.Y, A.Z)
    
    VoxLine B, Vec3I(B.X, B.Y, A.Z)
    VoxLine B, Vec3I(B.X, A.Y, B.Z)
    VoxLine B, Vec3I(A.X, B.Y, B.Z)
    
    VoxLine Vec3I(A.X, A.Y, B.Z), Vec3I(B.X, A.Y, B.Z)
    VoxLine Vec3I(A.X, A.Y, B.Z), Vec3I(A.X, B.Y, B.Z)
    
    VoxLine Vec3I(A.X, B.Y, A.Z), Vec3I(B.X, B.Y, A.Z)
    VoxLine Vec3I(A.X, B.Y, A.Z), Vec3I(A.X, B.Y, B.Z)
    
    VoxLine Vec3I(B.X, A.Y, A.Z), Vec3I(B.X, B.Y, A.Z)
    VoxLine Vec3I(B.X, A.Y, A.Z), Vec3I(B.X, A.Y, B.Z)
End Sub

Sub VoxCubeSolid(A As Vec3I, B As Vec3I)
    For X As Integer = A.X To B.X Step Sgn(B.X - A.X)
        VoxTriangle Vec3I(X, A.Y, B.Z), Vec3I(X, A.Y, A.Z), Vec3I(X, B.Y, B.Z)
        VoxTriangleFanTo Vec3I(X, B.Y, A.Z)
    Next X
End Sub

Sub PerlinNoise3Block(Threshold As Single, Red As UByte, Green As UByte, Blue As UByte)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                Var N = Perlin.Noise3(Type(X *2.5, Y *2.5, Z *2.5))
                VoxSetColor RGB((N + 1) * Red, (N + 1) * Green, (N + 1) * Blue)
                If N < Threshold Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
End Sub

Sub MakeTerrainTiles(VolTile() As Vox_Volume)
    
    Dim QBColor(15) As ULong = { RGB(0,0,0), RGB(0,0,168), RGB(0,168,0), RGB(0,168,168), RGB(168,0,0), RGB(168,0,168), _
                                 RGB(168,88,0), RGB(168,168,168), RGB(88,88,88), RGB(88,88,255), RGB(88,255,88), _
                                 RGB(88,255,255), RGB(255,88,88), RGB(255,88,255), RGB(255,255,88), RGB(255,255,255) }
    
    For X As Integer = 0 To 15
        For Y As Integer = 0 To 3
            VolTile(X, Y) = VoxNewVolume(Vec3I(16, 16, 16))
            
            VoxSetColor QBColor(X)
            For I As Integer = 0 To 2 * Y
                VoxCubeEdges Vec3I(I, I, I), Vec3I(15 - I, 15 - I, 15 - I)
            Next I
        Next Y
    Next X
    
    Perlin.Init
    
    VoxSetVolume VolTile(0, 0) 'Air
    VoxCls
    
    VoxSetVolume VolTile(1, 0) 'Dirt
    VoxCls
    PerlinNoise3Block 0.15, 128, 96, 0
    VoxSetColor RGB(128,128,128)
    For I As Integer = 0 To 150
        VSet Vec3I(Rnd * 16, Rnd * 16, Rnd * 16)
    Next I
    
    VoxSetVolume VolTile(2, 0) 'Grass
    VoxSetSource VolTile(1, 0)
    VoxBlit Vec3I(0, 0, 0), Vec3I(0, 0, 0), Vec3I(16, 16, 16)
    'VoxCls
    'PerlinNoise3Block 0.15, 64, 164, 64
    For X As Single = 0 To 15
        For Y As Single = 12 To 15
            For Z As Single = 0 To 15
                Var N = Perlin.Noise3(Type(X *2.5, Y *2.5, Z *2.5))
                VoxSetColor RGB((N + 1) * 64, (N + 1) * 164, (N + 1) * 64)
                If N < 0.15 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(3, 0) 'Stone
    VoxCls
    PerlinNoise3Block 0.15, 128, 128, 128
    
    VoxSetVolume VolTile(4, 0) 'Sand
    VoxCls
    PerlinNoise3Block 0.15, 128, 128, 0
    
    VoxSetVolume VolTile(5, 0) 'Snow
    'PerlinNoise3Block 0.55, 164, 164, 164
    VoxCls
    VoxSetColor RGB(196, 196, 196)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                Var N = Perlin.Noise3(Type(X *2.5, Y *2.5, Z *2.5))
                If N < 0.15 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(6, 0) 'Stone Brick Wall
    VoxCls
    VoxSetColor RGB(80, 80, 80)
    VoxCubeSolid Vec3I(1, 1, 1), Vec3I(14, 14, 14)
    VoxSetColor RGB(128, 128, 128)
    For X As Integer = 0 To 3
        For Y As Integer = 0 To 3
            For Z As Integer = 0 To 3
                VoxCubeSolid Vec3I(X*6 - ((Y+Z) Mod 2) * 3, Y*4, Z*4), Vec3I(X*6 - ((Y+Z) Mod 2)*3 + 4, Y*4 + 2, Z*4 + 2)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(7, 0) 'Red Brick Wall
    VoxCls
    VoxSetColor RGB(96, 96, 96)
    VoxCubeSolid Vec3I(1, 1, 1), Vec3I(14, 14, 14)
    VoxSetColor RGB(136, 0, 0)
    For X As Integer = 0 To 3
        For Y As Integer = 0 To 3
            For Z As Integer = 0 To 3
                VoxCubeSolid Vec3I(X*6 - ((Y+Z) Mod 2) * 3, Y*4, Z*4), Vec3I(X*6 - ((Y+Z) Mod 2)*3 + 4, Y*4 + 2, Z*4 + 2)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(8, 0) 'Wooden Board Block
    VoxCls
    VoxSetColor RGB(160, 128, 32)
    VoxCubeSolid Vec3I(1, 1, 1), Vec3I(14, 14, 14)
    VoxSetColor RGB(128, 96, 0)
    For X As Integer = 0 To 2
        For Y As Integer = 0 To 3
            For Z As Integer = 0 To 3
                VoxCubeSolid Vec3I(X*9 - ((Y+Z) Mod 2) * 3, Y*4, Z*4), Vec3I(X*9 - ((Y+Z) Mod 2)*3 + 7, Y*4 + 2, Z*4 + 2)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(0, 1) 'Black
    VoxCls
    VoxSetColor RGB(0, 0, 0)
    VoxCubeSolid Vec3I(1, 1, 1), Vec3I(14, 14, 14)
    VoxSetColor RGB(48, 48, 48)
    VoxCubeEdges Vec3I(0, 0, 0), Vec3I(15, 15, 15)
    VoxSetColor RGB(32, 32, 32)
    VoxCubeEdges Vec3I(1, 1, 1), Vec3I(14, 14, 14)
    
    VoxSetVolume VolTile(1, 1) 'Bush
    VoxCls
    PerlinNoise3Block -0.27, 128, 96, 0
    PerlinNoise3Block -0.30, 0, 128, 0
    VoxSetColor 0
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                If (X - 7) ^ 2 + (Y - 7) ^ 2 + (Z - 7) ^ 2 > 10 ^ 2 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(2, 1) 'Berry Bush
    'VoxCls
    'PerlinNoise3Block -0.28, 128, 96, 0
    'PerlinNoise3Block -0.30, 0, 128, 0
    VoxSetSource VolTile(1, 1)
    VoxBlit Vec3I(0, 0, 0), Vec3I(0, 0, 0), Vec3I(16, 16, 16)
    VoxSetColor RGB(128, 0, 196)
    For I As Integer = 1 To 20
        Var X = Rnd * 16, Y = Rnd*16, Z = Rnd*16
        VoxCubeEdges Vec3I(X, Y, Z), Vec3I(X+1, Y+1, Z+1)
    Next I
    
    VoxSetVolume VolTile(3, 1) 'Tall Grass
    VoxCls
    VoxSetColor RGB(32, 132, 32)
    For I As Integer = 1 To 32
        Var X = Rnd * 16
        Var Z = Rnd * 16
        VoxLine Vec3I(X, 0, Z), Vec3I(X + 2 * Rnd - 1, 5, Z + 2 * Rnd - 1)
    Next I
    
    VoxSetVolume VolTile(4, 1) 'Taller Grass
    VoxCls
    VoxSetColor RGB(32, 132, 32)
    For I As Integer = 1 To 32
        Var X = Rnd * 16
        Var Z = Rnd * 16
        VoxLine Vec3I(X, 0, Z), Vec3I(X + 4 * Rnd - 2, 10 - 2 * Rnd, Z + 4 * Rnd - 2)
    Next I
    
    VoxSetVolume VolTile(5, 1) 'Thick Grass
    VoxCls
    For I As Integer = 1 To 96
        VoxSetColor RGB(24 + Rnd * 16, 116 + Rnd * 32, 24 + Rnd * 16)
        Var X = Rnd * 16
        Var Z = Rnd * 16
        VoxLine Vec3I(X, 0, Z), Vec3I(X + 4 * Rnd - 2, 10 - 2 * Rnd, Z + 4 * Rnd - 2)
    Next I
    
    VoxSetVolume VolTile(6, 1) 'Ladder
    VoxCls
    VoxSetColor RGB(128, 96, 0)
    'VoxLine Vec3I(4, 0, 8), Vec3I(4, 15, 8)
    'VoxLine Vec3I(4, 0, 6), Vec3I(4, 15, 6)
    VoxLine Vec3I(4, 0, 7), Vec3I(4, 15, 7)
    VoxLine Vec3I(3, 0, 7), Vec3I(3, 15, 7)
    VoxLine Vec3I(12, 0, 7), Vec3I(12, 15, 7)
    VoxLine Vec3I(13, 0, 7), Vec3I(13, 15, 7)
    For I As Integer = 1 To 4
        VoxLine Vec3I(2, I*4-2, 7), Vec3I(14, I*4-2, 7)
        'VoxLine Vec3I(2, I*5-3, 6), Vec3I(14, I*5-3, 6)
        'VoxLine Vec3I(2, I*5-3, 8), Vec3I(14, I*5-3, 8)
    Next I
    'VoxCubeEdges Vec3I(0, 0, 0), Vec3I(15, 15, 15)
    
    VoxSetVolume VolTile(7, 1) 'Ladder 2
    VoxCls
    VoxSetColor RGB(128, 96, 0)
    VoxLine Vec3I(7, 0, 4), Vec3I(7, 15, 4)
    VoxLine Vec3I(7, 0, 3), Vec3I(7, 15, 3)
    VoxLine Vec3I(7, 0, 12), Vec3I(7, 15, 12)
    VoxLine Vec3I(7, 0, 13), Vec3I(7, 15, 13)
    For I As Integer = 1 To 4
        VoxLine Vec3I(7, I*4-2, 2), Vec3I(7, I*4-2, 14)
    Next I
    
    VoxSetVolume VolTile(8, 1) 'Root
    VoxCls
    VoxSetColor RGB(128, 96, 0)
    'VoxCubeEdges Vec3I(4, 0, 4), Vec3I(12, 15, 12)
    'VoxCubeEdges Vec3I(5, 0, 5), Vec3I(11, 15, 11)
    'VoxCubeEdges Vec3I(6, 0, 6), Vec3I(10, 15, 10)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                If (X - 7) ^ 2 + (Z - 7) ^ 2 < (8 - Y/4) ^ 2 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(9, 1) 'Trunk
    VoxCls
    VoxSetSource VolTile(1, 1)
    VoxBlit Vec3I(0, 0, 0), Vec3I(0, 0, 0), Vec3I(16, 16, 16)
    VoxSetColor RGB(128, 96, 0)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                If (X - 7) ^ 2 + (Z - 7) ^ 2 < 4 ^ 2 Then VSet Vec3I(X, Y, Z)
                If (X - 7) ^ 2 + (Y - 7) ^ 2 <= 2 ^ 2 Then VSet Vec3I(X, Y, Z)
                If (Y - 7) ^ 2 + (Z - 7) ^ 2 <= 2 ^ 2 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(10, 1) 'Tree Top
    VoxCls
    VoxSetSource VolTile(1, 1)
    VoxBlit Vec3I(0, 0, 0), Vec3I(0, 0, 0), Vec3I(16, 16, 16)
    VoxSetColor RGB(128, 96, 0)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                If (X - 7) ^ 2 + (Z - 7) ^ 2 < (4 - Y/5) ^ 2 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(8, 2) 'Branch X Axis
    VoxCls
    VoxSetSource VolTile(1, 1)
    VoxBlit Vec3I(0, 0, 0), Vec3I(0, 0, 0), Vec3I(16, 16, 16)
    VoxSetColor RGB(128, 96, 0)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                
                If (X - 7) ^ 2 + (Y - 7) ^ 2 <= 2 ^ 2 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    VoxSetVolume VolTile(9, 2) 'Branch Z Axis
    VoxCls
    VoxSetSource VolTile(1, 1)
    VoxBlit Vec3I(0, 0, 0), Vec3I(0, 0, 0), Vec3I(16, 16, 16)
    VoxSetColor RGB(128, 96, 0)
    For X As Single = 0 To 15
        For Y As Single = 0 To 15
            For Z As Single = 0 To 15
                
                If (Y - 7) ^ 2 + (Z - 7) ^ 2 <= 2 ^ 2 Then VSet Vec3I(X, Y, Z)
            Next Z
        Next Y
    Next X
    
    'VoxSetVolume VolTile(5, 1) 'Glass (meep)
    'VoxCls
    'VoxSetColor RGB(128, 196, 196)
    'VoxCubeEdges Vec3I(0, 0, 0), Vec3I(15, 15, 15)
    'VoxLine Vec3I(0, 6, 6), Vec3I(0, 10, 10)
    'VoxSetColor 0
    'For I As Integer = 1 To 2048
    '    VSet Vec3I(Rnd * 16, Rnd * 16, Rnd * 16)
    'Next I
    
End Sub

Sub MakeSprites(VolSprite() As Vox_Volume)
    ReDim VolSprite(0)
    
    VolSprite(0) = VoxNewVolume(Vec3I(16, 32, 16))
    
    'Head
    VoxSetColor RGB(88,255,88) 'RGB(200, 0, 0)
    VoxCubeEdges Vec3I(3, 31 - 4, 4), Vec3I(12, 31 - 11, 11)
    VoxCubeEdges Vec3I(4, 31 - 3, 4), Vec3I(11, 31 - 12, 11)
    VoxCubeEdges Vec3I(4, 31 - 4, 3), Vec3I(11, 31 - 11, 12)
    
    VoxTriangle Vec3I(4, 31 - 4, 4), Vec3I(11, 31 - 4, 4), Vec3I(4, 31 - 11, 4): VoxTriangleFanTo Vec3I(11, 31 - 11, 4)
    VoxTriangle Vec3I(4, 31 - 4, 12), Vec3I(11, 31 - 4, 12), Vec3I(4, 31 - 11, 12): VoxTriangleFanTo Vec3I(11, 31 - 11, 12)
    
    'Torso
    VoxSetColor RGB(88,88,255) 'RGB(0, 200, 0)
    VoxCubeEdges Vec3I(2, 31 - 12, 5), Vec3I(13, 31 - 24, 10)
    
    VoxTriangle Vec3I(3, 31 - 13, 6), Vec3I(12, 31 - 13, 6), Vec3I(3, 31 - 23, 6): VoxTriangleFanTo Vec3I(12, 31 - 23, 6)
    
    VoxSetColor RGB(0,0,168)
    VoxTriangle Vec3I(3, 31 - 13, 11), Vec3I(12, 31 - 13, 11), Vec3I(3, 31 - 23, 11): VoxTriangleFanTo Vec3I(12, 31 - 23, 11)
    VoxSetColor RGB(180,180,180)
    VoxLine Vec3I(3, 14, 12), Vec3I(5, 14, 12)
    
    VoxSetColor RGB(220,220,220)
    VoxLine Vec3I(11, 9, 5), Vec3I(11, 15, 5) 'F
    VoxLine Vec3I(11, 15, 5), Vec3I(8, 15, 5)
    VoxLine Vec3I(11, 12, 5), Vec3I(9, 12, 5)
    
    VoxLine Vec3I(6, 10, 5), Vec3I(6, 15, 5) 'B
    VoxLine Vec3I(6, 15, 5), Vec3I(4, 15, 5)
    VoxLine Vec3I(3, 14, 5), Vec3I(3, 13, 5)
    VoxLine Vec3I(6, 12, 5), Vec3I(4, 12, 5)
    VoxLine Vec3I(3, 11, 5), Vec3I(3, 10, 5)
    VoxLine Vec3I(6, 9, 5), Vec3I(4, 9, 5)
    
    'Face
    VoxSetColor RGB(255,88,88) 'RGB(88,255,255) 'RGB(0, 0, 200)
    VSet Vec3I(9, 31 - 5, 2)
    VSet Vec3I(6, 31 - 5, 2)
    
    VoxLine Vec3I(9, 31 - 8, 2), Vec3I(6, 31 - 8, 2)
    VoxLine Vec3I(8, 31 - 9, 2), Vec3I(7, 31 - 9, 2)
End Sub

Sub MapGen(WorldMap() As UByte, Size As Vec3I)
    ReDim WorldMap(Size.X, Size.Y, Size.Z)
    
    For X As Integer = 0 To UBound(WorldMap, 1)
        For Z As Integer = 0 To UBound(WorldMap, 3)
            WorldMap(X, 0, Z) = BtDirt 'Grass
            If Perlin.Noise2(Type(X * 2.5, Z * 2.5)) < 0 Then
                If Rnd < 0.5 Then WorldMap(X, 0, Z) = BtGrass
                WorldMap(X, 1, Z) = BtTallGrass
            ElseIf Perlin.Noise2(Type(X * 2.5, Z * 2.5)) < 0.3 Then
                WorldMap(X, 0, Z) = BtGrass
                WorldMap(X, 1, Z) = BtShortGrass
            End If
        Next Z
    Next X
    For X As Integer = 2 To UBound(WorldMap, 1) - 2
        For Z As Integer = 2 To UBound(WorldMap, 3) - 2
            If Perlin.Noise2(Type(X * 2.5 + 100, Z * 2.5 - 100)) > 0.5 Then
                For X2 As Integer = X-2 To X+2
                    For Z2 As Integer = Z-2 To Z+2
                        If Rnd < 0.8 Then
                            WorldMap(X2, 0, Z2) = BtStone
                            If Rnd < 0.5 Then
                                WorldMap(X2, 1, Z2) = BtStone
                                If Rnd < 0.5 Then WorldMap(X2, 2, Z2) = BtStone
                               Else
                                WorldMap(X2, 1, Z2) = BtAir
                            End If
                        End If
                    Next Z2
                Next X2
                'WorldMap(X, 0, Z) = BtStone
                'WorldMap(X, 0, Z+1) = BtStone
                'WorldMap(X-1, 0, Z+1) = BtStone
                'WorldMap(X, 1, Z+1) = BtAir
                'WorldMap(X-1, 1, Z+1) = BtAir
                'WorldMap(X, 1, Z) = BtStone
            End If
        Next Z
    Next X
    For X As Integer = 2 To UBound(WorldMap, 1) - 2
        For Z As Integer = 2 To UBound(WorldMap, 3) - 2
            If Perlin.Noise2(Type(X * 2.5, Z * 2.5)) > 0.5 Then
                'For X2 As Integer = -1 To 1
                '    For Y As Integer = 2 To 3
                '        For Z2 As Integer = -1 To 1
                '            WorldMap(X2 + X, Y, Z2 + Z) = BtBerryBush
                '        Next Z2
                '    Next Y
                'Next X2
                WorldMap(X, 0, Z) = BtDirt
                WorldMap(X+1, 0, Z) = BtDirt
                WorldMap(X-1, 0, Z) = BtDirt
                WorldMap(X, 0, Z+1) = BtDirt
                WorldMap(X, 0, Z-1) = BtDirt
                
                WorldMap(X, 1, Z) = BtTreeBase
                WorldMap(X, 2, Z) = BtTreeTrunk
                WorldMap(X, 3, Z) = BtTreeTrunk
                WorldMap(X, 4, Z) = BtTreeTop
                
                WorldMap(X+1, 1, Z) = BtAir
                WorldMap(X-1, 1, Z) = BtAir
                WorldMap(X, 1, Z+1) = BtAir
                WorldMap(X, 1, Z-1) = BtAir
               
                WorldMap(X+1, 2, Z) = BtTreeBranchX
                WorldMap(X-1, 2, Z) = BtTreeBranchX
                WorldMap(X+1, 3, Z) = BtBerryBush 'BtTreeBranchX
                WorldMap(X-1, 3, Z) = BtBerryBush 'BtTreeBranchX
                
                WorldMap(X, 2, Z+1) = BtTreeBranchZ
                WorldMap(X, 2, Z-1) = BtTreeBranchZ
                WorldMap(X, 3, Z+1) = BtBerryBush 'BtTreeBranchZ
                WorldMap(X, 3, Z-1) = BtBerryBush 'BtTreeBranchZ
            End If
        Next Z
    Next X
End Sub