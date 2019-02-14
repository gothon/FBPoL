
'#Include "Src/PoL.bi"
#Include Once "fbgfx.bi"
#Include "Inc/VoxelGFX.bi"
#Include "Src/modGraphics.bi"

Common Shared glGenFramebuffers As Sub(ByVal n As GLsizei, ByVal framebuffers As GLuint Ptr)
Common Shared glDeleteFramebuffers As Sub(ByVal n As GLsizei, ByVal framebuffers As GLuint Ptr)
Common Shared glBindFramebuffer As Sub(ByVal target As GLenum, ByVal framebuffer As GLuint)
Common Shared glFramebufferTexture2D As Sub(ByVal target As GLenum, ByVal attachment As GLenum, ByVal textarget As GLenum, ByVal texture As GLuint, ByVal level As GLint)
Common Shared glCheckFramebufferStatus As Function(ByVal target As GLenum) As GLenum

Common Shared glGenRenderbuffers As Sub(ByVal n As GLsizei, ByVal renderbuffers As GLuint Ptr)
Common Shared glDeleteRenderbuffers As Sub(ByVal n As GLsizei, ByVal renderbuffers As GLuint Ptr)
Common Shared glBindRenderbuffer As Sub(ByVal target As GLenum, ByVal renderbuffer As GLuint)
Common Shared glRenderbufferStorageMultisample As Sub(ByVal target As GLenum, ByVal samples As GLsizei, ByVal internalformat As GLenum, ByVal Width As GLsizei, ByVal height As GLsizei)
Common Shared glFramebufferRenderbuffer As Sub(ByVal target As GLenum, ByVal attachment As GLenum, ByVal renderbuffertarget As GLenum, ByVal renderbuffer As GLuint)

Common Shared glBlitFramebuffer As Sub(ByVal srcX0 As GLint, ByVal srcY0 As GLint, ByVal srcX1 As GLint, ByVal srcY1 As GLint, ByVal dstX0 As GLint, ByVal dstY0 As GLint, ByVal dstX1 As GLint, ByVal dstY1 As GLint, ByVal mask As GLbitfield, ByVal filter As GLenum)

#Define BindGlShaderExtFunc(FUNC) FUNC = SDL_GL_GetProcAddress(#FUNC): If FUNC = NULL Then RI.GlShaderExt = 0

Sub InitGraphics(RI As RenderInfo)
    RI.GlContext = SDL_GL_CreateContext(RI.Wind)
    
    VoxInit @SDL_GL_GetProcAddress
    
    ScreenRes 1, 1, 32, , FB.GFX_NULL
    RI.GfxFont = MakeFBGfxFontTexture
    
    RI.GlShaderExt = -1
    
    BindGlShaderExtFunc(glGenFramebuffers)
    BindGlShaderExtFunc(glDeleteFramebuffers)
    BindGlShaderExtFunc(glBindFramebuffer)
    BindGlShaderExtFunc(glFramebufferTexture2D)
    BindGlShaderExtFunc(glCheckFramebufferStatus)
    
    BindGlShaderExtFunc(glGenRenderbuffers)
    BindGlShaderExtFunc(glDeleteRenderbuffers)
    BindGlShaderExtFunc(glBindRenderbuffer)
    BindGlShaderExtFunc(glRenderbufferStorageMultisample)
    BindGlShaderExtFunc(glFramebufferRenderbuffer)
    
    BindGlShaderExtFunc(glBlitFramebuffer)
    
    glGetIntegerv GL_MAX_SAMPLES, @RI.MaxMSAA
    'RI.MultiSampleAntiAlias = RI.MaxMSAA
    If RI.GlShaderExt Then
        ' Create a Frame Buffer
        glGenFramebuffers(1, @RI.TileFrameBuffer)
        glBindFramebuffer(GL_FRAMEBUFFER_EXT, RI.TileFrameBuffer)
        ' Attach a Color Buffer
        glGenRenderbuffers(1, @RI.TileColorBuffer)
        glBindRenderbuffer(GL_RENDERBUFFER_EXT, RI.TileColorBuffer)
        glRenderbufferStorageMultisample(GL_RENDERBUFFER, RI.MultiSampleAntiAlias, GL_RGBA8, 2*512, 2*128)
        glFramebufferRenderbuffer(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_RENDERBUFFER_EXT, RI.TileColorBuffer)
        ' Attach a Depth Buffer
        glGenRenderbuffers(1, @RI.TileDepthBuffer)
        glBindRenderbuffer(GL_RENDERBUFFER_EXT, RI.TileDepthBuffer)
        glRenderbufferStorageMultisample(GL_RENDERBUFFER_EXT, RI.MultiSampleAntiAlias, GL_DEPTH_COMPONENT, 2*512, 2*128)
        glFramebufferRenderbuffer(GL_FRAMEBUFFER_EXT, GL_DEPTH_ATTACHMENT_EXT, GL_RENDERBUFFER_EXT, RI.TileDepthBuffer)
        ' Check Framebuffer and Unbind it
        If glCheckFramebufferStatus(GL_FRAMEBUFFER_EXT) <> GL_FRAMEBUFFER_COMPLETE Then RI.GlShaderExt = 0
        glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0)
        
        
        ' Create another Frame Buffer
        glGenFramebuffers(1, @RI.TexFrameBuffer)
        glBindFramebuffer(GL_FRAMEBUFFER_EXT, RI.TexFrameBuffer)
        ' Attach a Texture
        glGenTextures(1, @RI.TexTiles)
        glBindTexture(GL_TEXTURE_2D, RI.TexTiles)
        
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
        
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, 2*512, 2*128, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL)
        glFramebufferTexture2D(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT, GL_TEXTURE_2D, RI.TexTiles, 0)
        ' Check Framebuffer and Unbind it
        If glCheckFramebufferStatus(GL_FRAMEBUFFER_EXT) <> GL_FRAMEBUFFER_COMPLETE Then RI.GlShaderExt = 0
        glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0)
    End If
    If Not RI.GlShaderExt Then
        ' Write Code here to render to the display buffer then copy to texture,
        ' because render to terxture is unsupported :-(
    End If
End Sub

Sub GraphicsCleanup(RI As RenderInfo)
    If RI.GfxFont <> NULL Then glDeleteTextures 1, @RI.GfxFont: RI.GfxFont = NULL
    If RI.TexTiles <> NULL Then glDeleteTextures 1, @RI.TexTiles: RI.TexTiles = NULL
    If RI.TileDepthBuffer <> NULL Then glDeleteRenderbuffers(1, @RI.TileDepthBuffer): RI.TileDepthBuffer = NULL
    If RI.TileColorBuffer <> NULL Then glDeleteRenderbuffers(1, @RI.TileColorBuffer): RI.TileColorBuffer = NULL
    If RI.TileFrameBuffer <> NULL Then glDeleteFramebuffers(1, @RI.TileFrameBuffer): RI.TileFrameBuffer = NULL
    If RI.TexFrameBuffer <> NULL Then glDeleteFramebuffers(1, @RI.TexFrameBuffer): RI.TexFrameBuffer = NULL
    If RI.GlContext <> NULL Then SDL_GL_DeleteContext RI.GlContext: RI.GlContext = NULL
End Sub

Sub ConsolePrint(S As Const String)
    Dim As Integer F = FreeFile
    Open Cons For Output As #F
        Print #F, S
    Close #F
End Sub

Sub LineNumPrint(S As Const String)
    Dim As Integer F = FreeFile, LN = 1, I = 1, J
    Open Cons For Output As #F
        Do
            J = InStr(I, S, !"\n")
            If J = 0 Then J = Len(S)
            Print #F, Str(LN) & ": " & Mid(S, I, J - I)
            I = J + 1
            LN += 1
        Loop Until I > Len(S)
    Close #F
End Sub

#Define SwapRB(C) (CUInt(C) Shr 16 And &HFF Or (CUInt(C) And &HFF) Shl 16 Or CUInt(C) And &HFF00FF00)

Sub FBImageToTexture(Im As Any Ptr, ByRef TexName As GLuint)
    Dim As Integer Pitch, W, H, ByPP, X, Y
    Dim Pixels As Any Ptr
    Dim Row As ULong Ptr
    
    If 0 <> ImageInfo(Im, W, H, ByPP, Pitch, Pixels) Then Exit Sub
    If ByPP <> 4 Then Exit Sub
    
    Dim Tex As ULong Ptr = Allocate(W * H * SizeOf(ULong))
    Dim TexRow As ULong Ptr 
    
    For Y As Integer = 0 To H - 1
        Row = Pixels + (H - 1 - Y) * Pitch
        TexRow = Tex + W * Y
        For X As Integer = 0 To W - 1
            TexRow[X] = SwapRB(Row[X])
        Next X
    Next Y
    
    If TexName = 0 Then
        glGenTextures 1, @TexName
        
        glBindTexture GL_TEXTURE_2D, TexName
        
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP
        
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
        glTexParameteri GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
        
        glTexImage2D GL_TEXTURE_2D, 0, 4, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, Tex
       Else
        glBindTexture GL_TEXTURE_2D, TexName
        glTexImage2D GL_TEXTURE_2D, 0, 4, W, H, 0, GL_RGBA, GL_UNSIGNED_BYTE, Tex
    End If
    
    Deallocate Tex
End Sub

Function MakeFBGfxFontTexture() As GLuint
    Var Im = ImageCreate(8 * 16, 8 * 16, RGBA(0, 0, 0, 0), 32)
    
    For I As Integer = 0 To 255
        Draw String Im, ((I Mod 16) * 8, (I \ 16) * 8), Chr(I)
    Next I
    
    Dim TexName As GLuint
    FBImageToTexture Im, TexName
    ImageDestroy Im
    Return TexName
End Function

Sub DrawTextGL(Text As String, X As Integer = 0, Y As Integer = 0, Size As Integer = 1, Col As ULong = RGB(255, 255, 255))
    Dim As Integer I, Char, SX, SY
    Dim As Single S, T
    
    glMatrixMode GL_MODELVIEW
    glPushMatrix
    glColor4ub Col Shr 16, (Col Shr 8) And 255, Col And 255, 255
    glTranslatef X, Y, 0
    glScalef Size, Size, 1
    
    glBegin GL_QUADS
    For I = 1 To Len(Text)
        Char = Asc(Mid(Text, I, 1))
        Do While Char = 13 Or Char = 10
            I += 1
            If I > Len(Text) Then Exit For
            If Char = 13 And Asc(Mid(Text, I, 1)) = 10 Then
                I += 1
                If I > Len(Text) Then Exit For
            End If
            SY += 8
            SX = X
            Char = Asc(Mid(Text, I, 1))
        Loop
        S = (Char Mod 16) / 16.0
        T = 1.0 - (Char \ 16) / 16.0 - 0.0625
        
        glTexCoord2f S, (T + 0.0625): glVertex2i SX, SY
        glTexCoord2f S, T: glVertex2i SX, SY + 8
        glTexCoord2f (S + 0.0625), T: glVertex2i SX + 8, SY + 8
        glTexCoord2f (S + 0.0625), (T + 0.0625): glVertex2i SX + 8, SY
        SX += 8
    Next I
    glEnd
    glPopMatrix
End Sub

Sub SetupGl2D(W As Integer, H As Integer)
    glViewport 0, 0, W, H
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    glOrtho 0, W, H, 0, -2000, 2000 'Origin at top
    glMatrixMode GL_MODELVIEW
    glLoadIdentity
    
    'glTexEnvf GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE
    glAlphaFunc GL_GREATER, 0.0
    glEnable GL_ALPHA_TEST
End Sub

Sub SetupGlOrtho3D(W As Integer, H As Integer)
    glViewport 0, 0, W, H
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    glOrtho 0, W, 0, H, -2000, 2000 'Origin at bottom
    glMatrixMode GL_MODELVIEW
    glLoadIdentity
    
    glAlphaFunc GL_GREATER, 0.0
    glEnable GL_ALPHA_TEST
End Sub

Sub RenderTileTexture(VolTile() As Vox_Volume, Tilt As Single, Spin As Single)
    glClearColor 0, 0, 0, 0
    glClear GL_DEPTH_BUFFER_BIT Or GL_COLOR_BUFFER_BIT
    SetupGlOrtho3D 2*512, 2*128
    
    glDisable GL_BLEND
    glEnable GL_NORMALIZE
    VoxGlRenderState 0, 0, VOXEL_NOCLEAR Or VOXEL_NOMODELVIEW 'Or VOXEL_NOGLSTATE 'Or VOXEL_NOLIGHT
    
    glPushMatrix
    glTranslatef 32, 32, 32
    For X As Integer = 0 To 15
        glPushMatrix
        For Y As Integer = 0 To 3
            glPushMatrix
                glRotatef Tilt, 1, 0, 0
                glRotatef Spin, 0, 1, 0
                glTranslatef -16, -16, -16
                glScalef 2, 2, 2
                VoxGlRenderState 0, 0, VOXEL_NOCLEAR Or VOXEL_NOMODELVIEW Or VOXEL_NOGLSTATE 'Or VOXEL_NOLIGHT
                'glScalef 2/Sqr(3), 2/Sqr(3), 2/Sqr(3) 'Careful scaling will only save 15% of texture space
                VoxRenderVolume VolTile(X, Y)
            glPopMatrix
            glTranslatef 0, 64, 0
        Next Y
        glPopMatrix
        glTranslatef 64, 0, 0
    Next X
    glDisable GL_NORMALIZE
    glDisable GL_TEXTURE_3D
    glDisable GL_CULL_FACE
    glDisable GL_ALPHA_TEST
    glDisable GL_DEPTH_TEST
    glPopMatrix
End Sub

Sub RenderTileMap(RI As RenderInfo, WS As WorldState, ByVal A As Vec3I, ByVal B As Vec3I)
    glEnable GL_DEPTH_TEST
    If RI.MultiSampleAntiAlias > 0 Then
        glBlendFunc GL_ONE, GL_ONE_MINUS_SRC_ALPHA
        'glBlendFunc GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA
        glEnable GL_BLEND
    End If
    
    glColor4ub 255, 255, 255, 255
    glBindTexture GL_TEXTURE_2D, RI.TexTiles
    glEnable GL_TEXTURE_2D
    glPushMatrix
    glRotatef RI.TiltAng, 1, 0, 0
    glRotatef RI.SpinAng, 0, 1, 0
    
    glTranslatef (A.X - B.X - 1) * 16, (A.Y - B.Y - 1) * 16, (A.Z - B.Z - 1) * 16
    
    Dim As GLdouble MvMat(15)
    glGetDoublev GL_MODELVIEW_MATRIX, @MvMat(0)
    
    Var AZ = MvMat(2) * A.X + MvMat(6) * A.Y + MvMat(10) * A.Z + MvMat(14)
    Var BZ = MvMat(2) * B.X + MvMat(6) * A.Y + MvMat(10) * A.Z + MvMat(14)
    If AZ < BZ XOr RI.MultiSampleAntiAlias > 0 Then Swap A.X, B.X
    
    AZ = MvMat(2) * A.X + MvMat(6) * A.Y + MvMat(10) * A.Z + MvMat(14)
    BZ = MvMat(2) * A.X + MvMat(6) * B.Y + MvMat(10) * A.Z + MvMat(14)
    If AZ < BZ XOr RI.MultiSampleAntiAlias > 0  Then Swap A.Y, B.Y
    
    AZ = MvMat(2) * A.X + MvMat(6) * A.Y + MvMat(10) * A.Z + MvMat(14)
    BZ = MvMat(2) * A.X + MvMat(6) * A.Y + MvMat(10) * B.Z + MvMat(14)
    If AZ < BZ XOr RI.MultiSampleAntiAlias > 0  Then Swap A.Z, B.Z
    
    For X As Integer = A.X To B.X Step Sgn(B.X - A.X)
        For Y As Integer = A.Y To B.Y Step Sgn(B.Y - A.Y)
            For Z As Integer = A.Z To B.Z Step Sgn(B.Z - A.Z)
                'If X <> A.X And X <> B.X And Y <> A.Y And Y <> B.Y And Z <> A.Z And Z <> B.Z Then Continue For
                'Var I = 0
                'If X <= UBound(WS.WorldMap, 1) And Y <= UBound(WS.WorldMap, 2) And Z <= UBound(WS.WorldMap, 3) Then I = WS.WorldMap(X, Y, Z)
                Var I = WS.WorldMap(X, Y, Z)
                Var P = WS.Ent(0).Posn
                If I > 0 And (X <> P.X Or Y <> P.Y Or Z <> P.Z) Then 'And Abs(X - P.X) + Abs(Y - P.Y) + Abs(Z - P.Z) > 3 Then
                    glPushMatrix
                    glTranslatef 32 * X, 32 * Y, 32 * Z
                    glRotatef -RI.SpinAng, 0, 1, 0
                    glRotatef -RI.TiltAng, 1, 0, 0
                    
                    Var XI = I Mod 16
                    Var YI = I \ 16
                    Var U = XI * 32 / 512, V = YI * 32 / 128
                    glBegin GL_QUADS
                        glTexCoord2f U, V + 32 / 128
                        glVertex3i 0, 64, 32
                        
                        glTexCoord2f U + 32 / 512, V + 32 / 128
                        glVertex3i 64, 64, 32
                        
                        glTexCoord2f U + 32 / 512, V
                        glVertex3i 64, 0, 32
                        
                        glTexCoord2f U, V
                        glVertex3i 0, 0, 32
                    glEnd
                    glPopMatrix
                End If
            Next Z
        Next Y
    Next X
    glPopMatrix
    glDisable GL_TEXTURE_2D
    glDisable GL_BLEND
    
    'glPushMatrix 'Render Tiles Near the sprite in full (working on this)
    'glTranslatef 32, 32, 32
    'glRotatef RI.TiltAng, 1, 0, 0
    'glRotatef RI.SpinAng, 0, 1, 0
    ''glTranslatef (A.X - B.X - 1) * 16, (A.Y - B.Y - 1) * 16, (A.Z - B.Z - 1) * 16
    'glTranslatef (-RI.TileMapSizeBound.X - 1) * 16, (-RI.TileMapSizeBound.Y - 1) * 16, (-RI.TileMapSizeBound.Z - 1) * 16
    'glTranslatef -16, -16, -16
    'VoxGlRenderState 0, 0, VOXEL_NOCLEAR Or VOXEL_NOMODELVIEW' Or VOXEl_NOLIGHT
    'glFrontFace GL_CW
    'For X As Integer = A.X To B.X Step Sgn(B.X - A.X)
    '    For Y As Integer = A.Y To B.Y Step Sgn(B.Y - A.Y)
    '        For Z As Integer = A.Z To B.Z Step Sgn(B.Z - A.Z)
    '            Var I = WS.WorldMap(X, UBound(WS.WorldMap, 2) - Y, Z)
    '            Var P = WS.Ent(0).Posn
    '            If I > 0 And Abs(X - P.X) + Abs(Y - P.Y) + Abs(Z - P.Z) <= 3 Then
    '                glPushMatrix
    '                glTranslatef 32 * X, 32 * Y, 32 * Z
    '                glScalef 2, 2, 2
    '                VoxRenderVolume RI.VolTile(I Mod 16, I \ 16)
    '                glPopMatrix
    '            End If
    '        Next Z
    '    Next Y
    'Next X
    'glPopMatrix
End Sub

Sub DrawRect(X1 As Integer, Y1 As Integer, X2 As Integer, Y2 As Integer)
    glBegin GL_QUADS
    glVertex3i X1, Y1, 0
    glVertex3i X1 + 1, Y1, 0
    glVertex3i X1 + 1, Y2 + 1, 0
    glVertex3i X1, Y2 + 1, 0
    
    glVertex3i X1 + 1, Y1, 0
    glVertex3i X1 + 1, Y1 + 1, 0
    glVertex3i X2 + 1, Y1 + 1, 0
    glVertex3i X2 + 1, Y1, 0
    
    glVertex3i X2 + 1, Y1 + 1, 0
    glVertex3i X2 + 1, Y2 + 1, 0
    glVertex3i X2, Y2 + 1, 0
    glVertex3i X2, Y1 + 1, 0
    
    glVertex3i X1 + 1, Y2 + 1, 0
    glVertex3i X2, Y2 + 1, 0
    glVertex3i X2, Y2, 0
    glVertex3i X1 + 1, Y2, 0
    glEnd
End Sub

Sub DrawWireCube()
    glEnable GL_LINE_SMOOTH
    glHint GL_LINE_SMOOTH, GL_NICEST
    glEnable GL_BLEND
    glBegin GL_LINES
    glVertex3f 0, 0, 0: glVertex3f 0, 0, 1
    glVertex3f 1, 0, 0: glVertex3f 1, 0, 1
    glVertex3f 0, 1, 0: glVertex3f 0, 1, 1
    glVertex3f 1, 1, 0: glVertex3f 1, 1, 1
    
    glVertex3f 0, 0, 0: glVertex3f 0, 1, 0
    glVertex3f 1, 0, 0: glVertex3f 1, 1, 0
    glVertex3f 0, 0, 1: glVertex3f 0, 1, 1
    glVertex3f 1, 0, 1: glVertex3f 1, 1, 1
    
    glVertex3f 0, 0, 0: glVertex3f 1, 0, 0
    glVertex3f 0, 1, 0: glVertex3f 1, 1, 0
    glVertex3f 0, 0, 1: glVertex3f 1, 0, 1
    glVertex3f 0, 1, 1: glVertex3f 1, 1, 1
    glEnd
    glDisable GL_BLEND
    glDisable GL_LINE_SMOOTH
End Sub