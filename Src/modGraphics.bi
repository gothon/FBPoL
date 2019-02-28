
#Pragma Once

#Include Once "SDL2/SDL.bi"

'#Include Once "GL/gl.bi"
#Include Once "GL/glext.bi"

'#Include "Src/modBASIC.bi"
#Include "Src/modGameState.bi"

Extern glGenFramebuffers As Sub(ByVal n As GLsizei, ByVal framebuffers As GLuint Ptr)
Extern glDeleteFramebuffers As Sub(ByVal n As GLsizei, ByVal framebuffers As GLuint Ptr)
Extern glBindFramebuffer As Sub(ByVal target As GLenum, ByVal framebuffer As GLuint)
Extern glFramebufferTexture2D As Sub(ByVal target As GLenum, ByVal attachment As GLenum, ByVal textarget As GLenum, ByVal texture As GLuint, ByVal level As GLint)
Extern glCheckFramebufferStatus As Function(ByVal target As GLenum) As GLenum

Extern glGenRenderbuffers As Sub(ByVal n As GLsizei, ByVal renderbuffers As GLuint Ptr)
Extern glDeleteRenderbuffers As Sub(ByVal n As GLsizei, ByVal renderbuffers As GLuint Ptr)
Extern glBindRenderbuffer As Sub(ByVal target As GLenum, ByVal renderbuffer As GLuint)
Extern glRenderbufferStorageMultisample As Sub(ByVal target As GLenum, ByVal samples As GLsizei, ByVal internalformat As GLenum, ByVal Width As GLsizei, ByVal height As GLsizei)
Extern glFramebufferRenderbuffer As Sub(ByVal target As GLenum, ByVal attachment As GLenum, ByVal renderbuffertarget As GLenum, ByVal renderbuffer As GLuint)

Extern glBlitFramebuffer As Sub(ByVal srcX0 As GLint, ByVal srcY0 As GLint, ByVal srcX1 As GLint, ByVal srcY1 As GLint, _'// source rectangle
                       ByVal dstX0 As GLint, ByVal dstY0 As GLint, ByVal dstX1 As GLint, ByVal dstY1 As GLint, _'// destination rect
                       ByVal mask As GLbitfield, _
                       ByVal filter As GLenum)

#Define RI_AllScreenWindow -2
#Define RI_PlainWindow -1
#Define RI_WholeMonitorWindow 0

Type RenderInfo
    As SDL_Window Ptr Wind, Wind2
    As Any Ptr GlContext
    As GLuint GfxFont, TexTiles, TileColorBuffer, TileDepthBuffer, TileFrameBuffer, TexFrameBuffer
    As SDL_Rect Bounds = Type(100, 100, 800, 600)
    As Long MultiSampleAntiAlias = 0, MaxMSAA
    As Integer MonitorNum = -1 '-1 Windowed, -2 Allscreen
    As Integer ShowControllerInput, ShowStatusText, GlShaderExt, SwapInterval = 0
    As Vox_Volume VolTile(15, 3), VolSprite(Any)
    As Vec3I TileMapLowBound, TileMapSizeBound
    As Single TiltAng, SpinAng, TileScale = 1.7
End Type

#Define ULng_2_UBx3(C) (C) Shr 16, ((C) Shr 8) And 255, (C) And 255

Declare Sub ConsolePrint(S As Const String)
Declare Sub LineNumPrint(S As Const String)
Declare Sub FBImageToTexture(Im As Any Ptr, ByRef TexName As GLuint)
Declare Function MakeFBGfxFontTexture() As GLuint
Declare Sub DrawTextGL(Text As String, X As Integer = 0, Y As Integer = 0, Size As Integer = 1, Col As ULong = RGB(255, 255, 255))

Declare Sub InitGraphics(RI As RenderInfo)
Declare Sub GraphicsCleanup(RI As RenderInfo)
Declare Sub SetupGl2D(W As Integer, H As Integer)
Declare Sub SetupGlOrtho3D(W As Integer, H As Integer)
Declare Sub RenderTileTexture(VolTile() As Vox_Volume, Tilt As Single, Spin As Single)
Declare Sub RenderTileMap(RI As RenderInfo, WS As WorldState, ByVal A As Vec3I, ByVal B As Vec3I)

Declare Sub DrawRect(X1 As Integer, Y1 As Integer, X2 As Integer, Y2 As Integer)
Declare Sub DrawWireCube()