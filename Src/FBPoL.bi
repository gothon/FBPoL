
#IfDef __FB_WIN32__
    #IfDef __FB_64BIT__
        #LibPath "Lib\Win64"
        #LibPath "Bin\Win64"
    #Else
        #LibPath "Lib\Win32"
        #LibPath "Bin\Win32"
    #EndIf
#EndIf
#IfDef __FB_LINUX__
    #IfDef __FB_64BIT__
        #LibPath "Lib/Lin64"
        #LibPath "Bin/Lin64"
    #Else
        #LibPath "Lib/Lin32"
        #LibPath "Bin/Lin32"
    #EndIf
#EndIf

#Include Once "Inc/libtcc.bi"
#Include Once "SDL2/SDL_mixer.bi"
#Include Once "SDL2/SDL.bi"

#Include Once "GL/gl.bi"
#Include Once "GL/glext.bi"

#Include Once "fbgfx.bi"

#Include "Inc/VoxelGFX.bi"
'#Include "Inc/VarArray.bi"

#Include "Src/modBASIC.bi"
#Include "Src/modGameState.bi"
#Include "Src/modGraphics.bi"
#Include "Src/modMenu.bi"
#Include "Src/modTile.bi"

'How fast do player programs run? (ie how many Ops can they execute per frame)
#Define THREAD_TIME_SLICE 64 'Good Values Range from 32 to 1024