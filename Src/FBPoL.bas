
'''''''''''''''''''''''''''''''''''''''''''''''''''
' Programs of Legend
'   by Alex Thomson Copyright (C) 2018-2019
'
' This program is released under the terms of the GNU General Public License (GPL)
' as published by the Free Software Foundation, GPL version 2 or any later version.
'''''''''''''''''''''''''''''''''''''''''''''''''''

#Include "Src/FBPoL.bi"

Type FrameTime
    As LongInt FrameT, RendT, SleepT
End Type

Type UserInterfaceState
    As Integer SFPS, VFPS 'Simulation/Video Frames Per Second
    Menu(Any) As SuperMenuWidget
    GamePad(Any) As SDL_GameController Ptr
    GUIS As GUI_State
End Type

Function GetAllScreenBounds As SDL_Rect
    Dim As SDL_Rect Bounds, All
    For I As Integer = 0 To SDL_GetNumVideoDisplays - 1
        SDL_GetDisplayBounds I, @Bounds
        If Bounds.X < All.X Then All.W += All.X - Bounds.X: All.X = Bounds.X
        If Bounds.Y < All.Y Then All.H += All.Y - Bounds.Y: All.Y = Bounds.Y
        If Bounds.X + Bounds.W > All.X + All.W Then All.W = Bounds.X + Bounds.W - All.X
        If Bounds.Y + Bounds.H > All.Y + All.H Then All.H = Bounds.Y + Bounds.H - All.Y
    Next I
    Return All
End Function

Sub SetWindowMode(RI As RenderInfo)
    Select Case RI.MonitorNum
    Case RI_AllScreenWindow
        RI.Bounds = GetAllScreenBounds
        SDL_SetWindowFullscreen RI.Wind, 0
        SDL_SetWindowPosition RI.Wind, RI.Bounds.X, RI.Bounds.Y
        SDL_SetWindowSize RI.Wind, RI.Bounds.W, RI.Bounds.H
    Case RI_PlainWindow
        SDL_SetWindowFullscreen RI.Wind, 0
        SDL_SetWindowSize RI.Wind, RI.Bounds.W, RI.Bounds.H
        SDL_SetWindowPosition RI.Wind, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED
    Case Else
        If SDL_GetNumVideoDisplays > RI.MonitorNum Then
            SDL_GetDisplayBounds RI.MonitorNum, @RI.Bounds
            If SDL_GetWindowFlags(RI.Wind) And SDL_WINDOW_FULLSCREEN_DESKTOP Then SDL_SetWindowFullscreen RI.Wind, 0
            SDL_SetWindowPosition RI.Wind, RI.Bounds.X, RI.Bounds.Y
            SDL_SetWindowSize RI.Wind, RI.Bounds.W, RI.Bounds.H
            SDL_SetWindowFullscreen RI.Wind, SDL_WINDOW_FULLSCREEN_DESKTOP
        End If
    End Select
End Sub

Function DoBasicEvents(Event As SDL_Event) As Integer
    Select Case Event.Type
    Case SDL_KEYDOWN
        Select Case Event.key.keysym.sym
        Case SDLK_RETURN
            If Event.key.keysym.mod_ And KMOD_ALT Then
            End If
        Case SDLK_ESCAPE
            Return -1
        End Select
    'Case SDL_WINDOWEVENT
    '    Select Case Event.window.event
    '    'Case SDL_WINDOWEVENT_MOVED
    '    'Case SDL_WINDOWEVENT_RESIZED
    '    Case SDL_WINDOWEVENT_CLOSE
    '        'If event.window.windowID = RI.Wind Then
    '        Return -1
    '    End Select
    Case SDL_QUIT_
        Return -1
    End Select
    Return 0
End Function

Function SDL_Keycode_to_InKeyStr(KeyCode As SDL_Keycode, KeyMod As SDL_Keymod) As String
    If KeyCode < 123 Then 'Not using KMOD_ALT or KMOD_CTRL yet
        If KeyMod And KMOD_SHIFT Then 
            Select Case As Const KeyCode
            Case Asc("`"): Return "~"
            Case Asc("1"): Return "!"
            Case Asc("2"): Return "@"
            Case Asc("3"): Return "#"
            Case Asc("4"): Return "$"
            Case Asc("5"): Return "%"
            Case Asc("6"): Return "^"
            Case Asc("7"): Return "&"
            Case Asc("8"): Return "*"
            Case Asc("9"): Return "("
            Case Asc("0"): Return ")"
            Case Asc("-"): Return "_"
            Case Asc("="): Return "+"
            Case Asc("["): Return "{"
            Case Asc("]"): Return "}"
            Case Asc("\"): Return "|"
            Case Asc(";"): Return ":"
            Case Asc("'"): Return """"
            Case Asc(","): Return "<"
            Case Asc("."): Return ">"
            Case Asc("/"): Return "?"
            Case Else
                If KeyMod And KMOD_CAPS Then
                    Return Chr(KeyCode)
                   Else
                    Return UCase(Chr(KeyCode))
                End If
            End Select
           Else
            If KeyMod And KMOD_CAPS Then
                Return UCase(Chr(KeyCode))
               Else
                Return Chr(KeyCode)
            End If
        End If
       Else
        Select Case As Const KeyCode
        Case SDLK_F1: Return Chr(255) & ";"
        Case SDLK_F2: Return Chr(255) & "<"
        Case SDLK_F3: Return Chr(255) & "="
        Case SDLK_F4: Return Chr(255) & ">"
        Case SDLK_F5: Return Chr(255) & "?"
        Case SDLK_F6: Return Chr(255) & "@"
        Case SDLK_F7: Return Chr(255) & "A"
        Case SDLK_F8: Return Chr(255) & "B"
        Case SDLK_F9: Return Chr(255) & "C"
        Case SDLK_F10: Return Chr(255) & "D"
        Case SDLK_F11: Return Chr(255) & Chr(133)
        Case SDLK_F12: Return Chr(255) & Chr(134)
        Case SDLK_END: Return Chr(255) & "O"
        Case SDLK_DOWN: Return Chr(255) & "P"
        Case SDLK_PAGEDOWN: Return Chr(255) & "Q"
        Case SDLK_LEFT: Return Chr(255) & "K"
        Case SDLK_RIGHT: Return Chr(255) & "M"
        Case SDLK_HOME: Return Chr(255) & "G"
        Case SDLK_UP: Return Chr(255) & "H"
        Case SDLK_PAGEUP: Return Chr(255) & "I"
        Case SDLK_INSERT: Return Chr(255) & "R"
        Case SDLK_KP_DIVIDE: Return "/"
        Case SDLK_KP_MULTIPLY: Return "*"
        Case SDLK_KP_MINUS: Return "-"
        Case SDLK_KP_PLUS: Return "+"
        Case SDLK_KP_ENTER: Return Chr(13)
        Case Else
            If KeyMod And KMOD_NUM Then
                Select Case As Const KeyCode
                Case SDLK_KP_1: Return "1"
                Case SDLK_KP_2: Return "2"
                Case SDLK_KP_3: Return "3"
                Case SDLK_KP_4: Return "4"
                Case SDLK_KP_5: Return "5"
                Case SDLK_KP_6: Return "6"
                Case SDLK_KP_7: Return "7"
                Case SDLK_KP_8: Return "8"
                Case SDLK_KP_9: Return "9"
                Case SDLK_KP_0: Return "0"
                Case SDLK_KP_PERIOD: Return "."
                End Select
               Else
                Select Case As Const KeyCode
                Case SDLK_KP_1: Return Chr(255) & "O"
                Case SDLK_KP_2: Return Chr(255) & "P"
                Case SDLK_KP_3: Return Chr(255) & "Q"
                Case SDLK_KP_4: Return Chr(255) & "K"
                Case SDLK_CLEAR: Return Chr(255) & "L"
                Case SDLK_KP_6: Return Chr(255) & "M"
                Case SDLK_KP_7: Return Chr(255) & "G"
                Case SDLK_KP_8: Return Chr(255) & "H"
                Case SDLK_KP_9: Return Chr(255) & "I"
                Case SDLK_KP_0: Return Chr(255) & "R"
                Case SDLK_KP_PERIOD: Return Chr(255) & "S"
                End Select
            End If
            If KeyCode = SDLK_DELETE Then Return Chr(255) & "S"
        End Select
    End If
    Return ""
End Function

Sub Render(RI As RenderInfo, WS As WorldState, UI As UserInterfaceState)
    WS.TickOfLastRender = WS.Tick
    
    'Copy Player Screens To Textures for OpenGL display
    For I As Integer = 0 To UBound(WS.RunProgIO)
        If WS.RunProgIO(I).ScreenImg2 <> NULL Then
            If WS.RunProgIO(I).ScreenUpdateFrame = 0 Then FBImageToTexture WS.RunProgIO(I).ScreenImg2, WS.RunProgIO(I).ScreenTex
            ImageDestroy WS.RunProgIO(I).ScreenImg2: WS.RunProgIO(I).ScreenImg2 = NULL
        End If
        If WS.RunProgIO(I).ScreenUpdateFrame AndAlso WS.RunProgIO(I).ScreenImg <> NULL Then
            FBImageToTexture WS.RunProgIO(I).ScreenImg, WS.RunProgIO(I).ScreenTex
            WS.RunProgIO(I).ScreenUpdateFrame = 0
        End If
    Next I
    
    SDL_GL_MakeCurrent RI.Wind, RI.GlContext
    SDL_GL_SwapWindow RI.Wind 'Flip
    
    ' PreRender Tiles to Texture
    Static As Single PSpin, pTilt
    If pSpin <> RI.SpinAng Or pTilt <> RI.TiltAng Then
        glBindFramebuffer(GL_FRAMEBUFFER_EXT, RI.TileFrameBuffer)
            RenderTileTexture RI.VolTile(), RI.TiltAng, RI.SpinAng
        glBindFramebuffer(GL_FRAMEBUFFER_EXT, 0)
        
        glBindFramebuffer(GL_READ_FRAMEBUFFER, RI.TileFrameBuffer)
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, RI.TexFrameBuffer)
        
        glBlitFramebuffer(0, 0, 2*512, 2*128, 0, 0, 2*512, 2*128, GL_COLOR_BUFFER_BIT, GL_LINEAR) ' GL_NEAREST)
        
        glBindFramebuffer(GL_READ_FRAMEBUFFER, 0)
        glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0)
    End If
    pSpin = RI.SpinAng
    pTilt = RI.TiltAng
    
    ' Clear The Screen
    glClearColor 0.1f, 0.3f, 0.6f, 1.0f
    glClear GL_DEPTH_BUFFER_BIT Or GL_COLOR_BUFFER_BIT
    
    ' Tile Map Render
    glDisable GL_LIGHTING
    SetupGlOrtho3D RI.Bounds.W, RI.Bounds.H
    glMatrixMode GL_MODELVIEW
    glLoadIdentity
    glTranslatef RI.Bounds.W \ 2, RI.Bounds.H \ 2, 0
    glScalef RI.TileScale, RI.TileScale, RI.TileScale
    RenderTileMap RI, WS, RI.TileMapLowBound, RI.TileMapLowBound + RI.TileMapSizeBound
    SetupGl2D RI.Bounds.W, RI.Bounds.H
    
    ' Render Sprites
    glEnable GL_NORMALIZE
    glDisable GL_BLEND
    SetupGlOrtho3D RI.Bounds.W, RI.Bounds.H
    glMatrixMode GL_MODELVIEW
    glLoadIdentity
    glTranslatef RI.Bounds.W \ 2, RI.Bounds.H \ 2, 0
    glScalef RI.TileScale, RI.TileScale, RI.TileScale
    VoxGlRenderState 0, 0, VOXEL_NOCLEAR Or VOXEL_NOMODELVIEW
    For I As Integer = 0 To UBound(WS.Ent)
        glPushMatrix
        glTranslatef 32, 32, 32
        glRotatef RI.TiltAng, 1, 0, 0
        glRotatef RI.SpinAng, 0, 1, 0
        
        Var P = (WS.Ent(I).Posn + WS.Ent(I).PrvPosn) * 32 \ 2
        P -= (RI.TileMapSizeBound \ 2 + Vec3I(1, 1, 1)) * 32
        
        glTranslatef P.X, P.Y, P.Z
        glTranslatef -16, -16, -16
        glScalef 2, 2, 2
        VoxGlRenderState 0, 0, VOXEL_NOCLEAR Or VOXEL_NOMODELVIEW Or VOXEL_NOGLSTATE 'Or VOXEL_NOLIGHT
        VoxRenderVolume RI.VolSprite(WS.Ent(I).SpriteIdx)
        glPopMatrix
    Next I
    glDisable GL_NORMALIZE
    glDisable GL_TEXTURE_3D
    glDisable GL_CULL_FACE
    glDisable GL_ALPHA_TEST
    glDisable GL_DEPTH_TEST
    
    '' Render the Tile Texture
    'glDisable GL_LIGHTING
    'glMatrixMode GL_MODELVIEW
    'SetupGl2D RI.Bounds.W, RI.Bounds.H
    'glLoadIdentity
    'glBindTexture GL_TEXTURE_2D, RI.TexTiles
    'glEnable GL_TEXTURE_2D
    ''glBlendFunc GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA
    'glEnable GL_BLEND
    'glColor4ub 255, 255, 255, 255
    'glBegin GL_QUADS
    'glTexCoord2f 0, 1: glVertex3i 0, 0, 0
    'glTexCoord2f 0, 0: glVertex3i 0, 2*128, 0
    'glTexCoord2f 1, 0: glVertex3i 2*512, 2*128, 0
    'glTexCoord2f 1, 1: glVertex3i 2*512, 0, 0
    'glEnd
    'glDisable GL_TEXTURE_2D
    
    Scope 'Render Player Screen
        Dim As Integer W, H, R, WndX, WndY
        Dim As Integer GW, GH, CW, CH
        CW = 8 * WS.RunProgIO(0).ConsoleWidth
        CH = 8 * (UBound(WS.RunProgIO(0).ConsoleLine) + 1)
        R = ImageInfo(WS.RunProgIO(0).ScreenImg, GW, GH)
        If R = 0 Then
            W = IIf(GW > CW, GW, CW)
            H = IIf(GH > CH, GH, CH)
           Else
            W = CW
            H = CH
        End If
        If RI.Wind2 <> NULL Then
            SDL_GL_MakeCurrent RI.Wind2, RI.GlContext
            SetupGl2D W, H
            glClearColor 0.0f, 0.0f, 0.0f, 1.0f
            glClear GL_DEPTH_BUFFER_BIT Or GL_COLOR_BUFFER_BIT
           Else
            SetupGl2D RI.Bounds.W, RI.Bounds.H
            WndX = RI.Bounds.W - W
            WndY = RI.Bounds.H - H
        End If
        glDisable GL_BLEND
        glDisable GL_DEPTH_TEST
        glDisable GL_LIGHTING
        glMatrixMode GL_MODELVIEW
        glLoadIdentity
        If GW < CW Or GH < CH Then 'Display Console
            glPushMatrix
            glTranslatef WndX, WndY, 0
            glDisable GL_TEXTURE_2D
            glEnable GL_BLEND
            glBlendFunc GL_ONE, GL_ONE_MINUS_SRC_ALPHA
            glColor4ub 64, 64, 200, 196
            DrawSolidRect 0, 0, CW, CH
            glDisable GL_BLEND
            glColor4ub 64, 64, 200, 255
            DrawRect -1, -1, CW, CH
            
            glBindTexture GL_TEXTURE_2D, RI.GfxFont
            glEnable GL_ALPHA_TEST
            glEnable GL_TEXTURE_2D
            
            For I As Integer = 0 To UBound(WS.RunProgIO(0).ConsoleLine)
                DrawTextGL WS.RunProgIO(0).ConsoleLine(I), 0, 8 * I + 1, 1, RGB(0, 0, 0)
                DrawTextGL WS.RunProgIO(0).ConsoleLine(I), 0, 8 * I, 1, RGB(255, 255, 255)
            Next I
            glPopMatrix
        End If
        If R = 0 Then 'Display FBGFX Screen
            glBindTexture GL_TEXTURE_2D, WS.RunProgIO(0).ScreenTex
            glDisable GL_ALPHA_TEST
            glEnable GL_TEXTURE_2D
            glColor4ub 255, 255, 255, 255
            glTranslatef WndX, WndY, 0
            glBegin GL_QUADS
            glTexCoord2f 0, 1: glVertex3i 0, 0, 0
            glTexCoord2f 0, 0: glVertex3i 0, GH, 0
            glTexCoord2f 1, 0: glVertex3i GW, GH, 0
            glTexCoord2f 1, 1: glVertex3i GW, 0, 0
            glEnd
        End If
        If RI.Wind2 <> NULL Then SDL_GL_SwapWindow RI.Wind2: SDL_GL_MakeCurrent RI.Wind, RI.GlContext
    End Scope
    
    ' Render Game Text
    glDisable GL_LIGHTING
    glDisable GL_DEPTH_TEST
    SetupGl2D RI.Bounds.W, RI.Bounds.H
    glMatrixMode GL_MODELVIEW
    glLoadIdentity
    glBindTexture GL_TEXTURE_2D, RI.GfxFont
    glEnable GL_TEXTURE_2D
    
    For I As Integer = 0 To UBound(UI.Menu)
        UI.Menu(I).Render
    Next I
    
    'DrawTextGL "FB Programs of Legend", 101, 101, 2, RGB(0, 0, 0)
    'DrawTextGL "FB Programs of Legend", 100, 100, 2, RGB(255, 255, 255)
    
    DrawTextGL " VFPS: " & UI.VFPS, RI.Bounds.W - 79, 11, 1, RGB(0, 0, 0)
    DrawTextGL " VFPS: " & UI.VFPS, RI.Bounds.W - 80, 10, 1
    
    DrawTextGL " SFPS: " & UI.SFPS, RI.Bounds.W - 79, 31, 1, RGB(0, 0, 0)
    DrawTextGL " SFPS: " & UI.SFPS, RI.Bounds.W - 80, 30, 1
    
    glDisable GL_TEXTURE_2D
    
    VoxSetVolume RI.VolSprite(0) 'Animate Cursor on Back of Player Sprite
    VoxSetColor IIf(WS.Tick \ 30 Mod 2 = 0, 0, RGB(180,180,180))
    VoxLine Vec3I(3, 14, 12), Vec3I(5, 14, 12)
End Sub

Sub MenuLoop(RI As RenderInfo, WS As WorldState, UI As UserInterfaceState)
    Dim As ButtonEvent PrvBtnEvt, BtnEvt
    Dim As Integer PrvBtnState, BtnState, PrvBtnY, BtnY
    Dim As LongInt LastEventTime
    Do
        Render RI, WS, UI
        
        'Translate State Vector into a Falling Edge Event Trigger
        PrvBtnState XOr= BtnState
        PrvBtnState And= BtnState
        For BtnEvt = BtnUp To BtnNone - 1
            If PrvBtnState = 1 Shl BtnEvt Then Exit For
        Next BtnEvt
        
        If BtnState = 0 Then PrvBtnEvt = BtnNone 'Button Repeat
        If BtnEvt <> BtnNone Then PrvBtnEvt = BtnEvt: LastEventTime = SDL_GetTicks
        If PrvBtnEvt <> BtnNone And (SDL_GetTicks - LastEventTime) > 300 Then BtnEvt = PrvBtnEvt: LastEventTime = SDL_GetTicks - 270
        
        For I As Integer = 0 To UBound(UI.Menu)
            If BtnEvt = BtnMouseDown Then 'Change Focus if click inside rectangle
                Var X = UI.GUIS.MX - UI.Menu(I).X
                Var Y = UI.GUIS.MY - UI.Menu(I).Y
                If X >= 0 And Y >= 0 And X <= UI.Menu(I).W And Y <= UI.Menu(I).H Then UI.GUIS.FocusIndex = I
            End If
            If I = UI.GUIS.FocusIndex Then 'Send Event to Widget
                UI.Menu(I).DoInput BtnEvt, UI.GUIS
               Else
                UI.Menu(I).DoInput BtnNone, UI.GUIS
            End If
        Next I
        PrvBtnState = BtnState
        BtnState = 0
        
        Dim Event As SDL_Event
        Do While SDL_PollEvent(@Event) <> 0
            Select Case Event.Type
            Case SDL_KEYDOWN
                If Event.Key.KeySym.Sym = SDLK_BACKQUOTE And (Event.Key.KeySym.Mod_ And KMOD_ALT) <> 0 Then
                    Select Case RI.MonitorNum  'Change Display Mode
                    Case -2
                        RI.MonitorNum = -1
                        RI.Bounds = Type(100, 100, 640, 480)
                    Case -1
                        Select Case RI.Bounds.H
                        Case 480: RI.Bounds = Type(100, 100, 800, 600)
                        Case 600: RI.Bounds = Type(100, 100, 1024, 768)
                        Case 768: RI.Bounds = Type(100, 100, 1600, 1024)
                        'Case 1024: RI.Bounds = Type(100, 100, 1920, 1080)
                        Case Else: RI.MonitorNum = 0
                        End Select
                    Case Else
                        RI.MonitorNum += 1
                        If RI.MonitorNum >= SDL_GetNumVideoDisplays Then RI.MonitorNum = -2
                        If SDL_GetNumVideoDisplays = 1 Then
                            RI.MonitorNum = -1
                            RI.Bounds = Type(100, 100, 640, 480)
                        End If
                    End Select
                    
                    SetWindowMode RI
                End If
                
                Var K = SDL_Keycode_to_InKeyStr(Event.Key.KeySym.Sym, Event.Key.KeySym.Mod_)
                UI.GUIS.InK = K 'Send Input to Widgets
                
                Select Case Event.Key.KeySym.Sym
                Case SDLK_UP: BtnState Or= 1 Shl BtnUp
                Case SDLK_DOWN: BtnState Or= 1 Shl BtnDown
                Case SDLK_PAGEUP: BtnState Or= 1 Shl BtnPgUp
                Case SDLK_PAGEDOWN: BtnState Or= 1 Shl BtnPgDown
                Case SDLK_LEFT: BtnState Or= 1 Shl BtnLeft
                Case SDLK_RIGHT: BtnState Or= 1 Shl BtnRight
                Case SDLK_KP_ENTER: BtnState Or= 1 Shl BtnSelect
                Case SDLK_DELETE, SDLK_ESCAPE: BtnState Or= 1 Shl BtnBack
                Case SDLK_TAB: UI.GUIS.FocusIndex = (UI.GUIS.FocusIndex + 1) Mod (UBound(UI.Menu) + 1)
                Case Else: BtnState Or= 1 Shl BtnKeyTyped
                End Select
                
            Case SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP
                Dim As ULong MB, MX, MY
                MB = SDL_GetMouseState(@MX, @MY)
                UI.GUIS.MX = MX
                UI.GUIS.MY = MY
                UI.GUIS.MB = 0
                If MB And SDL_BUTTON_LMASK Then UI.GUIS.MB Or= 1
                If MB And SDL_BUTTON_RMASK Then UI.GUIS.MB Or= 2
                If MB And SDL_BUTTON_MMASK Then UI.GUIS.MB Or= 4
            Case SDL_MOUSEWHEEL
                UI.GUIS.MS = Event.Wheel.Y
                BtnState Or= 1 Shl BtnMouseRoll
            Case SDL_WINDOWEVENT
                Select Case Event.window.event
                'Case SDL_WINDOWEVENT_MOVED
                'Case SDL_WINDOWEVENT_RESIZED
                Case SDL_WINDOWEVENT_CLOSE
                    If Event.window.windowID = SDL_GetWindowID(RI.Wind) Then
                        Event.type = SDL_QUIT_
                        SDL_PushEvent @Event
                    End If
                End Select
            Case SDL_CONTROLLERDEVICEADDED
                ConsolePrint "Controller Device Added " & Event.cdevice.which
                Var TmpPad = SDL_GameControllerOpen(Event.cdevice.which)
                For I As Integer = 0 To UBound(UI.GamePad)
                    If UI.GamePad(I) = NULL Then UI.GamePad(I) = TmpPad: TmpPad = NULL: Exit For
                Next I
                If TmpPad <> NULL Then
                    ReDim Preserve UI.GamePad(UBound(UI.GamePad) + 1)
                    UI.GamePad(UBound(UI.GamePad)) = TmpPad
                End If
            Case SDL_CONTROLLERDEVICEREMOVED
                ConsolePrint "Controller Device Removed " & Event.cdevice.which ' Useless Value
                For I As Integer = 0 To UBound(UI.GamePad)
                    If SDL_GameControllerGetAttached(UI.GamePad(I)) = SDL_FALSE Then
                        SDL_GameControllerClose UI.GamePad(I)
                        UI.GamePad(I) = NULL
                    End If
                Next I
            End Select
            If DoBasicEvents(Event) Then Exit Do, Do
        Loop
        
        If UI.GUIS.MB And 3 Then BtnState Or= 1 Shl BtnMouseDown
        'Map GamePad data
        BtnY = 0
        For I As Integer = 0 To UBound(UI.GamePad)
            If UI.GamePad(I) <> NULL AndAlso SDL_GameControllerGetAttached(UI.GamePad(I)) Then
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_A) Then BtnState Or= 1 Shl BtnSelect
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_B) Then BtnState Or= 1 Shl BtnBack
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_BACK) Then BtnState Or= 1 Shl BtnBack
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_Y) Then BtnY = 1
                
                Select Case SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_LEFTX)
                Case Is > 3 * 2 ^ 13: BtnState Or= 1 Shl BtnLeft
                Case Is < -3 * 2 ^ 13: BtnState Or= 1 Shl BtnRight
                End Select
                Select Case SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_LEFTY)
                Case Is > 3 * 2 ^ 13
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_X) Then
                        BtnState Or= 1 Shl BtnPgDown
                       Else
                        BtnState Or= 1 Shl BtnDown
                    End If
                Case Is < -3 * 2 ^ 13
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_X) Then
                        BtnState Or= 1 Shl BtnPgUp
                       Else
                        BtnState Or= 1 Shl BtnUp
                    End If
                End Select
                
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_DOWN) Then BtnState Or= 1 Shl BtnDown
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_UP) Then BtnState Or= 1 Shl BtnUp
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_RIGHT) Then BtnState Or= 1 Shl BtnRight
                If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_LEFT) Then BtnState Or= 1 Shl BtnLeft
            End If
        Next I
        If PrvBtnY = 0 And BtnY = 1 Then UI.GUIS.FocusIndex = (UI.GUIS.FocusIndex + 1) Mod (UBound(UI.Menu) + 1) 'Tab
        PrvBtnY = BtnY
    Loop
End Sub

' >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
' Entry Point
' <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
Scope
    #If __FB_ERR__ > 0 'Display Build Info
        Print Command(0);
        #IfDef __FB_64BIT__
            Print " (64 bit)"
        #Else
            Print " (32 bit)"
        #EndIf
        Print "      - Compiled: " & __TIME__ & " " & __DATE__
        Print "        Using fbc " & __FB_VERSION__
        If Command <> "" Then Print "Ran with command: " & Command
        Print ""
    #EndIf
    
    Dim RI As RenderInfo, WS As WorldState, UI As UserInterfaceState
    
    'Initialize SDL
    If SDL_Init(SDL_INIT_VIDEO Or SDL_INIT_TIMER Or SDL_INIT_GAMECONTROLLER Or SDL_INIT_HAPTIC) <> 0 Then End 1
    
    Dim As SDL_Joystick Ptr JoyStick()
    If SDL_NumJoysticks > 0 Then 'Enable Joysticks
        SDL_JoystickEventState SDL_ENABLE
        ReDim JoyStick(SDL_NumJoysticks - 1)
        For I As Integer = 0 To UBound(JoyStick)
            JoyStick(I) = SDL_JoystickOpen(I)
        Next I
        'Dim J As Integer = 0 'Obtain Gamepads
        'For I As Integer = 0 To UBound(JoyStick)
        '    If SDL_IsGameController(I) Then J += 1
        'Next I
        'If J > 0 Then ReDim GamePad(J - 1)
        'J = 0
        'For I As Integer = 0 To UBound(JoyStick)
        '    If SDL_IsGameController(I) Then
        '        GamePad(J) = SDL_GameControllerOpen(I)
        '        J += 1
        '    End If
        'Next I
        #If __FB_ERR__ > 0 'Display Joystick Info
            Print "Joysticks Detected: " & (UBound(JoyStick) + 1)
            For I As Integer = 0 To UBound(JoyStick)
                If JoyStick(I) <> NULL Then
                    If SDL_IsGameController(I) Then Print "  Game Controller:";
                    Print "  " & *SDL_JoystickNameForIndex(I)
                    Print "    Axies: " & SDL_JoystickNumAxes(JoyStick(I))
                    Print "    Buttons: " & SDL_JoystickNumButtons(JoyStick(I))
                    Print "    Hats: " & SDL_JoystickNumHats(JoyStick(I))
                    Print "    Balls: " & SDL_JoystickNumBalls(JoyStick(I))
                End If
            Next I
            Print ""
        #EndIf
    End If
    
    Dim KeyDownSize As Long
    Dim As Const UByte Ptr KeyDown = SDL_GetKeyboardState(@KeyDownSize)
    
    #If __FB_ERR__ > 0 'Display Video Display Info
        Print "Displays"; SDL_GetNumVideoDisplays
        Scope
            Dim As SDL_Rect Bounds
            For I As Integer = 0 To SDL_GetNumVideoDisplays - 1
                SDL_GetDisplayBounds I, @Bounds
                Print "  " & I & " " & *SDL_GetDisplayName(I) & " " & Bounds.W & "x" & Bounds.H & " (" & Bounds.X & ", " & Bounds.Y & ")"
            Next I
            Print ""
        End Scope
        Print KeyDownSize & " Keys in Keyboard State"
        Print ""
    #EndIf
    
    ' Create Window
    Select Case RI.MonitorNum
    Case RI_AllScreenWindow
        RI.Bounds = GetAllScreenBounds
        RI.Wind = SDL_CreateWindow("FB Programs of Legend", RI.Bounds.X, RI.Bounds.Y, RI.Bounds.W, RI.Bounds.H, SDL_WINDOW_OPENGL) ' Or SDL_WINDOW_BORDERLESS)
    Case RI_PlainWindow
        RI.Wind = SDL_CreateWindow("FB Programs of Legend", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, RI.Bounds.W, RI.Bounds.H, SDL_WINDOW_OPENGL)
    Case Else
        If SDL_GetNumVideoDisplays > RI.MonitorNum Then
            SDL_GetDisplayBounds RI.MonitorNum, @RI.Bounds
            RI.Wind = SDL_CreateWindow("FB Programs of Legend", RI.Bounds.X, RI.Bounds.Y, RI.Bounds.W, RI.Bounds.H, SDL_WINDOW_OPENGL Or SDL_WINDOW_FULLSCREEN_DESKTOP)
        End If
    End Select
    
    #IfNDef __FB_WIN32__ 'Load the window Icon (Linux doesn't load it from the windows resource file PoL.rc)
        Dim Icon As SDL_Surface Ptr = SDL_LoadBMP(ExePath & "/Img/ColorCube16x16.bmp")
        'SDL_SetColorKey Icon, 1, RGB(255, 255, 255)
        SDL_SetWindowIcon RI.Wind, Icon
    #EndIf
    
    'RI.Wind2 = SDL_CreateWindow("Player1.bas", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 320, 200, SDL_WINDOW_OPENGL)
    
    InitGraphics RI
    
    If Not RI.GlShaderExt Then ConsolePrint "WARNING: Extensions Failed to Load! (eg, Render to Texture might be Unsupported)"
    
    'SDL_GL_GetSwapInterval
    If SDL_GL_SetSwapInterval(RI.SwapInterval) <> 0 Then ConsolePrint "WARNING: Swap Interval not supported"
    
    'Generate Tiles & Sprites
    MakeTerrainTiles RI.VolTile()
    MakeSprites RI.VolSprite()
    
    RI.TileMapSizeBound = Vec3I(31, 4, 31)
    RI.TileMapLowBound = Vec3I(0, 0, 0)
    RI.SpinAng = -20
    RI.TiltAng = 40
    
    'Initialize Game State
    MapGen WS.WorldMap(), Vec3I(255, 4, 255)
    ReDim WS.Ent(0)
    WS.Ent(0).Posn = Vec3I(16, 1, 16)
    WS.Ent(0).Facing = Vec3I(1, 0, 0)
    WS.Ent(0).PrvPosn = WS.Ent(0).Posn
    WS.Ent(0).SpriteIdx = 0
    WS.Ent(0).HP = 100
    WS.Ent(0).ProgIdx = 0
    WS.Ent(0).IO_Idx = 0
    
    MakeDefaultPlayerProg WS
    WS.RunProgIO(0).KeyDown = KeyDown
    WS.RunProgIO(0).WS = @WS
    For I As Integer = 0 To SDL_NumJoysticks - 1
        WS.RunProgIO(0).JoyStick(I).GamePadNum = I
    Next I
    
    Dim As LongInt SFrames, GFrames, pTI, TI, RollOverCount
    Dim As LongInt LastFrame, RT, RunSleepT, RunRendT
    Dim As FrameTime FrameHist(119) '120 Frames (2 Secs) of History
    
    LastFrame = SDL_GetTicks()
    
    ThreadsInit
    'Start Programs
    WS.RunProgIO(0).RunStart WS.ProgUnit(0)
    ThreadsRun
    'WS.RunProgIO(0).EndRun
    
    'Init Menu
    ReDim UI.Menu(1)
    UI.Menu(0).X = 10
    UI.Menu(0).Y = 10
    UI.Menu(0).W = RI.Bounds.W - 100
    UI.Menu(0).H = 37
    UI.Menu(0).Sel = 2
    UI.Menu(0).DefaultColorCodingStyles
    ReDim (UI.Menu(0).OptionText)(UBound(WS.ProgUnit(0).Code.Proc) + 2)
    UI.Menu(0).OptionText(0).AddText "Shared ", 1
    UI.Menu(0).OptionText(0).AddText "Globals", 0
    For I As Integer = 0 To UBound(WS.ProgUnit(0).Code.Proc)
        If WS.ProgUnit(0).Code.Proc(I).ProcName = "Run" Then
            UI.Menu(0).OptionText(I + 1).Text = "Main Program Body"
           Else
            If WS.ProgUnit(0).Code.Proc(I).ReturnType = VtVOID Then
                UI.Menu(0).OptionText(I + 1).AddText "Sub ", 1
               Else
                UI.Menu(0).OptionText(I + 1).AddText "Function ", 1
            End If
            UI.Menu(0).OptionText(I + 1).AddText WS.ProgUnit(0).Code.Proc(I).ProcName, 2
        End If
    Next I
    UI.Menu(0).OptionText(UBound(UI.Menu(0).OptionText)).AddText "New ", 4
    UI.Menu(0).OptionText(UBound(UI.Menu(0).OptionText)).AddText "Sub", 1
    UI.Menu(0).OptionText(UBound(UI.Menu(0).OptionText)).AddText " or "
    UI.Menu(0).OptionText(UBound(UI.Menu(0).OptionText)).AddText "New ", 4
    UI.Menu(0).OptionText(UBound(UI.Menu(0).OptionText)).AddText "Function", 1
    
    UI.Menu(1).X = 10
    UI.Menu(1).Y = 57
    UI.Menu(1).W = RI.Bounds.W - 100
    UI.Menu(1).H = RI.Bounds.H - 68
    UI.Menu(1).Sel = 0
    UI.Menu(1).SelBoxCol = RGB(200, 64, 64)
    UI.Menu(1).DefaultColorCodingStyles
    ReDim (UI.Menu(1).OptionText)(UBound(WS.ProgUnit(0).Code.Proc(1).Lines))
    Var LenLineNums = Len(Str(UBound(WS.ProgUnit(0).Code.Proc(1).Lines) + 1))
    For I As Integer = 0 To UBound(WS.ProgUnit(0).Code.Proc(1).Lines)
        Var CodeLine = @WS.ProgUnit(0).Code.Proc(1).Lines(I)
        UI.Menu(1).OptionText(I) = LineFromCode(I + 1, LenLineNums, *CodeLine)
    Next I
    
    For I As Integer = 0 To UBound(UI.Menu)
        UI.Menu(I).Index = I
    Next I
    
    MenuLoop RI, WS, UI
    
    ' Message Loop
    Do
        If FrameHist(TI).FrameT > SDL_GetTicks Then RollOverCount += 1 '49 Day RollOver
        TI = (TI + 1) Mod (UBound(FrameHist) + 1)
        FrameHist(TI).FrameT = SDL_GetTicks() + RollOverCount * &H100000000ull
        SFrames += 1
        If WS.Tick = WS.TickOfLastRender Then GFrames += 1
        If FrameHist(TI).FrameT - LastFrame >= 1000 Then
            UI.SFPS = SFrames
            UI.VFPS = GFrames
            LastFrame = FrameHist(TI).FrameT
            SFrames = 0
            GFrames = 0
        End If
        WS.Tick += 1
        
        ThreadsRun
        
        For I As Integer = 0 To UBound(WS.RunProgIO) 'Let Threads Exit
            If WS.RunProgIO(I).EndProgNow Then WS.RunProgIO(I).EndRun
        Next I
        
        'Allocate CPU Time to Program Threads
        For I As Integer = 0 To UBound(WS.RunProgIO)
            If WS.RunProgIO(I).Globals <> NULL Then
                WS.RunProgIO(I).Globals->CodeToGo += THREAD_TIME_SLICE '* PlayerCPU_Level
                If WS.RunProgIO(I).Globals->CodeToGo > 3 * THREAD_TIME_SLICE \ 2 Then WS.RunProgIO(0).Globals->CodeToGo = 3 * THREAD_TIME_SLICE \ 2
            End If
            'WS.RunProgIO(I).TickTime = WS.Tick 'Advance bot clocks
        Next I
        
        Dim Event As SDL_Event
        Do While SDL_PollEvent(@Event) <> 0
            Select Case Event.Type
            Case SDL_KEYDOWN
                If Event.Key.KeySym.Sym = SDLK_BACKQUOTE And (Event.Key.KeySym.Mod_ And KMOD_ALT) <> 0 Then
                    Select Case RI.MonitorNum  'Change Display Mode
                    Case -2
                        RI.MonitorNum = -1
                        RI.Bounds = Type(100, 100, 640, 480)
                    Case -1
                        Select Case RI.Bounds.H
                        Case 480: RI.Bounds = Type(100, 100, 800, 600)
                        Case 600: RI.Bounds = Type(100, 100, 1024, 768)
                        Case 768: RI.Bounds = Type(100, 100, 1600, 1024)
                        'Case 1024: RI.Bounds = Type(100, 100, 1920, 1080)
                        Case Else: RI.MonitorNum = 0
                        End Select
                    Case Else
                        RI.MonitorNum += 1
                        If RI.MonitorNum >= SDL_GetNumVideoDisplays Then RI.MonitorNum = -2
                        If SDL_GetNumVideoDisplays = 1 Then
                            RI.MonitorNum = -1
                            RI.Bounds = Type(100, 100, 640, 480)
                        End If
                    End Select
                    
                    SetWindowMode RI
                End If
                
                'SDL_GetKeyName
                'SDL_GetModState
                Var K = SDL_Keycode_to_InKeyStr(Event.Key.KeySym.Sym, Event.Key.KeySym.Mod_)
                If K <> "" Then WS.RunProgIO(0).EnQueueInKey K 'Fill InKey Buffer
                
                'If Event.Key.KeySym.Sym = SDLK_LALT Or Event.Key.KeySym.Sym = SDLK_RALT Then
                'End If
                
            Case SDL_MOUSEMOTION, SDL_MOUSEBUTTONDOWN, SDL_MOUSEBUTTONUP
                Dim As Integer W, H, R, WndX, WndY
                Dim As Long MX, MY, MB
                R = ImageInfo(WS.RunProgIO(0).ScreenImg, W, H)
                If R <> 0 Then W = 320: H = 200
                If RI.Wind2 = NULL Then
                    WndX = RI.Bounds.W - W
                    WndY = RI.Bounds.H - H
                End If
                
                MB = SDL_GetMouseState(@MX, @MY)
                If MX >= WndX And MY >= WndY Then
                    WS.RunProgIO(0).MouseX = MX - WndX
                    WS.RunProgIO(0).MouseY = MY - WndY
                   Else
                    WS.RunProgIO(0).MouseX = -1
                    WS.RunProgIO(0).MouseY = -1
                End If
                WS.RunProgIO(0).MouseButtons = 0
                If MB And SDL_BUTTON_LMASK Then WS.RunProgIO(0).MouseButtons Or= 1
                If MB And SDL_BUTTON_RMASK Then WS.RunProgIO(0).MouseButtons Or= 2
                If MB And SDL_BUTTON_MMASK Then WS.RunProgIO(0).MouseButtons Or= 4
                
            Case SDL_MOUSEWHEEL
                WS.RunProgIO(0).MouseWheel += Event.Wheel.Y
                
            Case SDL_WINDOWEVENT
                Select Case Event.window.event
                'Case SDL_WINDOWEVENT_MOVED
                'Case SDL_WINDOWEVENT_RESIZED
                Case SDL_WINDOWEVENT_CLOSE
                    If Event.window.windowID = SDL_GetWindowID(RI.Wind) Then
                        Event.type = SDL_QUIT_
                        SDL_PushEvent @Event
                       Else
                        WS.RunProgIO(0).EnQueueInKey Chr(255) & "k" 'Send the close window signal to the program
                        'WS.RunProgIO(0).EndRun 'Kill the program w/o giving it a chance to use the signal :P
                        'If RI.Wind2 <> NULL Then SDL_DestroyWindow RI.Wind2: RI.Wind2 = NULL
                    End If
                End Select
            Case SDL_CONTROLLERDEVICEADDED
                ConsolePrint "Controller Device Added " & Event.cdevice.which
                Var TmpPad = SDL_GameControllerOpen(Event.cdevice.which)
                For I As Integer = 0 To UBound(UI.GamePad)
                    If UI.GamePad(I) = NULL Then UI.GamePad(I) = TmpPad: TmpPad = NULL: Exit For
                Next I
                If TmpPad <> NULL Then
                    ReDim Preserve UI.GamePad(UBound(UI.GamePad) + 1)
                    UI.GamePad(UBound(UI.GamePad)) = TmpPad
                End If
            Case SDL_CONTROLLERDEVICEREMOVED
                ConsolePrint "Controller Device Removed " & Event.cdevice.which ' Useless Value
                For I As Integer = 0 To UBound(UI.GamePad)
                    If SDL_GameControllerGetAttached(UI.GamePad(I)) = SDL_FALSE Then
                        SDL_GameControllerClose UI.GamePad(I)
                        UI.GamePad(I) = NULL
                    End If
                Next I
            End Select
            If DoBasicEvents(Event) Then Exit Do, Do
        Loop
        
        'Copy GamePad data for GetJoystick
        For J As Integer = 0 To UBound(WS.RunProgIO(0).JoyStick)
            With WS.RunProgIO(0).JoyStick(J)
                Var I = .GamePadNum
                If I >= 0 AndAlso I <= UBound(UI.GamePad) AndAlso UI.GamePad(I) <> NULL AndAlso SDL_GameControllerGetAttached(UI.GamePad(I)) Then
                    .Attached = -1
                    .Btn = 0
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_A) Then .Btn Or= 1
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_B) Then .Btn Or= 2
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_X) Then .Btn Or= 4
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_Y) Then .Btn Or= 8
                    
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_LEFTSHOULDER) Then .Btn Or= &H10
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_RIGHTSHOULDER) Then .Btn Or= &H20
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_BACK) Then .Btn Or= &H40
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_START) Then .Btn Or= &H80
                    
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_LEFTSTICK) Then .Btn Or= &H100
                    If SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_RIGHTSTICK) Then .Btn Or= &H200
                    'If SDL_GameControllerGetButton(GamePad(I), SDL_CONTROLLER_BUTTON_GUIDE) Then .Btn Or= &H400
                    
                    .Axis(0) = SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_LEFTX) / 2 ^ 15
                    .Axis(1) = SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_LEFTY) / 2 ^ 15
                    
                    .Axis(2) = (SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_TRIGGERLEFT) - SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_TRIGGERRIGHT)) / 2 ^ 15
                    
                    .Axis(3) = SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_RIGHTY) / 2 ^ 15
                    .Axis(4) = SDL_GameControllerGetAxis(UI.GamePad(I), SDL_CONTROLLER_AXIS_RIGHTX) / 2 ^ 15
                    
                    .Axis(5) = -1000
                    
                    .Axis(6) = SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_RIGHT) - SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_LEFT)
                    .Axis(7) = SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_DOWN) - SDL_GameControllerGetButton(UI.GamePad(I), SDL_CONTROLLER_BUTTON_DPAD_UP)
                   Else
                    .Attached = 0
                End If
            End With
        Next J
        
        'Execute Player Commands
        RI.SpinAng += WS.RunProgIO(0).Settings(0) / 50
        RI.TiltAng += WS.RunProgIO(0).Settings(1) / 100
        RI.TileScale *= Exp(WS.RunProgIO(0).Settings(2) / 10000)
        If WS.RunProgIO(0).Settings(3) <> 0 Then
            If RI.Wind2 = NULL Then
                RI.Wind2 = SDL_CreateWindow("Player1.bas", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 320, 200, SDL_WINDOW_OPENGL)
               Else
                SDL_DestroyWindow RI.Wind2: RI.Wind2 = NULL
            End If
        End If
        
        If RI.TileScale < 0.1 Then RI.TileScale = 0.1
        If RI.TileScale > 10 Then RI.TileScale = 10
        For I As Integer = 0 To UBound(WS.RunProgIO(0).Settings)
            WS.RunProgIO(0).Settings(I) = 0
        Next I
        
        For I As Integer = 0 To UBound(WS.RunProgIO)
            Select Case WS.RunProgIO(I).NextAction
            Case BotMoveTo
                WS.Ent(I).Posn += WS.RunProgIO(I).ActionVec
                WS.Ent(I).PrvPosn = WS.Ent(I).Posn
            End Select
            
            WS.RunProgIO(I).NextAction = BotNoAction
            WS.RunProgIO(I).ActionVec = Vec3I(0,0,0)
            WS.RunProgIO(I).ActionTarget = ""
        Next I
        
        Var J = -1 'Transmit Messages
        For I As Integer = 0 To UBound(WS.RunProgIO)
            If WS.RunProgIO(I).OutMsg.Channel <> 0 Then J += 1
        Next I
        If J = -1 Then
            Erase WS.MsgTrans
           Else
            ReDim WS.MsgTrans(J)
            J = 0
            For I As Integer = 0 To UBound(WS.RunProgIO)
                If WS.RunProgIO(I).OutMsg.Channel <> 0 Then
                    WS.MsgTrans(J) = WS.RunProgIO(I).OutMsg
                    J += 1
                End If
            Next I
        End If
        For I As Integer = 0 To UBound(WS.RunProgIO)
            With WS.RunProgIO(I)
                If UBound(WS.MsgTrans) = -1 Then
                    Erase .UnreadMsgIdx
                   Else
                    ReDim .UnreadMsgIdx(UBound(WS.MsgTrans))
                    For J = 0 To UBound(.UnreadMsgIdx)
                        .UnreadMsgIdx(J) = J
                    Next J
                End If
            End With
        Next I
        
        'Control Simulation Rate, by Sleeping (if too fast) or skipping Render (if too slow)
        pTI = (TI + UBound(FrameHist) + 1 - 12) Mod (UBound(FrameHist) + 1) 'Index From 12 Frames (0.2 sec) ago
        RT = 0
        If FrameHist(TI).FrameT - FrameHist(pTI).FrameT - RunSleepT < 200 Then
            RT = SDL_GetTicks
            Render RI, WS, UI
            RT = SDL_GetTicks - RT
            
            Dim As LongInt XtraT = FrameHist(pTI).FrameT + 200 - SDL_GetTicks
            If XtraT > RT Then
                FrameHist(TI).SleepT = SDL_GetTicks
                SDL_Delay XtraT - RT 'Sleep XtraT - RT, 1
                FrameHist(TI).SleepT = SDL_GetTicks - FrameHist(TI).SleepT
                If FrameHist(TI).SleepT < 0 Then FrameHist(TI).SleepT = 1 '49 Day Rollover
            End If
            'Do: Loop Until SDL_GetTicks - pT >= 200 'Accurate Non-Sleeping WaitLoop (Don't use, needlessly eats CPU)
           Else
            FrameHist(TI).SleepT = 0
            If WS.Tick > WS.TickOfLastRender + 2 Or SDL_GetTicks - FrameHist(pTI).FrameT < 200 - RunRendT / 12 Then
                RT = SDL_GetTicks
                'glFlush
                Render RI, WS, UI
                RT = SDL_GetTicks - RT
            End If
        End If
        If RT < 0 Then RT = 1 '49 Day Rollover
        FrameHist(TI).RendT = RT
        
        RunSleepT += FrameHist(TI).SleepT - FrameHist(pTI).SleepT
        RunRendT += FrameHist(TI).RendT - FrameHist(pTI).RendT
    Loop
    
    'Cleanup
    Erase WS.RunProgIO
    Erase WS.ProgUnit
    ThreadsCleanup
    For I As Integer = 0 To UBound(UI.GamePad)
        If UI.GamePad(I) <> NULL Then SDL_GameControllerClose UI.GamePad(I)
    Next I
    GraphicsCleanup RI
    If RI.Wind2 <> NULL Then SDL_DestroyWindow RI.Wind2: RI.Wind2 = NULL
    If RI.Wind <> NULL Then SDL_DestroyWindow RI.Wind: RI.Wind = NULL
    SDL_Quit
    'End 1
End Scope