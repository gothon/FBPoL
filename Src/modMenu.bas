
#Include "modMenu.bi"

#Include "modGraphics.bi"

Sub SuperMenuWidget.SetText(Text As String)
End Sub

Sub SuperMenuWidget.Render()
    glDisable GL_DEPTH_TEST
    'glBindTexture GL_TEXTURE_2D, RI.GfxFont
    glEnable GL_TEXTURE_2D
    glMatrixMode GL_MODELVIEW
    glPushMatrix
    glLoadIdentity
    glTranslatef X, Y, 0
    
    Var T = (SDL_GetTicks \ 500) Mod 2 'Blink Time
    For I As Integer = TopLine To UBound(OptionText)
        If 9 * I > H Then Exit For
        
        Var Txt = @OptionText(I).LineTxt(0).Text
        
        If Sel = I Then
            glDisable GL_TEXTURE_2D
            'glColor4ub 64 + 128 * T, 64 + 128 * T, 200 + 55 * T, 255
            glColor4ub 200, 64 + 128 * T, 64 + 128 * T, 255
            DrawRect 0, 9 * I, 8 * Len(*Txt), 9 * I + 9
            glEnable GL_TEXTURE_2D
        End If
        
        For J As Integer = 0 To UBound(OptionText(I).LineTxt)
            DrawTextGL *Txt, 2, 9 * I + 2, 1, RGB(0, 0, 0)
            DrawTextGL *Txt, 1, 9 * I + 1, 1
        Next J
    Next I
    
    glPopMatrix
End Sub