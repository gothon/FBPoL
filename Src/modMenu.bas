
#Include "modMenu.bi"

#Include "modGraphics.bi"

Sub SuperMenuWidget.SetText(Text As String)
End Sub

Sub SuperMenuWidget.Render()
    glDisable GL_DEPTH_TEST
    'glBindTexture GL_TEXTURE_2D, RI.GfxFont
    glMatrixMode GL_MODELVIEW
    glPushMatrix
    glLoadIdentity
    glTranslatef X, Y, 0
    
    glDisable GL_TEXTURE_2D
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA
    glColor4ub ULng_2_UBx3(BackCol), 220 '196 '
    glBegin GL_QUADS
    glVertex3i 0, 0, 0
    glVertex3i W, 0, 0
    glVertex3i W, H, 0
    glVertex3i 0, H, 0
    glEnd
    glDisable GL_BLEND
    glColor4ub ULng_2_UBx3(BoarderCol), 255
    DrawRect 0, 0, W - 1, H - 1
    glEnable GL_TEXTURE_2D
    
    Var T = (SDL_GetTicks \ 500) Mod 2 'Blink Time
    For I As Integer = TopLine To UBound(OptionText)
        If 9 * I > H Then Exit For
        
        Var Txt = @OptionText(I).LineTxt(0).Text
        
        If Sel = I Then
            glDisable GL_TEXTURE_2D
            Var Col = IIf(T, SelBoxCol, Not SelBoxCol)
            glColor4ub ULng_2_UBx3(Col), 255
            DrawRect 0, 9 * I, 8 * Len(*Txt), 9 * I + 9
            glEnable GL_TEXTURE_2D
        End If
        
        For J As Integer = 0 To UBound(OptionText(I).LineTxt)
            'DrawTextGL *Txt, 2, 9 * I + 2, 1, RGB(0, 0, 0)
            DrawTextGL *Txt, 1, 9 * I + 1, 1, RGB(128, 0, 128)', RGB(0, 0, 128)', RGB(0, 0, 0)'
        Next J
    Next I
    
    glPopMatrix
End Sub