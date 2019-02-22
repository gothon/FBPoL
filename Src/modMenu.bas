
#Include "modMenu.bi"

#Include "modGraphics.bi"

Sub SuperMenuWidget.SetText(Text As String)
End Sub

Sub SuperMenuWidget.Render(X As Integer = 0, Y As Integer = 0, W As Integer = 0, H As Integer = 0, StartLine As Integer = 0)
    glDisable GL_DEPTH_TEST
    'glBindTexture GL_TEXTURE_2D, RI.GfxFont
    glEnable GL_TEXTURE_2D
    glMatrixMode GL_MODELVIEW
    glPushMatrix
    glLoadIdentity
    glTranslatef X, Y, 0
    
    For I As Integer = StartLine To UBound(OptionText)
        If 8 * I > H Then Exit For
        
        For J As Integer = 0 To UBound(OptionText(I).LineTxt)
            DrawTextGL OptionText(I).LineTxt(J).Text, 1, 8 * I + 1, 0, RGB(0, 0, 0)
            DrawTextGL OptionText(I).LineTxt(J).Text, 0, 8 * I, 0
        Next J
    Next I
    
    glPopMatrix
End Sub