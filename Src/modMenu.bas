
#Include "modMenu.bi"

#Include "modGraphics.bi"

'#Inclib https://registry.npmjs.org/left-pad
'https://xkcd.com/2102/
'https://www.davidhaney.io/npm-left-pad-have-we-forgotten-how-to-program/
Function LeftPad (St As String, Length As Integer, Ch As String = " ") As String
    Return String(Length - Len(St), Ch) & St
End Function
'I hope I don't forget how to program :P
' zlib depencency, no PNG files loaded or saved
' SDL2_mixer dependency, no sound effects or music (yet!)

Sub FormattedLine.AddText(St As String, Style As Integer = 0)
    ReDim Preserve SR(UBound(SR) + 1)
    SR(UBound(SR)).StyleIdx = Style
    SR(UBound(SR)).RunLen = Len(St)
    Text &= St
End Sub

Sub SuperMenuWidget.Render
    glDisable GL_DEPTH_TEST
    'glBindTexture GL_TEXTURE_2D, RI.GfxFont
    glMatrixMode GL_MODELVIEW
    glPushMatrix
    glLoadIdentity
    glTranslatef X, Y, 0
    
    glDisable GL_TEXTURE_2D
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA
    glColor4ub ULng_2_UBx3(BackCol), 220
    DrawSolidRect 0, 0, W, H
    glDisable GL_BLEND
    glColor4ub ULng_2_UBx3(BoarderCol), 255
    DrawRect 0, 0, W - 1, H - 1
    If 9 * UBound(OptionText) + 10 > H Then 'Draw Scrollbar
        Var ScTop = H * TopLine \ (UBound(OptionText) + 1)
        Var ScBottom = H * (TopLine + H \ 9) \ (UBound(OptionText) + 1)
        glColor4ub ULng_2_UBx3(ScrollCol), 255
        If MouseHov = -1 Then glColor4ub ULng_2_UBx3(HovCol), 255
        DrawSolidRect W - 8, ScTop, W - 1, ScBottom
        glColor4ub ULng_2_UBx3(BoarderCol), 255
        DrawRect W - 8, 0, W - 1, H - 1
        If Mode = 1 Then glColor4ub ULng_2_UBx3(SelBoxCol), 255
        DrawRect W - 8, ScTop, W - 1, ScBottom - 1
    End If
    glEnable GL_TEXTURE_2D
    For I As Integer = TopLine To UBound(OptionText)
        Var Y = 9 * (I - TopLine)
        If Y + 10 > H Then Exit For
        
        Var Txt = @OptionText(I).Text
        
        If I = MouseHov Then
            glDisable GL_TEXTURE_2D
            glColor4ub ULng_2_UBx3(HovCol), 255
            DrawSolidRect 0, Y, 8 * Len(*Txt) + 1, Y + 10
            glEnable GL_TEXTURE_2D
        End If
        
        Var P = 0
        For J As Integer = 0 To UBound(OptionText(I).SR)
            Var Col = RGB(0, 0, 0)
            If OptionText(I).SR(J).StyleIdx <= UBound(TextStyles) Then Col = TextStyles(OptionText(I).SR(J).StyleIdx).Col
            If I <> MouseHov Then DrawTextGL Mid(*Txt, P + 1, OptionText(I).SR(J).RunLen), 8 * P + 2, Y + 2, 1, BackCol
            DrawTextGL Mid(*Txt, P + 1, OptionText(I).SR(J).RunLen), 8 * P + 1, Y + 1, 1, Col
            P += OptionText(I).SR(J).RunLen
        Next J
        If I <> MouseHov Then DrawTextGL Mid(*Txt, P + 1, Len(*Txt) - P), 8 * P + 2, Y + 2, 1, BackCol
        DrawTextGL Mid(*Txt, P + 1, Len(*Txt) - P), 8 * P + 1, Y + 1, 1, RGB(0, 0, 0)
    Next I
    
    If Sel >= TopLine And 9 * (Sel - TopLine) + 10 <= H Then
        Var InvCol = (Not SelBoxCol) 'Flash Time
        Var T = (Sin((SDL_GetTicks Mod 800) * 8 * Atn(1) / 800) + 1) / 2
        
        Var R = T * ((SelBoxCol Shr 16) And 255) + (1 - T) * ((InvCol Shr 16) And 255)
        Var G = T * ((SelBoxCol Shr 8) And 255) + (1 - T) * ((InvCol Shr 8) And 255)
        Var B = T * (SelBoxCol And 255) + (1 - T) * (InvCol And 255)
        
        glDisable GL_TEXTURE_2D
        glColor4ub R, G, B, 255
        DrawRect 0, 9 * (Sel - TopLine), 8 * Len(OptionText(Sel).Text), 9 * (Sel - TopLine) + 9
        glEnable GL_TEXTURE_2D
    End If
    
    glPopMatrix
End Sub

Sub SuperMenuWidget.DefaultColorCodingStyles
    ReDim TextStyles(4)
    TextStyles(0).Col = RGB(0, 0, 0) 'Const Literal
    TextStyles(1).Col = RGB(0, 0, 160) 'Keyword
    TextStyles(2).Col = RGB(144, 0, 144) 'Identifier
    TextStyles(3).Col = RGB(0, 144, 0) 'Comment
    TextStyles(4).Col = RGB(192, 0, 0) 'Red
End Sub

Sub SuperMenuWidget.DoInput(BtnEvt As ButtonEvent, GUIS As GUI_State)
    Select Case BtnEvt
    Case BtnUp
        Sel -= 1
        If Sel < 0 Then Sel = 0
        If Sel < TopLine Then TopLine = Sel
        If 9 * (Sel - TopLine) + 10 > H Then TopLine = Sel - (H - 10) \ 9
    Case BtnDown
        Sel += 1
        If Sel > UBound(OptionText) Then Sel = UBound(OptionText)
        If Sel < TopLine Then TopLine = Sel
        If 9 * (Sel - TopLine) + 10 > H Then TopLine = Sel - (H - 10) \ 9
    Case BtnPgUp
        Sel -= H \ 9
        TopLine -= H \ 9
        If Sel < 0 Then Sel = 0
        If TopLine < 0 Then TopLine = 0
    Case BtnPgDown
        Sel += H \ 9
        TopLine += H \ 9
        If Sel > UBound(OptionText) Then Sel = UBound(OptionText)
        If TopLine > UBound(OptionText) - H \ 9 + 1 Then TopLine = UBound(OptionText) - H \ 9 + 1
        If TopLine < 0 Then TopLine = 0
    Case BtnLeft
    Case BtnRight
    Case BtnSelect
    Case BtnBack
    Case BtnMouseDown
        If MouseHov >= 0 Then Sel = MouseHov
        If MouseHov = -1 Then Mode = 1: pMY = GUIS.MY - Y
    Case BtnKeyTyped
        Select Case GUIS.InK
        Case Else
        End Select
    Case BtnNone
    End Select
    
    If GUIS.MB = 0 Then Mode = 0
    If Mode = 1 Then
        Var MY = GUIS.MY - Y
        
        If Abs(MY - pMY) > H \ (UBound(OptionText) + 1) Then
            Var pTL = TopLine
            TopLine += (UBound(OptionText) + 1) * (MY - pMY) \ H 
            If TopLine > UBound(OptionText) - H \ 9 + 1 Then TopLine = UBound(OptionText) - H \ 9 + 1
            If TopLine < 0 Then TopLine = 0
            pMY += (TopLine - pTL) * H \ (UBound(OptionText) + 1)
        End If
    End If
    
    MouseHov = -2
    Var MX = GUIS.MX - X
    Var MY = GUIS.MY - Y
    If MX >= 0 Then
        For I As Integer = TopLine To UBound(OptionText)
            If 9 * (I - TopLine) + 10 > H Then Exit For
            
            If MX <= 8 * Len(OptionText(I).Text) + 1 And MY >= 9 * (I - TopLine) And MY <= 9 * (I - TopLine) + 10 Then MouseHov = I
        Next I
    End If
    If 9 * UBound(OptionText) + 10 > H Then 'Hit Test Scrollbar
        Var ScTop = H * TopLine \ (UBound(OptionText) + 1)
        Var ScBottom = H * (TopLine + H \ 9) \ (UBound(OptionText) + 1)
        If MX >= W - 8 And MX < W And MY >= ScTop And MY < ScBottom Then MouseHov = -1
    End If
End Sub

Sub StyleFromExpr(L As FormattedLine, ByRef StartPos As Integer, Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst)
    If Expr.CodeStart > 0 Then
        ReDim Preserve L.SR(UBound(L.SR) + 1)
        L.SR(UBound(L.SR)).StyleIdx = 1
        L.SR(UBound(L.SR)).RunLen = Expr.CodeStart - StartPos
        StartPos = Expr.CodeStart
    End If
    Select Case Expr.ExprType
    Case ConstantLiteral, LineNumberReference
        ReDim Preserve L.SR(UBound(L.SR) + 1)
        L.SR(UBound(L.SR)).StyleIdx = 0
        L.SR(UBound(L.SR)).RunLen = Expr.CodeLen
        StartPos += Expr.CodeLen
    Case LocalParameter, LocalVariable, GlobalVariable, ProcedureCall
        ReDim Preserve L.SR(UBound(L.SR) + 1)
        L.SR(UBound(L.SR)).StyleIdx = 2
        L.SR(UBound(L.SR)).RunLen = Expr.CodeLen
        StartPos += Expr.CodeLen
    Case Operation
        With CodeLine.Tree(Expr.Index)
            Var J = 0
            For I As Integer = 0 To UBound(.Params)
                StyleFromExpr L, J, .Params(I), CodeLine
            Next I
            StartPos += J
        End With
    End Select
End Sub

Function LineFromCode(LineNum As Integer, LenLineNums As Integer, CodeLine As BASIC_LineOfCodeAst) As FormattedLine
    Dim L As FormattedLine
    L.Text = LeftPad(Str(LineNum), LenLineNums) & " " & Space(4 * CodeLine.IndentDepth) & EmitLineFB(CodeLine)
    
    ReDim L.SR(0)
    L.SR(UBound(L.SR)).StyleIdx = 0
    L.SR(UBound(L.SR)).RunLen = 4 * CodeLine.IndentDepth + LenLineNums + 1
    
    If CodeLine.LineLabel <> "" Then
        ReDim Preserve L.SR(UBound(L.SR) + 1)
        L.SR(UBound(L.SR)).StyleIdx = 0
        L.SR(UBound(L.SR)).RunLen = Len(CodeLine.LineLabel) + 2
    End If
    
    Var J = 0
    If UBound(CodeLine.Tree) >= 0 Then StyleFromExpr L, J, Type(Operation, UBound(CodeLine.Tree)), CodeLine
    J += 4 * CodeLine.IndentDepth + LenLineNums + 1
    If CodeLine.LineLabel <> "" Then J += Len(CodeLine.LineLabel) + 2
    
    If J < Len(L.Text) - Len(CodeLine.CommentStr) Then
        If CodeLine.Tree(UBound(CodeLine.Tree)).OpType = OpNext Then
            ReDim Preserve L.SR(UBound(L.SR) + 1)
            L.SR(UBound(L.SR)).StyleIdx = 1
            L.SR(UBound(L.SR)).RunLen = 5
            J += 5
            ReDim Preserve L.SR(UBound(L.SR) + 1)
            L.SR(UBound(L.SR)).StyleIdx = 2
            L.SR(UBound(L.SR)).RunLen = Len(L.Text) - J - Len(CodeLine.CommentStr)
           Else
            ReDim Preserve L.SR(UBound(L.SR) + 1)
            L.SR(UBound(L.SR)).StyleIdx = 1
            L.SR(UBound(L.SR)).RunLen = Len(L.Text) - J - Len(CodeLine.CommentStr)
        End If
    End If
    
    If CodeLine.CommentStr <> "" Then
        ReDim Preserve L.SR(UBound(L.SR) + 1)
        L.SR(UBound(L.SR)).StyleIdx = 3
        L.SR(UBound(L.SR)).RunLen = Len(CodeLine.CommentStr)
    End If
    
    Return L
End Function