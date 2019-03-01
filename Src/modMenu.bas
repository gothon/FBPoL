
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
        If 9 * I + 10 > H Then Exit For
        
        Var Txt = @OptionText(I).Text
        
        Var P = 0
        For J As Integer = 0 To UBound(OptionText(I).SR)
            'DrawTextGL *Txt, 2, 9 * I + 2, 1, RGB(0, 0, 0)
            'DrawTextGL *Txt, 1, 9 * I + 1, 1, RGB(128, 0, 128)', RGB(0, 0, 128)', RGB(0, 0, 0)'
            Var Col = RGB(0, 0, 0)
            If OptionText(I).SR(J).StyleIdx < UBound(TextStyles) Then Col = TextStyles(OptionText(I).SR(J).StyleIdx).Col
            DrawTextGL Mid(*Txt, P + 1, OptionText(I).SR(J).RunLen), 8 * P + 2, 9 * I + 2, 1, BackCol
            DrawTextGL Mid(*Txt, P + 1, OptionText(I).SR(J).RunLen), 8 * P + 1, 9 * I + 1, 1, Col
            'glDisable GL_TEXTURE_2D
            'glColor4ub ULng_2_UBx3(Col), 255
            'DrawRect 8 * P + 1, 9 * I + 1, 8 * P + 1 + 8 * OptionText(I).SR(J).RunLen, 9 * I + 1 + 8
            'glEnable GL_TEXTURE_2D
            P += OptionText(I).SR(J).RunLen
        Next J
        DrawTextGL Mid(*Txt, P + 1, Len(*Txt) - P), 8 * P + 2, 9 * I + 2, 1, BackCol
        DrawTextGL Mid(*Txt, P + 1, Len(*Txt) - P), 8 * P + 1, 9 * I + 1, 1, RGB(0, 0, 0)
        
        If Sel = I Then
            glDisable GL_TEXTURE_2D
            Var Col = IIf(T, SelBoxCol, Not SelBoxCol)
            glColor4ub ULng_2_UBx3(Col), 255
            DrawRect 0, 9 * I, 8 * Len(*Txt), 9 * I + 9
            glEnable GL_TEXTURE_2D
        End If
    Next I
    
    glPopMatrix
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