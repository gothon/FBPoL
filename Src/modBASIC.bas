
#Include "Src/modBASIC.bi"

#Macro ReSize_Preserve(ARRAY, NEW_SIZE) 'Can avoid the 'ReDim ARRAY(-1)' error and handle size 0
    If (NEW_SIZE) > 0 Then
        ReDim Preserve (ARRAY)((NEW_SIZE) - 1)
       Else
        Erase (ARRAY)
    End If 
#EndMacro

Dim Shared CurProg As BASIC_Program Ptr
Dim Shared CurProc As BASIC_Procedure Ptr

'''''''''''''''''''
' Helper Functions
'''''''''''''''''''

'Would a Variadic Parameter work better here?
Sub BuildTopLvlOp(Op As BASIC_Operation, Param1 As BASIC_Expression = Type(EmptyExpr, 0), Param2 As BASIC_Expression = Type(EmptyExpr, 0), Param3 As BASIC_Expression = Type(EmptyExpr, 0), _
                                         Param4 As BASIC_Expression = Type(EmptyExpr, 0), Param5 As BASIC_Expression = Type(EmptyExpr, 0), Param6 As BASIC_Expression = Type(EmptyExpr, 0), _
                                         Param7 As BASIC_Expression = Type(EmptyExpr, 0), Param8 As BASIC_Expression = Type(EmptyExpr, 0), Param9 As BASIC_Expression = Type(EmptyExpr, 0))
    With CurProc->Lines(CurProc->NextLine)
        ReSize_Preserve(.ConstantLiterals, .NextLit)
        ReDim Preserve (.Tree)(.NextNode)
        With .Tree(.NextNode)
            .OpType = Op
            
            If Param1.ExprType = EmptyExpr Then
            ElseIf Param2.ExprType = EmptyExpr Then ReDim .Params(0)
            ElseIf Param3.ExprType = EmptyExpr Then ReDim .Params(1)
            ElseIf Param4.ExprType = EmptyExpr Then ReDim .Params(2)
            ElseIf Param5.ExprType = EmptyExpr Then ReDim .Params(3)
            ElseIf Param6.ExprType = EmptyExpr Then ReDim .Params(4)
            ElseIf Param7.ExprType = EmptyExpr Then ReDim .Params(5)
            ElseIf Param8.ExprType = EmptyExpr Then ReDim .Params(6)
            ElseIf Param9.ExprType = EmptyExpr Then ReDim .Params(7)
            Else ReDim .Params(8)
            End If
            
            If Param1.ExprType <> EmptyExpr Then
                .Params(0).ExprType = Param1.ExprType
                .Params(0).Index = Param1.Index
            End If
            If Param2.ExprType <> EmptyExpr Then
                .Params(1).ExprType = Param2.ExprType
                .Params(1).Index = Param2.Index
            End If
            If Param3.ExprType <> EmptyExpr Then
                .Params(2).ExprType = Param3.ExprType
                .Params(2).Index = Param3.Index
            End If
            If Param4.ExprType <> EmptyExpr Then
                .Params(3).ExprType = Param4.ExprType
                .Params(3).Index = Param4.Index
            End If
            If Param5.ExprType <> EmptyExpr Then
                .Params(4).ExprType = Param5.ExprType
                .Params(4).Index = Param5.Index
            End If
            If Param6.ExprType <> EmptyExpr Then
                .Params(5).ExprType = Param6.ExprType
                .Params(5).Index = Param6.Index
            End If
            If Param7.ExprType <> EmptyExpr Then
                .Params(6).ExprType = Param7.ExprType
                .Params(6).Index = Param7.Index
            End If
            If Param8.ExprType <> EmptyExpr Then
                .Params(7).ExprType = Param8.ExprType
                .Params(7).Index = Param8.Index
            End If
            If Param9.ExprType <> EmptyExpr Then
                .Params(8).ExprType = Param9.ExprType
                .Params(8).Index = Param9.Index
            End If
        End With
        '.CodeStr = EmitTopExprFB(Type(Operation, UBound(.Tree)), CurProc->Lines(CurProc->NextLine)) 'Foward Refrencing Issue with GOTO statement
    End With
    With *CurProc
        .NextLine += 1
        If .NextLine > UBound(.Lines) Then ReDim Preserve .Lines(2 * (UBound(.Lines) + 1) - 1)
        ReDim (.Lines(.NextLine).ConstantLiterals)(0)
        ReDim (.Lines(.NextLine).Tree)(0)
        .Lines(.NextLine).IndentDepth = .FlowStackTop + 1
    End With
End Sub

Function BuildOp(Op As BASIC_Operation, Param1 As BASIC_Expression = Type(EmptyExpr, 0), Param2 As BASIC_Expression = Type(EmptyExpr, 0), Param3 As BASIC_Expression = Type(EmptyExpr, 0), Param4 As BASIC_Expression = Type(EmptyExpr, 0)) As BASIC_Expression
    With CurProc->Lines(CurProc->NextLine)
        With .Tree(.NextNode)
            .OpType = Op
            
            If Param1.ExprType = EmptyExpr Then
            ElseIf Param2.ExprType = EmptyExpr Then ReDim .Params(0)
            ElseIf Param3.ExprType = EmptyExpr Then ReDim .Params(1)
            ElseIf Param4.ExprType = EmptyExpr Then ReDim .Params(2)
            Else ReDim .Params(3)
            End If
            
            If Param1.ExprType <> EmptyExpr Then
                .Params(0).ExprType = Param1.ExprType
                .Params(0).Index = Param1.Index
            End If
            If Param2.ExprType <> EmptyExpr Then
                .Params(1).ExprType = Param2.ExprType
                .Params(1).Index = Param2.Index
            End If
            If Param3.ExprType <> EmptyExpr Then
                .Params(2).ExprType = Param3.ExprType
                .Params(2).Index = Param3.Index
            End If
            If Param4.ExprType <> EmptyExpr Then
                .Params(3).ExprType = Param4.ExprType
                .Params(3).Index = Param4.Index
            End If
        End With
        .NextNode += 1
        If .NextNode > UBound(.Tree) Then ReDim Preserve .Tree(2 * (UBound(.Tree) + 1) - 1)
        
        Return Type(Operation, .NextNode - 1)
    End With
End Function

'''''''''''''''''''''''''''''''''''''''''''
' BASIC_Expression Functions And Overloads
'''''''''''''''''''''''''''''''''''''''''''

Constructor BASIC_Expression() 'Dummy
End Constructor

Constructor BASIC_Expression(ExprType As BASIC_ExpressionType, Index As Integer)
    This.ExprType = ExprType
    This.Index = Index
End Constructor

'Constant Literal Conversions
Constructor BASIC_Expression(Rhs As Integer)
    Let (This.ExprType, This.Index) = CurProc->CLit(Str(Rhs), VtInteger)
End Constructor

Constructor BASIC_Expression(Rhs As Double)
    Let (This.ExprType, This.Index) = CurProc->CLit(Str(Rhs), VtDouble)
End Constructor

Constructor BASIC_Expression(Rhs As String)
    Let (This.ExprType, This.Index) = CurProc->CLit(Rhs, VtString)
End Constructor

'Variables And Arrays
Operator BASIC_Expression.Let(Rhs As BASIC_Expression)
    BuildTopLvlOp OpAssign, This, Rhs
End Operator

Operator BASIC_Expression.[] (Index As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpArrayAccess, This, Index)
End Operator

Property BASIC_Expression.AA(Index As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpArrayAccess, This, Index)
End Property

Property BASIC_Expression.AA(Index As BASIC_Expression, Rhs As BASIC_Expression)
    BuildTopLvlOp OpAssign, BuildOp(OpArrayAccess, This, Index), Rhs
End Property

Sub _ReDim(V As BASIC_Expression, NewUBound As BASIC_Expression)
    BuildTopLvlOp OpReDim, V, NewUBound
End Sub

Sub _ReDim_Preserve(V As BASIC_Expression, NewUBound As BASIC_Expression)
    BuildTopLvlOp OpReDimPreserve, V, NewUBound
End Sub

Function _UBound(V As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpUBound, V)
End Function

Sub _Erase(V As BASIC_Expression)
    BuildTopLvlOp OpErase, V
End Sub

'Flow Control
Function _AndAlso(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpAndAlso, Lhs, Rhs)
End Function

Function _OrElse(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpOrElse, Lhs, Rhs)
End Function

Sub GOTO_(Label As BASIC_Expression)
    If Label.ExprType <> LineNumberReference Then
        ReDim Preserve CurProc->LineNums(UBound(CurProc->LineNums) + 1)
        Label.ExprType = LineNumberReference
        Label.Index = UBound(CurProc->LineNums)
    End If
    BuildTopLvlOp OpGOTO, Label
End Sub

'Procedures
Function BASIC_Expression.FC(Param1 As BASIC_Expression = Type(EmptyExpr, 0), Param2 As BASIC_Expression = Type(EmptyExpr, 0), Param3 As BASIC_Expression = Type(EmptyExpr, 0)) As BASIC_Expression
    Return BuildOp(OpCallFunction, This, Param1, Param2, Param3)
End Function

Sub BASIC_Expression.SC(Param1 As BASIC_Expression = Type(EmptyExpr, 0), Param2 As BASIC_Expression = Type(EmptyExpr, 0))
    BuildTopLvlOp OpCallSub, This, Param1, Param2
End Sub

Sub _End
    BuildTopLvlOp OpEnd
End Sub

'Arithmetic
Operator +(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpAdd, Lhs, Rhs)
End Operator

Operator -(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpSub, Lhs, Rhs)
End Operator

Operator -(Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpNeg, Rhs)
End Operator

Operator *(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpMult, Lhs, Rhs)
End Operator

Operator /(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpDiv, Lhs, Rhs)
End Operator

Operator \(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpIntDiv, Lhs, Rhs)
End Operator

Operator Mod(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpMod, Lhs, Rhs)
End Operator

Operator ^(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpPower, Lhs, Rhs)
End Operator

'Comparison
Operator =(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpEquil, Lhs, Rhs)
End Operator

Operator <>(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpNotEquil, Lhs, Rhs)
End Operator

Operator >(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpGreater, Lhs, Rhs)
End Operator

Operator <(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpLessthan, Lhs, Rhs)
End Operator

Operator >=(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpGreaterEquil, Lhs, Rhs)
End Operator

Operator <=(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpLessthanEquil, Lhs, Rhs)
End Operator

'Logical
Operator And(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpAnd, Lhs, Rhs)
End Operator

Operator Or(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpOr, Lhs, Rhs)
End Operator

Operator XOr(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpXOr, Lhs, Rhs)
End Operator

Operator Not(Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpNot, Rhs)
End Operator

'String
Operator &(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpStrConcat, Lhs, Rhs)
End Operator

Function _Len(St As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpLen, St)
End Function

Function Left(St As BASIC_Expression, N As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpLeft, St, N)
End Function

Function Right(St As BASIC_Expression, N As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpRight, St, N)
End Function

Function _Mid(St As BASIC_Expression, Start As BASIC_Expression, N As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpMid, St, Start, N)
End Function

Function Val(St As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpVal, St)
End Function

Function _Str(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpStr, Number)
End Function

'Math Functions
Operator Abs(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpAbs, Number)
End Operator

Operator Sgn(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpSgn, Number)
End Operator

Operator Int(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpInt, Number)
End Operator

Operator Fix(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpFix, Number)
End Operator

Operator Sin(Angle As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpSin, Angle)
End Operator

Operator Cos(Angle As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpCos, Angle)
End Operator

Operator Tan(Angle As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpTan, Angle)
End Operator

Operator Atn(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpAtn, Number)
End Operator

Operator Exp(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpExp, Number)
End Operator

Operator Log(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpLog, Number)
End Operator

Operator Sqr(Number As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpSqr, Number)
End Operator

'Player Inputs

Function _InKey() As BASIC_Expression
    Return BuildOp(OpInKey)
End Function

Function _MultiKey(ScanCode As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpMultiKey, ScanCode)
End Function

Function _GetJoystick(ID As BASIC_Expression, Buttons As BASIC_Expression, A1 As BASIC_Expression, A2 As BASIC_Expression, A3 As BASIC_Expression, A4 As BASIC_Expression, A5 As BASIC_Expression, A6 As BASIC_Expression, A7 As BASIC_Expression, A8 As BASIC_Expression) As BASIC_Expression
    With CurProc->Lines(CurProc->NextLine) 'Soo many parameters
        With .Tree(.NextNode)
            ReDim .Params(9)
            .Params(0).ExprType = ID.ExprType
            .Params(0).Index = ID.Index
            .Params(1).ExprType = Buttons.ExprType
            .Params(1).Index = Buttons.Index
            .Params(2).ExprType = A1.ExprType
            .Params(2).Index = A1.Index
            .Params(3).ExprType = A2.ExprType
            .Params(3).Index = A2.Index
            .Params(4).ExprType = A3.ExprType
            .Params(4).Index = A3.Index
            .Params(5).ExprType = A4.ExprType
            .Params(5).Index = A4.Index
            .Params(6).ExprType = A5.ExprType
            .Params(6).Index = A5.Index
            .Params(7).ExprType = A6.ExprType
            .Params(7).Index = A6.Index
            .Params(8).ExprType = A7.ExprType
            .Params(8).Index = A7.Index
            .Params(9).ExprType = A8.ExprType
            .Params(9).Index = A8.Index
        End With
    End With
    Return BuildOp(OpGetJoystick)
End Function

Function _GetMouse(X As BASIC_Expression, Y As BASIC_Expression, Wheel As BASIC_Expression = 0, Buttons As BASIC_Expression = 0) As BASIC_Expression
    Return BuildOp(OpGetMouse, X, Y, Wheel, Buttons)
End Function

'Robot I/O

Sub _BotGo(Action As BASIC_Expression, Target As BASIC_Expression = "", X As BASIC_Expression = 0, Y As BASIC_Expression = 0, Z As BASIC_Expression = 0)
    BuildTopLvlOp OpBotGo, Action, Target, X, Y, Z
End Sub

Sub _SendMsg(Msg As BASIC_Expression, Channel As BASIC_Expression = 10, TransmitTime As BASIC_Expression = 16)
    BuildTopLvlOp OpSendMsg, Msg, Channel, TransmitTime
End Sub

Function _ReadMsg(Msg As BASIC_Expression, LowChannel As BASIC_Expression = 10, HighChannel As BASIC_Expression = 10) As BASIC_Expression
    Return BuildOp(OpReadMsg, Msg, LowChannel, HighChannel)
End Function


'Image Buffer Commands

Sub _Cls()
    BuildTopLvlOp OpCls
End Sub

Sub _PSet(Buffer As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, _Color As BASIC_Expression)
    BuildTopLvlOp OpPSet, Buffer, X, Y, _Color
End Sub

Sub _Line(Buffer As BASIC_Expression, X1 As BASIC_Expression, Y1 As BASIC_Expression, X2 As BASIC_Expression, Y2 As BASIC_Expression, _Color As BASIC_Expression, ModeB_BF As Integer = 0)
    BuildTopLvlOp OpLine, Buffer, X1, Y1, X2, Y2, _Color, ModeB_BF
End Sub

Sub _Circle(Buffer As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, R As BASIC_Expression, _Color As BASIC_Expression, Start As BASIC_Expression = 0.0, EndAng As BASIC_Expression = 6.283186, Aspect As BASIC_Expression = 1.0, ModeF As Integer = 0)
    BuildTopLvlOp OpCircle, Buffer, X, Y, R, _Color, Start, EndAng, Aspect, ModeF
End Sub

Sub _Draw_String(Buffer As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, Text As BASIC_Expression, _Color As BASIC_Expression)
    BuildTopLvlOp OpDrawString, Buffer, X, Y, Text, _Color
End Sub

Sub _Get(Src As BASIC_Expression, X1 As BASIC_Expression, Y1 As BASIC_Expression, X2 As BASIC_Expression, Y2 As BASIC_Expression, Dest As BASIC_Expression)
    BuildTopLvlOp OpGet, Src, X1, Y1, X2, Y2, Dest
End Sub

Sub _Put(Target As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, Src As BASIC_Expression, Method As Integer = 0)
    BuildTopLvlOp OpPut, Target, X, Y, Src, Method
End Sub

Sub _ScreenLock()
    BuildTopLvlOp OpScreenLock
End Sub

Sub _ScreenUnlock()
    BuildTopLvlOp OpScreenUnlock
End Sub

'Image Buffer Management

Sub _ScreenRes(W As BASIC_Expression, H As BASIC_Expression)
    BuildTopLvlOp OpScreenRes, W, H
End Sub

Function _ImageCreate(W As BASIC_Expression, H As BASIC_Expression, Col As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpImageCreate, W, H, Col)
End Function

Sub _ImageDestroy(Image As BASIC_Expression)
    BuildTopLvlOp OpImageDestroy, Image
End Sub

'Console

Sub _Locate(Row As BASIC_Expression = 0, Column As BASIC_Expression = 0, State As BASIC_Expression = -1)
    BuildTopLvlOp OpLocate, Row, Column, State
End Sub

Sub _Print(Text As BASIC_Expression, NoNewLine As Integer = 0)
    BuildTopLvlOp OpPrint, Text, NoNewLine
End Sub

'MISC

Function _RGB(R As BASIC_Expression, G As BASIC_Expression, B As BASIC_Expression) As BASIC_Expression
    Return BuildOp(OpRGB, R, G, B)
End Function

Function _Timer() As BASIC_Expression
    Return BuildOp(OpTimer)
End Function

Sub _Randomize(Seed As BASIC_Expression = -1.0)
    BuildTopLvlOp OpRandomize, Seed
End Sub

Function _Rnd(Seed As BASIC_Expression = 1.0) As BASIC_Expression
    Return BuildOp(OpRnd, Seed)
End Function

Sub Sleep__(Ammount As BASIC_Expression = -1)
    BuildTopLvlOp OpSleep, Ammount
End Sub

''''''''''''''''''''''''''''''''''''''''
' BASIC_Procedure Line Level Operations
''''''''''''''''''''''''''''''''''''''''

Function BASIC_Procedure.Dim_(VarName As String, VarType As BASIC_VarType) As BASIC_Expression
    LocalVars(NextVar).VarName = VarName
    LocalVars(NextVar).VarType = VarType
    NextVar += 1
    If NextVar > UBound(LocalVars) Then ReDim Preserve LocalVars(2 * (UBound(LocalVars) + 1) - 1)
    
    BuildTopLvlOp OpDim, BASIC_Expression(LocalVariable, NextVar - 1)
    Return Type(LocalVariable, NextVar - 1)
End Function

Function BASIC_Procedure.CLit(ConstLitVal As String, VarType As BASIC_VarType) As BASIC_Expression
    With Lines(NextLine)
        .ConstantLiterals(.NextLit).VarName = ConstLitVal
        .ConstantLiterals(.NextLit).VarType = VarType Or VtConstantLiteral
        
        .NextLit += 1
        If .NextLit > UBound(.ConstantLiterals) Then ReDim Preserve .ConstantLiterals(2 * (UBound(.ConstantLiterals) + 1) - 1)
        
        Return Type(ConstantLiteral, .NextLit - 1)
    End With
End Function

Function BASIC_Procedure.AddParam(VarName As String, VarType As BASIC_VarType) As BASIC_Expression
    ReDim Preserve Params(UBound(Params) + 1)
    Params(UBound(Params)).VarName = VarName
    Params(UBound(Params)).VarType = VarType
    Return Type(LocalParameter, UBound(Params))
End Function

Sub BASIC_Procedure._Return()
    BuildTopLvlOp OpReturn
End Sub

Sub BASIC_Procedure._Return(ReturnVal As BASIC_Expression)
    BuildTopLvlOp OpReturn, ReturnVal
End Sub

Sub BASIC_Procedure.BlankLine(LineComment As String = "")
    Lines(NextLine).CommentStr = LineComment
    Erase Lines(NextLine).ConstantLiterals
    Erase Lines(NextLine).Tree
    
    NextLine += 1
    If NextLine > UBound(Lines) Then ReDim Preserve Lines(2 * (UBound(Lines) + 1) - 1)
    ReDim (Lines(NextLine).ConstantLiterals)(0)
    ReDim (Lines(NextLine).Tree)(0)
    Lines(NextLine).IndentDepth = FlowStackTop + 1
End Sub

Sub BASIC_Procedure.LLabel_(LineLabel As String, LineNumRef As BASIC_Expression)
    Lines(NextLine).LineLabel = LineLabel
    If LineNumRef.ExprType <> LineNumberReference Then
        ReDim Preserve LineNums(UBound(LineNums) + 1)
        LineNumRef.ExprType = LineNumberReference
        LineNumRef.Index = UBound(LineNums)
    End If
    LineNums(LineNumRef.Index) = NextLine
End Sub

Sub BASIC_Procedure.LComment(LineComment As String)
    If NextLine > 0 Then Lines(NextLine - 1).CommentStr = LineComment
End Sub

Sub BASIC_Procedure._If(Cond As BASIC_Expression)
    Let (TmpCond.ExprType, TmpCond.Index) = Cond
End Sub

Sub BASIC_Procedure._Then()
    NextLine -= 1
    Lines(NextLine).NextNode +=  1
    ReDim Preserve (Lines(NextLine).Tree)(Lines(NextLine).NextNode)
    BuildTopLvlOp OpIfThen, TmpCond, Type(Operation, Lines(NextLine).NextNode - 1)
End Sub

Sub BASIC_Procedure.If_(Cond As BASIC_Expression)
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStackTop += 1
    If FlowStackTop > UBound(FlowStack) Then ReDim Preserve FlowStack((UBound(FlowStack) + 1) * 2)
    FlowStack(FlowStackTop) = UBound(LineNums)
    
    BuildTopLvlOp OpIf, Type(LineNumberReference, UBound(LineNums)), Cond
End Sub

Sub BASIC_Procedure.ElseIf_(Cond As BASIC_Expression)
    LineNums(FlowStack(FlowStackTop)) = NextLine
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStack(FlowStackTop) = UBound(LineNums)
    
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpElseIf, Type(LineNumberReference, UBound(LineNums)), Cond
End Sub

Sub BASIC_Procedure.Else_
    LineNums(FlowStack(FlowStackTop)) = NextLine
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStack(FlowStackTop) = UBound(LineNums)
    
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpElse, Type(LineNumberReference, UBound(LineNums))
End Sub

Sub BASIC_Procedure.End_If
    LineNums(FlowStack(FlowStackTop)) = NextLine
    FlowStackTop -= 1
    
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpEndIf
End Sub

Sub BASIC_Procedure.Select_Case(Expr As BASIC_Expression)
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStackTop += 1
    If FlowStackTop > UBound(FlowStack) Then ReDim Preserve FlowStack((UBound(FlowStack) + 1) * 2)
    FlowStack(FlowStackTop) = UBound(LineNums)
    
    BuildTopLvlOp OpSelectCase, Type(LineNumberReference, UBound(LineNums)), Expr
End Sub

Sub BASIC_Procedure._Case(Expr As BASIC_Expression)
    LineNums(FlowStack(FlowStackTop)) = NextLine
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStack(FlowStackTop) = UBound(LineNums)
    
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpCase, Type(LineNumberReference, UBound(LineNums)), Expr
End Sub

Sub BASIC_Procedure.Case_Else
    LineNums(FlowStack(FlowStackTop)) = NextLine
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStack(FlowStackTop) = UBound(LineNums)
    
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpCaseElse, Type(LineNumberReference, UBound(LineNums))
End Sub

Sub BASIC_Procedure.End_Select
    LineNums(FlowStack(FlowStackTop)) = NextLine
    FlowStackTop -= 1
    
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpEndSelect
End Sub

Sub BASIC_Procedure._Do
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStackTop += 1
    If FlowStackTop > UBound(FlowStack) Then ReDim Preserve FlowStack((UBound(FlowStack) + 1) * 2)
    FlowStack(FlowStackTop) = UBound(LineNums)
    LineNums(UBound(LineNums)) = NextLine
    
    BuildTopLvlOp OpDo
End Sub

Sub BASIC_Procedure.Do_While(Cond As BASIC_Expression)
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStackTop += 1
    If FlowStackTop > UBound(FlowStack) Then ReDim Preserve FlowStack((UBound(FlowStack) + 1) * 2)
    FlowStack(FlowStackTop) = UBound(LineNums)
    LineNums(UBound(LineNums)) = NextLine
    
    BuildTopLvlOp OpDoWhile, Cond
End Sub

Sub BASIC_Procedure.Do_Until(Cond As BASIC_Expression)
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStackTop += 1
    If FlowStackTop > UBound(FlowStack) Then ReDim Preserve FlowStack((UBound(FlowStack) + 1) * 2)
    FlowStack(FlowStackTop) = UBound(LineNums)
    LineNums(UBound(LineNums)) = NextLine
    
    BuildTopLvlOp OpDoUntil, Cond
End Sub

Sub BASIC_Procedure._Loop
    FlowStackTop -= 1
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpLoop, Type(LineNumberReference, FlowStack(FlowStackTop + 1))
End Sub

Sub BASIC_Procedure.Loop_While(Cond As BASIC_Expression)
    FlowStackTop -= 1
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpLoopWhile, Type(LineNumberReference, FlowStack(FlowStackTop + 1)), Cond
End Sub

Sub BASIC_Procedure.Loop_Until(Cond As BASIC_Expression)
    FlowStackTop -= 1
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpLoopUntil, Type(LineNumberReference, FlowStack(FlowStackTop + 1)), Cond
End Sub

Sub BASIC_Procedure._For(LoopVar As BASIC_Expression, From As BASIC_Expression, _To As BASIC_Expression, _Step As BASIC_Expression = Type(EmptyExpr, 0))
    ReDim Preserve LineNums(UBound(LineNums) + 1)
    FlowStackTop += 1
    If FlowStackTop > UBound(FlowStack) Then ReDim Preserve FlowStack((UBound(FlowStack) + 1) * 2)
    FlowStack(FlowStackTop) = UBound(LineNums)
    LineNums(UBound(LineNums)) = NextLine
    
    If _Step.ExprType = EmptyExpr Then
        BuildTopLvlOp OpFor, LoopVar, From, _To
       Else
        BuildTopLvlOp OpForStep, LoopVar, From, _To, _Step
    End If
End Sub

Sub BASIC_Procedure._Next
    FlowStackTop -= 1
    CurProc->Lines(CurProc->NextLine).IndentDepth -= 1
    BuildTopLvlOp OpNext, Type(LineNumberReference, FlowStack(FlowStackTop + 1))
End Sub

''''''''''''''''''''''''''
' BASIC_Program Functions
''''''''''''''''''''''''''

Function BASIC_Program.Dim_Shared_(VarName As String, VarType As BASIC_VarType) As BASIC_Expression
    ReDim Preserve GlobalVars(UBound(GlobalVars) + 1)
    GlobalVars(UBound(GlobalVars)).VarName = VarName
    GlobalVars(UBound(GlobalVars)).VarType = VarType
    Return Type(GlobalVariable, UBound(GlobalVars))
End Function

Function BASIC_Program.Procedure(FunName As String, RetType As BASIC_VarType) As BASIC_Expression
    ReDim Preserve Proc(UBound(Proc) + 1)
    
    Proc(UBound(Proc)).ProcName = FunName
    Proc(UBound(Proc)).ReturnType = RetType
    ReDim (Proc(UBound(Proc)).LocalVars)(0)
    ReDim (Proc(UBound(Proc)).Lines)(0)
    ReDim (Proc(UBound(Proc)).Lines(0).ConstantLiterals)(0)
    ReDim (Proc(UBound(Proc)).Lines(0).Tree)(0)
    CurProc = @Proc(UBound(Proc))
    Return Type(ProcedureCall, UBound(Proc))
End Function

Sub BASIC_Program.End_Procedure
    With *CurProc
        ReSize_Preserve(.Lines, .NextLine)
        ReSize_Preserve(.LocalVars, .NextVar)
    End With
End Sub

''''''''''''''''''
' Stub Functions '
''''''''''''''''''
Function __Abs CDecl (Number As Double) As Double
    Return Abs(Number)
End Function

Function __Sgn CDecl (Number As Double) As Double
    Return Sgn(Number)
End Function

Function __Int CDecl (Number As Double) As Double
    Return Int(Number)
End Function

Function __Fix CDecl (Number As Double) As Double
    Return Fix(Number)
End Function

Function __Sin CDecl (Angle As Double) As Double
    Return Sin(Angle)
End Function

Function __Cos CDecl (Angle As Double) As Double
    Return Cos(Angle)
End Function

Function __Tan CDecl (Angle As Double) As Double
    Return Tan(Angle)
End Function

Function __Atn CDecl (Number As Double) As Double
    Return Atn(Number)
End Function

Function __Exp CDecl (Number As Double) As Double
    Return Exp(Number)
End Function

Function __Log CDecl (Number As Double) As Double
    Return Log(Number)
End Function

Function __Sqr CDecl (Number As Double) As Double
    Return Sqr(Number)
End Function

''''''''''''''''''
' Code Generators
''''''''''''''''''

Function CodeIndent(CodeLine As BASIC_LineOfCodeAst) As String
    Return Space(4 * CodeLine.IndentDepth + 4)
End Function

Function ExprVarType(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As BASIC_VarType
    Select Case As Const Expr.ExprType
    Case EmptyExpr: Return VtVOID
    Case ConstantLiteral: Return CodeLine.ConstantLiterals(Expr.Index).VarType
    Case LocalParameter: Return CurProc->Params(Expr.Index).VarType
    Case LocalVariable: Return CurProc->LocalVars(Expr.Index).VarType
    Case GlobalVariable: Return CurProg->GlobalVars(Expr.Index).VarType
    Case ProcedureCall: Return CurProg->Proc(Expr.Index).ReturnType
    Case LineNumberReference: Return VtVOID
    Case Operation
        With CodeLine.Tree(Expr.Index)
        Select Case As Const .OpType
        Case OpArrayAccess: Return ExprVarType(.Params(0), CodeLine)
        Case OpCallFunction: Return CurProg->Proc(.Params(0).Index).ReturnType
        Case OpUBound, OpAndAlso, OpOrElse, OpIntDiv, OpMod, OpLen, _
             OpEquil, OpNotEquil, OpGreater, OpLessthan, OpGreaterEquil, OpLessthanEquil, _
             OpAnd, OpOr, OpXOr, OpNot, _
             OpMultiKey, OpGetJoystick, OpGetMouse, OpRGB
            Return VtInteger
        Case OpAdd, OpSub, OpNeg, OpMult, OpDiv, OpPower, OpVal, _
             OpAbs, OpSgn, OpInt, OpFix, OpSin, OpCos, OpTan, OpAtn, OpExp, OpLog, OpSqr, _
             OpTimer, OpRnd
            Return VtDouble
        Case OpStrConcat, OpLeft, OpRight, OpMid, OpStr, _
             OpInKey
            Return VtString
        Case OpImageCreate
            Return VtImageBuffer
        Case Else: Return VtVOID
        End Select
        End With
    End Select
End Function

Function IsConstNull(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As Integer
    If Expr.ExprType <> ConstantLiteral Then Return 0
    If (CodeLine.ConstantLiterals(Expr.Index).VarType And &HFF) <> VtInteger And _
       (CodeLine.ConstantLiterals(Expr.Index).VarType And &HFF) <> VtImageBuffer Then Return 0
    If Val(CodeLine.ConstantLiterals(Expr.Index).VarName) <> 0 Then Return 0
    Return -1
End Function

Function IsConstNum(V As Double, Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As Integer
    If Expr.ExprType <> ConstantLiteral Then Return 0
    If (CodeLine.ConstantLiterals(Expr.Index).VarType And &HFF) <> VtInteger And _
       (CodeLine.ConstantLiterals(Expr.Index).VarType And &HFF) <> VtDouble Then Return 0
    If Val(CodeLine.ConstantLiterals(Expr.Index).VarName) <> V Then Return 0
    Return -1
End Function

''''''''''''''''''''''
' FB Display Backend '
''''''''''''''''''''''

Function TypeNameFB(VType As BASIC_VarType) As String
    Select Case VType And &HFF
    Case VtVOID: Return ""
    Case VtImageBuffer: Return "fb.Image Ptr"
    Case VtDouble: Return "Double"
    Case VtInteger: Return "Integer"
    Case VtString: Return "String"
    End Select
End Function

Function VarReferenceFB(V As BASIC_Variable) As String
    If V.VarType And VtArray Then Return V.VarName & "()"
    Return V.VarName
End Function

Function StrEscapeFB(S As String) As String
    Dim As Integer I = 1, J
    Dim As String R = ""
    Do
        J = InStr(I, S, """")
        If J = 0 Then J = Len(S) + 1
        R &= Mid(S, I, J - I)
        If J > Len(S) Then Exit Do
        R &= """""" 'And six become two become one 
        I = J + 1
    Loop
    Return """" & R & """"
End Function

Function VarNameFromExpr(Expr As BASIC_Expression) As String
    Select Case Expr.ExprType
    Case LocalParameter: Return CurProc->Params(Expr.Index).VarName
    Case LocalVariable: Return CurProc->LocalVars(Expr.Index).VarName
    Case GlobalVariable: Return CurProg->GlobalVars(Expr.Index).VarName
    Case Else: Return "/'Error Expression Not A Variable'/"
    End Select
End Function

Function GenExprFB(Lhs As String = "", Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    Var Code = EmitExprFB(Expr, CodeLine)
    Expr.CodeStart = Len(Lhs)
    Expr.CodeLen = Len(Code)
    Return Lhs & Code
End Function

Function GenTopExprFB(Lhs As String, Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    Var Code = EmitTopExprFB(Expr, CodeLine)
    Expr.CodeStart = Len(Lhs)
    Expr.CodeLen = Len(Code)
    Return Lhs & Code
End Function

Function EmitExprFB(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    Select Case Expr.ExprType
    Case ConstantLiteral
        Var VarTmp = CodeLine.ConstantLiterals(Expr.Index)
        If VarTmp.VarType = (VtString Or VtConstantLiteral) Then
            Return StrEscapeFB(VarTmp.VarName)
           Else
            Return VarTmp.VarName
        End If
    Case LocalParameter: Return VarReferenceFB(CurProc->Params(Expr.Index))
    Case LocalVariable: Return VarReferenceFB(CurProc->LocalVars(Expr.Index))
    Case GlobalVariable: Return VarReferenceFB(CurProg->GlobalVars(Expr.Index))
    Case ProcedureCall: Return CurProg->Proc(Expr.Index).ProcName
    Case Operation
        With CodeLine.Tree(Expr.Index)
        Select Case As Const .OpType
        'Variables and Arrays
        Case OpArrayAccess: Return GenExprFB(VarNameFromExpr(.Params(0)) & "(", .Params(1), CodeLine) & ")"
        Case OpUBound: Return "UBound(" & VarNameFromExpr(.Params(0)) & ")"
        'Flow Control
        Case OpAndAlso: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " AndAlso ", .Params(1), CodeLine) & ")"
        Case OpOrElse: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " OrElse ", .Params(1), CodeLine) & ")"
        'Procedures
        Case OpCallFunction
            Var Code = GenExprFB(, .Params(0), CodeLine) & "("
            For I As Integer = 1 To UBound(.Params) - 1
                Code = GenExprFB(Code, .Params(I), CodeLine) & ", "
            Next I
            If UBound(.Params) >= 1 Then Code = GenExprFB(Code, .Params(UBound(.Params)), CodeLine)
            Return Code & ")"
        'Arithmetic
        Case OpAdd: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " + ", .Params(1), CodeLine) & ")"
        Case OpSub: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " - ", .Params(1), CodeLine) & ")"
        Case OpNeg: Return GenExprFB("-", .Params(0), CodeLine)
        Case OpMult: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " * ", .Params(1), CodeLine) & ")"
        Case OpDiv: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " / ", .Params(1), CodeLine) & ")"
        Case OpIntDiv: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " \ ", .Params(1), CodeLine) & ")"
        Case OpMod: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " Mod ", .Params(1), CodeLine) & ")"
        Case OpPower: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " ^ ", .Params(1), CodeLine) & ")"
        'String
        Case OpStrConcat: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " & ", .Params(1), CodeLine) & ")"
        Case OpLen: Return GenExprFB("Len(", .Params(0), CodeLine) & ")"
        Case OpLeft: Return GenExprFB(GenExprFB("Left(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ")"
        Case OpRight: Return GenExprFB(GenExprFB("Right(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ")"
        Case OpMid: Return GenExprFB(GenExprFB(GenExprFB("Mid(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ")"
        Case OpVal: Return GenExprFB("Val(", .Params(0), CodeLine) & ")"
        Case OpStr: Return GenExprFB("Str(", .Params(0), CodeLine) & ")"
        'Comparison
        Case OpEquil: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " = ", .Params(1), CodeLine) & ")"
        Case OpNotEquil: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " <> ", .Params(1), CodeLine) & ")"
        Case OpGreater: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " > ", .Params(1), CodeLine) & ")"
        Case OpLessthan: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " < ", .Params(1), CodeLine) & ")"
        Case OpGreaterEquil: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " >= ", .Params(1), CodeLine) & ")"
        Case OpLessthanEquil: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " <= ", .Params(1), CodeLine) & ")"
        'Logical
        Case OpAnd: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " And ", .Params(1), CodeLine) & ")"
        Case OpOr: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " Or ", .Params(1), CodeLine) & ")"
        Case OpXOr: Return GenExprFB(GenExprFB("(", .Params(0), CodeLine) & " XOr ", .Params(1), CodeLine) & ")"
        Case OpNot: Return GenExprFB("Not ", .Params(0), CodeLine)
        'Math Functions
        Case OpAbs: Return GenExprFB("Abs(", .Params(0), CodeLine) & ")"
        Case OpSgn: Return GenExprFB("Sgn(", .Params(0), CodeLine) & ")"
        Case OpInt: Return GenExprFB("Int(", .Params(0), CodeLine) & ")"
        Case OpFix: Return GenExprFB("Fix(", .Params(0), CodeLine) & ")"
        Case OpSin: Return GenExprFB("Sin(", .Params(0), CodeLine) & ")"
        Case OpCos: Return GenExprFB("Cos(", .Params(0), CodeLine) & ")"
        Case OpTan: Return GenExprFB("Tan(", .Params(0), CodeLine) & ")"
        Case OpAtn: Return GenExprFB("Atn(", .Params(0), CodeLine) & ")"
        Case OpExp: Return GenExprFB("Exp(", .Params(0), CodeLine) & ")"
        Case OpLog: Return GenExprFB("Log(", .Params(0), CodeLine) & ")"
        Case OpSqr: Return GenExprFB("Sqr(", .Params(0), CodeLine) & ")"
        'Player Inputs
        Case OpInKey: Return "Inkey"
        Case OpMultiKey: Return GenExprFB("MultiKey(", .Params(0), CodeLine) & ")"
        Case OpGetJoystick
            Return GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB( _
                   "GetJoystick(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", " _
                                 , .Params(2), CodeLine) & ", ", .Params(3), CodeLine) & ", ", .Params(4), CodeLine) & ", ", .Params(5), CodeLine) & ", " _
                                 , .Params(6), CodeLine) & ", ", .Params(7), CodeLine) & ", ", .Params(8), CodeLine) & ", ", .Params(9), CodeLine) & ")"
        Case OpGetMouse: Return GenExprFB(GenExprFB(GenExprFB(GenExprFB("GetMouse(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ", ", .Params(3), CodeLine) & ")"
        'Robot I/O
        Case OpReadMsg: Return GenExprFB(GenExprFB(GenExprFB("ReadMsg(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ")"
        'Image Buffer Management
        Case OpImageCreate: Return GenExprFB(GenExprFB(GenExprFB("ImageCreate(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ")"
        'MISC
        Case OpRGB: Return GenExprFB(GenExprFB(GenExprFB("RGB(", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ")"
        Case OpTimer: Return "Timer"
        Case OpRnd: Return GenExprFB("Rnd(", .Params(0), CodeLine) & ")"
        End Select
        End With
    End Select
    
    Return ""
End Function

Function EmitTopExprFB(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    With CodeLine.Tree(Expr.Index)
    Select Case .OpType
    'Variables and Arrays
    Case OpDim
        Var LVar = CurProc->LocalVars(.Params(0).Index)
        Var Code = "Dim " & LVar.VarName
        If (LVar.VarType And VtArray) <> 0 Then Code &= "()"
        Return Code & " As " & TypeNameFB(LVar.VarType)
    Case OpAssign: Return GenExprFB(GenExprFB(, .Params(0), CodeLine) & " = ", .Params(1), CodeLine)
    Case OpReDim: Return GenExprFB("ReDim " & VarNameFromExpr(.Params(0)) & "(", .Params(1), CodeLine) & ")"
    Case OpReDimPreserve: Return GenExprFB("ReDim Preserve " & VarNameFromExpr(.Params(0)) & "(", .Params(1), CodeLine) & ")"
    Case OpErase: Return GenExprFB("Erase ", .Params(0), CodeLine)
    'Flow Control
    Case OpIfThen: Return GenTopExprFB(GenExprFB("If ", .Params(0), CodeLine) & " Then ", .Params(1), CodeLine)
    Case OpIf: Return GenExprFB("If ", .Params(1), CodeLine) & " Then"
    Case OpElseIf: Return GenExprFB("ElseIf ", .Params(1), CodeLine) & " Then"
    Case OpElse: Return "Else"
    Case OpEndIf: Return "End If"
    Case OpSelectCase:  Return GenExprFB("Select Case ", .Params(1), CodeLine)
    Case OpCase:  Return GenExprFB("Case ", .Params(1), CodeLine)
    Case OpCaseElse: Return "Case Else"
    Case OpEndSelect:  Return "End Select"
    Case OpDo: Return "Do"
    Case OpDoWhile: Return GenExprFB("Do While ", .Params(0), CodeLine)
    Case OpDoUntil: Return GenExprFB("Do Until ", .Params(0), CodeLine)
    Case OpLoop: Return "Loop"
    Case OpLoopWhile: Return GenExprFB("Loop While ", .Params(1), CodeLine)
    Case OpLoopUntil: Return GenExprFB("Loop Until ", .Params(1), CodeLine)
    Case OpFor: Return GenExprFB(GenExprFB(GenExprFB("For ", .Params(0), CodeLine) & " = ", .Params(1), CodeLine) & " To ", .Params(2), CodeLine)
    Case OpForStep: Return GenExprFB(GenExprFB(GenExprFB(GenExprFB("For ", .Params(0), CodeLine) & " = ", .Params(1), CodeLine) & " To ", .Params(2), CodeLine) & " Step ", .Params(3), CodeLine)
    Case OpNext
        Var Tmp = @CurProc->Lines(CurProc->LineNums(.Params(0).Index))
        Return "Next " & EmitExprFB(Tmp->Tree(UBound(Tmp->Tree)).Params(0), *Tmp)
    Case OpGOTO: Return "GOTO " & CurProc->Lines(CurProc->LineNums(.Params(0).Index)).LineLabel
    'Procedures
    Case OpCallSub
        Var Code = GenExprFB(, .Params(0), CodeLine) & " "
        For I As Integer = 1 To UBound(.Params) - 1
            Code = GenExprFB(Code, .Params(I), CodeLine) & ", "
        Next I
        If UBound(.Params) >= 1 Then Code = GenExprFB(Code, .Params(UBound(.Params)), CodeLine)
        Return Code
    Case OpReturn
        If UBound(.Params) = 0 Then
            Return GenExprFB("Return ", .Params(0), CodeLine)
           Else
            Return "Return"
        End If
    Case OpEnd: Return "End"
    'Robot I/O
    Case OpBotGo: Return GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB("BotGo ", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ", ", .Params(3), CodeLine) & ", ", .Params(4), CodeLine)
    Case OpSendMsg: Return GenExprFB(GenExprFB(GenExprFB("SendMsg ", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine)
    'Image Buffer Commands
    Case OpCls: Return "Cls"
    Case OpPSet
        Var Code = "PSet "
        If Not IsConstNull(.Params(0), CodeLine) Then Code = GenExprFB(Code, .Params(0), CodeLine) & ", "
        Code = GenExprFB(GenExprFB(GenExprFB(Code & "(", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & "), ", .Params(3), CodeLine)
        Return Code
    Case OpLine
        Var Code = "Line "
        If Not IsConstNull(.Params(0), CodeLine) Then Code = GenExprFB(Code, .Params(0), CodeLine) & ", "
        Code = GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(Code & "(", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ")-(", .Params(3), CodeLine) & ", ", .Params(4), CodeLine) & "), ", .Params(5), CodeLine)
        Select Case Val(CodeLine.ConstantLiterals(.Params(6).Index).VarName)
        Case 0
        Case 1: Code &= ", B"
        Case 2: Code &= ", BF"
        End Select
        Return Code
    Case OpCircle
        Var Code = "Circle "
        If Not IsConstNull(.Params(0), CodeLine) Then Code = GenExprFB(Code, .Params(0), CodeLine) & ", "
        Code = GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(Code & "(", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & "), ", .Params(3), CodeLine) & ", ", .Params(4), CodeLine) & ", ", .Params(5), CodeLine) & ", ", .Params(6), CodeLine) & ", ", .Params(7), CodeLine)
        If IsConstNum(1, .Params(8), CodeLine) Then Code &= ", F"
        Return Code
    Case OpDrawString
        Var Code = "Draw String "
        If Not IsConstNull(.Params(0), CodeLine) Then Code = GenExprFB(Code, .Params(0), CodeLine) & ", "
        Code = GenExprFB(GenExprFB(GenExprFB(GenExprFB(Code & "(", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & "), ", .Params(3), CodeLine) & ", ", .Params(4), CodeLine)
        Return Code
    Case OpGet
        Var Code = "Get "
        If Not IsConstNull(.Params(0), CodeLine) Then Code = GenExprFB(Code, .Params(0), CodeLine) & ", "
        Code = GenExprFB(GenExprFB(GenExprFB(GenExprFB(GenExprFB(Code & "(", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & ")-(", .Params(3), CodeLine) & ", ", .Params(4), CodeLine) & "), ", .Params(5), CodeLine)
        Return Code
    Case OpPut
        Var Code = "Put "
        If Not IsConstNull(.Params(0), CodeLine) Then Code = GenExprFB(Code, .Params(0), CodeLine) & ", "
        Code = GenExprFB(GenExprFB(GenExprFB(Code & "(", .Params(1), CodeLine) & ", ", .Params(2), CodeLine) & "), ", .Params(3), CodeLine)
        Select Case Val(CodeLine.ConstantLiterals(.Params(4).Index).VarName)
        Case 0 'XOr
        Case 1: Code &= ", PSet"
        Case 2: Code &= ", Preset"
        Case 3: Code &= ", Trans"
        Case 4: Code &= ", And"
        Case 5: Code &= ", Or"
        Case 6: Code &= ", XOr"
        Case 7: Code &= ", Alpha"
        End Select
        Return Code
    Case OpScreenLock: Return "ScreenLock"
    Case OpScreenUnlock: Return "ScreenUnlock"
    'Image Buffer Management
    Case OpScreenRes: Return GenExprFB(GenExprFB("ScreenRes ", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", 32"
    Case OpImageDestroy: Return GenExprFB("ImageDestroy ", .Params(0), CodeLine)
    'Console
    Case OpLocate
        If IsConstNum(-1, .Params(2), CodeLine) Then
            Return GenExprFB(GenExprFB("Locate ", .Params(0), CodeLine) & ", ", .Params(1), CodeLine)
           Else
            Return GenExprFB(GenExprFB(GenExprFB("Locate ", .Params(0), CodeLine) & ", ", .Params(1), CodeLine) & ", ", .Params(2), CodeLine)
        End If
    Case OpPrint: Return GenExprFB("Print ", .Params(0), CodeLine) & IIf(IsConstNum(0, .Params(1), CodeLine), "", ";")
    'MISC
    Case OpRandomize: Return GenExprFB("Randomize ", .Params(0), CodeLine)
    Case OpSleep: Return GenExprFB("Sleep ", .Params(0), CodeLine)
    End Select
    End With
    
    Return ""
End Function

Function EmitLineFB(CodeLine As BASIC_LineOfCodeAst) As String
    Dim Code As String
    If CodeLine.LineLabel <> "" Then Code = CodeLine.LineLabel & ": "
    
    If UBound(CodeLine.Tree) < 0 Then
        CodeLine.CodeStr = ""
       Else
        CodeLine.CodeStr = EmitTopExprFB(Type(Operation, UBound(CodeLine.Tree)), CodeLine)
    End If
    
    Return Code & CodeLine.CodeStr & CodeLine.CommentStr
End Function

Function EmitProgFB(Prog As BASIC_Program) As String
    Dim Code As String = ""
    CurProg = @Prog
    For I As Integer = 0 To UBound(Prog.GlobalVars)
        Var LVar = Prog.GlobalVars(I)
        Code &= "Dim Shared " & LVar.VarName
        If (LVar.VarType And VtArray) <> 0 Then Code &= "()"
        Code &= " As " & TypeNameFB(LVar.VarType) & !"\n"
    Next I
    If UBound(Prog.GlobalVars) >= 0 Then Code &= !"\n"
    For I As Integer = 0 To UBound(Prog.Proc)
        CurProc = @Prog.Proc(I)
        Code &= IIf(Prog.Proc(I).ReturnType = VtVOID, "Sub ", "Function ")
        Code &= Prog.Proc(I).ProcName & "("
        For J As Integer = 0 To UBound(Prog.Proc(I).Params)
            Code &= Prog.Proc(I).Params(J).VarName
            If (Prog.Proc(I).Params(J).VarType And VtArray) <> 0 Then Code &= "()"
            Code &= " As " & TypeNameFB(Prog.Proc(I).Params(J).VarType)
            If J < UBound(Prog.Proc(I).Params) Then Code &= ", "
        Next J
        Code &= ")"
        If Prog.Proc(I).ReturnType <> VtVOID Then Code &= " As " & TypeNameFB(Prog.Proc(I).ReturnType)
        Code &= !"\n"
        For J As Integer = 0 To UBound(Prog.Proc(I).Lines)
            Code &= CodeIndent(Prog.Proc(I).Lines(J)) & EmitLineFB(Prog.Proc(I).Lines(J)) & !"\n"
        Next J
        Code &= IIf(Prog.Proc(I).ReturnType = VtVOID, "End Sub", "End Function") & !"\n"
        If I < UBound(Prog.Proc) Then Code &= !"\n"
    Next I
    Return Code
End Function

'''''''''''''
' C Backend '
'''''''''''''

#IfDef __FB_64BIT__
    #Define TYPEDEF_FBINTEGER !"typedef signed long long FBInteger;\n"
#Else
    #Define TYPEDEF_FBINTEGER !"typedef signed long FBInteger;\n"
#EndIf
Function EmitCHeader As String
    Return TYPEDEF_FBINTEGER _
           !"typedef struct { char *data; FBInteger len; FBInteger size; } FBString;\n" _
           !"\n" _
           !"struct __FB_ArrayDIMTB {\n" _
           !"    FBInteger ELEMENTS;\n" _
           !"    FBInteger LBOUND;\n" _
           !"    FBInteger UBOUND;\n" _
           !"};\n" _
           !"\n" _
           !"typedef struct {\n" _
           !"    void* DATA;\n" _
           !"    void* PTR;\n" _
           !"    FBInteger SIZE;\n" _
           !"    FBInteger ELEMENT_LEN;\n" _
           !"    FBInteger DIMENSIONS;\n" _
           !"    struct __FB_ArrayDIMTB DIMTB[8];\n" _
           !"} FBArray;\n" _
           !"\n" _
           !"FBInteger IntDiv(void* io, FBInteger lhs, FBInteger rhs, char* location);\n" _
           !"double Pow(double lhs, double rhs);\n" _
           !"\n" _
           !"double Abs(double number);\n" _
           !"double Sgn(double number);\n" _
           !"double Int(double number);\n" _
           !"double Fix(double number);\n" _
           !"double Sin(double angle);\n" _
           !"double Cos(double angle);\n" _
           !"double Tan(double angle);\n" _
           !"double Atn(double number);\n" _
           !"double Exp(double number);\n" _
           !"double Log(double number);\n" _
           !"double Sqr(double number);\n" _
           !"\n" _
           !"FBString* InKey(void* io);\n" _
           !"FBInteger MultiKey(void* io, FBInteger scancode);\n" _
           !"FBInteger GetJoystick(void* io, FBInteger id, FBInteger* buttons, double* a1, double* a2, double* a3, double* a4, double* a5, double* a6, double* a7, double* a8);\n" _
           !"FBInteger GetMouse(void* io, FBInteger* x, FBInteger* y, FBInteger* wheel, FBInteger* buttons);\n" _
           !"\n" _
           !"void StrDelete(FBString*);\n" _
           !"FBString* StrInit(char* s);\n" _
           !"void StrAssign(FBString* Lhs, FBString* Rhs);\n" _
           !"int StrCompare(FBString* Lhs, FBString* Rhs);\n" _
           !"FBString* StrConcat(FBString* Lhs, FBString* Rhs);\n" _
           !"int Len(FBString* S);\n" _
           !"FBString* Left(FBString* S, FBInteger N);\n" _
           !"FBString* Right(FBString* S, FBInteger N);\n" _
           !"FBString* Mid(FBString* S, FBInteger Start, FBInteger N);\n" _
           !"double Val(FBString* S);\n" _
           !"FBString* Str(double Number);\n" _
           !"\n" _
           !"void ReDim(void* io, FBArray* Array, FBInteger ElementSize, FBInteger NewUBound, char* location);\n" _
           !"void ReDim_Preserve(void* io, FBArray* Array, FBInteger ElementSize, FBInteger NewUBound, char* location);\n" _
           !"FBInteger UBound(FBArray* Array);\n" _
           !"void Erase(FBArray* Array, FBInteger isvarlen);\n" _
           !"FBInteger* IntegerAA(void* io, FBArray* Array, FBInteger Index, char* location);\n" _
           !"double* DoubleAA(void* io, FBArray* Array, FBInteger Index, char* location);\n" _
           !"FBString* StringAA(void* io, FBArray* Array, FBInteger Index, char* location);\n" _
           !"\n" _
           !"void End(void* io);\n" _
           !"\n" _
           !"void ScreenRes(void* io, FBInteger w, FBInteger h);\n" _
           !"void* ImageCreate(void* io, FBInteger w, FBInteger h, FBInteger color);\n" _
           !"void ImageDestroy(void* io, void* image);\n" _
           !"\n" _
           !"void BotGo(void* io, FBInteger action, FBString* target, FBInteger x, FBInteger y, FBInteger z);\n" _
           !"void SendMsg(void* io, FBString* msg, FBInteger channel, FBInteger TransmitTime);\n" _
           !"FBInteger ReadMsg(void* io, FBString* msg, FBInteger LowChannel, FBInteger HighChannel);\n" _
           !"\n" _
           !"void Cls(void* io);\n" _
           !"void PSet(void* io, void* buffer, FBInteger x, FBInteger y, FBInteger color);\n" _
           !"void Line(void* io, void* buffer, FBInteger x1, FBInteger y1, FBInteger x2, FBInteger y2, FBInteger color, FBInteger ModeB_BF);\n" _
           !"void Circle(void* io, void* buffer, FBInteger x, FBInteger y, FBInteger r, FBInteger color, double start, double end, double aspect, FBInteger ModeF);\n" _
           !"void Draw_String(void* io, void* buffer, FBInteger x, FBInteger y, FBString* text, FBInteger color);\n" _
           !"void Get(void* io, void* src, FBInteger x1, FBInteger y1, FBInteger x2, FBInteger y2, void* dest);\n" _
           !"void Put(void* io, void* target, FBInteger x, FBInteger y, void* src, FBInteger method);\n" _
           !"void ScreenLock(void* io);\n" _
           !"void ScreenUnlock(void* io);\n" _
           !"\n" _
           !"void Locate(void* io, FBInteger row, FBInteger column, FBInteger state);\n" _
           !"void Print(void* io, FBString* text, FBInteger NoNewLine);\n" _
           !"\n" _
           !"unsigned long RGB(FBInteger r, FBInteger g, FBInteger b);\n" _
           !"double Timer(void* io);\n" _
           !"void Randomize(void* io, double seed);\n" _
           !"double Rnd(void* io, float seed);\n" _
           !"void Sleep(void* io, FBInteger Ammount);\n" _
           !"\n" _
           !"void ThreadFrameDone(void* io);\n" _
           !"\n"
End Function

Function TypeNameC(VType As BASIC_VarType) As String
    If VType And VtArray Then Return "FBArray"
    Select Case VType And &HFF
    Case VtVOID: Return "void"
    Case VtImageBuffer: Return "void*"
    Case VtDouble: Return "double"
    Case VtInteger: Return "FBInteger"
    Case VtString: Return "FBString"
    End Select
End Function

Function StrEscapeC(S As String) As String
    Dim As Integer I = 1, J
    Dim As String R = ""
    Do
        J = InStr(I, S, """")
        If J = 0 Then J = Len(S) + 1
        R &= Mid(S, I, J - I)
        If J > Len(S) Then Exit Do
        R &= "\"""
        I = J + 1
    Loop
    Return """" & R & """"
End Function

Function EmitExprC(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    Select Case Expr.ExprType
    Case ConstantLiteral
        Var VarTmp = CodeLine.ConstantLiterals(Expr.Index)
        If VarTmp.VarType = (VtString Or VtConstantLiteral) Then
            Return "*StrInit(" & StrEscapeC(VarTmp.VarName) & ")"
           Else
            Return VarTmp.VarName
        End If
    Case LocalParameter: Return UCase(CurProc->Params(Expr.Index).VarName)
    Case LocalVariable: Return UCase(CurProc->LocalVars(Expr.Index).VarName)
    Case GlobalVariable: Return "g->" & UCase(CurProg->GlobalVars(Expr.Index).VarName)
    Case ProcedureCall: Return CurProg->Proc(Expr.Index).ProcName
    Case Operation
        With CodeLine.Tree(Expr.Index)
        Select Case As Const .OpType
        'Variables And Arrays
        Case OpArrayAccess
            Select Case ExprVarType(.Params(0), CodeLine) And &HFF
            Case VtInteger: Return "*IntegerAA(io, &" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", """ & CurProc->NextLine & " of " & CurProc->ProcName & "()"")"
            Case VtDouble: Return "*DoubleAA(io, &" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", """ & CurProc->NextLine & " of " & CurProc->ProcName & "()"")"
            Case VtString: Return "*StringAA(io, &" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", """ & CurProc->NextLine & " of " & CurProc->ProcName & "()"")"
            End Select
        Case OpUBound: Return "UBound(&" & EmitExprC(.Params(0), CodeLine) & ")"
        'Flow Control
        Case OpAndAlso: Return "(" & EmitExprC(.Params(0), CodeLine) & " && " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpOrElse: Return "(" & EmitExprC(.Params(0), CodeLine) & " || " & EmitExprC(.Params(1), CodeLine) & ")"
        'Procedures
        Case OpCallFunction
            Var Code = EmitExprC(.Params(0), CodeLine) & "(g"
            If UBound(.Params) >= 1 Then Code &= ", "
            For I As Integer = 1 To UBound(.Params) - 1
                Code &= EmitExprC(.Params(I), CodeLine) & ", "
            Next I
            If UBound(.Params) >= 1 Then Code &= EmitExprC(.Params(UBound(.Params)), CodeLine)
            Return Code & ")"
        'Arithmetic
        Case OpAdd: Return "(" & EmitExprC(.Params(0), CodeLine) & " + " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpSub: Return "(" & EmitExprC(.Params(0), CodeLine) & " - " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpNeg: Return "-" & EmitExprC(.Params(0), CodeLine)
        Case OpMult: Return "(" & EmitExprC(.Params(0), CodeLine) & " * " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpDiv: Return "(" & EmitExprC(.Params(0), CodeLine) & " / " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpIntDiv: Return "IntDiv(g->io, " & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", """ & CurProc->NextLine & " of " & CurProc->ProcName & "()"")"
        Case OpMod: Return "(" & EmitExprC(.Params(0), CodeLine) & " Mod " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpPower: Return "Pow(" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ")"
        'String
        Case OpStrConcat: Return "*StrConcat(&" & EmitExprC(.Params(0), CodeLine) & ", &" & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpLen: Return "Len(&" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpLeft: Return "*Left(&" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpRight: Return "*Right(&" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpMid: Return "*Mid(&" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", " & EmitExprC(.Params(2), CodeLine) & ")"
        Case OpVal: Return "Val(&" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpStr: Return "*Str(" & EmitExprC(.Params(0), CodeLine) & ")"
        'Comparison
        Case OpEquil
            If ExprVarType(.Params(0), CodeLine) = VtString Then
                Return "StrCompare(&" & EmitExprC(.Params(0), CodeLine) & ", &" & EmitExprC(.Params(1), CodeLine) & ")"
               Else
                Return "(" & EmitExprC(.Params(0), CodeLine) & " == " & EmitExprC(.Params(1), CodeLine) & ")"
            End If
        Case OpNotEquil
            If ExprVarType(.Params(0), CodeLine) = VtString Then
                Return "!StrCompare(&" & EmitExprC(.Params(0), CodeLine) & ", &" & EmitExprC(.Params(1), CodeLine) & ")"
               Else
                Return "(" & EmitExprC(.Params(0), CodeLine) & " != " & EmitExprC(.Params(1), CodeLine) & ")"
            End If
        Case OpGreater: Return "(" & EmitExprC(.Params(0), CodeLine) & " > " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpLessthan: Return "(" & EmitExprC(.Params(0), CodeLine) & " < " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpGreaterEquil: Return "(" & EmitExprC(.Params(0), CodeLine) & " >= " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpLessthanEquil: Return "(" & EmitExprC(.Params(0), CodeLine) & " <= " & EmitExprC(.Params(1), CodeLine) & ")"
        'Logical
        Case OpAnd: Return "(" & EmitExprC(.Params(0), CodeLine) & " & " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpOr: Return "(" & EmitExprC(.Params(0), CodeLine) & " | " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpXOr: Return "(" & EmitExprC(.Params(0), CodeLine) & " ^ " & EmitExprC(.Params(1), CodeLine) & ")"
        Case OpNot: Return "!" & EmitExprC(.Params(0), CodeLine)
        'Math Functions
        Case OpAbs: Return "Abs(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpSgn: Return "Sgn(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpInt: Return "Int(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpFix: Return "Fix(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpSin: Return "Sin(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpCos: Return "Cos(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpTan: Return "Tan(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpAtn: Return "Atn(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpExp: Return "Exp(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpLog: Return "Log(" & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpSqr: Return "Sqr(" & EmitExprC(.Params(0), CodeLine) & ")"
        'Player Inputs
        Case OpInKey: Return "*InKey(io)"
        Case OpMultiKey: Return "MultiKey(io, " & EmitExprC(.Params(0), CodeLine) & ")"
        Case OpGetJoystick
            Return "GetJoystick(io, " & EmitExprC(.Params(0), CodeLine) & ", &" & EmitExprC(.Params(1), CodeLine) & ", &" & _
                                        EmitExprC(.Params(2), CodeLine) & ", &" & EmitExprC(.Params(3), CodeLine) & ", &" & EmitExprC(.Params(4), CodeLine) & ", &" & EmitExprC(.Params(5), CodeLine) & ", &" & _
                                        EmitExprC(.Params(6), CodeLine) & ", &" & EmitExprC(.Params(7), CodeLine) & ", &" & EmitExprC(.Params(8), CodeLine) & ", &" & EmitExprC(.Params(9), CodeLine) & ")"
        Case OpGetMouse: Return "GetMouse(io, &" & EmitExprC(.Params(0), CodeLine) & ", &" & EmitExprC(.Params(1), CodeLine) & ", &" & EmitExprC(.Params(2), CodeLine) & ", &" & EmitExprC(.Params(3), CodeLine) & ")"
        'Robot I/O
        Case OpReadMsg: Return "ReadMsg(io, " & EmitExprListC(.Params(), CodeLine) & ")"
        'Image Buffer Management
        Case OpImageCreate: Return "ImageCreate(io, " & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", " & EmitExprC(.Params(2), CodeLine) & ")"
        'MISC
        Case OpRGB: Return "RGB(" & EmitExprListC(.Params(), CodeLine) & ")"
        Case OpTimer: Return "Timer(io)"
        Case OpRnd: Return "Rnd(io, " & EmitExprC(.Params(0), CodeLine) & ")"
        Case Else: Return "/*UnknownOp*/"
        End Select
        End With
    End Select
    
    Return "/*Unknown ExprType*/"
End Function

Function EmitExprListC(Params() As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    Var P = ""
    For I As Integer = 0 To UBound(Params) - 1
        If (ExprVarType(Params(I), CodeLine) And &HFF) = VtString Then P &= "&"
        P &= EmitExprC(Params(I), CodeLine) & ", "
    Next I
    P &= EmitExprC(Params(UBound(Params)), CodeLine)
    Return P
End Function

Function EmitTopExprC(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
    With CodeLine.Tree(Expr.Index)
    Select Case .OpType
    'Variables And Arrays
    Case OpDim
        Var LVar = CurProc->LocalVars(.Params(0).Index)
        Var Code = TypeNameC(LVar.VarType) & " " & UCase(LVar.VarName)
        Select Case LVar.VarType
        Case VtInteger, VtDouble, VtImageBuffer: Code &= " = 0"
        Case VtString: Code &= " = { 0, 0, 0 };"
        End Select
        If (LVar.VarType And VtArray) <> 0 Then
            Code &= " = {0, 0, 0, sizeof(" & TypeNameC(LVar.VarType And &HFF) & "), 0, " _
                    "{{0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}, {0,0,0}}};"
            'Code &= "; memset((char*)&" & UCase(LVar.VarName) & ", 0, sizeof(" & TypeNameC(VtArray) & ")); "
            'Code &= UCase(LVar.VarName) & ".ELEMENT_LEN = sizeof(" & TypeNameC(LVar.VarType And &HFF) & ");"
        End If
        Return Code
    Case OpAssign
        If (ExprVarType(.Params(0), CodeLine) And &HFF) = VtString Then
            Return "StrAssign(&" & EmitExprC(.Params(0), CodeLine) & ", &" & EmitExprC(.Params(1), CodeLine) & ")"
           Else
            Return EmitExprC(.Params(0), CodeLine) & " = " & EmitExprC(.Params(1), CodeLine)
        End If
    Case OpReDim
        Var VType = CurProc->LocalVars(.Params(0).Index).VarType And &HFF
        Return "ReDim(io, &" & EmitExprC(.Params(0), CodeLine) & ", sizeof(" & TypeNameC(VType) & "), " & EmitExprC(.Params(1), CodeLine) & ", """ & CurProc->NextLine & " of " & CurProc->ProcName & "()"")"
    Case OpReDimPreserve
        Var VType = CurProc->LocalVars(.Params(0).Index).VarType And &HFF
        Return "ReDim_Preserve(io, &" & EmitExprC(.Params(0), CodeLine) & ", sizeof(" & TypeNameC(VType) & "), " & EmitExprC(.Params(1), CodeLine) & ", """ & CurProc->NextLine & " of " & CurProc->ProcName & "()"")"
    Case OpErase
        Var VType = CurProc->LocalVars(.Params(0).Index).VarType And &HFF
        Return "Erase(&" & EmitExprFB(.Params(0), CodeLine) & ", " & IIf(VType = VtString, "-1", "0") & ")"
    'Flow Control
    Case OpIfThen: Return "if (" & EmitExprC(.Params(0), CodeLine) & ") { " & EmitTopExprC(.Params(1), CodeLine) & "; };"
    Case OpIf: Return "if (" & EmitExprC(.Params(1), CodeLine) & ") {"
    Case OpElseIf: Return "} else if (" & EmitExprC(.Params(1), CodeLine) & ") {"
    Case OpElse: Return "} else {"
    Case OpEndIf: Return "}"
    Case OpSelectCase:  Return "switch (" & EmitExprC(.Params(1), CodeLine) & ") {"
    Case OpCase: Return !"break;\ncase " & EmitExprC(.Params(1), CodeLine) & ":" 'Warning: switch (.) { break; case... may be bad hmmm 
    Case OpCaseElse: Return !"break;\ndefault:"
    Case OpEndSelect:  Return "}"
    Case OpDo: Return "do {"
    Case OpDoWhile: Return "while (" & EmitExprC(.Params(0), CodeLine) & ") {"
    Case OpDoUntil: Return "while (!" & EmitExprC(.Params(0), CodeLine) & ") {"
    Case OpLoop
        Var Tmp = @(CurProc->Lines(CurProc->LineNums(.Params(0).Index)))
        If Tmp->Tree(UBound(Tmp->Tree)).OpType = OpDo Then
            Return "} while (!0);"
           Else 
            Return "}"
        End If
    Case OpLoopWhile: Return "} while (" & EmitExprC(.Params(1), CodeLine) & ");"
    Case OpLoopUntil: Return "} while (!" & EmitExprC(.Params(1), CodeLine) & ");"
    Case OpFor
        Var IterName = EmitExprC(.Params(0), CodeLine)
        Var ToName = "to" & CurProc->NextLine
        Return TypeNameC(ExprVarType(.Params(2), CodeLine)) & " " & ToName & " = " & EmitExprC(.Params(2), CodeLine) & !";\n" & CodeIndent(CodeLine) & _
               "for (" & IterName & " = " & EmitExprC(.Params(1), CodeLine) & "; " & _
                         IterName & " <= " & ToName & "; " & _
                         IterName & "++) {"
    Case OpForStep
        Var IterName = EmitExprC(.Params(0), CodeLine)
        Var ToName = "to" & CurProc->NextLine
        Var StepName = "step" & CurProc->NextLine
        Return TypeNameC(ExprVarType(.Params(2), CodeLine)) & " " & ToName & " = " & EmitExprC(.Params(2), CodeLine) & !";\n" & CodeIndent(CodeLine) & _
               TypeNameC(ExprVarType(.Params(3), CodeLine)) & " " & StepName & " = " & EmitExprC(.Params(3), CodeLine) & !";\n" & CodeIndent(CodeLine) & _
               "for (" & IterName & " = " & EmitExprC(.Params(1), CodeLine) & "; " & _
                         StepName & " < 0 ? " & IterName & " >= " & ToName & " : " & IterName & " <= " & ToName & "; " & _
                         IterName & " += " & StepName & ") {"
    Case OpNext: Return "}"
    Case OpGOTO: Return "goto " & CurProc->Lines(CurProc->LineNums(.Params(0).Index)).LineLabel
    'Procedures
    Case OpCallSub
        Var Code = EmitExprC(.Params(0), CodeLine) & "(g"
        If UBound(.Params) >= 1 Then Code &= ", "
        For I As Integer = 1 To UBound(.Params) - 1
            Code &= EmitExprC(.Params(I), CodeLine) & ", "
        Next I
        If UBound(.Params) >= 1 Then Code &= EmitExprC(.Params(UBound(.Params)), CodeLine)
        Return Code & ")"
    Case OpReturn
        Var Code = "return"
        If UBound(.Params) = 0 Then
            Code &= " "
            If (ExprVarType(.Params(0), CodeLine) And &HFF) = VtString Then Code &= "&"
            Code &= EmitExprC(.Params(0), CodeLine)
        End If
        Return Code
    Case OpEnd: Return "End(io)"
    'Robot I/O
    Case OpBotGo: Return "BotGo(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpSendMsg: Return "SendMsg(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    'Image Buffer Commands
    Case OpCls: Return "Cls(io)"
    Case OpPSet: Return "PSet(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpLine: Return "Line(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpCircle: Return "Circle(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpDrawString: Return "Draw_String(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpGet: Return "Get(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpPut: Return "Put(io, " & EmitExprListC(.Params(), CodeLine) & ")"
    Case OpScreenLock: Return "ScreenLock(io)"
    Case OpScreenUnlock: Return "ScreenUnlock(io)"
    'Image Buffer Management
    Case OpScreenRes: Return "ScreenRes(io, " & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ")"
    Case OpImageDestroy: Return "ImageDestroy(io, " & EmitExprC(.Params(0), CodeLine) & ")"
    'Console
    Case OpLocate: Return "Locate(io, " & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ", " & EmitExprC(.Params(2), CodeLine) & ")"
    Case OpPrint: Return "Print(io, &" & EmitExprC(.Params(0), CodeLine) & ", " & EmitExprC(.Params(1), CodeLine) & ")"
    'MISC
    Case OpRandomize: Return "Randomize(io, " & EmitExprC(.Params(0), CodeLine) & ")"
    Case OpSleep: Return "Sleep(io, " & EmitExprC(.Params(0), CodeLine) & ")"
    End Select
    End With
    
    Return ""
End Function

Function EmitLineC(CodeLine As BASIC_LineOfCodeAst) As String
    Dim Code As String
    If CodeLine.LineLabel <> "" Then Code = CodeLine.LineLabel & ": "
    
    If UBound(CodeLine.Tree) < 0 Then Return Code & !"\n"
    
    Code &= EmitTopExprC(Type(Operation, UBound(CodeLine.Tree)), CodeLine)
    
    If Right(Code, 1) = "{" Or Right(Code, 1) = "}" Or Right(Code, 1) = ";" Then Return Code & !"\n"
    Return Code & !";\n"
End Function

Enum CodeBranchType
    BranchNone
    BranchSkipBlock
    BranchProcCall
    BranchProcReturn
    BranchStartLoop
    BranchEndLoop
    BranchGoto
End Enum

Function BranchType(CodeLine As BASIC_LineOfCodeAst) As CodeBranchType
    If UBound(CodeLine.Tree) < 0 Then Return BranchNone
    For I As Integer = 0 To UBound(CodeLine.Tree)
        If CodeLine.Tree(I).OpType = OpCallFunction Then Return BranchProcCall
    Next I
    Select Case As Const CodeLine.Tree(UBound(CodeLine.Tree)).OpType
    Case OpIfThen
        Var Node = CodeLine.Tree(UBound(CodeLine.Tree)).Params(1).Index
        Select Case CodeLine.Tree(Node).OpType
        Case OpCallSub: Return BranchProcCall
        Case OpReturn, OpExitSub, OpExitFunction: Return BranchProcReturn
        Case OpGOTO: Return BranchGoto
        Case Else: Return BranchNone
        End Select
    Case OpIf, OpElseIf, OpElse, OpEndIf, OpSelectCase, OpCase, OpCaseElse, OpEndSelect: Return BranchSkipBlock
    Case OpCallSub, OpCallFunction: Return BranchProcCall
    Case OpReturn, OpExitSub, OpExitFunction: Return BranchProcReturn
    Case OpDo, OpDoWhile, OpDoUntil, OpFor, OpForStep: Return BranchStartLoop
    Case OpLoop, OpLoopWhile, OpLoopUntil, OpNext: Return BranchEndLoop
    Case OpGOTO: Return BranchGoto
    Case Else: Return BranchNone
    End Select
End Function

Function EmitProgC(Prog As BASIC_Program) As String
    Dim Code As String = ""
    CurProg = @Prog
    
    'Global Variable Struct, Initalizer, Destroyer
    Code &= !"typedef struct {\n" _
            !"    FBInteger CodeToGo;\n" _
            !"    void* io;\n"
    For I As Integer = 0 To UBound(Prog.GlobalVars)
        Code &= "    " & TypeNameC(Prog.GlobalVars(I).VarType) & " " & UCase(Prog.GlobalVars(I).VarName) & !";\n"
    Next I
    Code &= !"} Global;\n" _
            !"int Init_Globals(Global* g) {\n" _
            !"    if (g == 0) return sizeof(Global);\n"
    For I As Integer = 0 To UBound(Prog.GlobalVars)
        Var LVar = Prog.GlobalVars(I)
        If (LVar.VarType And VtArray) <> 0 Then _
            Code &= "    g->" & UCase(LVar.VarName) & ".ELEMENT_LEN = sizeof(" & TypeNameC(LVar.VarType And &HFF) & !");\n"
    Next I
    Code &= !"    return 0;\n" _
            !"}\n" _
            !"void Destroy_Globals(Global* g) {\n"
    For I As Integer = 0 To UBound(Prog.GlobalVars)
        Var LVar = Prog.GlobalVars(I)
        If LVar.VarType = VtString Then Code &= "    StrDelete(&g->" & UCase(LVar.VarName) & !");\n"
        If LVar.VarType = VtImageBuffer Then Code &= "    ImageDestroy(g->io, g->" & UCase(LVar.VarName) & !");\n"
        If (LVar.VarType And VtArray) <> 0 Then _
            Code &= "    Erase(&g->" & UCase(LVar.VarName) & ", " & IIf((LVar.VarType And &HFF) = VtString, "-1", "0") & !");\n"
    Next I
    Code &= !"}\n" _
            !"\n"
    
    For I As Integer = 0 To UBound(Prog.Proc)
        CurProc = @Prog.Proc(I)
        Code &= TypeNameC(Prog.Proc(I).ReturnType)
        If Prog.Proc(I).ReturnType = VtString Then Code &= "*"
        Code &= " " & Prog.Proc(I).ProcName & "(Global* g"
        If 0 <= UBound(Prog.Proc(I).Params) Then Code &= ", "
        For J As Integer = 0 To UBound(Prog.Proc(I).Params)
            Code &= TypeNameC(Prog.Proc(I).Params(J).VarType)
            If Prog.Proc(I).Params(J).VarType = VtString Then Code &= "*"
            Code &= " " & UCase(Prog.Proc(I).Params(J).VarName)
            If J < UBound(Prog.Proc(I).Params) Then Code &= ", "
        Next J
        Code &= !") {\n" _
                !"    void* io = g->io;\n" _
                !"    if (g->CodeToGo <= 0) ThreadFrameDone(io);\n"
        Var CodeCount = 0
        For J As Integer = 0 To UBound(Prog.Proc(I).Lines)
            
            Var CdIdt = CodeIndent(Prog.Proc(I).Lines(J))
            
            Select Case BranchType(Prog.Proc(I).Lines(J))
            Case BranchNone: CodeCount += UBound(Prog.Proc(I).Lines(J).Tree) + 1
            Case BranchProcReturn, BranchProcCall
                CodeCount += UBound(Prog.Proc(I).Lines(J).Tree) + 1
                If CodeCount > 0 Then Code &= CdIdt & !"g->CodeToGo -= " & CodeCount & !";\n"
                CodeCount = 0
            Case BranchStartLoop, BranchSkipBlock
                If CodeCount > 0 Then Code &= CdIdt & !"g->CodeToGo -= " & CodeCount & !";\n"
                CodeCount = UBound(Prog.Proc(I).Lines(J).Tree) + 1
            Case BranchEndLoop, BranchGoto
                CodeCount += UBound(Prog.Proc(I).Lines(J).Tree) + 1
                If CodeCount > 0 Then Code &= CdIdt & !"    g->CodeToGo -= " & CodeCount & !";\n"
                Code &= CdIdt & !"    if (g->CodeToGo <= 0) ThreadFrameDone(io);\n"
                CodeCount = 0
            End Select
            
            Prog.Proc(I).NextLine = J + 1
            Code &= CdIdt & EmitLineC(Prog.Proc(I).Lines(J))
        Next J
        Code &= !"}\n"
    Next I
    Return Code
End Function