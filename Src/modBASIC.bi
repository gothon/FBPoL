
#Pragma Once

Enum BASIC_Operation
    'Variables And Arrays
    OpDim
    OpDimShared
    OpAssign
    'OpLet
    OpReDim
    OpReDimPreserve
    OpArrayAccess
    OpUBound
    OpErase
    'OpArrayAccessAssign
    
    'Flow Control
    OpAndAlso
    OpOrElse
    OpIfThen
    OpIfThenElse'
    OpIf
    OpElseIf
    OpElse
    OpEndIf
    OpSelectCase
    OpCase
    OpCaseElse
    OpEndSelect
    OpDo
    OpDoWhile
    OpDoUntil
    OpLoop
    OpLoopWhile
    OpLoopUntil
    OpFor
    OpForStep
    OpNext
    OpExitLoop'
    OpContinueLoop'
    'OpLineLabel
    OpGOTO
    OpGOSUB 'May or may not implement
    
    'Procedures
    OpCallSub
    OpCallFunction
    OpReturn
    OpExitSub'
    OpExitFunction'
    OpEnd
    'OpParramArray
    
    'Arithmetic
    OpAdd
    OpSub
    OpNeg
    OpMult
    OpDiv
    OpIntDiv
    OpMod
    OpPower
    
    'String
    OpStrConcat
    OpLen
    OpLeft
    OpRight
    OpMid
    OpVal
    OpStr
    'OpAsc
    'OpChr
    'OpUCase
    'OpLCase
    'OpString
    'OpInStr
    
    'Comparison
    OpEquil
    OpNotEquil
    OpGreater
    OpLessthan
    OpGreaterEquil
    OpLessthanEquil
    
    'Logical
    OpAnd
    OpOr
    OpXOr
    OpNot
    
    'Math Functions
    OpAbs
    OpSgn
    OpInt
    OpFix
    OpSin
    OpCos
    OpTan
    OpAtn
    OpExp
    OpLog
    OpSqr
    
    'Player Inputs
    OpInKey
    OpMultiKey
    OpGetJoystick
    OpGetMouse
    
    'Robot I/O
    OpReadSensor
    OpBotGo
    OpSendMsg
    OpReadMsg
    
    'OpFrameSleep ' OpWaitForInMsg
    
    'Image Buffer Commands
    OpCls
    OpPSet
    OpLine
    OpCircle
    OpDrawString
    OpGet
    OpPut
    OpScreenLock
    OpScreenUnlock
    
    'Image Buffer Management
    OpScreenRes  '1 Screen per Player
    'OpScreenInfo
    OpScreenControl'
    OpImageCreate  'Memory buffers are fixed size and must be purchased with currency
    'OpImageInfo 'OpCreateImage
    OpImageDestroy
    
    'Console
    OpColor'
    OpLocate
    OpPrint
    OpLineInput'
    
    'MISC
    OpRGB
    OpTimer
    OpRandomize  'Each Thread has its own sepreate Seed state
    OpRnd
    OpSleep
    OpBeep'
    
    'Apparently Popular
    OpLeftPad 'Lets hope we don't forget how to program :P
    
    OpLastOperation
End Enum

Enum BASIC_VarType
    VtVOID = 0
    'VtUDT = 1
    VtImageBuffer = 2
    VtInteger = 3
    VtDouble = 4
    VtString = 5
    VtArray = &H100
    VtConstantLiteral = &H200
    'VtByRef = &H400
End Enum

Type BASIC_Variable
    As BASIC_VarType VarType
    As String VarName
End Type

Enum BASIC_ExpressionType
    EmptyExpr
    ConstantLiteral
    LocalParameter
    LocalVariable
    GlobalVariable
    ProcedureCall
    LineNumberReference
    Operation
End Enum

''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
' Using the Expression UDT in functions named like keywords and overloaded '
' operators allows BASIC Programs to be written in natural looking FB code '
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

Type BASIC_Expression
    As BASIC_ExpressionType ExprType
    As Integer Index
    
    Declare Constructor()
    Declare Constructor(ExprType As BASIC_ExpressionType, Index As Integer)
    Declare Constructor(Rhs As Integer)
    Declare Constructor(Rhs As Double)
    Declare Constructor(Rhs As String)
    
    Declare Operator Let(Rhs As BASIC_Expression)
    Declare Operator [] (Index As BASIC_Expression) As BASIC_Expression
    Declare Property AA(Index As BASIC_Expression) As BASIC_Expression 'AA is for Array Access
    Declare Property AA(Index As BASIC_Expression, Rhs As BASIC_Expression)
    
    'FC is for Function Call, SC is for Sub Call
    Declare Function FC(Param1 As BASIC_Expression = Type(EmptyExpr, 0), Param2 As BASIC_Expression = Type(EmptyExpr, 0), Param3 As BASIC_Expression = Type(EmptyExpr, 0)) As BASIC_Expression
    Declare Sub SC(Param1 As BASIC_Expression = Type(EmptyExpr, 0), Param2 As BASIC_Expression = Type(EmptyExpr, 0))
End Type

Type BASIC_OperationNode
    'As BASIC_VarType VarType
    As BASIC_Operation OpType
    As BASIC_Expression Params(Any)
End Type

Type BASIC_LineOfCodeAst
    As BASIC_Variable ConstantLiterals(Any)
    As BASIC_OperationNode Tree(Any)
    As String LineLabel, CommentStr
    
    As Integer NextNode, NextLit, IndentDepth
End Type

Type BASIC_Procedure
    As String ProcName
    As BASIC_VarType ReturnType
    As BASIC_Variable Params(Any)
    As BASIC_Variable LocalVars(Any)
    As BASIC_LineOfCodeAst Lines(Any)
    
    As Integer LineNums(Any)
    As Integer FlowStack(Any)
    As Integer NextLine, NextVar, FlowStackTop = -1
    
    Declare Function Dim_(VarName As String, VarType As BASIC_VarType) As BASIC_Expression
    Declare Function CLit(ConstLitVal As String, VarType As BASIC_VarType) As BASIC_Expression
    Declare Function AddParam(VarName As String, VarType As BASIC_VarType) As BASIC_Expression
    
    Declare Sub _Return()
    Declare Sub _Return(ReturnVal As BASIC_Expression)
    
    
    Declare Sub BlankLine(LineComment As String = "")
    Declare Sub LLabel_(LineLabel As String, LineNumRef As BASIC_Expression)
    Declare Sub LComment(LineComment As String)
    'Declare Sub Let_(Lhs As BASIC_Expression, Rhs As BASIC_Expression)
    
    As BASIC_Expression TmpCond
    Declare Sub _If(Cond As BASIC_Expression)
    Declare Sub _Then
    
    Declare Sub If_(Cond As BASIC_Expression)
    Declare Sub ElseIf_(Cond As BASIC_Expression)
    Declare Sub Else_
    Declare Sub End_If
    
    Declare Sub Select_Case(Expr As BASIC_Expression)
    Declare Sub _Case(Expr As BASIC_Expression)
    Declare Sub Case_Else
    Declare Sub End_Select
    
    Declare Sub _Do
    Declare Sub Do_While(Cond As BASIC_Expression)
    Declare Sub Do_Until(Cond As BASIC_Expression)
    Declare Sub _Loop
    Declare Sub Loop_While(Cond As BASIC_Expression)
    Declare Sub Loop_Until(Cond As BASIC_Expression)
    
    Declare Sub _For(LoopVar As BASIC_Expression, From As BASIC_Expression, _To As BASIC_Expression, _Step As BASIC_Expression = Type(EmptyExpr, 0))
    Declare Sub _Next
End Type

Type BASIC_Program
    As BASIC_Variable GlobalVars(Any)
    As BASIC_Procedure Proc(Any)
    
    Declare Function Dim_Shared_(VarName As String, VarType As BASIC_VarType) As BASIC_Expression
    Declare Function Procedure(FunName As String, RetType As BASIC_VarType) As BASIC_Expression
    Declare Sub End_Procedure
End Type

Extern CurProg As BASIC_Program Ptr
Extern CurProc As BASIC_Procedure Ptr

#Define _Sub(SubName) Var SubName = CurProg->Procedure(#SubName, VtVOID): With *CurProc: Scope
#Define _Sub_Run CurProg->Procedure("Run", VtVOID): With *CurProc: Scope
#Define _End_Sub End Scope: End With: CurProg->End_Procedure
#Define _Function(FunName, RetType) Var FunName = CurProg->Procedure(#FunName, RetType): With *CurProc: Scope
#Define _End_Function End Scope: End With: CurProg->End_Procedure

#Define _Dim(VarName, VarType) Var VarName = .Dim_(#VarName, VarType)
#Define Add_Param(VarName, VarType) Var VarName = .AddParam(#VarName, VarType)
#Define _Dim_Shared(VarName, VarType) Var VarName = CurProg->Dim_Shared_(#VarName, VarType)

#Macro LLabel(LINE_LABEL)
    #IfNDef LINE_LABEL
        Dim LINE_LABEL As BASIC_Expression
    #EndIf
    .LLabel_(#LINE_LABEL, LINE_LABEL)
#EndMacro

' Overloading Operators so I can build User Programs within the main program
Declare Sub _ReDim(V As BASIC_Expression, NewUBound As BASIC_Expression)
Declare Sub _ReDim_Preserve(V As BASIC_Expression, NewUBound As BASIC_Expression)
Declare Function _UBound(V As BASIC_Expression) As BASIC_Expression
Declare Sub _Erase(V As BASIC_Expression)

Declare Function _AndAlso(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Function _OrElse(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression

Declare Operator +(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator -(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator -(Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator *(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator /(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator \(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator Mod(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator ^(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression

Declare Operator =(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator <>(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator >(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator <(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator >=(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator <=(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression

Declare Operator And(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator Or(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator XOr(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Operator Not(Rhs As BASIC_Expression) As BASIC_Expression

Declare Operator &(Lhs As BASIC_Expression, Rhs As BASIC_Expression) As BASIC_Expression
Declare Function _Len(Str As BASIC_Expression) As BASIC_Expression
Declare Function Left(Str As BASIC_Expression, N As BASIC_Expression) As BASIC_Expression
Declare Function Right(Str As BASIC_Expression, N As BASIC_Expression) As BASIC_Expression
Declare Function _Mid(Str As BASIC_Expression, Start As BASIC_Expression, N As BASIC_Expression) As BASIC_Expression
Declare Function Val(Str As BASIC_Expression) As BASIC_Expression
Declare Function _Str(Number As BASIC_Expression) As BASIC_Expression

Declare Operator Abs(Number As BASIC_Expression) As BASIC_Expression
Declare Operator Sgn(Number As BASIC_Expression) As BASIC_Expression
Declare Operator Int(Number As BASIC_Expression) As BASIC_Expression
Declare Operator Fix(Number As BASIC_Expression) As BASIC_Expression
Declare Operator Sin(Angle As BASIC_Expression) As BASIC_Expression
Declare Operator Cos(Angle As BASIC_Expression) As BASIC_Expression
Declare Operator Tan(Angle As BASIC_Expression) As BASIC_Expression
Declare Operator Atn(Number As BASIC_Expression) As BASIC_Expression
Declare Operator Exp(Number As BASIC_Expression) As BASIC_Expression
Declare Operator Log(Number As BASIC_Expression) As BASIC_Expression
Declare Function _Sqr(Number As BASIC_Expression) As BASIC_Expression

Declare Function _InKey As BASIC_Expression
Declare Function _MultiKey(ScanCode As BASIC_Expression) As BASIC_Expression
Declare Function _GetJoystick(ID As BASIC_Expression, Buttons As BASIC_Expression = 0, A1 As BASIC_Expression = 0, A2 As BASIC_Expression = 0, A3 As BASIC_Expression = 0, A4 As BASIC_Expression = 0, A5 As BASIC_Expression = 0, A6 As BASIC_Expression = 0, A7 As BASIC_Expression = 0, A8 As BASIC_Expression = 0) As BASIC_Expression
Declare Function _GetMouse(X As BASIC_Expression, Y As BASIC_Expression, Wheel As BASIC_Expression = 0, Buttons As BASIC_Expression = 0) As BASIC_Expression

Declare Sub _BotGo(Action As BASIC_Expression, Target As BASIC_Expression = "", X As BASIC_Expression = 0, Y As BASIC_Expression = 0, Z As BASIC_Expression = 0)
Declare Sub _SendMsg(Msg As BASIC_Expression, Channel As BASIC_Expression = 10, TransmitTime As BASIC_Expression = 16)
Declare Function _ReadMsg(Msg As BASIC_Expression, LowChannel As BASIC_Expression = 10, HighChannel As BASIC_Expression = 10) As BASIC_Expression

Declare Sub _Cls
Declare Sub _PSet(Buffer As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, _Color As BASIC_Expression)
Declare Sub _Line(Buffer As BASIC_Expression, X1 As BASIC_Expression, Y1 As BASIC_Expression, X2 As BASIC_Expression, Y2 As BASIC_Expression, _Color As BASIC_Expression, ModeB_BF As Integer = 0)
Declare Sub _Circle(Buffer As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, R As BASIC_Expression, _Color As BASIC_Expression, Start As BASIC_Expression = 0.0, EndAng As BASIC_Expression = 6.283186, Aspect As BASIC_Expression = 1.0, ModeF As Integer = 0)
Declare Sub _Draw_String(Buffer As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, Text As BASIC_Expression, _Color As BASIC_Expression)
Declare Sub _Get(Src As BASIC_Expression, X1 As BASIC_Expression, Y1 As BASIC_Expression, X2 As BASIC_Expression, Y2 As BASIC_Expression, Dest As BASIC_Expression)
Declare Sub _Put(Target As BASIC_Expression, X As BASIC_Expression, Y As BASIC_Expression, Src As BASIC_Expression, Method As Integer = 0)
Declare Sub _ScreenLock()
Declare Sub _ScreenUnlock()

Declare Sub _ScreenRes(W As BASIC_Expression, H As BASIC_Expression)
Declare Function _ImageCreate(W As BASIC_Expression, H As BASIC_Expression, Col As BASIC_Expression) As BASIC_Expression
Declare Sub _ImageDestroy(Image As BASIC_Expression)

Declare Sub _Locate(Row As BASIC_Expression = 0, Column As BASIC_Expression = 0, State As BASIC_Expression = -1)
Declare Sub _Print(Text As BASIC_Expression, NoNewLine As Integer = 0)

Declare Sub GOTO_(LineLabel As BASIC_Expression)
#Macro _GOTO(LINE_LABEL)
    #IfNDef LINE_LABEL
        Dim LINE_LABEL As BASIC_Expression
    #EndIf
    GOTO_ LINE_LABEL
#EndMacro

Declare Sub _End

Declare Function _RGB(R As BASIC_Expression, G As BASIC_Expression, B As BASIC_Expression) As BASIC_Expression
Declare Function _Timer() As BASIC_Expression
Declare Sub _Randomize(Seed As BASIC_Expression = -1.0)
Declare Function _Rnd(Seed As BASIC_Expression = 1.0) As BASIC_Expression
Declare Sub Sleep__(Ammount As BASIC_Expression = -1)

'Stub Functions (called by the backend)
Declare Function __Abs CDecl (Number As Double) As Double
Declare Function __Sgn CDecl (Number As Double) As Double
Declare Function __Int CDecl (Number As Double) As Double
Declare Function __Fix CDecl (Number As Double) As Double
Declare Function __Sin CDecl (Angle As Double) As Double
Declare Function __Cos CDecl (Angle As Double) As Double
Declare Function __Tan CDecl (Angle As Double) As Double
Declare Function __Atn CDecl (Number As Double) As Double
Declare Function __Exp CDecl (Number As Double) As Double
Declare Function __Log CDecl (Number As Double) As Double
Declare Function __Sqr CDecl (Number As Double) As Double

' Front End Code Generator for Screen Display
Declare Function EmitExprFB(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
Declare Function EmitLineFB(CodeLine As BASIC_LineOfCodeAst) As String
Declare Function EmitProgFB(Prog As BASIC_Program) As String

' Back End Code Generator for libtcc execution
Declare Function EmitCHeader As String
Declare Function EmitExprC(Expr As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
Declare Function EmitExprListC(Params() As BASIC_Expression, CodeLine As BASIC_LineOfCodeAst) As String
Declare Function EmitLineC(CodeLine As BASIC_LineOfCodeAst) As String
Declare Function EmitProgC(Prog As BASIC_Program) As String

' Interpeter Back End
'Declare Function EvalProg(Prog As BASIC_Program, CommandParam As String) As Integer
