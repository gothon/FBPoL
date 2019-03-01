
#Include "modGameState.bi"

#Include "fbgfx.bi"
Using FB

Sub GenPongProg
    _Sub_Run
        _Dim(BallX, VtInteger)
        _Dim(BallY, VtInteger)
        _Dim(BarX1, VtInteger)
        _Dim(BarY1, VtInteger)
        _Dim(BarX2, VtInteger)
        _Dim(BarY2, VtInteger)
        _Dim(DirectionX, VtInteger)
        _Dim(DirectionY, VtInteger)
        _Dim(Message, VtString)
        .BlankLine
        BallX = 160
        BallY = Int(200 * _Rnd(1))
        BarX1 = 20
        BarY1 = 20
        BarX2 = 300
        BarY2 = Int(120 * _Rnd(1)) + 20
        DirectionX = 8
        DirectionY = 4
        Message = "Do Battle"
        .BlankLine
        _ScreenRes 320, 200
        ._Do
            .BlankLine "'''''''''''''''''''''''''"
            .BlankLine "' Show the Ball and Bars:"
            .BlankLine
            _ScreenLock
                _Cls
                _Draw_String 0, 0, 0, "    " & Message, &HFFFFFF
                _Circle 0, BallX, BallY, 4, &HFF0000, , , , 1
                _Line 0, BarX1, BarY1, BarX1 + 10, BarY1 + 60, &HFF, 2
                _Line 0, BarX2, BarY2, BarX2 + 10, BarY2 + 60, &HFF00, 2
            _ScreenUnlock
            .BlankLine
            .BlankLine "''''''''''''"
            .BlankLine "' Wait a sec"
            .BlankLine
            Sleep__ 16 'Sleep 1 frame
            .BlankLine
            .BlankLine "'''''''''''''''''''''''''''"
            .BlankLine "' Player Input Changes Bar1"
            .BlankLine
            ._If _MultiKey(SC_Q): _End: ._Then
            ._If _MultiKey(SC_UP): BarY1 = BarY1 - 3: ._Then
            ._If _MultiKey(SC_DOWN): BarY1 = BarY1 + 3: ._Then
            .BlankLine
            .BlankLine "''''''''''''''''''''''''''''''''''''''''''''"
            .BlankLine "' Bar2 and the Ball changed by the computer:"
            .BlankLine
            .If_ BallY > BarY2 + 30
                BarY2 = BarY2 + 3
            .Else_
                ._If BallY < BarY2 + 30: BarY2 = BarY2 - 3: ._Then
            .End_If
            .BlankLine
            BallX = BallX + DirectionX
            BallY = BallY + DirectionY
            .BlankLine
            .If_ BallX >= 296
                .If_ BallY > BarY2 And BallY < BarY2 + 60
                    DirectionX = -DirectionX
                .Else_
                    Message = "Computer looses! Press Q."
                .End_If
            .End_If
            .If_ BallX <= 34
                .If_ BallY > BarY1 And BallY < BarY1 + 60
                    DirectionX = -DirectionX
                .Else_
                    Message = "You loose!  Press Q."
                .End_If
            .End_If
            .If_ BallY >= 196 Or BallY <= 4
                DirectionY = -DirectionY
            .End_If
        ._Loop
    _End_Sub
End Sub

Sub GenGarbageCode
    _Dim_Shared(Dragon_BallZ, VtInteger)
    _Dim_Shared(BallZ, VtString Or VtArray)
    
    _Sub(RunPong)
        _Dim(BallX, VtInteger)
        _Dim(BallY, VtInteger)
        _Dim(BarX1, VtInteger)
        _Dim(BarX2, VtInteger)
        _Dim(BarY2, VtInteger)
        _Dim(Message, VtString)
        
        _Dim(A, VtString Or VtArray)
        _ReDim(A, 8)
        A.AA(5) = "7"
        'BarX1.AA(20 + BarX1) = 7 + BarX1.AA(3)
        _GOTO(Drag)
        .If_ BallX = BarX1
        ._If BallX = BallY: BarY2 = 20 + BarX2: ._Then
        .Else_
        .End_If
        .BlankLine
        LLabel(Drag): Dragon_BallZ = 5: .LComment " 'This line is unneeded"
        
        _GOTO(Drag)
        
        ._If _AndAlso(Message = "Do Battle", 0)
        RunPong.SC
        ._Then
        
        RunPong.SC
        
        ._Do
        .Select_Case BarX1
        ._Case 3
            RunPong.SC
        .Case_Else
        .End_Select
        ._Loop
        .BlankLine
        _Dim(I, VtInteger)
        ._For I, 0, 10
        ._Next
    _End_Sub
    
    _Function(Factorial, VtDouble): Add_Param(N, VtInteger)
        ._If N = 0: ._Return 1: ._Then
        ._Return Abs(N * Factorial.FC(N - 1))
    _End_Function
    
    _Function(Hi, VtString)
        ._Return "Hello FB String"
    _End_Function

    '_Dim(X, VtInteger)
    '_Dim(Y, VtInteger)
    '_Dim(W, VtInteger)
    'I = _GetMouse(X, Y, W, B)
    '.If_ I = 0 And B <> 0
    '_Line 0, X-2, Y-2, X+2, Y+2, _RGB(255, 0, 0), 2
    '.End_If
    'In = _InKey
    '._If In <> "": _Print In: ._Then
    'Do
    '    Do
    '        Var OutPoo = EatProc(InFood)
    '        If DoneFood Then Exit Do, Do
    '    Loop
    'Loop
End Sub

Sub GenDefPlayerProg
    _Dim_Shared(A, VtDouble Or VtArray)
    _Dim_Shared(In, VtString)
    
    _Function(GetJoystickArray, VtInteger): Add_Param(ID, VtInteger): Add_Param(A, VtDouble Or VtArray)
        _Dim(Btn, VtInteger)
        ._If 0 <> _GetJoystick(ID, Btn, A.AA(0), A.AA(1), A.AA(2), A.AA(3), A.AA(4), A.AA(5), A.AA(6), A.AA(7)): ._Return -1: ._Then
        ._Return Btn
    _End_Function
    
    _Sub_Run
        _Dim(I, VtInteger)
        _Dim(ID, VtInteger)
        _Dim(B, VtInteger)
        _ReDim A, 7
        
        _Cls
        _Print "Booting Up", 1
        Sleep__ 1000
        _Print ".", 1
        Sleep__ 1000
        _Print ".", 1
        Sleep__ 1000
        _Print ".", 1
        Sleep__ 1000
        _Print " Done"
        Sleep__ 200
        _Print ""
        _Print ""
        _Print "    BASIC Operating System Software"
        Sleep__ 200
        _Print ""
        _Print "           BOSS 1.0 Running"
        _Print ""
        _Print ""
        _Print ""
        Sleep__ 200
        _Print " Press any key to continue"
        _Print ""
        Sleep__ 200
        _Print "                or"
        _Print ""
        Sleep__ 200
        _Print " Push a button for gamepad operation"
        
        ._Do
            ._For ID, 0, 15
                B = GetJoystickArray.FC(ID, A)
                ._If B > 0
                    _GOTO(ExitDo): ._Then
            ._Next
            ._If _InKey <> ""
                _GOTO(ExitDo): ._Then
            Sleep__ 16: .LComment " 'Sleep 1 Frame"
        ._Loop
        
        LLabel(ExitDo)
        .BlankLine
        
        .If_ ID = 16
            _Cls
            _Print ""
            _Print "    Keyboard Operation Selected"
            _Print ""
            _Print " Press"
            _Print "  " & Chr(24) & " " & Chr(25) & " " & Chr(26) & " " & Chr(27) & " to move"
            _Print "  Page Up/Down, + -, < >"
            _Print "  to Tilt, Zoom and Spin"
            _Print "  T to toggle detached window"
            _Print ""
            ._Do
                In = _InKey
                '._If In <> "": _Print " InKey = """ & In & """": ._Then
                .If_ In <> ""
                    _Locate 10, 1
                    _Print " InKey = """ & In & """  "
                .End_If
                _BotGo BotSettings, "", 0, (_MultiKey(SC_COMMA) - _MultiKey(SC_PERIOD)) * 100
                _BotGo BotSettings, "", 1, (_MultiKey(SC_PAGEUP) - _MultiKey(SC_PAGEDOWN)) * 100
                _BotGo BotSettings, "", 2, (_MultiKey(SC_MINUS) - _MultiKey(SC_EQUALS)) * 100
                ._If In = "t" Or In = "T": _BotGo BotSettings, "", 3, 1: ._Then
                '._If In = "q": _End: ._Then
                
                _BotGo BotMoveTo, "", _MultiKey(SC_LEFT) - _MultiKey(SC_RIGHT), 0, _MultiKey(SC_UP) - _MultiKey(SC_DOWN)
            ._Loop
        .End_If
        
        _Cls
        _Print ""
        _Print "    Reading GetJoystick(" & _Str(ID) & ", ...)"
        ._Do
            
            B = GetJoystickArray.FC(ID, A)
            .If_ -1 <> B
                '_Cls
                '_Print ""
                '_Print "    Reading GetJoystick(" & _Str(ID) & ", ...)"
                '_Print ""
                _Locate 4, 1
                _Print " Buttons = " & _Str(B) & "     "
                _Print ""
                ._For I, 0, 7
                    _Print "                           "
                    _Locate 6 + I, 1
                    _Print " Axis" & _Str(I + 1) & " = " & _Str(A.AA(I))
                ._Next
                'For J As Integer = 0 To 7
                '    _Print " Axis" & Str(J + 1) & " = " & _Str(A.AA(J))
                'Next J
                
                ._If Abs(A.AA(4)) > 0.10: _BotGo BotSettings, "", 0, A.AA(4) * 100: ._Then
                ._If Abs(A.AA(3)) > 0.10: _BotGo BotSettings, "", 1, A.AA(3) * 100: ._Then
                ._If Abs(A.AA(2)) > 0.10: _BotGo BotSettings, "", 2, A.AA(2) * 100: ._Then
                .If_ B And 64
                    _BotGo BotSettings, "", 3, 1
                    Sleep__ 300
                .End_If
            .End_If
            _BotGo BotMoveTo, "", A.AA(6), 0, A.AA(7) 'DPad
        ._Loop
    _End_Sub
End Sub