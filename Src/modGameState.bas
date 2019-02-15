
#Include "modGameState.bi"
#Include "modGraphics.bi"

#Include "fbgfx.bi"
Using FB

#Include "Inc/libtcc.bi"

#Include "fbthread.bi"

#IfDef __FB_LINUX__
    '#Include "pthread.bi"
    Extern "C"
        Declare Sub pthread_exit(ByVal value_ptr As Any Ptr)
    End Extern
#EndIf

Dim Shared As Integer NumThreadsAwake, NumLiveThreads, ThreadEndSignal ', NumActiveEntities
Dim Shared As Any Ptr AwakeCountMutex, FrameAMutex, FrameBMutex

#Define THREAD_STACK_SIZE 0 '<- May Need to Adjust??

Declare Sub __Print CDecl (IO As ProgIO, Text As Const String, NoNewLine As Integer = 0)

Sub ThreadFrameDone CDecl(IO As ProgIO)
    Do
        Var FrameMutex = FrameAMutex
        MutexLock AwakeCountMutex
        NumThreadsAwake -= 1
        MutexUnlock AwakeCountMutex
        
        MutexLock FrameMutex
        MutexUnlock FrameMutex
        
        If ThreadEndSignal AndAlso IO.EndProgNow Then
            #IfDef __FB_WIN32__
                ExitThread 0
            #Else '#IfDef __FB_LINUX__
                pthread_exit NULL
            #EndIf
        End If
    Loop Until IO.Globals->CodeToGo > 0 And Not ThreadEndSignal
End Sub

Sub PlayerThreadAbort(IO As ProgIO, Msg As String = "")
    __Print IO, Msg
    
    IO.EndProgNow = -1
    ThreadFrameDone IO
End Sub

Sub ThreadRun(ByVal UserData As Any Ptr)
    Var IO = CPtr(ProgIO Ptr, UserData)
    
    'ThreadFrameDone
    IO->EntryPoint(IO->Globals)
    
    IO->EndProgNow = -1
    ThreadFrameDone *IO
End Sub

Sub ProgIO.RunStart(BU As BuildUnit)
    NumLiveThreads += 1
    EndProgNow = 0
    Globals = CAllocate(BU.Init_Globals(NULL), 1)
    BU.Init_Globals(Globals)
    Globals->IO = @This
    EntryPoint = BU.EntryPoint
    Destroy_Globals = BU.Destroy_Globals
    MutexLock(AwakeCountMutex)
    NumThreadsAwake += 1
    MutexUnlock(AwakeCountMutex)
    
    ThreadHandle = ThreadCreate(@ThreadRun, @This, THREAD_STACK_SIZE)
    'ThreadHandle = ThreadCreate(BU.EntryPoint, @This, THREAD_STACK_SIZE)
End Sub

Sub ProgIO.EndRun
    If ThreadHandle <> NULL Then
        NumLiveThreads -= 1
        'MutexLock AwakeCountMutex
        NumThreadsAwake -= 1
        'MutexUnlock AwakeCountMutex
        EndProgNow = -1
        ThreadEndSignal = -1
        ThreadsRun
        ThreadWait ThreadHandle: ThreadHandle = NULL
        ThreadEndSignal = 0
    End If
    EndProgNow = 0
    If Globals <> NULL Then
        Destroy_Globals(Globals)
        Deallocate Globals: Globals = NULL
    End If
    If ScreenImg <> NULL Then ImageDestroy ScreenImg: ScreenImg = NULL
    If ScreenImg2 <> NULL Then ImageDestroy ScreenImg2: ScreenImg2 = NULL
    If ScreenTex <> NULL Then glDeleteTextures 1, @ScreenTex: ScreenTex = NULL
End Sub

Sub ProgIO.EnQueueInKey(KeyStr As String)
    InKeyBufferSize += 1
    If InKeyBufferSize > UBound(InKeyBuffer) + 1 Then InKeyBufferSize = UBound(InKeyBuffer) + 1
    
    InKeyBuffer(InKeyBufferTop) = KeyStr
    InKeyBufferTop = (InKeyBufferTop + 1) Mod (UBound(InKeyBuffer) + 1)
    KeyPressSinceSleep = -1
End Sub

Destructor ProgIO
    EndRun
End Destructor

Sub StrDelete CDecl (S As String)
    fb_StrDelete S
End Sub

Function StrInit CDecl (S As ZString Ptr) As String
    Return *S
End Function

Sub StrAssign CDecl (Lhs As String, Rhs As String)
    Lhs = Rhs
End Sub

Function StrCompare CDecl (Lhs As String, Rhs As String) As Integer
    Return Lhs = Rhs
End Function

Function StrConcat CDecl (Lhs As String, Rhs As String) As String
    Return Lhs & Rhs
End Function

Function __Len CDecl (S As String) As Integer
    Return Len(S)
End Function

Function __Left CDecl (S As String, N As Integer) As String
    Return Left(S, N)
End Function

Function __Right CDecl (S As String, N As Integer) As String
    Return Right(S, N)
End Function

Function __Mid CDecl (S As String, Start As Integer, N As Integer) As String
    Return Mid(S, Start, N)
End Function

Function __Val CDecl (S As String) As Integer
    Return Val(S)
End Function

Function __Str CDecl (Num As Double) As String
    Return Str(Num)
End Function

Function IntDiv CDecl (IO As ProgIO, Lhs As Integer, Rhs As Integer, Location As ZString Ptr) As Integer
    If Rhs = 0 Then 'Make Player program crash
        'PlayerThreadAbort IO, "Aborting due to runtime error 11 (""floating point error"" signal) in " & *Location
        PlayerThreadAbort IO, "Aborting due to runtime error 11 (integer division by zero) at line " & *Location
    End If
    Return Lhs \ Rhs
End Function

Function Pow CDecl (Lhs As Double, Rhs As Double) As Double
    Return Lhs ^ Rhs
End Function

Sub __ReDim CDecl (IO As ProgIO, Array() As Any Ptr, ElementSize As Integer, NewUBound As Integer, Location As ZString Ptr)
    If NewUBound < 0 Then 'Crash Player Program
        PlayerThreadAbort IO, "Aborting due to runtime error 1 (illegal function call) at line " & *Location
    End If
    fb_ArrayRedimEx Array(), ElementSize, -1, 0, 1, 0, NewUBound
End Sub

Sub __ReDim_Preserve CDecl (IO As ProgIO, Array() As Any Ptr, ElementSize As Integer, NewUBound As Integer, Location As ZString Ptr)
    If NewUBound < 0 Then 'Crash Player Program
        PlayerThreadAbort IO, "Aborting due to runtime error 1 (illegal function call) at line " & *Location
    End If
    fb_ArrayRedimPresvEx Array(), ElementSize, -1, 0, 1, 0, NewUBound
End Sub

Function __UBound CDecl (Array() As Any Ptr) As Integer
    Return UBound(Array)
End Function

Sub __Erase CDecl (Array() As Any Ptr, IsVarLen As Integer)
    fb_ArrayErase Array(), IsVarLen 'Erase Array
End Sub

Function IntegerAA CDecl (IO As ProgIO, Array() As Integer, Index As Integer, Location As ZString Ptr) ByRef As Integer 'Ptr or ByRef?
    If Index > UBound(Array) Or Index < 0 Then 'Must Crash Player Program
        PlayerThreadAbort IO, "Aborting due to runtime error 6 (out of bounds array access) at line " & *Location
    End If
    Return Array(Index)
End Function

Function DoubleAA CDecl (IO As ProgIO, Array() As Double, Index As Integer, Location As ZString Ptr) As Double Ptr
    If Index > UBound(Array) Or Index < 0 Then 'Must Crash Player Program
        PlayerThreadAbort IO, "Aborting due to runtime error 6 (out of bounds array access) at line " & *Location
    End If
    Return @(Array(Index))
End Function

Function StringAA CDecl (IO As ProgIO, Array() As String, Index As Integer, Location As ZString Ptr) As String Ptr
    If Index > UBound(Array) Or Index < 0 Then 'Must Crash Player Program
        PlayerThreadAbort IO, "Aborting due to runtime error 6 (out of bounds array access) at line " & *Location
    End If
    Return @(Array(Index))
End Function

Sub __End CDecl (IO As ProgIO)
    IO.EndProgNow = -1
    ThreadFrameDone IO
End Sub

Function __InKey CDecl (IO As ProgIO) As String
    If IO.InKeyBufferSize = 0 Then Return ""
    
    IO.InKeyBufferSize -= 1    
    Return IO.InKeyBuffer((IO.InKeyBufferTop - IO.InKeyBufferSize + UBound(IO.InKeyBuffer)) Mod (UBound(IO.InKeyBuffer) + 1))
End Function

Function __MultiKey CDecl (IO As ProgIO, ScanCode As Integer) As Integer
    If IO.KeyDown = NULL Then Return 0
    Select Case As Const ScanCode 'Scan Code Translation
    Case SC_ESCAPE: Return -IO.KeyDown[SDL_SCANCODE_ESCAPE]
    Case SC_1: Return -IO.KeyDown[SDL_SCANCODE_1] Or -IO.KeyDown[SDL_SCANCODE_KP_1]
    Case SC_2: Return -IO.KeyDown[SDL_SCANCODE_2] Or -IO.KeyDown[SDL_SCANCODE_KP_2]
    Case SC_3: Return -IO.KeyDown[SDL_SCANCODE_3] Or -IO.KeyDown[SDL_SCANCODE_KP_3]
    Case SC_4: Return -IO.KeyDown[SDL_SCANCODE_4] Or -IO.KeyDown[SDL_SCANCODE_KP_4]
    Case SC_5: Return -IO.KeyDown[SDL_SCANCODE_5] Or -IO.KeyDown[SDL_SCANCODE_KP_5]
    Case SC_6: Return -IO.KeyDown[SDL_SCANCODE_6] Or -IO.KeyDown[SDL_SCANCODE_KP_6]
    Case SC_7: Return -IO.KeyDown[SDL_SCANCODE_7] Or -IO.KeyDown[SDL_SCANCODE_KP_7]
    Case SC_8: Return -IO.KeyDown[SDL_SCANCODE_8] Or -IO.KeyDown[SDL_SCANCODE_KP_8]
    Case SC_9: Return -IO.KeyDown[SDL_SCANCODE_9] Or -IO.KeyDown[SDL_SCANCODE_KP_9]
    Case SC_0: Return -IO.KeyDown[SDL_SCANCODE_0] Or -IO.KeyDown[SDL_SCANCODE_KP_0]
    Case SC_MINUS        : Return -IO.KeyDown[SDL_SCANCODE_MINUS]
    Case SC_EQUALS       : Return -IO.KeyDown[SDL_SCANCODE_EQUALS]
    Case SC_BACKSPACE    : Return -IO.KeyDown[SDL_SCANCODE_KP_BACKSPACE]
    Case SC_TAB          : Return -IO.KeyDown[SDL_SCANCODE_KP_TAB]
    Case SC_Q            : Return -IO.KeyDown[SDL_SCANCODE_Q]
    Case SC_W            : Return -IO.KeyDown[SDL_SCANCODE_W]
    Case SC_E            : Return -IO.KeyDown[SDL_SCANCODE_E]
    Case SC_R            : Return -IO.KeyDown[SDL_SCANCODE_R]
    Case SC_T            : Return -IO.KeyDown[SDL_SCANCODE_T]
    Case SC_Y            : Return -IO.KeyDown[SDL_SCANCODE_Y]
    Case SC_U            : Return -IO.KeyDown[SDL_SCANCODE_U]
    Case SC_I            : Return -IO.KeyDown[SDL_SCANCODE_I]
    Case SC_O            : Return -IO.KeyDown[SDL_SCANCODE_O]
    Case SC_P            : Return -IO.KeyDown[SDL_SCANCODE_P]
    Case SC_LEFTBRACKET  : Return -IO.KeyDown[SDL_SCANCODE_LEFTBRACKET]
    Case SC_RIGHTBRACKET : Return -IO.KeyDown[SDL_SCANCODE_RIGHTBRACKET]
    Case SC_ENTER        : Return -IO.KeyDown[SDL_SCANCODE_KP_ENTER]
    Case SC_CONTROL      : Return -IO.KeyDown[SDL_SCANCODE_LCTRL] Or -IO.KeyDown[SDL_SCANCODE_RCTRL]
    Case SC_A            : Return -IO.KeyDown[SDL_SCANCODE_A]
    Case SC_S            : Return -IO.KeyDown[SDL_SCANCODE_S]
    Case SC_D            : Return -IO.KeyDown[SDL_SCANCODE_D]
    Case SC_F            : Return -IO.KeyDown[SDL_SCANCODE_F]
    Case SC_G            : Return -IO.KeyDown[SDL_SCANCODE_G]
    Case SC_H            : Return -IO.KeyDown[SDL_SCANCODE_H]
    Case SC_J            : Return -IO.KeyDown[SDL_SCANCODE_J]
    Case SC_K            : Return -IO.KeyDown[SDL_SCANCODE_K]
    Case SC_L            : Return -IO.KeyDown[SDL_SCANCODE_L]
    Case SC_SEMICOLON    : Return -IO.KeyDown[SDL_SCANCODE_SEMICOLON]
    Case SC_QUOTE        : Return -IO.KeyDown[SDL_SCANCODE_APOSTROPHE]
    Case SC_TILDE        : Return -IO.KeyDown[SDL_SCANCODE_GRAVE]
    Case SC_LSHIFT       : Return -IO.KeyDown[SDL_SCANCODE_LSHIFT]
    Case SC_BACKSLASH    : Return -IO.KeyDown[SDL_SCANCODE_BACKSLASH]
    Case SC_Z            : Return -IO.KeyDown[SDL_SCANCODE_Z]
    Case SC_X            : Return -IO.KeyDown[SDL_SCANCODE_X]
    Case SC_C            : Return -IO.KeyDown[SDL_SCANCODE_C]
    Case SC_V            : Return -IO.KeyDown[SDL_SCANCODE_V]
    Case SC_B            : Return -IO.KeyDown[SDL_SCANCODE_B]
    Case SC_N            : Return -IO.KeyDown[SDL_SCANCODE_N]
    Case SC_M            : Return -IO.KeyDown[SDL_SCANCODE_M]
    Case SC_COMMA        : Return -IO.KeyDown[SDL_SCANCODE_COMMA] 'Or -IO.KeyDown[SDL_SCANCODE_KP_COMMA]
    Case SC_PERIOD       : Return -IO.KeyDown[SDL_SCANCODE_PERIOD] 'Or -IO.KeyDown[SDL_SCANCODE_KP_PERIOD]
    Case SC_SLASH        : Return -IO.KeyDown[SDL_SCANCODE_SLASH]
    Case SC_RSHIFT       : Return -IO.KeyDown[SDL_SCANCODE_RSHIFT]
    Case SC_MULTIPLY     : Return -IO.KeyDown[SDL_SCANCODE_KP_MULTIPLY]
    Case SC_ALT          : Return -IO.KeyDown[SDL_SCANCODE_LALT] Or -IO.KeyDown[SDL_SCANCODE_RALT]
    Case SC_SPACE        : Return -IO.KeyDown[SDL_SCANCODE_KP_SPACE]
    Case SC_CAPSLOCK     : Return -IO.KeyDown[SDL_SCANCODE_CAPSLOCK]
    Case SC_F1: Return -IO.KeyDown[SDL_SCANCODE_F1]
    Case SC_F2: Return -IO.KeyDown[SDL_SCANCODE_F2]
    Case SC_F3: Return -IO.KeyDown[SDL_SCANCODE_F3]
    Case SC_F4: Return -IO.KeyDown[SDL_SCANCODE_F4]
    Case SC_F5: Return -IO.KeyDown[SDL_SCANCODE_F5]
    Case SC_F6: Return -IO.KeyDown[SDL_SCANCODE_F6]
    Case SC_F7: Return -IO.KeyDown[SDL_SCANCODE_F7]
    Case SC_F8: Return -IO.KeyDown[SDL_SCANCODE_F8]
    Case SC_F9: Return -IO.KeyDown[SDL_SCANCODE_F9]
    Case SC_F10: Return -IO.KeyDown[SDL_SCANCODE_F10]
    Case SC_NUMLOCK: Return -IO.KeyDown[SDL_SCANCODE_NUMLOCKCLEAR]
    Case SC_SCROLLLOCK: Return -IO.KeyDown[SDL_SCANCODE_SCROLLLOCK]
    Case SC_HOME: Return -IO.KeyDown[SDL_SCANCODE_HOME]
    Case SC_UP: Return -IO.KeyDown[SDL_SCANCODE_UP]
    Case SC_PAGEUP: Return -IO.KeyDown[SDL_SCANCODE_PAGEUP]
    Case SC_LEFT: Return -IO.KeyDown[SDL_SCANCODE_LEFT]
    Case SC_RIGHT: Return -IO.KeyDown[SDL_SCANCODE_RIGHT]
    Case SC_PLUS: Return -IO.KeyDown[SDL_SCANCODE_KP_PLUS]
    Case SC_END: Return -IO.KeyDown[SDL_SCANCODE_END]
    Case SC_DOWN: Return -IO.KeyDown[SDL_SCANCODE_DOWN]
    Case SC_PAGEDOWN: Return -IO.KeyDown[SDL_SCANCODE_PAGEDOWN]
    Case SC_INSERT: Return -IO.KeyDown[SDL_SCANCODE_INSERT]
    Case SC_DELETE: Return -IO.KeyDown[SDL_SCANCODE_DELETE]
    Case SC_F11: Return -IO.KeyDown[SDL_SCANCODE_F11]
    Case SC_F12: Return -IO.KeyDown[SDL_SCANCODE_F12]
    Case SC_LWIN: Return -IO.KeyDown[SDL_SCANCODE_LGUI]
    Case SC_RWIN: Return -IO.KeyDown[SDL_SCANCODE_RGUI]
    Case SC_MENU: Return -IO.KeyDown[SDL_SCANCODE_MENU]
    Case Else: Return 0
    End Select
End Function

Function __GetJoystick CDecl (IO As ProgIO, ByVal ID As Integer, ByRef Buttons As Integer = 0, ByRef A1 As Double = 0, ByRef A2 As Double = 0, ByRef A3 As Double = 0, ByRef A4 As Double = 0, ByRef A5 As Double = 0, ByRef A6 As Double = 0, ByRef A7 As Double = 0, ByRef A8 As Double = 0) As Integer
    If ID < 0 Or ID > UBound(IO.JoyStick) Then Return 1
    If Not IO.JoyStick(ID).Attached Then Return 1
    
    If @Buttons <> NULL Then Buttons = IO.JoyStick(ID).Btn
    If @A1 <> NULL Then A1 = IO.JoyStick(ID).Axis(0)
    If @A2 <> NULL Then A2 = IO.JoyStick(ID).Axis(1)
    If @A3 <> NULL Then A3 = IO.JoyStick(ID).Axis(2)
    If @A4 <> NULL Then A4 = IO.JoyStick(ID).Axis(3)
    If @A5 <> NULL Then A5 = IO.JoyStick(ID).Axis(4)
    If @A6 <> NULL Then A6 = IO.JoyStick(ID).Axis(5)
    If @A7 <> NULL Then A7 = IO.JoyStick(ID).Axis(6)
    If @A8 <> NULL Then A8 = IO.JoyStick(ID).Axis(7)
    
    Return 0
End Function

Function __GetMouse CDecl (IO As ProgIO, ByRef X As Integer, ByRef Y As Integer, ByRef Wheel As Integer = 0, ByRef Buttons As Integer = 0) As Integer
    X = IO.MouseX
    Y = IO.MouseY
    If @Wheel <> NULL Then Wheel = IO.MouseWheel
    If @Buttons <> NULL Then Buttons = IO.MouseButtons
    If IO.MouseX = -1 Then Return 1
    Return 0
End Function

'''''''''''
'Robot I/O'
'''''''''''
Function ReadSensor CDecl (IO As ProgIO, SensorIndex As Integer = 0, ByRef LookUpStr As String = "", ByRef X As Integer = 0, ByRef Y As Integer = 0, ByRef Z As Integer = 0) As Integer
    If SensorIndex < 0 Then Return 0
    
    Select Case SensorIndex
    Case 0: 'Directory
        Select Case X
        'Case 0: Info = "Sensor Directory"
        'Case 1: Info = "Equipment"
        'Case 2: Info = "Inventory"
        Case Else
        End Select
        
    Case 1: 'Equipment
    Case 2: 'Inventory
    Case Else: 'Other
    End Select
    
    Return 0 'Return number of things or magnitude of things detected
End Function

Sub BotGo CDecl (IO As ProgIO, Action As Integer = BotMoveTo, TargetTmp As String = "", X As Integer = 0, Y As Integer = 0, Z As Integer = 0)
    Var Target = TargetTmp
    
    If Action = BotSettings Then
        If X >= 0 And X <= UBound(IO.Settings) Then IO.Settings(X) = Y
       Else
        IO.NextAction = Action
        IO.ActionTarget = Target
        IO.ActionVec = Vec3I(X, Y, Z)
        
        ThreadFrameDone IO
    End If
End Sub

'BotUpgrades:
'Memory
'CPU
'Program Code
'Inventory Space
'Transmiter/Recever
'Sensors
'Weapons/Armor

Enum EquipmentType
    Weapon
    Armor
    Sensor
    Display
    CPU
    StackMemory
    HeapMemory
    VideoMemory
    ProgramCode
End Enum

Enum ItemType
    Equipment
    CodeOp
    ConsumableItem
    
End Enum

Type CodeItem
    As BASIC_Operation Op
    As BASIC_Expression FixedParams(Any)
End Type

'Can Equip:
'Weapons, Armor, Sensors
'DisplayScreen
'CPU
'Stack Memory Module, Heap Memory Module (for Arrays), Video Memory Module (for ImageBuffers)
'Completed Program Source Code

Declare Sub __Sleep CDecl (IO As ProgIO, ByVal Ammount As Integer = -1)
Sub SendMsg CDecl (IO As ProgIO, Msg As String, Channel As Integer = 10, TransmitTime As Integer = 16)
    'Var Msg = MsgTmp
    IO.OutMsg.MsgStr = Msg
    IO.OutMsg.Channel = Channel
    
    __Sleep IO, TransmitTime 'ThreadFrameDone IO
    
    IO.OutMsg.MsgStr = ""
End Sub

Function ReadMsg CDecl (IO As ProgIO, Msg As String, LowChannel As Integer = 10, HighChannel As Integer = 10) As Integer
    For I As Integer = 0 To UBound(IO.UnreadMsgIdx)
        Var C = IO.WS->MsgTrans(IO.UnreadMsgIdx(I)).Channel
        If C >= LowChannel And C <= HighChannel Then
            Msg = IO.WS->MsgTrans(IO.UnreadMsgIdx(I)).MsgStr
            
            Swap IO.UnreadMsgIdx(I), IO.UnreadMsgIdx(UBound(IO.UnreadMsgIdx)) 'Remove
            If UBound(IO.UnreadMsgIdx) > 0 Then
                ReDim Preserve IO.UnreadMsgIdx(UBound(IO.UnreadMsgIdx) - 1)
               Else
                Erase IO.UnreadMsgIdx
            End If
            
            Return C
        End If
    Next I
    Return 0 'No Message
End Function

Sub __Cls CDecl (IO As ProgIO)
    If IO.ScreenImg = NULL Then 'Clear Console Text
        For I As Integer = 0 To UBound(IO.ConsoleLine)
            IO.ConsoleLine(I) = ""
        Next I
        IO.ConCurX = 0
        IO.ConCurY = 0
       Else 'Clear Graphics Window
        Dim PixData As Byte Ptr
        Dim As Integer W, H, Pitch
        If 0 = ImageInfo(IO.ScreenImg, W, H, , Pitch, PixData) Then
            Clear *PixData, 0, H * Pitch 'Line IO.ScreenImg, (0, 0)-(W - 1, H - 1), 0, BF
            If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
        End If
    End If
End Sub

Sub __PSet CDecl (IO As ProgIO, ByVal Buffer As Image Ptr, X As Integer, Y As Integer, _Color As Integer)
    If Buffer = NULL Then Buffer = IO.ScreenImg
    If Buffer <> NULL Then PSet Buffer, (X, Y), _Color: If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
End Sub

Sub __Line CDecl (IO As ProgIO, ByVal Buffer As Image Ptr, X1 As Integer, Y1 As Integer, X2 As Integer, Y2 As Integer, _Color As Integer, ModeB_BF As Integer)
    If Buffer = NULL Then Buffer = IO.ScreenImg
    If Buffer <> NULL Then
        Select Case ModeB_BF
        Case 0: Line Buffer, (X1, Y1)-(X2, Y2), _Color
        Case 1: Line Buffer, (X1, Y1)-(X2, Y2), _Color, B
        Case 2: Line Buffer, (X1, Y1)-(X2, Y2), _Color, BF
        End Select
        If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
    End If
End Sub

Sub __Circle CDecl (IO As ProgIO, ByVal Buffer As Image Ptr, X As Integer, Y As Integer, R As Integer, _Color As Integer, Start As Double = 0, EndAng As Double = 6.283186, Aspect As Double = 1, ModeF As Integer)
    If Buffer = NULL Then Buffer = IO.ScreenImg
    If Buffer <> NULL Then
        Select Case ModeF
        Case 0: Circle Buffer, (X, Y), R, _Color, Start, EndAng, Aspect
        Case 1: Circle Buffer, (X, Y), R, _Color, Start, EndAng, Aspect, F
        End Select
        If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
    End If
End Sub

Sub __Draw_String CDecl (IO As ProgIO, ByVal Buffer As Image Ptr, X As Integer, Y As Integer, Text As String, _Color As Integer)
    If Buffer = NULL Then Buffer = IO.ScreenImg
    If Buffer <> NULL Then Draw String Buffer, (X, Y), Text, _Color: If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
End Sub

Sub __Get CDecl (IO As ProgIO, ByVal Src As Image Ptr, X1 As Integer, Y1 As Integer, X2 As Integer, Y2 As Integer, ByVal Dest As Image Ptr)
    If Src = NULL Then Src = IO.ScreenImg
    If Src <> NULL And Dest <> NULL Then Get Src, (X1, Y1)-(X2, Y2), Dest
End Sub

Sub __Put CDecl (IO As ProgIO, ByVal Target As Image Ptr, X As Integer, Y As Integer, ByVal Src As Image Ptr, Method As Integer)
    If Target = NULL Then Target = IO.ScreenImg
    If Target <> NULL And Src <> NULL Then
        Select Case As Const Method
        Case 0: Put Target, (X, Y), Src', XOr
        Case 1: Put Target, (X, Y), Src, PSet
        Case 2: Put Target, (X, Y), Src, Preset
        Case 3: Put Target, (X, Y), Src, Trans
        Case 4: Put Target, (X, Y), Src, And
        Case 5: Put Target, (X, Y), Src, Or
        Case 6: Put Target, (X, Y), Src, XOr
        Case 7: Put Target, (X, Y), Src, Alpha
        'Case 8: Put Target, (X, Y), Src, Add
        End Select
        If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
    End If
End Sub

Sub __ScreenLock CDecl (IO As ProgIO)
    If IO.ScreenLockCount = 0 And IO.ScreenUpdateFrame Then
        Dim As Integer W, H
        ImageInfo IO.ScreenImg, W, H
        If IO.ScreenImg2 <> NULL Then ImageDestroy IO.ScreenImg2
        IO.ScreenImg2 = ImageCreate(W, H, 32)
        Get IO.ScreenImg, (0, 0)-(W - 1, H - 1), IO.ScreenImg2
        IO.ScreenUpdateFrame = 0
    End If
    IO.ScreenLockCount += 1
End Sub

Sub __ScreenUnlock CDecl (IO As ProgIO)
    IO.ScreenLockCount -= 1
    If IO.ScreenLockCount < 0 Then IO.ScreenLockCount = 0
    If IO.ScreenLockCount = 0 Then IO.ScreenUpdateFrame = -1
End Sub

Sub __ScreenRes CDecl (IO As ProgIO, W As Integer, H As Integer)
    If IO.ScreenImg <> NULL Then ImageDestroy IO.ScreenImg
    IO.ScreenImg = ImageCreate(W, H, 0, 32) 'Faking: ScreenRes W, H, 32
    IO.ScreenUpdateFrame = -1
    IO.MouseWheel = 0
End Sub

Function __ImageCreate CDecl (IO As ProgIO, W As Integer, H As Integer, Col As Integer = 0) As Image Ptr
    Return ImageCreate(W, H, Col, 32)
End Function

Sub __ImageDestroy CDecl (IO As ProgIO, ByRef Im As Image Ptr)
    ImageDestroy Im: Im = NULL
End Sub

Sub __Locate CDecl (IO As ProgIO, Row As Integer = 0, Column As Integer = 0, State As Integer = -1)
    If Column > 0 And Column <= IO.ConsoleWidth Then IO.ConCurX = Column - 1
    If Row > 0 And Row <= UBound(IO.ConsoleLine) + 1 Then IO.ConCurY = Row - 1
    If State > -1 Then
        'Ignoring State for now, 0 is cursor off, 1 is blinking cursor on
    End If
End Sub

Sub AdvanceConsoleLine (IO As ProgIO)
    IO.ConCurY += 1
    If IO.ConCurY > UBound(IO.ConsoleLine) Then
        IO.ConCurY = UBound(IO.ConsoleLine)
        For I As Integer = 0 To UBound(IO.ConsoleLine) - 1
            IO.ConsoleLine(I) = IO.ConsoleLine(I + 1)
        Next I
        IO.ConsoleLine(IO.ConCurY) = Space(IO.ConsoleWidth)
       Else
        If IO.ConsoleLine(IO.ConCurY) = "" Then IO.ConsoleLine(IO.ConCurY) = Space(IO.ConsoleWidth)
    End If
End Sub

Sub __Print CDecl (IO As ProgIO, TextTmp As Const String, NoNewLine As Integer = 0)
    Var Text = TextTmp 'Prevent FB from deleting temporary string inputs prematurely by making an explicit copy
    'ConsolePrint Text
    'If IO.ScreenImg <> NULL Then
    '    Draw String IO.ScreenImg, (IO.ConCurX * 8, IO.ConCurY * 8), Text
    '    Exit Sub
    'End If
    
    If IO.ConsoleLine(IO.ConCurY) = "" Then IO.ConsoleLine(IO.ConCurY) = Space(IO.ConsoleWidth)
    Var L = Len(Text)
    Var K = IO.ConsoleWidth - IO.ConCurX
    If L >= K Then
        Mid(IO.ConsoleLine(IO.ConCurY), IO.ConCurX + 1) = Mid(Text, 1, K)
        AdvanceConsoleLine IO
        L -= K
        Do Until L < IO.ConsoleWidth
            IO.ConsoleLine(IO.ConCurY) = Mid(Text, Len(Text) - L + 1, IO.ConsoleWidth)
            L -= IO.ConsoleWidth
            AdvanceConsoleLine IO
        Loop
        IO.ConCurX = 0
    End If
    
    Mid(IO.ConsoleLine(IO.ConCurY), IO.ConCurX + 1) = Right(Text, L)
    
    If NoNewLine = 0 Then
        AdvanceConsoleLine IO
        IO.ConCurX = 0
       Else
        IO.ConCurX += L
    End If
End Sub

Function __RGB CDecl (R As Integer, G As Integer, B As Integer) As ULong
    Return RGB(R, G, B)
End Function

Function __Timer CDecl (IO As ProgIO) As Double
    Return IO.WS->Tick / 60 'SDL_GetTicks() / 1000 'Timer
End Function

Sub __Randomize CDecl (IO As ProgIO, ByVal Seed As Double) 'fb_Randomize(Seed, RND_MTWIST)
	Union UnionDtoI
        As Double D
        As UInteger<32> I(1)
    End Union
    Dim As UnionDtoI DtoI
    
    If Seed = -1.0 Then
        DtoI.D = Timer '__Timer(IO)
        Seed = CDbl(DtoI.I(0) XOr DtoI.I(1))
    End If
    
    IO.RndState(0) = CULng(Seed)
    For I As Integer = 1 To UBound(IO.RndState)
        IO.RndState(I) = (IO.RndState(I - 1) * 1664525) + 1013904223
    Next I
    IO.RndP = @IO.RndState(UBound(IO.RndState)) + 1
End Sub

'Mersene Twister Adapted from hRnd_MTWIST in math_rnd.c fbc rtlib source
Function __Rnd CDecl (IO As ProgIO, NumSeed As Single) As Double
    If NumSeed = 0.0 Then Return IO.LastRndNum
    
    Dim As UInteger<32> I, V, XOR_Mask(1) = { 0, &H9908B0DF }
    
    If IO.RndP = NULL Then __Randomize IO, 0.0 'initialize state starting with an initial seed
    
    If IO.RndP > @IO.RndState(UBound(IO.RndState)) Then
        /' generate another array of 624 numbers '/
        Const PERIOD = 397
        For I = 0 To UBound(IO.RndState) - PERIOD
            V = (IO.RndState(I) And &H80000000) Or (IO.RndState(I + 1) And &H7FFFFFFF)
            IO.RndState(I) = IO.RndState(I + PERIOD) XOr (V Shr 1) XOr XOR_Mask(V And &H1)
        Next I
        For I = I To UBound(IO.RndState) - 1
            V = (IO.RndState(I) And &H80000000) Or (IO.RndState(I + 1) And &H7FFFFFFF)
            IO.RndState(I) = IO.RndState(I + PERIOD - UBound(IO.RndState) - 1) XOr (V Shr 1) XOr XOR_Mask(V And &H1)
        Next I
        V = (IO.RndState(UBound(IO.RndState)) And &H80000000) Or (IO.RndState(0) And &H7FFFFFFF)
        IO.RndState(UBound(IO.RndState)) = IO.RndState(PERIOD - 1) XOr (V Shr 1) XOr XOR_Mask(V And &H1)
        IO.RndP = @IO.RndState(0)
    End If
    
    V = *IO.RndP: IO.RndP += 1
    V XOr= V Shr 11
    V XOr= (V Shl 7) And &H9D2C5680
    V XOr= (V Shl 15) And &HEFC60000
    V XOr= V Shr 18
    
    IO.LastRndNum = CDbl(V) / CDbl(4294967296ull)
    Return IO.LastRndNum
End Function

Sub __Sleep CDecl (IO As ProgIO, ByVal Ammount As Integer = -1)
    IO.KeyPressSinceSleep = 0
    If Ammount = -1 Then
        Do
            ThreadFrameDone IO
        Loop Until IO.KeyPressSinceSleep
       Else
        'Ammount += IO.TickTime * 16.66666666
        'Do
        '    ThreadFrameDone IO
        'Loop Until IO.TickTime * 16.66666666 > Ammount Or IO.KeyPressSinceSleep
        Do
            Ammount -= 17 'Each frame is 1/60th a simulation second
            ThreadFrameDone IO
            If Ammount <= 0 Or IO.KeyPressSinceSleep Then Exit Do
            Ammount -= 16' + IIf(3*Rnd < 2, 1, 0)
            ThreadFrameDone IO
            If Ammount <= 0 Or IO.KeyPressSinceSleep Then Exit Do
            Ammount -= 17
            ThreadFrameDone IO
        Loop Until Ammount <= 0 Or IO.KeyPressSinceSleep
    End If
End Sub

Function LeftPad CDecl (StTemp As String, Length As Integer, Ch As String) As String
    Var St = StTemp '#Inclib https://registry.npmjs.org/left-pad
    Return String(Length - Len(St), Ch) & St
End Function

Sub AddSymbols(State As TCCState Ptr)
    tcc_add_symbol State, "IntDiv", @IntDiv
    tcc_add_symbol State, "Pow", @Pow
    
    tcc_add_symbol State, "Abs", @__Abs
    tcc_add_symbol State, "Sgn", @__Sgn
    tcc_add_symbol State, "Int", @__Int
    tcc_add_symbol State, "Fix", @__Fix
    tcc_add_symbol State, "Sin", @__Sin
    tcc_add_symbol State, "Cos", @__Cos
    tcc_add_symbol State, "Tan", @__Tan
    tcc_add_symbol State, "Atn", @__Atn
    tcc_add_symbol State, "Exp", @__Exp
    tcc_add_symbol State, "Log", @__Log
    tcc_add_symbol State, "Sqr", @__Sqr
    
    tcc_add_symbol State, "InKey", @__InKey
    tcc_add_symbol State, "MultiKey", @__MultiKey
    tcc_add_symbol State, "GetJoystick", @__GetJoystick
    tcc_add_symbol State, "GetMouse", @__GetMouse
    
    tcc_add_symbol State, "End", @__End
    
    tcc_add_symbol State, "StrDelete", @StrDelete
    tcc_add_symbol State, "StrInit", @StrInit
    tcc_add_symbol State, "StrAssign", @StrAssign
    tcc_add_symbol State, "StrCompare", @StrCompare
    tcc_add_symbol State, "StrConcat", @StrConcat
    
    tcc_add_symbol State, "Len", @__Len
    tcc_add_symbol State, "Left", @__Left'Left
    tcc_add_symbol State, "Right", @__Right'Right
    tcc_add_symbol State, "Mid", @__Mid
    tcc_add_symbol State, "Val", @__Val'Val
    tcc_add_symbol State, "Str", @__Str
    
    tcc_add_symbol State, "ReDim", @__ReDim
    tcc_add_symbol State, "ReDim_Preserve", @__ReDim_Preserve
    tcc_add_symbol State, "UBound", @__UBound
    tcc_add_symbol State, "Erase", @__Erase
    tcc_add_symbol State, "IntegerAA", @IntegerAA
    tcc_add_symbol State, "DoubleAA", @DoubleAA
    tcc_add_symbol State, "StringAA", @StringAA
    
    tcc_add_symbol State, "BotGo", @BotGo
    
    tcc_add_symbol State, "Cls", @__Cls
    tcc_add_symbol State, "PSet", @__PSet
    tcc_add_symbol State, "Line", @__Line
    tcc_add_symbol State, "Circle", @__Circle
    tcc_add_symbol State, "Draw_String", @__Draw_String
    tcc_add_symbol State, "Get", @__Get
    tcc_add_symbol State, "Put", @__Put
    tcc_add_symbol State, "ScreenLock", @__ScreenLock
    tcc_add_symbol State, "ScreenUnlock", @__ScreenUnlock
    
    tcc_add_symbol State, "ScreenRes", @__ScreenRes
    tcc_add_symbol State, "ImageCreate", @__ImageCreate
    tcc_add_symbol State, "ImageDestroy", @__ImageDestroy
    
    tcc_add_symbol State, "Locate", @__Locate
    tcc_add_symbol State, "Print", @__Print
    
    tcc_add_symbol State, "RGB", @__RGB
    tcc_add_symbol State, "Timer", @__Timer
    tcc_add_symbol State, "Randomize", @__Randomize
    tcc_add_symbol State, "Rnd", @__Rnd
    tcc_add_symbol State, "Sleep", @__Sleep
    
    tcc_add_symbol State, "LeftPad", @LeftPad
    
    tcc_add_symbol State, "ThreadFrameDone", @ThreadFrameDone
    'tcc_add_symbol State, "IO", @RunProgIO(I)
End Sub

Sub BuildUnit.Build
    If BuildState <> NULL Then tcc_delete BuildState 'Setup
    BuildState = tcc_new
    tcc_add_library_path BuildState, ExePath
    
    tcc_set_output_type BuildState, TCC_OUTPUT_MEMORY 'Compile
    If tcc_compile_string(BuildState, EmitCHeader & EmitProgC(Code)) = -1 Then ConsolePrint "Error: tcc_compile_string !": Sleep: End
    
    AddSymbols BuildState 'Link
    If tcc_relocate(BuildState, TCC_RELOCATE_AUTO) < 0 Then ConsolePrint "Error: tcc_relocate !": Sleep: End
    
    Init_Globals = tcc_get_symbol(BuildState, "Init_Globals")
    EntryPoint = tcc_get_symbol(BuildState, "Run")
    Destroy_Globals = tcc_get_symbol(BuildState, "Destroy_Globals")
    If Init_Globals = NULL Or EntryPoint = NULL Or Destroy_Globals = NULL Then ConsolePrint "Error: tcc_get_symbol !": Sleep: End
End Sub

Destructor BuildUnit
    'Calling tcc_delete causes an unusual crash on exit and sometimes the message:
    'Aborting due to runtime error 12 ("segmentation violation" signal) in D:\Dev\FreeBasic\PoL\Src\modBASIC.bi::()
    
    'If BuildState <> NULL Then tcc_delete BuildState: BuildState = NULL
    Init_Globals = NULL
    EntryPoint = NULL
    Destroy_Globals = NULL
End Destructor

Declare Sub GenPongProg
Declare Sub GenDefPlayerProg

Sub MakeDefaultPlayerProg(WS As WorldState)
    ReDim WS.ProgUnit(0)
    ReDim WS.RunProgIO(0)
    CurProg = @WS.ProgUnit(0).Code
    
    'GenPongProg
    GenDefPlayerProg
    
    ConsolePrint EmitProgFB(WS.ProgUnit(0).Code)
    
    'ConsolePrint EmitCHeader
    'ConsolePrint EmitProgC(WS.ProgUnit(0).Code)
    'LineNumPrint EmitCHeader & EmitProgC(WS.ProgUnit(0).Code)
    
    WS.ProgUnit(0).Build
    'WS.ProgUnit(0).Build
    '
    'Exit Sub
    '
    'ThreadsInit
    '
    'WS.RunProgIO(0).RunStart WS.ProgUnit(0)
    '
    'ThreadsRun
    '
    'WS.RunProgIO(0).EndRun
    '
    'ThreadsCleanup
    '
    ''If WS.ProgUnit(0).BuildState <> NULL Then tcc_delete WS.ProgUnit(0).BuildState: WS.ProgUnit(0).BuildState = NULL
    ''WS.ProgUnit(0).Init_Globals = NULL
    ''WS.ProgUnit(0).EntryPoint = NULL
    ''WS.ProgUnit(0).Destroy_Globals = NULL
    ''WS.ProgUnit(0).Destructor
    '
    'WS.ProgUnit(0).Build
End Sub

Sub ThreadsInit
    NumThreadsAwake = 0
    NumLiveThreads = 0
    AwakeCountMutex = MutexCreate
    FrameAMutex = MutexCreate
    FrameBMutex = MutexCreate
    
    MutexLock FrameAMutex
    MutexLock FrameBMutex
End Sub

Sub ThreadsRun
    MutexUnlock FrameBMutex
    Do
        MutexLock AwakeCountMutex
        If NumThreadsAwake = 0 Then Exit Do
        MutexUnlock AwakeCountMutex
        Sleep 1
    Loop
    NumThreadsAwake = NumLiveThreads
    MutexUnlock AwakeCountMutex
    
    MutexLock FrameBMutex
    Swap FrameAMutex, FrameBMutex
End Sub

Sub ThreadsCleanup
    MutexUnlock FrameBMutex
    MutexUnlock FrameAMutex
    
    MutexDestroy FrameBMutex: FrameBMutex = NULL
    MutexDestroy FrameAMutex: FrameAMutex = NULL
    MutexDestroy AwakeCountMutex: AwakeCountMutex = NULL
End Sub