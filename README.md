FB Programs of Legend
=====================

FB Programs of Legend is a Robot Programming Game made for the FBGD 2018-2019 Competition.

FBPoL robots are controled by player programs made in a system that imitates a small subset of the FreeBASIC language.
These programs are converted to C and compiled in memory by 'libtcc'.  They are then run in their own threads that get a
game controlled timeslice every frame.

Player Programs suppot 7 FreeBASIC data types:

    'Basic Types
    Integer
    Double
    String
    fb.Image Ptr 'For Graphics Commands
    
    'Arrays (Arrays are 1D, 0 based and Variable Length)
    Array of Integer
    Array of Double 
    Array of String

Player Programs can control their Robots using the 4 KeyWords:

    ReadSensor
    BotGo
    SendMsg
    ReadMsg

Additionally the Backend supports some functionality from the following FreeBASIC constructs:

    'Variables And Arrays
    Dim
    Dim Shared
    = (Assign)
    ReDim
    ReDim Preserve
    () 'ArrayAccess
    UBound
    Erase
    
    'Flow Control
    AndAlso
    OrElse
    If ... Then ... 'Single Line
    If ... Then 'Multi Line
    ElseIf ... Then
    Else
    End If
    Select Case ...
    Case ...
    Case Else
    End Select
    Do
    Do While ...
    Do Until ...
    Loop
    Loop While ...
    Loop Until ...
    For ... To ...
    For ... To ... Step
    Next
    GOTO ...
    
    'Procedures
    Sub
    Function
    Return
    End
    
    'Arithmetic
    +
    - (subtract)
    - (negate)
    *
    /
    \
    Mod
    ^
    
    'String
    &
    Len
    Left
    Right
    Mid
    Val
    Str
    
    'Comparison
    = (equality)
    <>
    >
    <
    >=
    <=
    
    'Logical
    And
    Or
    XOr
    Not
    
    'Math Functions
    Abs
    Sgn
    Int
    Fix
    Sin
    Cos
    Tan
    Atn
    Exp
    Log
    Sqr
    
    'Player Inputs
    InKey
    MultiKey
    GetJoystick
    GetMouse
    
    'Image Buffer Drawing Commands
    Cls
    PSet
    Line
    Circle
    Draw String
    Get
    Put
    ScreenLock
    ScreenUnlock
    
    'Image Buffer Management
    ScreenRes
    ImageCreate
    ImageDestroy
    
    'Console
    Locate
    Print
    
    'MISC
    RGB
    Timer
    Randomize  'Each Robot has its own sepreate Seed state
    Rnd
    Sleep


Licensing
=========

 FBPoL is licensed under the GNU GPLv2 or later.
 Copyright (C) 2018-2019 Alex Thomson