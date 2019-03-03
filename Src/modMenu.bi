
#Pragma Once

#Include Once "GL/gl.bi"

#Include "modBASIC.bi"

Enum TextStyle
    Bold = 1
    Italic = 2
    Underline = 4
    Strikethrough = 8
End Enum

Type TextFormat
    As ULong Col, Size = 1
    As TextStyle Style
End Type

Type StyleRun
    As Integer StyleIdx, RunLen
End Type

Type FormattedLine
    As Integer W, H, Selectable = -1
    As StyleRun SR(Any)
    As String Text
    
    Declare Sub AddText(St As String, Style As Integer = 0)
End Type

Enum ButtonEvent
    BtnNone
    BtnUp
    BtnDown
    BtnPgUp
    BtnPgDown
    BtnLeft
    BtnRight
    BtnSelect
    BtnBack
    BtnMouseClick
    BtnKeyTyped
End Enum

Type GUI_State
    As Integer FocusIndex, TabStops, MX, MY, MS, MB
    InK As String
End Type

Type SuperMenuWidget
    Index As Integer
    As Integer X, Y, W, H, TopLine
    As ULong BackCol = RGB(255, 255, 255), BoarderCol = RGB(128, 128, 128), SelBoxCol = RGB(200, 64, 64)
    As Integer Sel, Selected, MouseHov = -1, PrvMouseBtn = 0, ShowSelected = 0, Horizontal = 0, Enabled = -1, ParentIdx = -1
    TextStyles(Any) As TextFormat
    OptionText(Any) As FormattedLine
    
    Declare Sub Render
    Declare Sub DefaultColorCodingStyles
    Declare Sub DoInput(BtnEvt As ButtonEvent, GUIS As GUI_State)
End Type

Declare Function LineFromCode(LineNum As Integer, LenLineNums As Integer, CodeLine As BASIC_LineOfCodeAst) As FormattedLine

'Keyboard Mapping
'  Tab        - Change Focus (to another MenuWidget)
'  Backspace  - Menu Back (Only when a Menu is Active and not Editing Text) 
'  Esc        - Menu Back (brings up Main Menu when no Menu is Active)
'  Enter      - Accept Selection
'  Arrow Keys - Change Selection
'  Page Up/Down - Scroll Page
'
'Joypad Mapping
'  Btn4 (XInput 'Y')    - Change Focus
'  Btn2 (XInput 'B')    - Menu Back (Only when a Menu is Active)
'  Btn7 (XInput 'Back') - Menu Back / Main Menu
'  Btn1 (XInput 'A')    - Accept Selection
'  Left Analogue & DPAD - Change Selection
'  + Btn3 (XInput 'X')  - Scroll Page
'
'Mouse
'  Click - Set Focus & Accept Selection
'  Hover - Highlight Click Preview
'  Click (on an Inactive Menu Area) - Menu Back
'  Drag Scrollbar - Scroll

' Features Needed:
'   multiple menus
'   horizontal menus
'   scrolling menus
'   menus w/ dynamic insertion and text entry
