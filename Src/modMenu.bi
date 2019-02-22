
#Pragma Once

#Include Once "GL/gl.bi"

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

Type FormattedText
    As Integer StyleIdx
    As String Text
End Type

Type FormattedLine
    As Integer W, H, Selectable = -1
    As FormattedText LineTxt(Any)
End Type

Type SuperMenuWidget
    Index As Integer
    As Integer X, Y, W, H, TopLine
    As ULong BackCol, BoarderCol, SelBoxCol
    As Integer Sel, Selected, MouseHov = -1, PrvMouseBtn = 0, ShowSelected = 0, Horizontal = 0, Enabled = -1
    TextStyles(Any) As TextFormat
    'TitleText As FormattedText
    OptionText(Any) As FormattedLine
    
    Declare Sub SetText(Text As String)
    Declare Sub Render()
End Type

Type GUI_State
    As Integer FocusIndex, TabStops, MX, MY, MS, MB
    InK As String
End Type

'Type Menu
'    Index As Integer
'    As Integer X, Y, Sel, Selected, MouseHov = -1, PrvMouseBtn = 0, ShowSelected = 0
'    TitleText As String
'    OptionText(Any) As String
'End Type

'Default Color Scheme?
'  +, - purple (black)
'  () - green (black)
'  Comment - grey (green)
'  constant literals - red (black)
'  keywords - blue
'  identifiers - purple

' Features Needed:
'   multiple menus
'   horizontal menus
'   scrolling menus
'   menus w/ dynamic insertion and text entry