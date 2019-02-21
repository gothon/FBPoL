
#Pragma Once

#Include Once "GL/gl.bi"

Enum TextStyle
    Bold
    Italic
    Underline
    Strikethrough
End Enum

Type FormattedText
    As String Text
    As ULong ForeCol, BackCol, Size = 1
    As TextStyle Style
End Type

Type FormattedLine
    As FormattedText LineTxt(Any)
End Type

Type SuperMenuWidget
    Index As Integer
    As Integer X, Y, Sel, Selected, MouseHov = -1, PrvMouseBtn = 0, ShowSelected = 0
    TitleText As FormattedText
    OptionText(Any) As FormattedLine
    
    Declare Sub SetText(Text As String)
    Declare Sub Render(X As Integer = 0, Y As Integer = 0, W As Integer = 0, H As Integer = 0, StartLine As Integer = 0)
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

' Features Needed:
'   multiple menus
'   horizontal menus
'   scrolling menus
'   menus w/ dynamic insertion and text entry