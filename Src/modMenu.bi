
#Pragma Once

#Include Once "GL/gl.bi"

Type SuperMenuWidget
    Index As Integer
    As Integer X, Y, Sel, Selected, MouseHov = -1, PrvMouseBtn = 0, ShowSelected = 0
    TitleText As String
    OptionText(Any) As String
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