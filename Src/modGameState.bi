
#Pragma Once

#Include "Inc/VoxelGFX.bi"
#Include "Src/modBASIC.bi"
#Include "Inc/libtcc.bi"

Enum BotAction
    BotNoAction
    BotSettings
    BotMoveTo
    BotStepTo
    BotTurn
    BotEquip
    BotAttack
    BotUseItem
    BotDropItem
    BotPickUpItem
End Enum

'Enum SensorType
'    STypeSensorDirectory
'    STypeEquipment
'    STypeInventory
'    STypeLocVisual
'    STypeProximity
'    STypeCompass
'End Enum

Type ProgIOPtr As ProgIO Ptr
Type WorldStatePtr As WorldState Ptr

Type ProgGlobals
    CodeToGo As Integer
    IO As ProgIOPtr
    '... PlayerGlobals ...
End Type

Type BuildUnit
    Code As BASIC_Program
    BuildState As TCCState Ptr
    Init_Globals As Function CDecl(G As ProgGlobals Ptr) As Integer
    EntryPoint As Sub CDecl(G As ProgGlobals Ptr)
    Destroy_Globals As Sub CDecl(G As ProgGlobals Ptr)
    
    Declare Sub Build
    
    Declare Destructor()
End Type

Type JoyData
    As Integer Btn, Attached, GamePadNum = -1
    As Single Axis(7)
End Type

Type BotMsg
    As String MsgStr
    As Integer Channel
End Type

Type ProgIO
    'Essential Stuff
    As ProgGlobals Ptr Globals
    As Sub CDecl(G As ProgGlobals Ptr) EntryPoint
    As Sub CDecl(G As ProgGlobals Ptr) Destroy_Globals
    As Any Ptr ThreadHandle
    As Integer EndProgNow
    
    'Output Stuff
    As String ConsoleLine(24) 'As ColorLine ConsoleLine(24)
    As Any Ptr ScreenImg, ScreenImg2 'fb.Image Ptr
    As Long ScreenTex 'GLuint
    As Integer ScreenLockCount, ScreenUpdateFrame, ConsoleWidth = 40
    
    As BotAction NextAction
    As Vec3I ActionVec
    As String ActionTarget
    As Integer Settings(3)
    As BotMsg OutMsg
    
    'Input Stuff
    As String * 3 InKeyBuffer(14)
    As Integer InKeyBufferSize, InKeyBufferTop, KeyPressSinceSleep
    As Const UByte Ptr KeyDown
    As Integer MouseX, MouseY, MouseWheel, MouseButtons
    AS JoyData JoyStick(15)
    As Integer UnreadMsgIdx(Any)
    
    'State Stuff
    As UInteger<32> RndState(623)
    As UInteger<32> Ptr RndP = 0
    As Double LastRndNum = 0.0
    As Integer ConCurX, ConCurY
    As WorldStatePtr WS
    
    As Integer EntIdx', GamePadNum
    'As ULongInt TickTime
    'As ULong CurColor
    'As Integer GrCurX, GrCurY', WndX, WndY
    
    'Procedures
    Declare Sub RunStart(BU As BuildUnit)
    Declare Sub EndRun
    Declare Sub EnQueueInKey(KeyStr As String)
    
    Declare Destructor()
End Type

Type Entity
    As Vec3I Posn, Facing, PrvPosn
    As Integer MoveSpeed, MoveDist, SpriteIdx, HP
    
    As Integer ProgIdx, IO_Idx
    As String EntName
End Type

Type ItemStack
    As Integer What, HowMany
End Type

Type PlayerInventory
    'CodeOps(Any) As ItemStack 'BASIC_Operation
    CodeOps(OpLastOperation - 1) As Integer
    
End Type

Type PlayerEquipment
    As Integer EqScreen, ProcessorSpeed
    
End Type

Type WorldState
    WorldMap(Any, Any, Any) As UByte
    
    'PlayerAgent(Any) As Entity
    'NPC(Any) As Entity
    
    'Bots(Any) As Entity
    
    Ent(Any) As Entity
    ProgUnit(Any) As BuildUnit
    RunProgIO(Any) As ProgIO
    
    MsgTrans(Any) As BotMsg
    
    Inventory(Any) As PlayerInventory
    
    As LongInt Tick, TickOfLastRender
End Type

Declare Sub MakeDefaultPlayerProg(WS As WorldState)

Declare Sub ThreadsInit
Declare Sub ThreadsRun
Declare Sub ThreadsCleanup