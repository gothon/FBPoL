
#Pragma Once

#Include "Inc/VoxelGFX.bi"

Enum BlockType
    BtAir = 0
    BtDirt = 1
    BtGrass = 2
    BtStone = 3
    BtSand = 4
    BtSnow = 5
    BtGreyBrick = 6
    BtRedBrick = 7
    BtWoodPlank = 8
    BtBlackness = 16
    BtBush = 17
    BtBerryBush = 18
    BtShortGrass = 19
    BtTallGrass = 20
    BtThickGrass = 21
    BtLadderX = 22
    BtLadderZ = 23
    BtTreeBase = 24
    BtTreeTrunk = 25
    BtTreeTop = 26
    BtTreeBranchZ = 40
    BtTreeBranchX = 41
End Enum

Declare Sub MakeTerrainTiles(VolTile() As Vox_Volume)
Declare Sub MakeSprites(VolSprite() As Vox_Volume)
Declare Sub MapGen(WorldMap() As UByte, Size As Vec3I)