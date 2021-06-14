;;;; The header stuff is basically for the emulator

; INes 1.0
.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $02 ; 2 * 8KB CHR ROM
.byte %00010011 ; mapper and mirroring
;.byte $0000  
;.byte $00
;.byte $00
;.byte $00
.byte $00
.byte $00
.byte $00
.byte "<"
.byte "3Matt" ; filler bytes


.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Note = 2
    Fireball = 3
    PlayerTwoType = 4
    Eurydice = 5
    NoteStatic = 6
    UpButton = 7 
    DownButton = 8
    LeftButton = 9
    RightButton = 10
    AButton = 11
    BButton = 12
    Crystal = 13
    NESButtons = 14
.endscope

.scope GameMode
    Game = 0
    Sing = 1
    Title = 2
.endscope

.scope ChannelConst
    SquareOne = $00
    SquareTwo = $01 
    Triangle = $02 
    Noise = $03 
.endscope

.scope Stream
    MusicSquareOne = 0
    MusicSquareTwo = 1
    MusicTriangle = 2
    MusicNoise = 3 
    SfxOne = 4
    SfxTwo = 5
.endscope

.scope Octave
    A1 = $00
    AS1 = $01
    BB1 = $01
    B1 = $02

    C2 = $03
    CS2 = $04
    DB2 = $04 
    D2 = $05
    DS2 = $06
    EB2 = $06 
    E2 = $07
    F2 = $08
    FS2 = $09
    GB2 = $09
    G2 = $0A
    GS2 = $0B
    AB2 = $0B 
    A2 = $0C 
    AS2 = $0D 
    BB2 = $0D 
    B2 = $0E

    C3 = $0F
    CS3 = $10
    DB3 = $10 
    D3 = $11
    DS3 = $12
    EB3 = $12 
    E3 = $13
    F3 = $14
    FS3 = $15
    GB3 = $15
    G3 = $16
    GS3 = $17
    AB3 = $17 
    A3 = $18
    AS3 = $19 
    BB3 = $19 
    B3 = $1A

    C4 = $1B
    CS4 = $1C
    DB4 = $1C 
    D4 = $1D
    DS4 = $1E
    EB4 = $1E 
    E4 = $1F
    F4 = $20
    FS4 = $21
    GB4 = $21
    G4 = $22
    GS4 = $23
    AB4 = $23 
    A4 = $24 
    AS4 = $25 
    BB4 = $25 
    B4 = $26

    C5 = $27
    CS5 = $28
    DB5 = $28 
    D5 = $29
    DS5 = $2A
    EB5 = $2A 
    E5 = $2B
    F5 = $2C
    FS5 = $2D
    GB5 = $2D
    G5 = $2E
    GS5 = $2F
    AB5 = $2F 
    A5 = $30 
    AS5 = $31 
    BB5 = $31 
    B5 = $32

    C6 = $33
    CS6 = $34
    DB6 = $34 
    D6 = $35
    DS6 = $36
    EB6 = $36 
    E6 = $37
    F6 = $38
    FS6 = $39
    GB6 = $39
    G6 = $3A
    GS6 = $3B
    AB6 = $3B 
    A6 = $3C 
    AS6 = $3D 
    BB6 = $3D 
    B6 = $3E

    C7 = $3F
    CS7 = $40
    DB7 = $40 
    D7 = $41
    DS7 = $42
    EB7 = $42 
    E7 = $43
    F7 = $44
    FS7 = $45
    GB7 = $45
    G7 = $46
    GS7 = $47
    AB7 = $47 
    A7 = $48 
    AS7 = $49 
    BB7 = $49 
    B7 = $4A

    C8 = $4B
    CS8 = $4C
    DB8 = $4C 
    D8 = $4D
    DS8 = $4E
    EB8 = $4E 
    E8 = $4F
    F8 = $50
    FS8 = $51
    GB8 = $51
    G8 = $52
    GS8 = $53
    AB8 = $53 
    A8 = $54 
    AS8 = $55 
    BB8 = $55 
    B8 = $56

    C9 = $57
    CS9 = $58
    DB9 = $58 
    D9 = $59
    DS9 = $5A
    EB9 = $5A 
    E9 = $5B
    F9 = $5C
    FS9 = $5D
    GB9 = $5D

    rest = $5E
    
.endscope

.scope NoteLength 
    thirtysecond = $80
    sixteenth = $81
    eighth = $82
    quarter = $83
    half = $84
    whole = $85
    d_sixteenth = $86
    d_eighth = $87
    d_quarter = $88
    d_half = $89
    d_whole = $8A
.endscope 

.scope Envelope 
    staccato = $00 
    fadein = $01 
    blipecho = $02
.endscope 

.scope Opcodes
    EndSound = $A0 
    InfiniteLoop = $A1 
    ChangeEnvelope = $A2 
    ChangeDuty = $A3 
    Loop1Counter = $A4 
    Loop1 = $A5 
    SetNoteOffset = $A6 
    ChangeNoteOffset = $A7 
.endscope 

.scope MapTileNo
    MapZero = 0
    MapOne = 1
    MapTwo = 2
    MapThree = 3
    MapFour = 4
    MapFive = 5
    MapSix = 6
.endscope

; TO add to this also add references in: 
    ; INIT_ENTITIES, ClearEntity, CLEARENTITIES
    ; You MUST also change CLEARENTITIES inx to match
.struct Entity
    type .byte
    xpos .byte
    ypos .byte 
    spriteno .byte
    palette .byte
    generalpurpose .byte 
.endstruct


;todo: entities currently cant be moved bc references to its abs loc in other places 
.segment "ZEROPAGE" ; LSB 0 - FF
;; Reserve memory for some specific things we need not to be futzed with
    seed: .res 2 ; unused
    world: .res 2  ; used during startup, not after that
    buttons: .res 1 ; used for polling controller
    nmidone: .res 1 ; ppu is done when this is 1 ;
    framecount: .res 1 ;  This will increment once per frame and reset over 59 (0 counts)
    twocount: .res 1   ; flips every frame
    ScrollX: .res 1 ;   scroll L/R
    ScrollY: .res 1 ;   scroll u/d
    loadattributeflag: .res 1 ; 0 is right, 1 is left, not used atm
    ScrollLeftInProgress: .res 1 
    ScrollRightInProgress: .res 1 
    attributeaddress: .res 2 
    anioffset: .res 1 ; unused
    facing: .res 1 ; player facing direction determines sprite
    facingframe: .res 1 ; unused?
    playeraddress: .res 2 ; unused
    mapposindex: .res 1 ; unused
   ;NB DO NOT DELETE ANY OF THESE EVEN IF THEY AREN'T BEING USED IT WILL MESS UP THE ENTITY HANDLER ATM

    MAXENTITIES =20
     ; max allowed number of entities
    entities: .res .sizeof(Entity) * MAXENTITIES 
    TOTALENTITIES = .sizeof(Entity) * MAXENTITIES

    columnhigh: .res 1
    columnlow: .res 1
    columnnumber: .res 1
    Columnflag: .res 1
    waveflip: .res 1 ; this is usedfort not movement
    nextnote: .res 1 ; this is an offset that changes the spite of the not every time you create one
    thirtyframe: .res 1 ; resets every 30 frames
    fifteenframe: .res 1 ; resetsevery 15 frames
    ButtonFlag: .res 1 ; used in controls for releasing a held button
    temp: .res 1 ;
    temp2: .res 1 
    boxx1: .res 1   ; collision box stuff
    boxy1: .res 1
    boxx2: .res 1
    boxy2: .res 1 
    currenttable: .res 1 ; scrolling
    ; 0     1
    ; 2     3
    return: .res 1  ; 
    dxhigh: .res 1
    dxlow: .res 1
    pageX: .res 1
    pageY: .res 1
    DrawColumnFlag: .res 1
    DrawColumnLeftFlag: .res 1
    columnaddress: .res 2
    allowrightscroll: .res 1
    allowleftscroll: .res 1
    gamemode: .res 1
    noteflag: .res 1
    notecount: .res 1
    paletteaddress: .res 2
    updatebackgroundpaletteflag: .res 1
    notepalette: .res 1
    ScrollXEight: .res 1
    sounddisableflag: .res 1
    soundframecount: .res 1
    sfxplaying: .res 1
    sfxindex: .res 1
;    streamcurrentsound: .res 6
;    streamstatus: .res 6
;    streamchannel: .res 6
;    streampointerlow: .res 6
;    streampointerhigh: .res 6  
;    streamvolduty: .res 6
;    streamnotelow: .res 6
;    streamnotehigh: .res 6
;    streamtempo: .res 6 
;    streamtickertotal: .res 6
;    streamnotelengthcount: .res 6 
;    streamnotelength: .res 6
    soundpointer: .res 2
    soundtemp1: .res 1
    soundtemp2: .res 1
    soundsquare1old: .res 1
    soundsquare2old: .res 1
    jumppointer: .res 2 
    currentbank: .res 1
    collisionoffset: .res 1
    scrollinprogress: .res 1
    collisionflags: .res 1 ;
;; This tells the nes what to do when it starts up
;; We basically disable most things initially and initialise some others

.segment "STARTUP"
Reset:
    SEI ; Disables all interrupts
    CLD ; disable decimal mode

    ; Disable sound IRQ
    LDX #$40
    STX $4017

    ; Initialize the stack register
    LDX #$FF
    TXS

    INX ; #$FF + 1 => #$00

    ; Zero out the PPU registers
    STX $2000
    STX $2001

    STX $4010

:
    BIT $2002 ; this waits for a vblank
    BPL :-

    TXA

.segment "CODE"

;; This clears out the memory when we start up
CLEARMEM:
    STA $0000, X ; Zero page memory
    STA $0100, X 
    STA $0300, X
    STA $0400, X
    STA $0500, X
    STA $0600, X
    STA $0700, X
    LDA #$FF
    STA $0200, X ; Sprite/entity data goes here
    LDA #$00
    INX
    BNE CLEARMEM    ; Keep incrementing so you clear out the whole thing

; Clear out all of the entities
; The player is treated as a special case and handled seperately here.
INIT_ENTITIES:
    LDA #$05 
    STA entities+Entity::xpos
    LDA #$B3
    STA entities+Entity::ypos
    LDA #$01
    STA entities+Entity::type
    LDA #$10
    STA entities+Entity::spriteno
    ; Temp object for testing
    ;LDX #$04
    ;LDA #$04
    ;STA entities+Entity::xpos, X 
    ;LDA #$A3 
    ;STA entities+Entity::ypos, X
    ;LDA #$04 
    ;STA entities+Entity::type, X  
    ;LDA #$10 
    ;STA entities+Entity::spriteno

    LDX #.sizeof(Entity) 
    LDA #$FF

CLEARENTITIES:
    STA entities+Entity::xpos, X
    STA entities+Entity::ypos, X
    STA entities+Entity::spriteno, X 
    LDA #$00
    STA entities+Entity::type, X
    STA entities+Entity::palette, X
    LDA #$FF 
    ; NB YOU MUST INX THE SAME AS THE NUMBER OF ENTRIES IN THE 'Entity' STRUCTURE OR EVERYTHING WILL BREAK
    INX
    INX
    INX
    INX 
    INX
    INX 
    CPX #TOTALENTITIES
    BNE CLEARENTITIES


; wait for vblank. We want to wait for the system to do one scan of the screen before we do anthing else
:
    BIT $2002
    BPL :-

    LDA #$02
    STA $4014
    NOP

    ; $3F00
    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00

;; Increment through the Palette data and store it into the PPU
LoadPalettes:
    FillPaletteRam:
        LDA PaletteData, X
        STA CurrentBackgroundPalette, X 
        INX 
        CPX #$20 
        BNE FillPaletteRam
        LDX #$00 
    LoadtoPPU:
        LDA CurrentBackgroundPalette, X 
        STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
        INX
        CPX #$20
        BNE LoadtoPPU    
 
SetMirroring: ; Doesn't work atm? Not sure if the mapper is incorrectly set up
    ;LDA #$80
    ;STA $8000
    LDA #%10000000
    STA $8000



    LDA #%00000010  
    STA $8000
    LSR
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
    

InitWorld:
    LDA #< WorldData ; take the low byte
    STA world ; store low byte in z page
    LDA #> WorldData ; take the high byte
    STA world+1 ; store high into the world address +1 i.e the second byte of the address

; setup address in PPU for nametable data
    BIT $2002
    LDA #$20
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00
    LDY #$00


LoadWorld:
    LDA (world), Y
    STA $2007
    INY
    CPX #$03
    BNE :+
    CPY #$E0
    BEQ DoneLoadingWorld
:
    CPY #$00
    BNE LoadWorld
    INX
    INC world+1
    JMP LoadWorld

DoneLoadingWorld:
    LDX #$00



SetAttributes:
    LDX #$00
    LDA #$23
    STA $2006
    LDA #$C0
    STA $2006
    AttributeLoop:
    LDA AttributesDock, X 
    STA $2007
    INX
    CPX #$40
    BNE AttributeLoop

    LDX #$00
    LDY #$00    

SetPlayerPos: ; initial player position
    LDA #$9F
    STA entities+Entity::ypos
    LDA #$A0
    STA entities+Entity::xpos

    LDA #< WorldData ; take the low byte
    STA world ; store low byte in z page
    LDA #> WorldData ; take the high byte
    STA world+1 ; store high into the world address +1 i.e the second byte of the address

    LDA #< WorldData ; take the low byte
    STA columnaddress ; store low byte in z page
    LDA #> WorldData ; take the high byte
    STA columnaddress+1 ; store high into the world address +1 i.e the second byte of the address

InitSeed: 
    LDA #$01 
    STA seed 
    STA seed+1
; map some memory baabbbby!
; All sprite data is to be stored here and retrieved every frame
; Sprite buffer takes 4x64 = 256 bytes
SpriteBuffer = $0200 ;$0200 -> $02FF
TileBuffer = $0300 ; $0300 -> 031F
PaletteBufferBackground = $0320 ; 03320 ->032F
PaletteBufferSprites =  $0330 ; -> 033f
ApuBuffer = $0340 ; 0340 -> 034F
streamcurrentsound = $0350 ; 0350 -> 0355
streamstatus = $0356 ; 0356 -> 035B 
streamchannel = $035C ; 035C -> 0361
streampointerlow = $0362 ; 0362 -> 0367
streampointerhigh = $0368 ; 0368 -> 036D
streamvolduty  = $036E ; 036E -> 0373
streamnotelow = $0374 ; 0374 -> 0379 
streamnotehigh = $037A ; 037A -> 037F
streamtempo = $0380 ; -> 0385
streamtickertotal = $0386 ; -> 038B
streamnotelengthcount = $038C ; -> 0391
streamnotelength = $0392 ; -> 0397
streamvolumeenvelope = $0398 ; -> 039D
streamvolumeenvelopeindex = $039E ; -> 03A3
streamloop1 = $03A4 ; -> 03A9
streamnoteoffset = $03AA ; -> 03AF 
TileBuffer2 = $03B0 ; -> 03CF
CollisionMap = $03D0 ; -> 04BF 240 bytes
NoteInputMem = $0400 ; dont delete this 
CurrentBackgroundPalette = $04C0 ; -> 04CF
CurrentSpritePalette = $03F0

FillTileBuffer:
LDA #$22
LDX #$00
    BufferLoop:
    STA TileBuffer, X 
    INX 
    CPX #$1F 
    BEQ :+
    JMP BufferLoop
:

LDA #$20 ; put this somewhere else? 
STA nextnote

LDA #$10
STA ScrollXEight

;Set movespeed
LDA #$FF
STA dxhigh
LDA #$00
STA dxlow  

;Set Zeropage variables
LDA #$01
STA pageY
LDA #$00
STA pageX

; Reset the nametable address 

; Enable the apu
JSR SoundInit
LDA #$00 ; load song #x
JSR SoundLoad

; Set Control

LDA #%00000011

STA $8000
LSR 
STA $8000
LSR 
STA $8000
LSR 
STA $8000
LSR 
STA $8000

; Set Bank
LDA #%00000010

STA $A000
LSR
STA $A000
LSR
STA $A000
LSR
STA $A000
LSR
STA $A000 

JSR CheckWorldMapRight

LDA #<LevelScreenDock
STA world
LDA #>LevelScreenDock+1
STA world+1

JSR ChangePaletteBlack
;JSR SpawnFourNotes
JSR ChangePaletteOrange
jSR SpawnEurydice
JSR SpawnCrystal
JSR SpawnNESButtons
LDA #$17
JSR SetBackGround
;JSR BrightenBackGroundPalette
;JSR BrightenSpritePalette
; Enable interrupts
    CLI

    LDA #%10010000 ; enable NMI change background to use second chr set of tiles ($1000)
    STA $2000
    ; Enabling sprites and background for left-most 8 pixels
    ; Enable sprites and background
    LDA #%00111110
    STA $2001


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    JSR DoGameLogic 
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR AlternateBanks
    ;JSR DoScroll       
    JSR OAMBuffer   ; Sprite data is written to the buffer here`
    JSR WriteToTileBuffer ; write to the tile buffer when scrolling

; Once the game logic loop is done, we hover here and wait for a vblank
; After a return from Vblank, we jump back to the logic loop    
IsVBlankDone:
    LDA nmidone
    CMP #$01
    BNE IsVBlankDone
    LDA #$00
    STA nmidone
    JMP Loop

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;; NMI Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Main loop that exectutes when the NMI interrupts. Here is where we want to do our drawing. All drawing must be done before the beam resets
MAINLOOP: 
    PHA 
    TXA 
    PHA 
    TYA 
    PHA

    LDA #$00
    STA $2001

    JSR DrawColumnNMI
    JSR ReadSprites ; Get the sprites from the sprite buffer and write them to the ppu  
    JSR ReadScroll  ; Send the current scroll to the ppu
    JSR UpdatePaletteNMI
    ;J SR LoadAttributesNMI

    LDA #%10010000
    ORA currenttable
    ORA #%00000100
    STA $2000

    LDA #%00111110
    STA $2001
    JSR SoundPlayFrame
    INC nmidone 

    PLA
    TAY 
    PLA 
    TAX 
    PLA 
    
    RTI

; Loading into 4014 automatically takes what you give as a high byte and writes 256 bytes to the PPU (so 02 == 0200 all thr way to FF)
; In a real nes this neads to be done every frame b/c dynamic ram degradation, its technically possible to avoid in some emulators, but best just to do it. 
ReadSprites:
    LDA #$00
    STA $2003
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display.
    STA $4014
    LDX #$00
RTS

ReadScroll:
    SetScroll:
    LDA $2002
    LDA ScrollX
    STA $2005
    LDA ScrollY
    STA $2005
RTS

DrawColumnNMI:
    ; Check if the flag has been set to draw a new column
    LDA scrollinprogress
    CMP #$01 
    BEQ :+
    JMP ColumnPPUCheck
    :
    ; Turn off the flag 
    LDA DrawColumnFlag
    EOR #$01
    STA DrawColumnFlag
    JMP SendColumnToPPU

    ColumnPPUCheck:
    LDA DrawColumnLeftFlag
    CMP #$01 
    BEQ:+
    RTS
    :

    LDA DrawColumnLeftFlag
    EOR #$01
    STA DrawColumnLeftFlag

    ; Grab the coords for where thecolumn needs to be drawn
    SendColumnToPPU:
    LDA $2002
    LDA columnhigh
    STA $2006
    LDA columnlow
    STA $2006
    LDX #$1E ; Loop equal to screenheight/8 
    LDY #$00
    DrawColumnNMILoop: ; Loop until 1 column is drawn
    LDA TileBuffer, X ; Grab the tile ids from the tilebuffer  
    STA $2007
    INY 
    DEX
    BNE DrawColumnNMILoop

    SendColumnToPPU2:
    LDA $2002
    LDA columnhigh
    STA $2006
    LDA columnlow
    CLC 
    ADC #$01
    STA $2006
    LDX #$1E ; Loop equal to screenheight/8 
    LDY #$00
    DrawColumnNMILoop2: ; Loop until 1 column is drawn
    LDA TileBuffer2, X ; Grab the tile ids from the tilebuffer  
    STA $2007
    INY 
    DEX
    BNE DrawColumnNMILoop2
RTS

    SendColumnToPPUHorizontal:
    LDA $2002
    LDA columnhigh
    STA $2006
    LDA columnlow
    STA $2006 

LoadAttributesNMI:
    LDA loadattributeflag
    BNE :+ 
    RTS 
    :
    LDA $2002
    LDA #$23 
    STA $2006 
    LDA #$C0 
    STA $2006

    LDA mapposindex
    CLC 
    ADC #$01 
    TAX 
    LDA WorldMap, X 
    ASL 
    TAX 
    LDA AttributeList, X
    STA world 
    LDA AttributeList+1, X 
    STA world+1 
    LDY #$00 
    LoadAttributesNMILoop: 
        LDA (world), Y
        STA $2007 
        INY 
        CPY #$40 
        BNE LoadAttributesNMILoop  
        LDA #$00
        STA loadattributeflag
RTS 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;   all the stuff in the main loop 
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DoGameLogic: 
    LDA scrollinprogress 
    BNE :+

    JSR DoWorldMap
    JSR CheckPlayerPosition 
    JSR WaveFlip    ; This simply flips between 1 and 0. Used for directional variance
    JSR NoteIndex   ; This changes the note sprite that will spawn
    JSR ReadButtons ; Duh
    JSR SpawnNote
    JSR XMovement ; the player movement
    JSR PlayerBoundary
    JSR ProcessEntities ; entity behaviour is handled here, the player has some special stuff elsewhere
    JMP EndDoGameLogic

    :
    JSR ScrollSingleScreen

EndDoGameLogic:
RTS

;;;; 
;RNG GENERATOR 
;;;;

Prng: 
    LDY #08     ; iteration count (generates 8 bits)
	LDA seed+0
:
	ASL        ; shift the register
	ROL seed+1
	BCC :+
	EOR #$39   ; apply XOR feedback whenever a 1 bit is shifted out
:
	DEY
	BNE :--
	STA seed+0
	CMP #00     ; reload flags
	RTS




;;;;;;;;;;
;; World Map functions go here
;;;;;;;;;;

; Get the player position in the world map
DoWorldMap:
    LDA pageY
    ASL 
    ASL 
    ASL
    CLC 
    ADC pageX
    STA mapposindex ; current plaer pos on the world map 
    
    CheckWorldMapRight:
    ADC #$01
    TAX
    LDA WorldMap, X 
    CMP #$FF 
    BNE :+
    LDA #$01
    STA allowrightscroll
    LDA #$00
    STA ScrollX
    JMP CheckWorldMapLeft
    :
    LDA #$00
    STA allowrightscroll

    CheckWorldMapLeft:
    LDA mapposindex 
    SEC 
    SBC #$01
    TAX 
    LDA WorldMap, X
    CMP #$FF 
    BNE :+
    LDA #$01
    STA allowleftscroll
    JMP EndDoWorldMap
    :
    LDA #$00
    STA allowleftscroll

           
EndDoWorldMap:
RTS   

GetMapPosRight:
    LDA mapposindex
    CLC 
    ADC #$01
    TAX 
    LDA WorldMap, X 
    ASL 
    TAX
    LDA ScreenList, X
    STA world
    LDA ScreenList+1, X 
    STA world+1
RTS

GetMapPosLeft: 
    LDA mapposindex
    SEC  
    SBC #$01 
    TAX 
    LDA WorldMap, X 
    ASL 
    TAX 
    LDA ScreenList, X 
    STA world 
    LDA ScreenList+1, X 
    STA world+1 
RTS

CheckPlayerPosition:
    CheckRightScroll:
    LDA entities+Entity::xpos 
    CMP #$F0 
    BCS ScrollRight2 
    JMP CheckLeftScroll
  
    ScrollRight2: 
        LDA allowrightscroll
        BEQ :+ 
        JMP EndCheckPlayerPosition
        :
        LDA #$20
        STA entities+Entity::xpos

        LDA #$01 
        STA scrollinprogress
        STA ScrollRightInProgress
        JSR SetDX0
    RTS 
        

    CheckLeftScroll:
    LDA entities+Entity::xpos 
    CMP #$10
    BCC ScrollLeft2
    JMP EndCheckPlayerPosition

    ScrollLeft2: 
        LDA allowleftscroll
        BEQ :+ 
        JMP EndCheckPlayerPosition
        :
        LDA #$E0 
        STA entities+Entity::xpos 

        LDA #$01 
        STA scrollinprogress
        STA ScrollLeftInProgress

        LDA #$10
        STA columnnumber
        JSR SetDX0
    RTS 

EndCheckPlayerPosition:
RTS 



; Wipe the sprite buffer
;;; TODO: Make buffer wiping dynamic, wastes cycles clearing empty space
ClearSpriteBuffer:
    LDX #$00
    LDA #$FF 
    ClearBufferLoop:
        STA $0200, X
        INX 
        CPX #$30
        BNE ClearBufferLoop 
    RTS

;This flips a byte back between 1/0
; I feel like this could be more efficient
WaveFlip:
    LDA thirtyframe ; this can be changed. Currently useful with 15,30,60 frame
    
    BEQ Flip 
    JMP EndFlip
    Flip:
    INC waveflip
    LDA waveflip
    CMP #$02
    BEQ ResetFlip
    JMP EndFlip
    ResetFlip:
    LDA #$00
    STA waveflip
    EndFlip:
RTS

NoteIndex: ; Change the next note sprite that will be spawned
    INC nextnote
    LDA nextnote
    CMP #$24
    BEQ ResetNextNote
    JMP EndNoteIndex
    ResetNextNote:
    LDA #$20
    STA nextnote
    EndNoteIndex:
RTS


;;;;;;;;;;;;;
;Input 
;;;;;;;;;;;;;
ReadButtons:
    ; Ping the address twice to get it ready to send buttons
    CLC
    LDA #$01
    STA $4016
    ;STA buttons         ; Put 1 into buttons so we can use it to manipulate the carry flag in 8 loops time
    ROR A
    STA $4016

    ;TODO make this a loop by slotting a one back into the carry flag after the 8th loop
     
    ButtonLoop:
    ; Ping the address to get A
    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons
    ; get B etc
    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons

    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons

    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons

    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons

    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons

    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons

    LDA $4016
    ROR A           ; Shift the A byte into the carry flag
    ROL buttons     ; Shift the A byte from the carry flag to the first position of Buttons





;;;;;;;;;;;;;;;; Controls for when player can walkaround
CheckA:
    LDA buttons 
    AND #%10000000 ; if the first nibble is set then a is pressed
    BEQ CheckARelease
    LDA ButtonFlag ; if the button is pressed, set this so that we can check release next frame
    ORA #$01
    STA ButtonFlag
    JSR InputA
    JMP CheckB

    CheckARelease: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA ButtonFlag
        AND #$01
        BEQ CheckB
        LDA ButtonFlag
        EOR #$01 
        STA ButtonFlag
       ; Any behaviour can go here and will happen when the button is released
        JSR InputARelease
        ;JSR ChangePaletteBlack
CheckB:

    LDA buttons 
    AND #%01000000
    BEQ CheckBRelease
    LDA ButtonFlag
    ORA #$02
    STA ButtonFlag
    JSR InputB
    JMP CheckSelect

    CheckBRelease:
        LDA ButtonFlag
        AND #$02
        BEQ CheckSelect
        LDA ButtonFlag
        EOR #$02 
        STA ButtonFlag
        JSR InputBRelease

CheckSelect:
    LDA buttons
    AND #%00100000
    BEQ CheckSelectRelease 
    LDA ButtonFlag
    ORA #$04 
    STA ButtonFlag
    JSR InputSelect
    JMP CheckStart

    CheckSelectRelease:
        LDA ButtonFlag
        AND #$04 
        BEQ CheckStart
        LDA ButtonFlag
        EOR #$04 
        STA ButtonFlag
        JSR InputSelectRelease

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckStartRelease
    LDA ButtonFlag
    ORA #$08
    STA ButtonFlag
    JSR InputStart
    JMP CheckUp

    CheckStartRelease:
        LDA ButtonFlag
        AND #$08 
        BEQ CheckUp
        LDA ButtonFlag
        EOR #$08 
        STA ButtonFlag
        JSR InputStartRelease

CheckUp:  
    LDA buttons
    AND #%00001000
    BEQ  CheckUpRelease
    LDA ButtonFlag
    ORA #$10
    STA ButtonFlag
    JSR InputUp
    JMP EndButtons 

    CheckUpRelease:
        LDA ButtonFlag
        AND #$10
        BEQ CheckDown
        LDA ButtonFlag 
        EOR #$10
        STA ButtonFlag
        JSR InputUpRelease
        
CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckDownRelease 
    LDA ButtonFlag 
    ORA #$20 
    STA ButtonFlag 
    JSR InputDown
    JMP EndButtons

    CheckDownRelease:
        LDA ButtonFlag
        AND #$20 
        BEQ CheckLeft 
        LDA ButtonFlag
        EOR #$20 
        STA ButtonFlag
        JSR InputDownRelease   

CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckLeftRelease
    LDA ButtonFlag
    ORA #$40 
    STA ButtonFlag 
    JSR InputLeft
    JMP EndButtons 

    CheckLeftRelease:
        LDA ButtonFlag
        AND #$40 
        BEQ CheckRight 
        LDA ButtonFlag
        EOR #$40 
        STA ButtonFlag
        JSR InputLeftRelease

CheckRight:

    LDA buttons
    AND #%00000001
    Bne CheckRightRelease
    LDA ButtonFlag 
    ORA #$80 
    STA ButtonFlag
    JSR InputRight

    CheckRightRelease:
        LDA ButtonFlag
        AND #$80
        BEQ EndButtons
        LDA ButtonFlag 
        EOR #$80 
        STA ButtonFlag
        JSR InputRightRelease
 
EndButtons:
RTS 
 
InputA:
    
RTS 

InputARelease:
    JSR SpawnNote
RTS

InputB:

RTS

InputBRelease:
    JSR ChangePaletteBlack
RTS

InputStart:
    JSR DarkenBackGroundPalette
RTS

InputStartRelease:
    JSR BrightenBackGroundPalette
RTS

InputSelect:

RTS

InputSelectRelease:
    LDA gamemode 
    EOR #$01 
    STA gamemode
RTS

InputUp:
    LDA gamemode
    BNE EndInputUp
    JSR CollideTestUp
    BEQ :+ 
    JSR SetDX0
    JMP EndInputUp
    :
    WalkUp:
        LDA entities+Entity::ypos
        SEC 
        SBC #$01
        STA entities+Entity::ypos
        LDA #$18
        STA entities+Entity::spriteno
EndInputUp:
RTS

InputUpRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteUp
    :
    LDA #$00 
    JSR SoundLoad 
RTS

InputDown:
    LDA gamemode 
     
    BNE EndInputDown
    JSR CollideTestDown
    BEQ :+ 
    JSR SetDX0
    JMP EndInputDown
    :
    WalkDown:
    LDA entities+Entity::ypos
    CLC
    ADC #$01 
    STA entities+Entity::ypos
    LDA #$10
    STA entities+Entity::spriteno
EndInputDown:    
RTS

InputDownRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteDown
    :
    LDA #$02
    JSR SoundLoad 
RTS

InputLeft:
    LDA gamemode 
     
    BNE EndInputLeft
    WalkLeft:
    JSR CollideTestLeft 
    BEQ :+ 
    JSR SetDX0
    JMP EndInputLeft
    :
    LDA dxlow
    SEC
    SBC PlayerSpeed
    STA dxlow
    LDA dxhigh
    SBC #$00
    STA dxhigh
    LDA #$00
    STA entities+Entity::spriteno
EndInputLeft:
RTS

InputLeftRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteLeft
    :
    LDA #$03
    JSR SoundLoad 
RTS

InputRight:
    LDA gamemode 
     
    BNE EndInputRight
    WalkRight:
    JSR CollideTestRight
    BEQ :+ 
    JSR SetDX0
    JMP EndInputRight
    :
    LDA dxlow
    CLC 
    ADC PlayerSpeed   
    STA dxlow 
    LDA dxhigh
    ADC #$00
    STA dxhigh
    LDA #$00
    STA entities+Entity::spriteno
    ;JSR CollideRight
EndInputRight:
RTS

InputRightRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteRight
    :
    LDA #$01 
    JSR SoundLoad 
RTS
;;;;;;
; Entity creation
;;;;;;
SpawnNESButtons:
    LDX #$00
    SpawnNESButtonsLoop:
        CPX #TOTALENTITIES
        BEQ EndNESButtonsSpawn

        LDA entities+Entity::type, X 
        CMP #EntityType::NoEntity
        BEQ AddNESButtons
        TXA 
        CLC
        ADC #.sizeof(Entity)
        TAX 
        JMP SpawnNESButtonsLoop
    AddNESButtons:
        LDA #$17
        STA entities+Entity::xpos, X
        LDA #$3B
        STA entities+Entity::ypos, X
        LDA #EntityType::NESButtons
        STA entities+Entity::type, X
        LDA #$3A
        STA entities+Entity::spriteno, X
        LDA #%00000010
        STA entities+Entity::palette, X
        JMP EndNESButtonsSpawn
EndNESButtonsSpawn:
RTS

SpawnCrystal: 
    LDX #$00
    CrystalLoop:
        CPX #TOTALENTITIES
        BEQ EndCrystalSpawn

        LDA entities+Entity::type, X 
        CMP #EntityType::NoEntity
        BEQ AddCrystal
        TXA 
        CLC
        ADC #.sizeof(Entity)
        TAX 
        JMP CrystalLoop
    AddCrystal:
        LDA #$77
        STA entities+Entity::xpos, X
        LDA #$4D 
        STA entities+Entity::ypos, X
        LDA #EntityType::Crystal
        STA entities+Entity::type, X
        LDA #$40
        STA entities+Entity::spriteno, X
        LDA #%00100001
        STA entities+Entity::palette, X
        JMP EndCrystalSpawn
EndCrystalSpawn:
RTS

SpawnEurydice:
    LDX #$00
    EurydiceLoop:
        CPX #TOTALENTITIES
        BEQ EndEurydiceSpawn

        LDA entities+Entity::type, X 
        CMP #EntityType::NoEntity
        BEQ AddEurydice
        TXA 
        CLC
        ADC #.sizeof(Entity)
        TAX 
        JMP EurydiceLoop
    AddEurydice:
        LDA entities+Entity::xpos 
        SEC 
        SBC #$08
        STA entities+Entity::xpos, X
        LDA entities+Entity::ypos 
        STA entities+Entity::ypos, X
        LDA #EntityType::Eurydice
        STA entities+Entity::type, X
        LDA #$0A
        STA entities+Entity::spriteno, X
        LDA #%00000001
        STA entities+Entity::palette, X
        JMP EndEurydiceSpawn
EndEurydiceSpawn:
    RTS

SpawnNote:
    LDX #$00
NoteLoop:
    CPX #TOTALENTITIES ; Check whether we're at the end of allowed entities
    BEQ EndNoteSpawn
; Checkif the current index has nothing in it   
    LDA entities+Entity::type, X 
    CMP #EntityType::NoEntity ; NO TYPE
    BEQ AddNote
    TXA 
    CLC 
    ADC #.sizeof(Entity) ; This adds to the X index so that we can keep looping through all entity memory
    TAX
    JMP NoteLoop

AddNote:
    LDA entities+Entity::xpos ; get player xpos
    CLC 
    ADC #$01 ;offset slightly the player pos
    STA entities+Entity::xpos, X ; set the new entity position
    LDA entities+Entity::ypos ; ditto for the y 
    SBC #$02
    STA entities+Entity::ypos, X
    LDA #$02 ; note type
    STA entities+Entity::type, X
    LDA nextnote
    STA entities+Entity::spriteno, X
    JSR Prng
    LSR 
    LSR 
    LSR 
    LSR 
    LSR 
    LSR 
    STA entities+Entity::palette, X
    JMP EndNoteSpawn 

EndNoteSpawn:
    RTS


SpawnFourNotes:
    LDX #$00
    LDY #$00
    STY notepalette

    LDA entities+Entity::xpos 
    SEC 
    SBC #$1B
    STA temp

    LDA entities+Entity::ypos
    SEC 
    SBC #$0A 
    STA temp2

    SpawnFourNotesLoop:
    CPX #TOTALENTITIES
    BEQ EndSpawnFourNotes
    LDA entities+Entity::type, X
    CMP #EntityType::NoEntity ; NO TYPE
    BEQ AddFourNotes
    TXA 
    CLC 
    ADC #.sizeof(Entity) ; This adds to the X index so that we can keep looping through all entity memory
    TAX 
    JMP SpawnFourNotesLoop

AddFourNotes:
    CPY #$02
    BCS AddFourNotes2

    TYA  
    ASL 
    ASL 
    ASL
    ASL
    ASL 
    CLC 
    ADC temp 
    STA entities+Entity::xpos, X 
    LDA entities+Entity::ypos ; ditto for the y 
    CLC 
    ADC #$04
    STA entities+Entity::ypos, X
    LDA #EntityType::NoteStatic ; note type
    STA entities+Entity::type, X
    TYA 
    CLC 
    ADC nextnote
    STA entities+Entity::spriteno, X 
    LDA notepalette
    STA entities+Entity::palette, X
    INY 
    JMP SpawnFourNotesLoop  

AddFourNotes2:
    LDA entities+Entity::xpos
    SEC 
    SBC #$0C ;;; TODO: WHY IS TIS NEEDED?
    STA entities+Entity::xpos, X
    
    
    TYA 
    SEC 
    SBC #$02
    ASL 
    ASL
    ASL 
    ASL 
    ASL 
    CLC 
    ADC temp2
    STA entities+Entity::ypos, X
    LDA #EntityType::NoteStatic
    STA entities+Entity::type, X 
    TYA  
    CLC 
    ADC nextnote
    STA entities+Entity::spriteno, X
    LDA notepalette
    STA entities+Entity::palette, X

    CPY #$03
    BEQ EndSpawnFourNotes
    INY  
    JMP SpawnFourNotesLoop



EndSpawnFourNotes:
RTS    

SpawnFire:
    LDX #$00
FireLoop:
    CPX #TOTALENTITIES ; Check whether we're at the end of allowed entities
    BEQ EndFireSpawn
; Check if the current index has nothing in it   
    LDA entities+Entity::type, X 
     ; NO TYPE
    BEQ AddFire
    TXA 
    CLC 
    ADC #.sizeof(Entity) ; This adds to the X index so that we can keep looping through all entity memory
    TAX
    JMP FireLoop

AddFire:
    LDA entities+Entity::xpos
    CLC 
    ADC #$10
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos
    CLC 
    ADC #$04        
    STA entities+Entity::ypos, X
    LDA #$03 ; fire type
    STA entities+Entity::type, X
    LDA #$26
    STA entities+Entity::spriteno, X 
    JMP EndFireSpawn 

EndFireSpawn:
    RTS

IncFrameCount: ; TODO why does this use x, just use a
    SixtyFrame:
        INC framecount
        LDX framecount
        INX
        CPX #$3B
        BNE ThirtyFrame
        LDA #$00
        STA framecount
    ThirtyFrame:
        INC thirtyframe
        LDX thirtyframe
        INX 
        CPX #$1E
        BNE FifteenFrame
        LDA #$00
        STA thirtyframe
    FifteenFrame:
        INC fifteenframe
        LDX fifteenframe
        INX 
        CPX #$0E
        BNE EndFrameCount
        LDA #$00
        STA fifteenframe
    EndFrameCount:

RTS

;;;;;;;;
;; entity processsing 
;;;;;;;;

ProcessEntities: ; TODO change this to a jump pointer  table 
    LDX #$00
    ProcessEntitiesLoop:
        LDA entities+Entity::type, X
        ASL 
        TAY  
        LDA ProcessEntityList, Y 
        STA jumppointer
        LDA ProcessEntityList+1, Y 
        STA jumppointer+1 
        JMP (jumppointer)

    ProcessPlayer:
        JMP EntityComplete

    ProcessUpButton:
        JMP EntityComplete

    ProcessDownButton:
        JMP EntityComplete

    ProcessLeftButton:
        JMP EntityComplete

    ProcessRightButton:
        JMP EntityComplete

    ProcessAButton:
        JMP EntityComplete

    ProcessBButton:
        JMP EntityComplete

    ProcessNote:
        NoteHorizontal:
        LDY entities+Entity::generalpurpose, X
        LDA NoteOffset, Y 
        CLC 
        ADC entities+Entity::xpos, X 
        STA entities+Entity::xpos, X 

        LDA entities+Entity::generalpurpose, X 
        CMP #$28

        BNE:+ 
        LDA #$00
        STA entities+Entity::generalpurpose, X
        : 
        INC entities+Entity::generalpurpose, X
 
         NoteVertical:
        LDA entities+Entity::ypos, X 
        SEC 
        SBC #$01
        STA entities+Entity::ypos, X
        LDA entities+Entity::ypos, X 
        CMP #$FE
        BNE EntityComplete
        JMP ClearEntity


    ProcessFire: ; move fire right
        LDA entities+Entity::xpos, X 
        CLC
        ADC #$01
        STA entities+Entity::xpos, X
        ;JSR PlayerCollide ; turned off for testing, does work
        CMP #$FF
        BCC EntityComplete ;BCC change
        JMP ClearEntity

    ProcessPlayerTwo:
        JMP EntityComplete

    ProcessEurydice:
        ProcessEurydiceX:
            LDA entities+Entity::xpos
            SEC 
            SBC #$10 
            STA entities+Entity::xpos, X 

        ProcessEurydiceY:
            LDA fifteenframe
            BEQ EntityComplete
            LDY entities+Entity::generalpurpose, X 
            LDA NoteOffset, Y 
            CLC 
            ADC entities+Entity::ypos, X
            STA entities+Entity::ypos, X
            TYA 
            BNE :+
            LDA #$28 
            STA entities+Entity::generalpurpose, X
            :
            DEC entities+Entity::generalpurpose, X
    JMP EntityComplete

    ProcessNoteStatic:
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$00
        STA entities+Entity::xpos, X
        JMP EntityComplete

    ProcessCrystal:
        ProcessCrystalY:
            LDA fifteenframe
            BNE :++
            LDY entities+Entity::generalpurpose, X 
            LDA CrystalOffset, Y 
            CLC 
            ADC entities+Entity::ypos, X
            STA entities+Entity::ypos, X
            TYA 
            BNE :+
            LDA #$0E
            STA entities+Entity::generalpurpose, X
            :
            DEC entities+Entity::generalpurpose, X
            :
    JMP EntityComplete

    ProcessNESButtons:
        JMP EntityComplete

    ClearEntity:
        LDA #EntityType::NoEntity
        STA entities+Entity::type, X
        LDA #$00
        STA entities+Entity::xpos, X
        STA entities+Entity::ypos, X
        STA entities+Entity::spriteno, X
 



    EntityComplete:
    SkipEntity:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #TOTALENTITIES  ;  Max entities?
    BEQ :+
    JMP ProcessEntitiesLoop
    :
    DoneProcessingEntities:
    NOP ;NOPE!
    NOP
    NOP
    NOP
RTS

; this attaches to an object and checks if that object is colliding with the player
PlayerCollide:
    LDA entities+Entity::xpos
    CLC 
    ADC #$0F 
    CMP entities+Entity::xpos, X 
    BCS:+
    RTS
    ;check 2 
    : LDA entities+Entity::xpos, X 
    CLC 
    ADC #$08
    CMP entities+Entity::xpos 
    BCS:+
    RTS
    ; check 3
    : LDA entities+Entity::ypos
    CLC 
    ADC #$0F 
    CMP entities+Entity::ypos, X 
    BCS:+
    RTS
    ; check 4
    : LDA entities+Entity::ypos, X 
    CLC 
    ADC #$08 
    CMP entities+Entity::ypos 
    BCS:+
    RTS
    :
    ; whatever is here happens when there is a collide
    LDA #EntityType::NoEntity
    STA entities+Entity::type
    LDA #$00 
    STA entities+Entity::xpos
    STA entities+Entity::ypos
    
    RTS 

DestroyPlayer: ; unused atm
    LDA #$00
    STA entities+Entity::xpos
    STA entities+Entity::ypos
    STA entities+Entity::type 
    STA entities+Entity::spriteno
RTS 

ScrollSingleScreen:
    LDA ScrollRightInProgress
    BNE ScrollSingleScreenRight
    LDA ScrollLeftInProgress
    BNE ScrollSingleScreenLeft
RTS

    ScrollSingleScreenRight:
    LDA ScrollX
    CLC 
    ADC #$10
    STA ScrollX   
    SEC 
    SBC #$10
    LSR 
    LSR 
    LSR 
    STA columnlow
    LDA #$20
    STA columnhigh

    LDA columnnumber
    CLC 
    ADC #$01
    STA columnnumber
RTS 
    
    ScrollSingleScreenLeft:
    LDA ScrollX 
    SEC 
    SBC #$10 
    STA ScrollX
    SEC 
    SBC #$10 
    LSR 
    LSR 
    LSR 
    STA columnlow
    LDA #$20 
    STA columnhigh

    LDA columnnumber
    SEC 
    SBC #$01 
    STA columnnumber


;;;;;;;;;
;;Collision madoodles
;;;;;;;

WriteToCollisionMap:
    JSR GetMapPosRight

    LDY #$00
    LDX #$00
    STY temp 
    WriteToCollisionMapLoop:
        LDA (world), Y
        STY temp
        TAX
        LDA CollisionList, X 
        STA CollisionMap, Y  
        INY 
        CPY #$F0 
        BEQ :+
        JMP WriteToCollisionMapLoop
        :
RTS 

; collide tests return 0 for no collide,1 for a hit
CollideTestRight:
    LDA entities+Entity::xpos
    CLC 
    ADC #$10
    LSR 
    LSR
    LSR
    LSR
    STA temp

    LDA entities+Entity::ypos
    LSR 
    LSR
    LSR
    LSR
    STA temp2

    ASL 
    ASL 
    ASL 
    ASL 
    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    JMP CollideResult

CollideTestLeft:
    LDA entities+Entity::xpos
    SEC 
    SBC #$01
    LSR 
    LSR
    LSR
    LSR
    STA temp

    LDA entities+Entity::ypos
    LSR 
    LSR
    LSR
    LSR
    STA temp2

    ASL 
    ASL 
    ASL 
    ASL 
    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    JMP CollideResult
CollideTestUp:
    LDA entities+Entity::xpos
    LSR 
    LSR
    LSR
    LSR
    STA temp

    LDA entities+Entity::ypos
    SEC 
    SBC #$01 
    LSR 
    LSR
    LSR
    LSR
    STA temp2

    ASL 
    ASL 
    ASL 
    ASL 
    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    JMP CollideResultY

CollideTestDown:
    LDA entities+Entity::xpos
    LSR 
    LSR
    LSR
    LSR
    STA temp

    LDA entities+Entity::ypos
    CLC 
    ADC #$10 
    LSR 
    LSR
    LSR
    LSR
    STA temp2

    ASL 
    ASL 
    ASL 
    ASL 
    CLC 
    ADC temp
    TAY 
    LDA CollisionMap, Y 
    JMP CollideResultY
CollideResult: 
    CMP #$01 
    BEQ :+
    LDA #$00
    STA return
RTS
    :
    LDA #$01 
    STA return 
RTS  

CollideResultY:
    CMP #$02 
    BEQ :+
    LDA #$01
    STA return
RTS 
    :
    LDA #$00
    STA return
RTS  

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; movement mechanics
;;;;;;;;;;;;;;;;;;;;;;;;;

XMovement:
    ; get movement
    ; apply friction
    ; if dx too high, limit it

    ; check for postive/negative
    LDA dxhigh
    
    BEQ EndXMovement

    BIT dxhigh
    BMI LimitXLeft 


    LimitXRight:
        JSR ApplyFriction
        LDA dxhigh
        CMP #$02 
        BCC :+
        LDA #$02
        STA dxhigh
        LDA #$00
        STA dxlow
        :
        JMP AddDx
    
    LimitXLeft:
        JSR ApplyFriction
        BCS AddDx ; stop a problem where dx goes positive then flips back to -2
        LDA dxhigh
        CMP #$FE 
        BCS :+
        LDA #$FE 
        STA dxhigh
        LDA #$00
        STA dxlow
        :

    AddDx:
        LDA entities+Entity::xpos
        CLC 
        ADC dxhigh
        STA entities+Entity::xpos 
 
EndXMovement:
RTS

ApplyFriction:
    BIT dxhigh 
    BMI FrictionP

FrictionN: ; going right    
    LDA dxlow
    SEC
    SBC FrictionValue
    STA dxlow
    BCS :+
    DEC dxhigh
    :
RTS 

FrictionP: ; going left

    LDA dxlow
    CLC 
    ADC FrictionValue
    STA dxlow
    LDA dxhigh
    ADC #$00
    STA dxhigh
RTS

SetDX0:
    LDA #$00 
    STA dxhigh
    STA dxlow 
RTS 

PlayerBoundary: 
    BoundRight:
        LDA allowrightscroll
        BEQ BoundLeft
        LDA entities+Entity::xpos 
        CMP #$F8
        BCC BoundLeft
        LDA #$F8
        STA entities+Entity::xpos
    BoundLeft:
        LDA allowleftscroll
        BEQ EndPlayerBoundary
        LDA entities+Entity::xpos 
        CMP #$02
        BCS EndPlayerBoundary
        LDA #$02
        STA entities+Entity::xpos
    EndPlayerBoundary:
RTS 

;;;;;;;;;;
; Stuff to do with singing
;;;;;;;;;;;;
InputNoteUp:
    LDA #$01
    LDX notecount
    STA NoteInputMem, X
    JMP CheckNoteMem 
InputNoteDown:
    LDA #$02
    LDX notecount
    STA NoteInputMem, X
    JMP CheckNoteMem
InputNoteLeft:
    LDA #$04
    LDX notecount
    STA NoteInputMem, X
    JMP CheckNoteMem
InputNoteRight:
    LDA #$08
    LDX notecount
    STA NoteInputMem, X
    JMP CheckNoteMem
CheckNoteMem:
    LDA notecount
    CMP #$03
    BEQ ProcessNoteMem
    INC notecount
    RTS
ProcessNoteMem:
    LDX #$00
    LDA NoteInputMem, X
    CMP #$01
    BNE ClearNoteMem
    INX 
    LDA NoteInputMem, X
    CMP #$02
    BNE ClearNoteMem
    INX
    LDA NoteInputMem, X
    CMP #$04
    BNE ClearNoteMem
    INX 
    LDA NoteInputMem, X
    CMP #$08
    BNE ClearNoteMem

NoteMemCheckComplete:
JSR ChangePaletteBlack

ClearNoteMem:
    LDA #$00
    STA notecount
EndNoteMem:
RTS

SingMode:
    LDA SingMode
    
    BNE :+
    RTS
    : 

    LDA noteflag
    
    BNE :+
    RTS
    :
RTS

;;;;
; Bank Switching
;;;;

AlternateBanks:
    LDA thirtyframe
    BEQ :+
    RTS
    :
    LDA currentbank
    EOR #$02 
    STA currentbank 
    JSR SetBank
    RTS

MapperControl: ; Takes value of A and uses shift register to set control

; Lower 5 bytes == CHR Bank Mode -> PRG Bank Mode x 2 -> Mirroring x2

    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
    LSR 
    STA $8000
RTS
; Set Bank

SetBank: ; sets A as the bank to be used
; lower 5 bits. lowest ignored in 8kb mode

    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
    LSR
    STA $A000
RTS 


;;;;;;
; Palette functions
;;;;;
ChangePaletteBlack:
    LDA #< BackGroundPaletteBlack
    STA paletteaddress
    LDA #> BackGroundPaletteBlack
    STA paletteaddress+1
    JMP UpdatePaletteBuffer

ChangePaletteOrange: 
    LDA #< PaletteData
    STA paletteaddress
    LDA #> PaletteData
    STA paletteaddress+1
    JMP UpdatePaletteBuffer


UpdatePaletteBuffer:
    LDA updatebackgroundpaletteflag
    EOR #$01
    STA updatebackgroundpaletteflag

    LDY #$00
    UpdatePaletteBufferLoop:
    LDA (paletteaddress), Y
    STA PaletteBufferBackground, Y 
    CPY #$20
    BEQ :+
    INY
    JMP UpdatePaletteBufferLoop
    : 
RTS

DarkenBackGroundPalette:
    LDX #$00
    DarkenBackGroundPaletteLoop:
        LDA PaletteBufferBackground, X 
        SEC 
        SBC #$10 
        BPL :+ 
        LDA #$0F
        :
        STA PaletteBufferBackground, X 
        INX 
        CPX #$10
        BNE DarkenBackGroundPaletteLoop
EndDarkenBackGroundPalette:
    LDA #$01 
    STA updatebackgroundpaletteflag
RTS 

SetBackGround: ; Sets background to val of A
    STA PaletteBufferBackground
    STA PaletteBufferSprites

    LDA #$01 
    STA updatebackgroundpaletteflag
RTS 

BrightenBackGroundPalette:
    LDX #$00
    BrightenBackGroundPaletteLoop:
        LDA PaletteBufferBackground, X 
        CLC
        ADC #$10 
        BPL :+ 
        LDA #$0F
        :
        STA PaletteBufferBackground, X 
        INX 
        CPX #$10
        BNE BrightenBackGroundPaletteLoop
EndBrightenBackGroundPalette:
    LDA #$01 
    STA updatebackgroundpaletteflag
RTS


BrightenSpritePalette:
    LDX #$00
    BrightenSpritePaletteLoop:
        LDA PaletteBufferSprites, X 
        CLC
        ADC #$10 
        BPL :+ 
        LDA #$0F
        :
        STA PaletteBufferSprites, X 
        INX 
        CPX #$10
        BNE BrightenSpritePaletteLoop
EndBrightenSpritePalette:
    LDA #$01 
    STA updatebackgroundpaletteflag
RTS

UpdatePaletteNMI:
    LDA updatebackgroundpaletteflag
    CMP #$01
    BNE EndUpdatePaletteNMI


    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    LDY #$00

    ;; Increment through the Palette data and store it into the PPU
    LoadPalettesNMI:
        LDA PaletteBufferBackground, Y
        STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
        INY
        CPY #$20
        BNE LoadPalettesNMI

        LDA updatebackgroundpaletteflag
        EOR #$01
        STA updatebackgroundpaletteflag
        
EndUpdatePaletteNMI:
RTS    

    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write to the OAM Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OAMBuffer:

    JSR ClearSpriteBuffer 

    BlankOutMem:
        ;LDA #$01 ; PLAYER TYPE
        ;STA $17

        ;LDA #$02
        ;STA $1A


        LDX #$00
        LDY #$00
        LDA #$00

       
        
    ; This checks the entity type of the current entity then branches accordingly
    DrawSprites:
        STY temp
        LDA entities+Entity::type, X 
        ASL 
        TAY 
        LDA DrawSpriteList, Y 
        STA jumppointer
        LDA DrawSpriteList+1, Y 
        STA jumppointer+1
        LDY temp
        JMP (jumppointer)
  
    DrawSingleSprite:
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer,Y 
        INY 
        LDA entities+Entity::spriteno, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY 
        JMP CheckEndSpriteDraw

    DrawFourBlockSprites: 
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        

        ;Sprite 2

        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC 
        ADC #$01
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY
         

        ;sprite 3
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC
        ADC #$02
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        

        ;sprite 4
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 

        CLC 
        ADC #$03
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY
        JMP CheckEndSpriteDraw
    
    DrawNESButtons:
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X 
        STA SpriteBuffer, Y
        INY  
        LDA entities+Entity::xpos,X 
        STA SpriteBuffer, Y 
        INY 

        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X
        CLC 
        ADC #$01 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X 
        STA SpriteBuffer, Y
        INY 
        LDA entities+Entity::xpos,X 
        STA SpriteBuffer, Y 
        INY
        
        LDA entities+Entity::ypos, X 
        CLC 
        ADC #$04
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC 
        ADC #$02
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X 
        STA SpriteBuffer, Y
        INY  
        LDA entities+Entity::xpos,X
        SEC 
        SBC #$04
        STA SpriteBuffer, Y 
        INY
        
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$04 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC 
        ADC #$03
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X 
        STA SpriteBuffer, Y
        INY  
        LDA entities+Entity::xpos,X
        CLC 
        ADC #$04 
        STA SpriteBuffer, Y 
        INY
        
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC 
        ADC #$04
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X 
        STA SpriteBuffer, Y
        INY  
        LDA entities+Entity::xpos,X 
        CLC 
        ADC #$26
        STA SpriteBuffer, Y 
        INY

        LDA entities+Entity::ypos, X 
        CLC 
        ADC #$08
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X 
        CLC 
        ADC #$05
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::palette, X 
        STA SpriteBuffer, Y
        INY  
        LDA entities+Entity::xpos,X
        CLC 
        ADC #$2F
        STA SpriteBuffer, Y 
        INY

    CheckEndSpriteDraw:
        TXA 
        CLC 
        ADC #.sizeof(Entity)
        TAX 
        CPX #TOTALENTITIES
        BEQ EndSpriteDraw
        JMP DrawSprites

    EndSpriteDraw:
        RTS 

;;;;;;;;;;;;;;;;;;;;;
; AUDIO ENGINE 
;;;;;;;;;;;;;;;;;

SoundInit:
    LDA #$0F 
    STA $4015 

    LDA #$00 
    STA sounddisableflag

    LDA #$FF 
    STA soundsquare1old
    STA soundsquare2old

SoundSilenceInit:
    LDA #$30 
    STA ApuBuffer
    STA ApuBuffer+4
    STA ApuBuffer+12
    LDA #$80
    STA ApuBuffer+8

    ;LDA #$00
    ;STA sounddisableflag
    ;STA sfxplaying
    ;STA sfxindex
    ;STA soundframecount
RTS 

SoundDisable: 
    LDA #$00 
    STA $4015 
    LDA #$01 
    STA sounddisableflag
RTS 

SoundLoad:
    STA soundtemp1
    ASL 
    TAY 
    LDA SongHeaders, Y 
    STA soundpointer
    LDA SongHeaders+1, Y 
    STA soundpointer+1 

    LDY #$00 
    LDA (soundpointer), Y 
    STA soundtemp2 
    INY 

    LDA #$00 
    STA streamloop1, X 
    STA streamnoteoffset, X 

    SoundLoadLoop: 
        LDA (soundpointer), Y 
        TAX 
        INY 

        LDA (soundpointer), Y 
        STA streamstatus, X 
        BEQ SoundNextStream
        INY 

        LDA (soundpointer), Y 
        STA streamchannel, X 
        INY 

        LDA (soundpointer), Y 
        STA streamvolduty, X 
        INY

        LDA (soundpointer), Y 
        STA streamvolumeenvelope, X 
        INY 

        LDA (soundpointer), Y 
        STA streampointerlow, X 
        INY

        LDA (soundpointer), Y 
        STA streampointerhigh, X
        INY 

        LDA (soundpointer), Y 
        STA streamtempo, X  
        
        LDA #$A0 
        STA streamtickertotal, X 

        LDA #$01 
        STA streamnotelengthcount, X 

        LDA #$00 
        STA streamvolumeenvelopeindex, X 

    SoundNextStream:
        INY 
        LDA soundtemp1 
        STA streamcurrentsound, X 
        DEC soundtemp2 
        BNE SoundLoadLoop 
RTS

SoundPlayFrame:
    LDA sounddisableflag
    BNE EndSoundPlayFrame

    JSR SoundSilenceInit

    LDX #$00
    SoundPlayFrameLoop: 
        LDA streamstatus, X 
        AND #$01
        BEQ EndSoundPlayFrameLoop

        LDA streamtickertotal, X 
        CLC 
        ADC streamtempo, X 
        STA streamtickertotal, X 
        BCC SoundSetBuffer

        DEC streamnotelengthcount, X 
        BNE SoundSetBuffer 
        LDA streamnotelength, X 
        STA streamnotelengthcount, X 

        JSR SoundGetByte

    SoundSetBuffer: 
        JSR SoundSetAPU

    EndSoundPlayFrameLoop:
        INX 
        CPX #$06 
        BNE SoundPlayFrameLoop

        JSR SoundWriteBufferToAPU
    EndSoundPlayFrame:
RTS 

SoundGetByte:
    LDA streampointerlow, X 
    STA soundpointer
    LDA streampointerhigh, X 
    STA soundpointer+1 

    LDY #$00
    SoundDoFetch: 
    LDA (soundpointer), Y 
    BPL SoundDoNote
    CMP #$A0 
    BCC SoundDoNoteLength

SoundDoOpcode:
    JSR SoundOPLaunch 
    INY
    LDA streamstatus, X 
    AND #%00000001 
    BNE SoundDoFetch
    RTS 
SoundDoNoteLength: 
    AND #%01111111
    STY soundtemp1 
    TAY 
    LDA NoteLengthTable, Y 
    STA streamnotelength, X 
    STA streamnotelengthcount, X 
    LDY soundtemp1 
    INY 
    JMP SoundDoFetch
SoundDoNote:
    STA soundtemp2
    LDA streamchannel, X
    CMP ChannelConst::Noise
    BNE NotNoise
    JSR SoundDoNoise 
    JMP ResetVolumeEnvelope
NotNoise:
    LDA soundtemp2
    STY soundtemp1 
    CLC 
    ADC streamnoteoffset, X 
    ASL 
    TAY 
    LDA NoteTable, Y 
    STA streamnotelow, X 
    LDA NoteTable+1, Y 
    STA streamnotehigh, X 
    LDY soundtemp1

    JSR SoundCheckRest

ResetVolumeEnvelope:
    LDA #$00 
    STA streamvolumeenvelopeindex, X

    SoundUpdatePointer:
    INY 
    TYA 
    CLC 
    ADC streampointerlow, X 
    STA streampointerlow, X 
    BCC EndSoundGetByte
    INC streampointerhigh, X 
EndSoundGetByte:
RTS 

SoundDoNoise:
    LDA soundtemp2 
    AND #%00010000
    BEQ NoiseMode0 
    NoiseMode1:
        LDA soundtemp2
        ORA #%10000000
        STA soundtemp2 
    NoiseMode0: 
        LDA soundtemp2 
        STA streamnotelow, X
RTS

SoundCheckRest: 
    LDA (soundpointer), Y 
    CMP Octave::rest 
    BNE SoundNotRest 
    LDA streamstatus, X 
    ORA #%00000010 
    BNE SoundStoreRest

    SoundNotRest: 
        LDA streamstatus, X 
        AND #%11111101 
    SoundStoreRest: 
        STA streamstatus, X 
        RTS 

SoundSetAPU: 
    LDA streamchannel, X 
    ASL 
    ASL 
    TAY

    JSR SoundSetStreamVolume

    LDA #$08 
    STA ApuBuffer+1, Y

    LDA streamnotelow, X 
    STA ApuBuffer+2, Y

    LDA streamnotehigh, X
    STA ApuBuffer+3, Y 
  
EndSoundSetAPU:
RTS 

SoundSetStreamVolume: 
    STY soundtemp1 

    LDA streamvolumeenvelope, X 
    ASL 

    TAY 
    LDA VolumeEnvelopes, Y 
    STA soundpointer
    LDA VolumeEnvelopes+1, Y 
    STA soundpointer+1 

    ReadEnvelope: 
        LDY streamvolumeenvelopeindex, X 
        LDA (soundpointer), Y 
        CMP #$FF 
        BNE SetVolume
        DEC streamvolumeenvelopeindex, X 
        JMP ReadEnvelope

    SetVolume:
        STA soundtemp2

        CPX ChannelConst::Triangle 
        BNE Squares 
        LDA soundtemp2 
        BNE Squares
        LDA #$80
        BMI StoreVolume 

    Squares: 
        LDA streamvolduty, X 
        AND #$F0
        ORA soundtemp2 

    StoreVolume:
        LDY soundtemp1
        STA ApuBuffer, Y 
        INC streamvolumeenvelopeindex, X 

    LDA streamstatus, X 
    AND #%00000010
    BEQ EndSetVolume

    LDA streamchannel, X 
    CMP ChannelConst::Triangle
    BEQ tri  
    LDA #$30 
    BNE store 

    tri: 
        LDA #$80 
    store: 
        STA ApuBuffer, Y

EndSetVolume:
RTS

SoundWriteBufferToAPU: 
    SoundWriteSquare1Buffer:
    LDA ApuBuffer+0   
    STA $4000
    LDA ApuBuffer+1
    STA $4001
    LDA ApuBuffer+2
    STA $4002
    LDA ApuBuffer+3
    CMP soundsquare1old
    BEQ SoundWriteSquare2Buffer
    STA $4003
    STA soundsquare1old

    SoundWriteSquare2Buffer:
    LDA ApuBuffer+4
    STA $4004
    LDA ApuBuffer+5
    STA $4005
    LDA ApuBuffer+6
    STA $4006
    LDA ApuBuffer+7
    CMP soundsquare2old
    BEQ SoundWriteTriangleBuffer
    STA $4007
    STA soundsquare2old

    SoundWriteTriangleBuffer:
    LDA ApuBuffer+8 
    STA $4008 
    LDA ApuBuffer+10
    STA $400A 
    LDA ApuBuffer+11 
    STA $400B 

    SoundWriteNoiseBuffer: 
    LDA ApuBuffer+12 
    STA $400C 
    LDA ApuBuffer+14
    STA $400E 
    LDA ApuBuffer+15 
    STA $400F 
RTS

;;
;; Opcodes
;;

SoundOPLaunch: 
    STY soundtemp1 
    SEC 
    SBC #$A0
    ASL 
    TAY 
    LDA SoundOPCodes, Y 
    STA jumppointer 
    LDA SoundOPCodes+1, Y
    STA jumppointer+1 
    LDY soundtemp1 
    INY 
    JMP (jumppointer) 

SoundOP_EndSound:
    LDA streamstatus, X 
    AND #%11111110 
    STA streamstatus, X 

    LDA streamchannel, X 
    CMP ChannelConst::Triangle
    BEQ SilenceTri
    LDA #$30
    BNE Silence 
    SilenceTri: 
        LDA #$80
    Silence: 
        STA streamvolduty, X
RTS 

SoundOP_InfLoop: 
    LDA (soundpointer), Y 
    STA streampointerlow, X 
    INY 
    LDA (soundpointer), Y 
    STA streampointerhigh, X 

    STA soundpointer+1 
    LDA streampointerlow, X 
    STA soundpointer 
    LDY #$FF
RTS 

SoundOP_ChangeEnvelope:
    LDA (soundpointer), Y
   STA streamvolumeenvelope, X 
    LDA #$00 
    STA streamvolumeenvelopeindex, X 
RTS 

SoundOP_ChangeDuty: 
    LDA (soundpointer), Y
    STA streamvolduty, X
RTS 

SoundOP_Loop1Counter: 
    LDA (soundpointer), Y 
    STA streamloop1, X 
RTS 

SoundOP_Loop1: 
    DEC streamloop1, X  
    LDA streamloop1, X 
    BEQ :+
    JMP SoundOP_InfLoop
    :
    INY     
RTS  

SoundOP_SetNoteOffset:
    LDA (soundpointer), Y 
    STA streamnoteoffset, X
RTS 

SoundOP_ChangeNoteOffset: 
    LDA (soundpointer), Y 
    CLC 
    ADC streamnoteoffset, X 
    STA streamnoteoffset 
RTS 

SoundOPCodes:
    .word SoundOP_EndSound ; A0
    .word SoundOP_InfLoop ; A1 etc
    .word SoundOP_ChangeEnvelope ;a2 
    .word SoundOP_ChangeDuty ;a3 
    .word SoundOP_Loop1Counter ;a4
    .word SoundOP_Loop1; a5
    .word SoundOP_SetNoteOffset
    .word SoundOP_ChangeNoteOffset
    
SongHeaders:
    .word song0header
    .word song1header
    .word song2header
    .word song3header
VolumeEnvelopes:
    .word SoundEnvelope0, SoundEnvelope1, SoundEnvelope2, SoundEnvelope3, SoundEnvelope4, SoundEnvelope5

;;;;;;;;;;;;;;;;;;;;
; Tilebuffer
;;;;;;;;;;;;;;;;;;;;
WriteToTileBuffer:
    CheckTileDrawFlag:
        LDA scrollinprogress
        BEQ :+
        JMP CheckScrollDirection
        :
        RTS

    CheckScrollDirection:
        LDA ScrollLeftInProgress
        BNE WriteColumnToBufferLeft
        LDA ScrollRightInProgress
        BNE WriteColumnToBuffer
    RTS 

    WriteColumnToBuffer:
        JSR GetMapPosRight

        LDA world ; take the low byte
        STA columnaddress ; store low byte in z page
        LDA world+1 ; take the high byte
        STA columnaddress+1 ; store high into the world address +1 i.e the second byte of the address


        LDA columnnumber
        SEC 
        SBC #$01
        ;lsr
        CLC 
        ADC columnaddress
        STA columnaddress 

        LDA columnaddress+1 
        ADC #$00
        STA columnaddress+1

        LDY #$00
        LDX #$1E
    WriteTileLoop:
        LDY #$00
        LDA (columnaddress), Y 

        ASL
        TAY  
        LDA MetaTileList, Y 
        STA world
        LDA MetaTileList+1, Y
        STA world+1

        

        LDY #$00

        LDA (world), Y 
        STA TileBuffer, X 
        INY 
        LDA (world), Y 
        STA TileBuffer2, X
        INY 
        DEX
        LDA (world), Y 
        STA TileBuffer, X 
        INY 
        LDA (world), Y 
        STA TileBuffer2, X 
        
        LDA columnaddress
        CLC 
        ADC #$10
        STA columnaddress
        LDA columnaddress+1   
        ADC #$00
        STA columnaddress+1

        DEX 
        BEQ :+
        JMP WriteTileLoop
        :
        LDA columnnumber
        CMP #$11
        BNE :+
        LDA #$00 
        STA scrollinprogress
        STA columnnumber
        STA ScrollRightInProgress
        STA ScrollX
        JSR WriteToCollisionMap
        INC pageX
        LDA #$01 
        STA loadattributeflag
        
        :

RTS 


RTS

    WriteColumnToBufferLeft:
        JSR GetMapPosLeft

        LDA world 
        STA columnaddress
        LDA world+1
        STA columnaddress+1 

        LDA columnnumber
        SEC
        SBC #$01
        CLC 
        ADC columnaddress
        STA columnaddress

        LDA columnaddress+1
        ADC #$00
        STA columnaddress+1

        LDX #$1E
    WriteTileLoopLeft:
        LDY #$00
        LDA (columnaddress), Y 

        ASL 
        TAY 
        LDA MetaTileList, Y 
        STA world 
        LDA MetaTileList+1, Y 
        STA world+1 

        LDY #$00 

        LDA (world), Y 
        STA TileBuffer, X 
        INY 
        LDA (world), Y 
        STA TileBuffer2, X 
        INY 
        DEX 
        LDA (world), Y 
        STA TileBuffer, X 
        INY 
        LDA (world), Y 
        STA TileBuffer2, X 

        LDA columnaddress
        CLC 
        ADC #$10 
        STA columnaddress
        LDA columnaddress+1
        ADC #$00 
        STA columnaddress+1 

        DEX 
        BEQ :+
        JMP WriteTileLoopLeft
        :
        LDA columnnumber 
        BNE :+ 
        LDA #$00
        STA scrollinprogress
        STA ScrollLeftInProgress
        STA columnnumber
        STA ScrollX
        JSR WriteToCollisionMap
        DEC pageX
        :
RTS

;;;;;;;;;;;;;;;;;;;

NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

;Variables

FrictionValue:
    .byte $18

PlayerSpeed: .byte $30

SpeedValue: ; nb, this includes the friction as an offset so change themboth if you change 1!
    .byte $19

ScrollBoundary:
    .byte $B7

ScrollBoundaryLeft:
    .byte $48

VerticalWave:
    .byte $00,$01,$01,$01,$01,$02,$02,$02,$02,$02,$02,$03,$03,$03,$03,$03,$03,$03,$03,$02,$02,$02,$02,$02,$02,$02,$01,$01,$01,$01
    .byte $00,$FF,$FF,$FF,$FF,$FE,$FE,$FE,$FE,$FE,$FE,$FD,$FD,$FD,$FD,$FD,$FD,$FD,$FD,$FE,$FE,$FE,$FE,$FE,$FE,$02,$FF,$FF,$FF,$FF

PaletteData:
    .byte $17,$0C,$00,$0F,  $17,$0A,$00,$0F,  $17,$1C,$0C,$0F, $0F,$06,$04,$0F  ;background palette data  
    .byte $17,$27,$14,$1A,  $22,$09,$1C,$0C,  $04,$16,$30,$27, $30,$0F,$36,$17  ;sprite palette data

BackGroundPaletteBlack:
    .byte $0F,$00,$0C,$0F,  $0F,$07,$17,$0F,  $0F,$07,$10,$0F, $0F,$14,$15,$16  ;background palette data  
    .byte $0F,$27,$14,$1A,  $22,$09,$1C,$0C,  $04,$16,$30,$27,$30,$0F,$36,$17  ;sprite palette data

AttributeData: 
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

SoundData: 
    .byte $00, $01, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $0C, $0D, $0E, $0F 

;Todo: Compression of worldata

WorldMap: ; read into this index to get screendata and when not to scroll further
    .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF 
    .byte $FF, $01, $02, $00, $ff, $01, $00, $FF
    .byte $FF, $06, $07, $08, $09, $0A, $0B, $FF
    .byte $FF, $0C, $0D, $0E, $0F, $10, $11, $FF
    .byte $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF

WorldData: ; Each row is 32
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32 ; Overscan blank line
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3D,$3E,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3D,$3E,$24,$24,$24,$24,$24,$24,$24,$24   
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24  
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$0E,$1D,$1A,$1D,$0D,$12,$13,$10,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24 
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$72,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$6C,$6A,$73,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$26,$69,$71,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$26,$69,$71,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$26  ,$67,$70,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$43,$44,$24,$24,$47,$47,$47,$47,$47,$47,$47,$47,$24,$24,$43,$44,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$26,$26,$26,$26,$26,$26,$26,$26,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$41,$41,$41,$41,$41,$41,$41,$41,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$42,$42,$42,$42,$42,$42,$42,$42,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$74,$75,$24,$24,$24,$24,$24,$24,$24,$4B,$25,$25,$25,$25,$25,$25,$25,$25,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$45,$45,$45,$45,$45,$45,$45,$45,$4C,$24,$24,$24,$24,$24,$24,$24,$3B,$3C,$24,$24
    .byte $33,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$46,$46,$46,$46,$46,$46,$46,$46,$4C,$24,$24,$24,$24,$24,$24,$24,$3D,$3E,$24,$24
    .byte $34,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$27,$27,$27,$27,$27,$27,$27,$27,$4C,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24

    .byte $33,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$27,$27,$27,$27,$27,$27,$27,$27,$4C,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24
    .byte $34,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4C,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24
    .byte $33,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24

    .byte $34,$24,$43,$44,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$43,$44,$24,$24
    .byte $37,$38,$37,$38,$37,$38,$37,$37,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38

    .byte $39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32; Overscan blank line
 
NoteOffset: 
    .byte $01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01,$00,$01
    .byte $00,$00,$00,$00
    .byte $FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF,$00,$FF
    .byte $00,$00,$00,$00

CrystalOffset:
    .byte $01,$01,$01,$01,$00,$00,$00,$FF,$FF,$FF,$FF,$00,$00,$00

NoteTable:  
    .word                                                                $07F1, $0780, $0713
    ; A1-B1
    .word $06AD, $064D, $05F3, $059D, $054D, $0500, $04B8, $0475, $0435, $03F8, $03BF, $0389  
    ; C2-B2
    .word $0356, $0326, $0259, $02CE, $02A6, $027F, $025C, $023A, $021A, $03F8, $01DF, $0389
    ; C3-B3   
    .word $01AB, $0193, $017C, $0167, $0151, $013F, $012D, $011C, $010C, $00FD, $00EF, $0389
    ; C4-B4
    .word $00D2, $00C9, $00BD, $00B3, $00A9, $009F, $0096, $008E, $0086, $007E, $0077, $0070
    ; C5-B5
    .word $006A, $0064, $005E, $0059, $0054, $004F, $004B, $0046, $0042, $003F, $003B, $0038
    ; C6-B6
    .word $0034, $0031, $002F, $002C, $0029, $0027, $0025, $0023, $0021, $001F, $001B, $001B
    ; C7-B7
    .word $001A, $0018, $0017, $0015, $0014, $0013, $0012, $0011, $0010, $000F, $000E, $000D
    ; C8-B8
    .word $000C, $000C, $000B, $000A, $000A, $0009, $0008

NoteLengthTable: 
    .byte $01 
    .byte $02 
    .byte $04
    .byte $08
    .byte $10
    .byte $20

    .byte $03
    .byte $06
    .byte $0C 
    .byte $18 
    .byte $30
    .byte $07

SoundEnvelope0:
    .byte $0F, $0E, $0D, $0C, $09, $05, $00
    .byte $FF 
SoundEnvelope1: 
    .byte $01, $01, $02, $02, $03, $03, $04, $04, $07, $07
    .byte $08, $08, $0A, $0A, $0C, $0C, $0D, $0D, $0E, $0E 
    .byte $0F, $0F 
    .byte $FF 
SoundEnvelope2: 
    .byte $0D, $0D, $0D, $0C, $0B, $00, $00, $00, $00, $00
    .byte $00, $00, $00, $00, $06, $06, $06, $05, $04, $00
    .byte $FF 
SoundEnvelope3:
    .byte $0F, $0B, $09, $08, $07, $06, $06
    .byte $FF 

SoundEnvelope4:
    .byte $0B, $0B, $0A, $09, $08, $07, $06, $06, $06, $05
    .byte $FF

SoundEnvelope5: 
    .byte $0E, $09, $08, $06, $04, $03, $02, $01, $00
    .byte $FF 

song0header:
    .byte $06 

    .byte Stream::MusicSquareOne
    .byte $00

    .byte Stream::MusicSquareTwo
    .byte $00

    .byte Stream::MusicTriangle
    .byte $00

    .byte Stream::MusicNoise
    .byte $00

    .byte Stream::SfxOne
    .byte $00

    .byte Stream::SfxTwo
    .byte $00



song1header:
    .byte $04 

    .byte Stream::MusicSquareOne 
    .byte $01 
    .byte ChannelConst::SquareOne
    .byte $70  
    .byte $03;SoundEnvelope3
    .word song1square1
    .byte $53

    .byte Stream::MusicSquareTwo 
    .byte $01 
    .byte ChannelConst::SquareTwo
    .byte $B0
    .byte $04 ;SoundEnvelope4
    .word song1square2
    .byte $53

    .byte Stream::MusicTriangle 
    .byte $01
    .byte ChannelConst::Triangle
    .byte $80
    .byte $04 ;SoundEnvelope4
    .word song1triangle
    .byte $53

    .byte Stream::MusicNoise
    .byte $01 
    .byte ChannelConst::Noise 
    .byte $30 
    .byte $06 
    .word song1noise
    .byte 53 

;    .byte Stream::SfxOne
;    .byte $00

;    .byte Stream::SfxTwo
;    .byte $00


song1square1:
    .byte NoteLength::eighth
    .byte Opcodes::Loop1Counter, $04
    song1square1Intro:
    .byte Octave::A2, Octave::A2, Octave::A2, Octave::A3, Octave::A2, Octave::A3, Octave::A2, Octave::A3  
    .byte Opcodes::Loop1 
    .word song1square1Intro
    .byte Octave::E3, Octave::E3, Octave::E3, Octave::E4, Octave::E3, Octave::E4, Octave::E3, Octave::E4  
    .byte Octave::E3, Octave::E3, Octave::E3, Octave::E4, Octave::E3, Octave::E4, Octave::E3, Octave::E4
    .byte Octave::DS3, Octave::DS3, Octave::DS3, Octave::DS4, Octave::DS3, Octave::DS4, Octave::DS3, Octave::DS4  
    .byte Octave::D3, Octave::D3, Octave::D3, Octave::D4, Octave::D3, Octave::D4, Octave::D3, Octave::D4
    .byte Octave::C3, Octave::C3, Octave::C3, Octave::C4, Octave::C3, Octave::C4, Octave::C3, Octave::C4
    .byte Octave::B2, Octave::B2, Octave::B2, Octave::B3, Octave::B2, Octave::B3, Octave::B2, Octave::B3
    .byte Octave::AS2, Octave::AS2, Octave::AS2, Octave::AS3, Octave::AS2, Octave::AS3, Octave::AS2, Octave::AS3 
    .byte Octave::A2, Octave::A2, Octave::A2, Octave::A3, Octave::A2, Octave::A3, Octave::A2, Octave::A3
    .byte Octave::GS2, Octave::GS2, Octave::GS2, Octave::GS3, Octave::GS2, Octave::GS3, Octave::GS2, Octave::GS3
    .byte Octave::G2, Octave::G2, Octave::G2, Octave::G3, Octave::G2, Octave::G3, Octave::G2, Octave::G3
    .byte Opcodes::InfiniteLoop
    .word song1square1

song1square2:
    .byte NoteLength::d_eighth
    .byte Octave::rest
    .byte NoteLength::eighth
    .byte Opcodes::Loop1Counter, $04
    song1square2loop:
    .byte Octave::A4, Octave::C5, Octave::B4, Octave::C5, Octave::A4, Octave::C5, Octave::B4, Octave::C5 
    .byte Opcodes::Loop1
    .word song1square2loop
    .byte Octave::AB4, Octave::B4, Octave::A4, Octave::B4, Octave::AB4, Octave::B4, Octave::A4, Octave::B4
    .byte Octave::B4, Octave::E5, Octave::D5, Octave::E5, Octave::B4, Octave::E5, Octave::D5, Octave::E5
    .byte Octave::A4, Octave::EB5, Octave::C5, Octave::EB5, Octave::A4, Octave::EB5, Octave::C5, Octave::EB5
    .byte Octave::A4, Octave::D5, Octave::DB5, Octave::D5, Octave::A4, Octave::D5, Octave::DB5, Octave::D5
    .byte Octave::A4, Octave::C5, Octave::F5, Octave::A5, Octave::C6, Octave::A5, Octave::F5, Octave::C5
    .byte Octave::GB4, Octave::B4, Octave::EB5, Octave::GB5, Octave::B5, Octave::GB5, Octave::EB5, Octave::B4
    .byte Octave::E4, Octave::A4, Octave::CS5, Octave::E5, Octave::A5, Octave::E5, NoteLength::sixteenth, Octave::CS5 , Octave::rest
    .byte NoteLength::eighth
    .byte Octave::DS4, Octave::GS4, Octave::C5, Octave::DS5, Octave::GS5, Octave::DS5, Octave::C5, Octave::GS4
    .byte NoteLength::sixteenth
    .byte Octave::G4, Octave::FS4, Octave::G4, Octave::FS4, Octave::G4, Octave::FS4, Octave::G4, Octave::FS4
    .byte NoteLength::eighth
    .byte Octave::G4, Octave::B4, Octave::D5, Octave::G5
    .byte Opcodes::InfiniteLoop
    .word song1square2loop
    ;.byte $FF 

song1triangle:
    .byte NoteLength::eighth
    .byte Opcodes::Loop1Counter, $04
    song1triangleIntro:
    .byte Octave::A5, Octave::C6, Octave::B5, Octave::C6, Octave::A5, Octave::C6, Octave::B5, Octave::C6
    .byte Opcodes::Loop1
    .word song1triangleIntro
    .byte Octave::AB5, Octave::B5, Octave::A5, Octave::B5, Octave::AB5, Octave::B5, Octave::A5, Octave::B5
    .byte Octave::B5, Octave::E6, Octave::D6, Octave::E6, Octave::B5, Octave::E6, Octave::D6, Octave::E6
    .byte Octave::A5, Octave::EB6, Octave::C6, Octave::EB6, Octave::A5, Octave::EB6, Octave::C6, Octave::EB6
    .byte Octave::A5, Octave::D6, Octave::DB6, Octave::D6, Octave::A5, Octave::D6, Octave::DB6, Octave::D6
    .byte Octave::A5, Octave::C6, Octave::F6, Octave::A6, Octave::C7, Octave::A6, Octave::F6, Octave::C6
    .byte Octave::GB5, Octave::B5, Octave::EB6, Octave::GB6, Octave::B6, Octave::GB6, Octave::EB6, Octave::B5
    .byte Octave::F5, Octave::BB5, Octave::D6, Octave::F6, Octave::GS6, Octave::F6, Octave::D6, Octave::AS5
    .byte Octave::E5, Octave::A5, Octave::CS6, Octave::E6, Octave::A6, Octave::E6, Octave::CS6, Octave::A5
    .byte Octave::DS5, Octave::GS5, Octave::C6, Octave::DS6, Octave::GS6, Octave::DS6, Octave::C6, Octave::GS5
    .byte NoteLength::sixteenth
    .byte Octave::G5, Octave::FS5, Octave::G5, Octave::FS5, Octave::G5, Octave::FS5, Octave::G5, Octave::FS5
    .byte Octave::G5, Octave::B5, Octave::D6, Octave::G6, Octave::B5, Octave::D6, Octave::B6, Octave::D7
    ;.byte Octave::REPLACE, Octave::REPLACE, Octave::REPLACE, Octave::REPLACE, Octave::REPLACE, Octave::REPLACE, Octave::REPLACE, Octave::REPLACE
        .byte Opcodes::InfiniteLoop 
        .word song1triangle 

song1noise: 
    .byte NoteLength::eighth 
    .byte $04 
    .byte NoteLength::sixteenth 
    .byte $04, $04, $04
    .byte NoteLength::eighth 
    .byte $04 
    .byte NoteLength::sixteenth 
    .byte $04, $04, $04, $04
    .byte NoteLength::eighth 
    .byte $04, $04 
    .byte Opcodes::InfiniteLoop 
    .word song1noise

song2header:

    .byte $04 

    .byte Stream::MusicSquareOne 
    .byte $01 
    .byte ChannelConst::SquareOne
    .byte $B0 
    .byte $03;SoundEnvelope3
    .word song2square1
    .byte $40

    .byte Stream::MusicSquareTwo 
    .byte $00 

    .byte Stream::MusicTriangle 
    .byte $01
    .byte ChannelConst::Triangle
    .byte $81
    .byte $04 ;SoundEnvelope4
    .word song2triangle
    .byte $40

    .byte Stream::MusicNoise
    .byte $00

song2square1: 
    .byte NoteLength::eighth 
    .byte Octave::D4, Octave::A4, Octave::F4, Octave::A4, Octave::D4, Octave::B4, Octave::G4, Octave::B4 
    .byte Octave::D4, Octave::C5, Octave::A4, Octave::C5, Octave::D4, Octave::AS4, Octave::F4, Octave::AS4 
    .byte Octave::E4, Octave::A4, Octave::E4, Octave::A4, Octave::D4, Octave::A4, Octave::FS4, Octave::A4
    .byte Octave::D4, Octave::A4, Octave::FS4, Octave::A4, Octave::G4, Octave::AS4, Octave::A4, Octave::C5 
    .byte Octave::D4, Octave::C5, Octave::A4, Octave::C5, Octave::D4, Octave::B4, Octave::G4, Octave::B4
    .byte Octave::D4, Octave::B4, Octave::G4, Octave::B4, Octave::D4, Octave::AS4, Octave::GS4, Octave::AS4 
    .byte Octave::CS4, Octave::A4, Octave::E4, Octave::A4, Octave::D4, Octave::A4, Octave::E4, Octave::A4
    .byte Octave::CS4, Octave::A4, Octave::E4, Octave::A4, Octave::B3, Octave::A4, Octave::CS4, Octave::A4
    .byte Opcodes::InfiniteLoop
    .word song2square1

song2triangle: 
    .byte NoteLength::quarter, Octave::D6, Octave::A6, NoteLength::half, Octave::G6
    .byte NoteLength::eighth, Octave::F6, Octave::E6, NoteLength::quarter, Octave::D6
    .byte NoteLength::eighth, Octave::C6, Octave::AS5, Octave::C6, Octave::A5
    .byte NoteLength::quarter, Octave::E6, NoteLength::d_whole, Octave::D6
    .byte NoteLength::quarter, Octave::A6, Octave::C7, NoteLength::d_half, Octave::B6
    .byte NoteLength::eighth, Octave::G6, Octave::F6, NoteLength::quarter, Octave::E6
    .byte NoteLength::eighth, Octave::F6, Octave::G6, NoteLength::whole, Octave::A6 
    .byte Opcodes::InfiniteLoop
    .word song2triangle





song3header: 

    .byte $04 

    .byte Stream::MusicSquareOne 
    .byte $00

    .byte Stream::MusicSquareTwo 
    .byte $00

    .byte Stream::MusicTriangle 
    .byte $00

    .byte Stream::MusicNoise
    .byte $01 
    .byte ChannelConst::Noise 
    .byte $30 
    .byte $06 
    .word song3noise
    .byte 53 


song3noise: 
    .byte NoteLength::eighth 
    .byte $18 
    .byte NoteLength::sixteenth 
    .byte $18, $18, $18
    .byte NoteLength::eighth 
    .byte $18 
    .byte NoteLength::sixteenth 
    .byte $18, $18, $18, $18
    .byte NoteLength::eighth 
    .byte $18, $18 
    .byte Opcodes::InfiniteLoop
    .word song3noise

ProcessEntityList:
    .word SkipEntity
    .word ProcessPlayer
    .word ProcessNote
    .word ProcessFire
    .word ProcessPlayerTwo
    .word ProcessEurydice
    .word ProcessNoteStatic
    .word ProcessUpButton
    .word ProcessDownButton
    .word ProcessLeftButton
    .word ProcessRightButton
    .word ProcessAButton
    .word ProcessBButton
    .word ProcessCrystal
    .word ProcessNESButtons

DrawSpriteList:
    .word CheckEndSpriteDraw
    .word DrawFourBlockSprites
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawFourBlockSprites
    .word DrawFourBlockSprites
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawSingleSprite
    .word DrawFourBlockSprites
    .word DrawNESButtons


MetaTileList:
.word Blank0 ;0
.word Blank1
.word Blank3
.word Dock
.word DockTop
.word Wave1 ;5
.word Wave2
.word Wave3
.word BrazierLeft
.word BrazierRight
.word PotLargeTop ;0a
.word PotLargeBottom
.word PotSmall 
.word LadderLeft ; d
.word LadderRight ; e
.word LadderLeftTop ;0f
.word PillarTop ;10
.word PillarBottom
.word PillarBlank
.word PillarVine 
.word Path
.word Path2
.word ArchTopLeft ;16
.word ArchTopRight
.word ArchBottomLeft
.word ArchBottomRight
.word TorchRight 
.word SlopeLeft
.word SlopeRight ;1c
.word WaterTransition
.word Grass
.word Grass2
.word CrackedWall1 ;20
.word CrackedWall2 ; 
.word HangingVine1
.word HangingVine2
.word BrickPath
.word BrickPathVine ; 25
.word PotLargeTopEnclosed
.word BrickTopVine
.word BrickTop ; 28 
.word ScreenTop1
.word ScreenTop2 ;2a
.word ScreenBot1 
.word ScreenBot2
.word ScreenBot3
.word ScreenBot4
.word ScreenBot5
.word ScreenBot6 ;30
.word BlockingBrick
.word Brick
.word BlankBlocking
.word BrickVine 
.word BrickVine2
.word Rain1 ;36
.word Rain2
.word PillarCircularLeft
.word PillarCircularRight
.word PillarHollowLeft ;3A
.word PillarHollowRight 
.word ArchMiddle
.word Controller1 ;3D
.word Controller2
.word Controller3 
.word Controller4 ;40
.word Controller5 ;41
.word Controller6 
.word Controller7 
.word Controller8
.word Bridge1 ;45
.word Bridge2
.word BridgePillar1
.word BridgePillar2Blank
.word BridgePillar2Vine1
.word BridgePillar2Vine2;4a
.word WeaponRack
.word LightPost
.word BrickLight
; 4x4 tile definitions
Blank0:
    .byte $24, $24 ; pattern tabl
    .byte $24, $24 ; pattern table
    .byte $00      ; collision

Blank1: 
    .byte $25, $25 
    .byte $25, $25
    .byte $01     

Blank2:
    .byte $26, $26
    .byte $26, $26
    .byte $01     

Blank3:
    .byte $27, $27
    .byte $27, $27
    .byte $01     

Dock:
    .byte $CA, $CB
    .byte $25, $25
    .byte $01     

DockTop:
    .byte $24, $BB
    .byte $CA, $CB
    .byte $01     

Wave1:
    .byte $BC, $BD
    .byte $25, $25
    .byte $01     

Wave2:
    .byte $BE, $BC
    .byte $25, $25
    .byte $01     

Wave3: 
    .byte $BD, $BE 
    .byte $25, $25
    .byte $01     

BrazierLeft:
    .byte $AA, $24
    .byte $BA, $24
    .byte $01     

BrazierRight:
    .byte $24, $AA
    .byte $24, $BA
    .byte $01     


PotLargeTop:
    .byte $24, $24
    .byte $81, $82
    .byte $01     

PotLargeBottom:
    .byte $83, $84
    .byte $87, $88
    .byte $01     

PotSmall:
    .byte $85, $86
    .byte $89, $8A
    .byte $01     

LadderLeft:
    .byte $90, $90
    .byte $90, $90
    .byte $00     

LadderRight:
    .byte $90, $90
    .byte $90, $90
    .byte $00     

LadderLeftTop:
    .byte $24, $24
    .byte $24, $24
    .byte $00     

PillarTop:
    .byte $56, $57
    .byte $54, $55
    .byte $01     

PillarBottom:
    .byte $52, $53
    .byte $4e, $4f
    .byte $01     

PillarBlank:
    .byte $50, $51
    .byte $50, $51
    .byte $01     

PillarVine:
    .byte $52, $53
    .byte $54, $55
    .byte $01     

Path:
    .byte $94, $94
    .byte $27, $27
    .byte $01     

Path2:
    .byte $94, $94
    .byte $27, $27
    .byte $01     

ArchTopLeft:
    .byte $80, $80
    .byte $8b, $8c
    .byte $01     

ArchTopRight:
    .byte $80, $80
    .byte $8d, $8e
    .byte $01     

ArchBottomLeft:
    .byte $9B, $9C
    .byte $AB, $24 
    .byte $01     

ArchBottomRight:
    .byte $9D, $9E
    .byte $AD, $AE 
    .byte $01     

TorchRight:
    .byte $24, $AA
    .byte $24, $24
    .byte $01     

SlopeLeft:
    .byte $24, $7E
    .byte $7E, $80
    .byte $01     

SlopeRight:
    .byte $7F, $24
    .byte $00, $7F
    .byte $01     

WaterTransition:
    .byte $BF,$BF 
    .byte $26,$26
    .byte $01     

Grass:
    .byte $24, $24
    .byte $93, $24
    .byte $01     

Grass2:
    .byte $24, $24
    .byte $24, $95
    .byte $01     

CrackedWall1:
    .byte $79, $7A
    .byte $7B, $77
    .byte $01     

CrackedWall2:
    .byte $77, $7C
    .byte $7D, $80
    .byte $01     

HangingVine1: 
    .byte $AF, $9F
    .byte $9A, $24
    .byte $01     

HangingVine2: 
    .byte $9F, $AF
    .byte $24, $9A
    .byte $01     

BrickPath: 
    .byte $24, $24
    .byte $80, $80 
    .byte $01     

BrickPathVine:
    .byte $24, $24 
    .byte $99, $9F
    .byte $01     
    
PotLargeTopEnclosed: 
    .byte $80, $80 
    .byte $81, $82
    .byte $01     

BrickTopVine:
    .byte $9F, $8F 
    .byte $24, $24
    .byte $01     

BrickTop:
    .byte $80, $80 
    .byte $24, $24
    .byte $01     

ScreenTop1:
    .byte $32, $32 
    .byte $38, $37
    .byte $01     

ScreenTop2:
    .byte $39, $3A 
    .byte $24, $24
    .byte $01     

ScreenBot1:
    .byte $24, $24 
    .byte $2C, $2D
    .byte $01     

ScreenBot2:
    .byte $2E, $2F 
    .byte $32, $32
    .byte $01   

ScreenBot3:
    .byte $24, $24 
    .byte $30, $2C
    .byte $01   

ScreenBot4:
    .byte $31, $2E 
    .byte $32, $32
    .byte $01   

ScreenBot5:
    .byte $24, $24 
    .byte $2D, $30
    .byte $01   
ScreenBot6:
    .byte $2F, $31 
    .byte $32, $32
    .byte $01   

BlockingBrick:
    .byte $80, $80 
    .byte $80, $80

Brick:
    .byte $80, $80 
    .byte $80, $80

BlankBlocking: 
    .byte $24, $24
    .byte $24, $24

BrickVine: 
    .byte $8F, $9F
    .byte $AF, $97

BrickVine2: 
    .byte $97, $99
    .byte $AF, $98

Rain1:
    .byte $A8, $A9
    .byte $A9, $A8

Rain2:
    .byte $A9, $A8
    .byte $A8, $A9

PillarCircularLeft:
    .byte $5B, $5C
    .byte $5B, $5C

PillarCircularRight:
    .byte $5D, $5E
    .byte $5D, $5E

PillarHollowLeft:
    .byte $5D, $5E
    .byte $5D, $5E

PillarHollowRight:
    .byte $5D, $5E
    .byte $5D, $5E

ArchMiddle:
    .byte $AC, $AC
    .byte $24, $24

Controller1:
    .byte $24, $24
    .byte $B4, $B5 

Controller2:
    .byte $24, $24
    .byte $C7, $B6 
    
Controller3:
    .byte $24, $24
    .byte $B6, $C6
    
Controller4:
    .byte $24, $24
    .byte $b5, $c3 
    
Controller5:
    .byte $B2, $27
    .byte $B0, $B3 
    
Controller6:
    .byte $C9, $B8
    .byte $CD, $C0 
    
Controller7:
    .byte $B8, $C8
    .byte $C0, $CC
    
Controller8:
    .byte $27, $C3
    .byte $B3, $C5 

Bridge1:
    .byte $CE, $CF
    .byte $24, $24 
 
Bridge2:
    .byte $D4, $D5
    .byte $D6, $24 

BridgePillar1:
    .byte $27, $C3
    .byte $B3, $C5 

BridgePillar2Blank:
    .byte $D6, $24
    .byte $D6, $24 

BridgePillar2Vine1:
    .byte $D0, $62
    .byte $D1, $24

BridgePillar2Vine2:
    .byte $D2, $24
    .byte $D3, $64 

WeaponRack: 
    .byte $28, $29 
    .byte $2A, $2B 

LightPost:
    .byte $91, $92 
    .byte $A1, $A2 

BrickLight:
    .byte $A3, $A4 
    .byte $A5, $A6 



CollisionList:
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$02,$02,$02 ;00 -> 0F
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;10 -> 1F
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;20 -> 2F
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;30 -> 3F
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;30 -> 3F
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;30 -> 3F
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00 ;30 -> 3F

ScreenList:
.word LevelScreenDock
.word LevelScreenArch
.word LevelScreenBridge

AttributeList: 
.word AttributesDock
.word AttributesArch
.word AttributesBridge 

LevelScreenDock:
.byte $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29
.byte $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A, $2A
.byte $10, $33, $33, $33, $33, $38, $39, $38, $39, $38, $39, $33, $33, $33, $33, $33
.byte $13, $33, $33, $33, $33, $38, $39, $38, $39, $38, $39, $33, $33, $33, $33, $33
.byte $13, $33, $33, $33, $33, $38, $39, $16, $17, $38, $39, $00, $00, $00, $00, $00
.byte $11, $0F, $33, $1E, $1B, $38, $39, $18, $19, $38, $39, $0F, $00, $33, $00, $00
.byte $15, $0E, $15, $15, $15, $15, $15, $15, $15, $15, $15, $0E, $15, $00, $00, $00
.byte $10, $0E, $33, $00, $00, $00, $00, $00, $00, $10, $00, $0E, $00, $00, $00, $00
.byte $12, $0E, $33, $00, $00, $25, $25, $25, $25, $11, $00, $0E, $00, $00, $00, $00
.byte $12, $0E, $33, $00, $1B, $26, $20, $22, $31, $26, $35, $0E, $00, $33, $00, $00
.byte $11, $0E, $00, $1B, $20, $0B, $31, $0C, $21, $0B, $34, $0E, $08, $00, $00, $00
.byte $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $03, $05, $06, $07
.byte $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D
.byte $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B
.byte $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C

AttributesDock:
    .byte $55, $55, $55, $55, $55, $55, $55, $55 
    .byte $55, $00, $00, $00, $00, $00, $55, $55
    .byte $55, $00, $00, $00, $00, $00, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $00, $00, $00, $00, $55, $AB, $AA
    .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
    .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA

LevelScreenArch:
.byte $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29
.byte $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a
.byte $00, $38, $39, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $38, $39, $00
.byte $00, $38, $39, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $38, $39, $00
.byte $00, $38, $39, $0F, $00, $00, $00, $00, $00, $00, $00, $00, $0F, $38, $39, $00
.byte $00, $38, $39, $0E, $14, $14, $14, $24, $14, $14, $14, $14, $0E, $38, $39, $00
.byte $00, $38, $39, $0E, $30, $31, $21, $20, $20, $21, $35, $34, $0E, $38, $39, $00
.byte $00, $38, $39, $0E, $00, $00, $00, $00, $00, $00, $00, $22, $0E, $38, $39, $00
.byte $00, $38, $39, $0E, $0C, $00, $00, $00, $00, $00, $00, $0C, $0E, $38, $39, $00
.byte $00, $38, $39, $0E, $10, $00, $00, $16, $17, $00, $00, $10, $0E, $38, $39, $00
.byte $00, $38, $39, $0E, $11, $0C, $08, $18, $19, $09, $0C, $11, $0E, $38, $39, $00
.byte $15, $15, $15, $0E, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15, $15
.byte $02, $02, $02, $0E, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02, $02
.byte $2B, $2D, $2F, $0E, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B
.byte $2C, $2E, $30, $0E, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C

AttributesArch:
    .byte $55, $55, $55, $55, $55, $55, $55, $55 
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
    .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA


LevelScreenBridge:
.byte $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29, $29
.byte $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a, $2a
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $3D, $3E, $3F, $40, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $41, $42, $43, $44, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
.byte $00, $4C, $00, $4C, $00, $4C, $4B, $4B, $00, $00, $4C, $00, $4C, $00, $4C, $00
.byte $45, $45, $45, $45, $45, $45, $32, $38, $39, $32, $45, $45, $45, $45, $45, $45
.byte $49, $49, $48, $48, $48, $48, $32, $38, $39, $32, $48, $48, $48, $48, $48, $48
.byte $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B, $2D, $2F, $2B
.byte $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C, $2E, $30, $2C

AttributesBridge:
    .byte $55, $55, $55, $55, $55, $55, $55, $55 
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $55, $55, $55, $55, $55, $55, $55, $55
    .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA
    .byte $AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA

.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "Bank1.chr"
    .incbin "Bank2.chr"

