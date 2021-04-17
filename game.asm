;;;; The header stuff is basically for the emulator

.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $01 ; 1 * 8KB CHR ROM
.byte %00010011 ; mapper and mirroring
.byte $0000  
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00 ; filler bytes


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

.scope MapTileNo
    MapZero = 0
    MapOne = 1
    MapTwo = 2
    MapThree = 3
    MapFour = 4
    MapFive = 5
    MapSix = 6
.endscope

.struct Entity
    xpos .byte
    xposlow .byte
    ypos .byte
    yposlow .byte 
    type .byte
    spriteno .byte
    palette .byte
.endstruct


;todo: entities currently cant be moved bc references to its abs loc in other places 
.segment "ZEROPAGE" ; LSB 0 - FF
;; Reserve memory for some specific things we need not to be futzed with
    SpriteMem: .res 2 ; unused
    world: .res 2  ; used during startup, not after that
    buttons: .res 1 ; used for polling controller
    nmidone: .res 1 ; ppu is done when this is 1 ;
    framecount: .res 1 ;  This will increment once per frame and reset over 59 (0 counts)
    twocount: .res 1   ; flips every frame
    ScrollX: .res 1 ;   scroll L/R
    ScrollY: .res 1 ;   scroll u/d
    FlipSprite: .res 1 ; 0 is right, 1 is left, not used atm
    animation: .res 1 ; unused 
    XOffset: .res 1 ; unused
    YOffset: .res 1 ; unused
    moving: .res 1 ;unused
    anioffset: .res 1 ; unused
    facing: .res 1 ; player facing direction determines sprite
    facingframe: .res 1 ; unused?
    playeraddress: .res 2 ; unused
    mapposindex: .res 1 ; unused
   ;NB DO NOT DELETE ANY OF THESE EVEN IF THEY AREN'T BEING USED IT WILL MESS UP THE ENTITY HANDLER ATM

    MAXENTITIES = 10
     ; max allowed number of entities
    entities: .res .sizeof(Entity) * MAXENTITIES 
    TOTALENTITIES = .sizeof(Entity) * MAXENTITIES

    columnhigh: .res 1
    columnlow: .res 1
    columnnumber: .res 1
    Columnflag: .res 1
    waveflip: .res 1 ; this is usedfor not movement
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
    LDA #$00
    STA entities+Entity::xposlow
    LDA #$B3
    STA entities+Entity::ypos
    LDA #$00
    STA entities+Entity::yposlow
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
    STA entities+Entity::xposlow, X
    STA entities+Entity::yposlow, X
    LDA #$00
    STA entities+Entity::type, X
    STA entities+Entity::palette, X
    LDA #$FF 
    INX
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

;; Increment through the Pallete data and store it into the PPU
LoadPalettes:
    LDA PaletteData, X
    STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
    INX
    CPX #$20
    BNE LoadPalettes    
 
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


InitWorld2:
    LDA #< WorldData2 ; take the low byte
    STA world ; store low byte in z page
    LDA #> WorldData2 ; take the high byte
    STA world+1 ; store high into the world address +1 i.e the second byte of the address

 ;setup address in PPU for nametable data
    BIT $2002
    LDA #$24
    STA $2006
    LDA #$00
    STA $2006

    LDX #$00
    LDY #$00


LoadWorld2:
    LDA (world), Y
    STA $2007
    INY
    CPX #$03
    BNE :+
    CPY #$E0
    BEQ DoneLoadingWorld2
:
    CPY #$00
    BNE LoadWorld2
    INX
    INC world+1
    JMP LoadWorld2

DoneLoadingWorld2:
    LDX #$00


SetAttributes:
    LDX #$00
    LDA #$23
    STA $2006
    LDA #$C0
    STA $2006
    AttributeLoop:
    LDA #$00
    STA $2007
    INX
    CPX #$40
    BNE AttributeLoop

    LDX #$00
    LDY #$00    

SetAttributes2:
    LDX #$00
    LDA #$27

    STA $2006
    LDA #$C0
    STA $2006
    AttributeLoop2:
    LDA #$00
    STA $2007
    INX
    CPX #$40
    BNE AttributeLoop2

    LDX #$00
    LDY #$00

;InitApu:
;   LDY #$13
;  InitApuLoop:
;    LDA APURegs, Y 
;    STA $4000, Y 
;    DEY 
;    BPL InitApuLoop
;    LDA #$0F 
;    STA $4015
;    LDA #$40
;    STA $4017

SetPlayerPos: ; initial player position
    LDA #$40
    STA entities+Entity::xpos
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


; map some memory baabbbby!
; All sprite data is to be stored here and retrieved every frame
; Sprite buffer takes 4x64 = 256 bytes
SpriteBuffer = $0200 ;$0200 -> $02FF
TileBuffer = $0300 ; $0300 -> 031F
PaletteBuffer = $0320 ; 03320 ->033F 
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
NoteInputMem = $0398 ; 0350 -> 0343

AudioMem = $0400

FillTileBuffer:
LDA #$22
LDX #$00
    BufferLoop:
    STA TileBuffer, X 
    INX 
    CPX #$FF 
    BEQ :+
    JMP BufferLoop
:

LDA #$20 ; put this somewhere else? 
STA nextnote

LDA #$08
STA ScrollXEight

;Set movespeed
LDA #$00
STA dxhigh
LDA #$00
STA dxlow  

;Set Zeropage variables
LDA #$01
STA pageY
LDA #$03
STA pageX

; Enable the apu
JSR SoundInit
LDA #$01
JSR SoundLoad

JSR ChangePalleteBlack
JSR SpawnFourNotes
JSR ChangePalleteOrange
;JSR SpawnEurydice

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

    JSR DoWorldMap 
    JSR WaveFlip    ; This simply flips between 1 and 0. Used for directional variance
    JSR NoteIndex   ; This changes the note sprite that will spawn
    JSR ReadButtons ; Duh
    ;JSR SoundLoad
    JSR CollideDown ; Think about moving this together with movement?
    JSR XMovement ; the player movement
    JSR ProcessEntities ; entity behaviour is handled here, the player has some special stuff elsewhere
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR DoScroll       
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
    JSR UpdatePalleteNMI

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
    LDA DrawColumnFlag
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
RTS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;   all the stuff in the main loop 
;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    CMP #MapTileNo::MapZero
    BEQ SetWorldDataZero
    CMP #MapTileNo::MapOne
    BEQ SetWorldDataOne
    CMP #MapTileNo::MapTwo
    BEQ SetWorldDataTwo
    CMP #MapTileNo::MapThree
    BEQ SetWorldDataThree
    CMP #MapTileNo::MapFour
    BEQ SetWorldDataFour
    CMP #MapTileNo::MapFive
    BEQ SetWorldDataFive
    CMP #MapTileNo::MapSix
    BEQ SetWorldDataSix
    JMP EndGetMapPosRight

    SetWorldDataZero:
        LDA #< WorldData
        STA world 
        LDA #> WorldData
        STA world+1
        JMP EndGetMapPosRight

    SetWorldDataOne:
        LDA #< WorldData
        STA world 
        LDA #> WorldData
        STA world+1
        JMP EndGetMapPosRight

    SetWorldDataTwo:
        LDA #< WorldData3
        STA world 
        LDA #> WorldData3
        STA world+1
        JMP EndGetMapPosRight

    SetWorldDataThree:
        LDA #< WorldData
        STA world 
        LDA #> WorldData
        STA world+1
        JMP EndGetMapPosRight

    SetWorldDataFour:
        LDA #< WorldData2
        STA world 
        LDA #> WorldData2
        STA world+1
        JMP EndGetMapPosRight

    SetWorldDataFive:
        LDA #< WorldData3
        STA world 
        LDA #> WorldData3
        STA world+1
        JMP EndGetMapPosRight

    SetWorldDataSix:
        LDA #< WorldData3
        STA world 
        LDA #> WorldData3
        STA world+1
        JMP EndGetMapPosRight




    EndGetMapPosRight:
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
    CMP #$00
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
        ;JSR ChangePalleteBlack
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
    BEQ CheckRightRelease
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
    JSR ChangePalleteOrange
RTS

InputB:
    JSR ChangePalleteBlack
RTS

InputBRelease:
 
RTS

InputStart:

RTS

InputStartRelease:

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
    CMP #$00
    BNE EndInputUp
    WalkUp:
        LDA entities+Entity::ypos
        SEC 
        SBC #$01
        STA entities+Entity::ypos
        LDA #$18
        STA entities+Entity::spriteno
        JSR CollideUp

EndInputUp:
RTS

InputUpRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteUp
    :
RTS

InputDown:
    LDA gamemode 
    CMP #$00 
    BNE EndInputDown
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
RTS

InputLeft:
    LDA gamemode 
    CMP #$00 
    BNE EndInputLeft
    WalkLeft:
    LDA dxlow
    SEC
    SBC #$10
    STA dxlow
    LDA dxhigh
    SBC #$00
    STA dxhigh
    LDA #$00
    STA entities+Entity::spriteno
    JSR CollideLeft
EndInputLeft:
RTS

InputLeftRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteLeft
    :
RTS

InputRight:
    LDA gamemode 
    CMP #$00 
    BNE EndInputRight
    WalkRight:
    LDA dxlow
    CLC 
    ADC #$12
    STA dxlow 
    LDA dxhigh
    ADC #$00
    STA dxhigh
    LDA #$00
    STA entities+Entity::spriteno
    JSR CollideRight
EndInputRight:
RTS

InputRightRelease:
    LDA gamemode
    CMP #$01
    BNE :+
    JSR InputNoteRight
    :
RTS
;;;;;;
; Entitity creation
;;;;;;
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
    CMP #$00 ; NO TYPE
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

ProcessEntities: ; TODO at some point there are going to be too many entity behaviours and BEQ wont branch far enough. Add a jmp index?
    LDX #$00
    ProcessEntitiesLoop:
        LDA entities+Entity::type, X 
        CMP #EntityType::PlayerType ; player id
        BEQ ProcessPlayerJump
        CMP #EntityType::Note ; note id
        BEQ ProcessNoteJump
        CMP #EntityType::Fireball
        BEQ ProcessFireJump
        CMP #EntityType::PlayerTwoType
        BEQ ProcessPlayerTwoJump
        CMP #EntityType::Eurydice
        BEQ ProcessEurydiceJump
        CMP #EntityType::NoteStatic
        BEQ ProcessNoteStaticJump
        CMP #EntityType::UpButton
        BEQ ProcessUpButtonJump
        CMP #EntityType::DownButton
        BEQ ProcessDownButtonJump
        CMP #EntityType::LeftButton
        BEQ ProcessLeftButtonJump
        CMP #EntityType::RightButton
        BEQ ProcessRightJump
        CMP #EntityType::AButton
        BEQ ProcessAButtonJump
        CMP #EntityType::BButton
        BEQ ProcessBButtonJump
        JMP SkipEntity

    ProcessPlayerJump:
        JMP ProcessPlayer
    ProcessNoteJump:
        JMP ProcessNote
    ProcessFireJump:
        JMP ProcessFire
    ProcessPlayerTwoJump:
        JMP ProcessPlayerTwo
ProcessEurydiceJump:
        JMP ProcessEurydice
ProcessNoteStaticJump:
        JMP ProcessNoteStatic
ProcessUpButtonJump:
        JMP ProcessUpButton
ProcessDownButtonJump:
        JMP ProcessDownButton
ProcessLeftButtonJump:
        JMP ProcessLeftButton
ProcessRightJump:
        JMP ProcessRightButton
ProcessAButtonJump:
        JMP ProcessAButton
ProcessBButtonJump:
        JMP ProcessBButton








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
        LDA waveflip
        CMP #$00
        BEQ NoteLeft
         NoteRight:
            LDA entities+Entity::xpos, X 
            CLC 
            ADC #$01
            JMP EndNoteLeftRight
         NoteLeft:
            LDA entities+Entity::xpos, X 
            SEC 
            SBC #$01
         EndNoteLeftRight:
        STA entities+Entity::xpos, X 
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
        LDA facing
        CMP #$00
        BEQ :+
        JMP ProcessEurydiceLeft
        :
        LDA entities+Entity::xpos 
        SBC #$10
        STA entities+Entity::xpos, X
        LDA entities+Entity::ypos
        LDY framecount
        ADC VerticalWave, Y
        STA entities+Entity::ypos, X
        JMP EntityComplete

        ProcessEurydiceLeft:
            LDA entities+Entity::xpos
            CLC 
            ADC #$18
            STA entities+Entity::xpos, X
            LDA entities+Entity::ypos
            LDY framecount
            ADC VerticalWave, Y
            STA entities+Entity::ypos, X
            JMP EntityComplete

    ProcessNoteStatic:
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$00
        STA entities+Entity::xpos, X
        JMP EntityComplete

    ClearEntity:
        LDA #EntityType::NoEntity
        STA entities+Entity::type, X
        LDA #$00
        STA entities+Entity::xpos, X
        STA entities+Entity::ypos, X
        STA entities+Entity::xposlow, X
        STA entities+Entity::yposlow, X
        STA entities+Entity::spriteno, X
 



    EntityComplete:
    SkipEntity:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #$46   ; Max entities?
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

DoScroll: ; check if the player is at the edge of the screen   
    LDA entities+Entity::xpos  
    CMP ScrollBoundary
    BCS ScrollRight
    CMP ScrollBoundaryLeft
    BCC ScrollLeft
    JMP EndDoScroll
    ScrollRight:
        LDA allowrightscroll
        CMP #$00
        BEQ :+
        JMP EndDoScroll
        :
        JSR DoRightScroll
        JMP EndDoScroll
    ScrollLeft:
        LDA allowleftscroll
        CMP #$00
        BEQ :+
        JMP EndDoScroll
        :
        JSR DoLeftScroll
    EndDoScroll: 
RTS  
    
    DoRightScroll:
        LDA entities+Entity::xpos
        SEC 
        SBC ScrollBoundary
        PHA 
        CLC 
        ADC ScrollXEight
        STA ScrollXEight
        PLA 
        CLC
        ADC ScrollX  
        STA ScrollX
        BCC MovePlayerBack
        LDA currenttable
        EOR #$01
        STA currenttable
        MovePlayerBack:
        LDA ScrollBoundary
        STA entities+Entity::xpos
    EndDoRightScroll:
        JSR GetMapPosRight
        JSR CheckRightDraw
    RTS

    CheckRightDraw:
        LDA ScrollXEight
        CMP #$08
        BCS DrawRightColumn
        RTS
        DrawRightColumn: ; divide by 8 for each tile
            ; Inrecent the column number. If it hits the limit wrap around 
            LDA ScrollXEight
            SEC 
            SBC #$08
            STA ScrollXEight

            LDA columnnumber
            CLC 
            ADC #$01
            ;AND #%00011111
            CMP #$21
            BNE :+
            INC pageX
            LDA #$01
            :
            STA columnnumber
        

            LDA ScrollX
            LSR 
            LSR 
            LSR
            STA columnlow

            LDA currenttable
            EOR #$01
            ASL 
            ASL  
            CLC 
            ADC #$20
            STA columnhigh

            
            LDA DrawColumnFlag ; set a flag that a new coloumn needs to be drawn to the nametable
            EOR #$01
            STA DrawColumnFlag       
        RTS

    DoLeftScroll:
        LDA entities+Entity::xpos 
        SEC  
        SBC ScrollBoundaryLeft
        PHA 
        SEC
        SBC ScrollXEight
        PLA 
        SEC 
        SBC ScrollX
        STA ScrollX
        BCS MovePlayerForward
        LDA currenttable
        EOR #$01
        STA currenttable
        MovePlayerForward:
        LDA ScrollBoundaryLeft
        STA entities+Entity::xpos
    EndDoLeftScroll:
        JSR CheckLeftDraw
    RTS

    CheckLeftDraw:
        LDA ScrollXEight
        CMP #$00
        BMI DrawLeftColumn
        RTS 
        DrawLeftColumn:
            LDA ScrollXEight
            CLC 
            ADC #$08 
            STA ScrollXEight

            DEC columnnumber
            LDA columnnumber
            AND #%00011111
            STA columnnumber
            BNE :+
            DEC pageX
            :

            LDA ScrollX
            SEC 
            SBC #$08

            LSR 
            LSR 
            LSR 

            STA columnlow

            LDA currenttable
            ;EOR #$01
            ASL 
            ASL 
            CLC 
            ADC #$20
            STA columnhigh

            LDA DrawColumnLeftFlag
            EOR #$01
            STA DrawColumnLeftFlag
        RTS
;;;;;;;;;
;;Collision madoodles
;;;;;;;

CollideDown:
    LDA #$00
    STA return
    ; LSR 4 times to divide by 16 (the size of our tiles on the tile map)
    LDA entities+Entity::ypos
    CLC 
    ADC #$0F ; add an offset of 16 for total entity height  
    LSR 
    LSR 
    LSR 
    LSR
    STA boxy1
    STA boxy2 

    LDA entities+Entity::xpos
    LSR 
    LSR 
    LSR 
    LSR
    STA boxx1 
    CLC 
    ADC #$01 ; The sprite width is 16, divide that by 16 to account for the tile map 
    STA boxx2

    CheckCollideOne:
        LDA boxy1 ; x16 for the width of the tile map
        ASL ;2
        ASL ;4
        ASL ;8
        ASL ; 16
        CLC 
        ADC boxx1

    
        TAX ; move to x so can be index
        STX return ; for debug 
        LDA TileMap, X 
        CMP #$01
        BEQ CollisionDown
  
    CheckCollideTwo:
    LDA boxy2 
    ASL ;2
    ASL ;4
    ASL ;8
    ASL 
    CLC
    ADC boxx2 

    TAX
    STX return 
    LDA TileMap, X 
    CMP #$01 
    BEQ CollisionDown
    RTS

    CollisionDown:
        LDA #$01 
        STA return
        LDA entities+Entity::ypos
        CLC 
        SBC #$01
        STA entities+Entity::ypos
        RTS


CollideUp:
    LDA #$00
    STA return
    ; LSR 4 times to divide by 16 (the size of our tiles on the tile map)
    LDA entities+Entity::ypos 
    LSR 
    LSR 
    LSR 
    LSR
    STA boxy1
    STA boxy2 

    LDA entities+Entity::xpos
    LSR 
    LSR 
    LSR 
    LSR
    STA boxx1 
    CLC 
    ADC #$01 ; The sprite width is 16, divide that by 16 to account for the tile map 
    STA boxx2

    CheckCollideOneUp:
        LDA boxy1 ; x16 for the width of the tile map
        ASL ;2
        ASL ;4
        ASL ;8
        ASL ; 16
        CLC 
        ADC boxx1

    
        TAX ; move to x so can be index
        STX return ; for debug 
        LDA TileMap, X 
        CMP #$01
        BEQ CollisionUp 
  
    CheckCollideTwoUp:
    LDA boxy2 
    ASL ;2
    ASL ;4
    ASL ;8
    ASL 
    CLC
    ADC boxx2 

    TAX
    STX return 
    LDA TileMap, X 
    CMP #$01 
    BEQ CollisionUp
    RTS

    CollisionUp:
        LDA #$01 
        STA return
        LDA entities+Entity::ypos
        CLC 
        ADC #$01
        STA entities+Entity::ypos
        RTS

    CollideRight:
    LDA #$00
    STA return
    ; LSR 4 times to divide by 16 (the size of our tiles on the tile map)
    LDA entities+Entity::ypos  
    LSR 
    LSR 
    LSR 
    LSR
    STA boxy1
    CLC 
    ADC #$01
    STA boxy2 

    LDA entities+Entity::xpos
    LSR 
    LSR 
    LSR 
    LSR 
    CLC 
    ADC #$01 ; The sprite width is 16, divide that by 16 to account for the tile map 
    STA boxx1
    STA boxx2

    CheckCollideOneRight:
        LDA boxy1 ; x16 for the width of the tile map
        ASL ;2
        ASL ;4
        ASL ;8
        ASL ; 16
        CLC 
        ADC boxx1

    
        TAX ; move to x so can be index
        STX return ; for debug 
        LDA TileMap, X 
        CMP #$01
        BEQ CollisionRight
  
    CheckCollideTwoRight:
    LDA boxy2 
    ASL ;2
    ASL ;4
    ASL ;8
    ASL 
    CLC
    ADC boxx2 

    TAX
    STX return 
    LDA TileMap, X 
    CMP #$01 
    BEQ CollisionRight
    RTS

    CollisionRight:
        LDA #$01 
        STA return
        LDA entities+Entity::xpos
        CLC 
        SBC #$01
        STA entities+Entity::xpos
        RTS

    CollideLeft:
    LDA #$00
    STA return
    ; LSR 4 times to divide by 16 (the size of our tiles on the tile map)
    LDA entities+Entity::ypos  
    LSR 
    LSR 
    LSR 
    LSR
    STA boxy1
    CLC 
    ADC #$01
    STA boxy2 

    LDA entities+Entity::xpos
    LSR 
    LSR 
    LSR 
    LSR 
    STA boxx1
    STA boxx2

    CheckCollideOneLeft:
        LDA boxy1 ; x16 for the width of the tile map
        ASL ;2
        ASL ;4
        ASL ;8
        ASL ; 16
        CLC 
        ADC boxx1

        TAX ; move to x so can be index
        STX return ; for debug 
        LDA TileMap, X 
        CMP #$01
        BEQ CollisionLeft
  
    CheckCollideTwoLeft:
    LDA boxy2 
    ASL ;2
    ASL ;4
    ASL ;8
    ASL 
    CLC
    ADC boxx2 

    TAX
    STX return 
    LDA TileMap, X 
    CMP #$01 
    BEQ CollisionLeft
    RTS

    CollisionLeft:
        LDA #$01 
        STA return
        LDA entities+Entity::xpos
        CLC 
        ADC #$02
        STA entities+Entity::xpos
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
    CMP #$00
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

FrictionN:
    LDA dxlow
    CLC 
    ADC FrictionValue
    STA dxlow
    LDA dxhigh
    ADC #$00
    STA dxhigh
RTS 

FrictionP:
    LDA dxlow
    SEC
    SBC FrictionValue
    STA dxlow
    LDA dxhigh
    SBC #$00
    STA dxhigh
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
JSR ChangePalleteBlack

ClearNoteMem:
    LDA #$00
    STA notecount
EndNoteMem:
RTS

SingMode:
    LDA SingMode
    CMP #$00
    BNE :+
    RTS
    : 

    LDA noteflag
    CMP #$00
    BNE :+
    RTS
    :
RTS

;;;;;;
; Palette functions
;;;;;
ChangePalleteBlack:
    LDA #< BackGroundPaletteBlack
    STA paletteaddress
    LDA #> BackGroundPaletteBlack
    STA paletteaddress+1
    JMP UpdatePalleteBuffer

ChangePalleteOrange: 
    LDA #< PaletteData
    STA paletteaddress
    LDA #> PaletteData
    STA paletteaddress+1
    JMP UpdatePalleteBuffer


UpdatePalleteBuffer:
    LDA updatebackgroundpaletteflag
    EOR #$01
    STA updatebackgroundpaletteflag

    LDY #$00
    UpdatePalleteBufferLoop:
    LDA (paletteaddress), Y
    STA PaletteBuffer, Y 
    CPY #$20
    BEQ :+
    INY
    JMP UpdatePalleteBufferLoop
    : 
RTS

UpdatePalleteNMI:
    LDA updatebackgroundpaletteflag
    CMP #$01
    BNE EndUpdatePalleteNMI


    LDA #$3F
    STA $2006
    LDA #$00
    STA $2006

    LDY #$00

    ;; Increment through the Pallete data and store it into the PPU
    LoadPalettesNMI:
        LDA PaletteBuffer, Y
        STA $2007 ; $3F00, $3F01, $3F02 => $3F1F
        INY
        CPY #$20
        BNE LoadPalettesNMI

        LDA updatebackgroundpaletteflag
        EOR #$01
        STA updatebackgroundpaletteflag
        
EndUpdatePalleteNMI:
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

        STA SpriteMem
        LDA #$02
        STA SpriteMem+1
        
       
        
    ; This checks the entity type of the current entity then branches accordingly
    DrawSprites:
        LDA entities+Entity::type, X 
        CMP #EntityType::NoEntity
        BEQ NoEntityJmp
        CMP #EntityType::PlayerType
        BEQ DrawPlayerJmp
        CMP #EntityType::Note
        BEQ DrawNoteJmp
        CMP #EntityType::Fireball
        BEQ DrawFireJmp
        CMP #$04
        BEQ DrawPlayerTwoJmp
        CMP #EntityType::Eurydice
        BEQ DrawEurydiceJump
        CMP #EntityType::NoteStatic
        BEQ DrawNoteStaticJump
        CMP #EntityType::UpButton
        BEQ DrawUpButtonJump
        CMP #EntityType::DownButton
        BEQ DrawDownButtonJump
        CMP #EntityType::LeftButton
        BEQ DrawLeftButtonJump
        CMP #EntityType::RightButton
        BEQ DrawRightButtonJump
        CMP #EntityType::AButton
        BEQ DrawAButtonJump
        CMP #EntityType::BButton
        BEQ DrawBButtonJump
        JMP EndSpriteDraw


    ;;;; This branch is to get aroud the limited range of BEQ 
    NoEntityJmp:
        JMP CheckEndSpriteDraw
    DrawPlayerJmp:
        JMP DrawPlayer
    DrawNoteJmp:
        JMP DrawNote 
    DrawFireJmp:
        JMP DrawFire
    DrawPlayerTwoJmp:
        JMP DrawPlayerTwo
    DrawEurydiceJump:
        JMP DrawEurydice
    DrawNoteStaticJump:
        JMP DrawNote
    DrawUpButtonJump:
        JMP DrawUp
    DrawDownButtonJump:
        JMP DrawDown
    DrawLeftButtonJump:
        JMP DrawLeft
    DrawRightButtonJump:
        JMP DrawRight
    DrawAButtonJump:
        JMP DrawA
    DrawBButtonJump:
        JMP DrawB
    ;;;;   

    DrawPlayer:
        LDA facing 
        CMP #$00
        BEQ :+
        JMP DrawPlayerLeft
        :
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        

        ;Sprite 2

        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC 
        ADC #$01
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
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
        LDA entities+Entity::spriteno
        CLC
        ADC #$02
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
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
        LDA entities+Entity::spriteno
        CLC 
        ADC #$03
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY
        JMP CheckEndSpriteDraw

        DrawPlayerLeft:

        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        STA SpriteBuffer, Y 
        INY 
        LDA #%01000000
        STA SpriteBuffer, Y
        INY
        LDA entities+Entity::xpos, X
        CLC
        ADC #$08
        STA SpriteBuffer, Y
        INY
        

        ;Sprite 2

        LDA entities+Entity::ypos, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC 
        ADC #$01
        STA SpriteBuffer, Y 
        INY 
        LDA #%01000000
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
         

        ;sprite 3
        LDA entities+Entity::ypos, X
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC
        ADC #$02
        STA SpriteBuffer, Y 
        INY 
        LDA #%01000000
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC
        ADC #$08
        STA SpriteBuffer, Y
        INY
        

        ;sprite 4
        LDA entities+Entity::ypos, X
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC 
        ADC #$03
        STA SpriteBuffer, Y 
        INY 
        LDA #%01000000
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        JMP CheckEndSpriteDraw 


DrawPlayerTwo: ; this is a pallete swap p1 but currently isn't spawned

        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        STA SpriteBuffer, Y 
        INY 
        LDA #$01
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        

        ;Sprite 2 p2

        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC 
        ADC #$01
        STA SpriteBuffer, Y 
        INY 
        LDA #$01
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY
         

        ;sprite 3 p2
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC
        ADC #$02
        STA SpriteBuffer, Y 
        INY 
        LDA #$01
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y
        INY
        

        ;sprite 4 p2
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno
        CLC 
        ADC #$03
        STA SpriteBuffer, Y 
        INY 
        LDA #$01
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y
        INY

        JMP CheckEndSpriteDraw


    DrawNote:
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
        CLC
        ADC  #$10
        STA SpriteBuffer, Y
        INY 
        JMP CheckEndSpriteDraw

    DrawFire:
        LDA entities+Entity::ypos, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y 
        INY  
        JMP CheckEndSpriteDraw
    
    DrawEurydice:
        LDA entities+Entity::ypos, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X
        STA SpriteBuffer, Y 
        INY 
        LDA #$01
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y 
        INY

        LDA entities+Entity::ypos, X
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::spriteno, X
        CLC 
        ADC #$01
        STA SpriteBuffer, Y 
        INY 
        LDA #$01
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y 
        INY  

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
        LDA #$01
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::xpos, X
        STA SpriteBuffer, Y 
        INY  

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
        LDA #$01
        STA SpriteBuffer, Y 
        INY 
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        STA SpriteBuffer, Y 
        INY  


        JMP CheckEndSpriteDraw    

    DrawUp:
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
        JMP CheckEndSpriteDraw

    DrawDown:
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
        JMP CheckEndSpriteDraw

    DrawLeft:
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
        JMP CheckEndSpriteDraw

    DrawRight:
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
        JMP CheckEndSpriteDraw
    
    DrawA:
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
        JMP CheckEndSpriteDraw
    
    DrawB:
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
        JMP CheckEndSpriteDraw
    
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

    LDA #$30 
    STA ApuBuffer
    STA ApuBuffer+4
    STA ApuBuffer+12
    LDA #$80
    STA ApuBuffer+8

    LDA #$00
    STA sounddisableflag
    STA sfxplaying
    STA sfxindex
    STA soundframecount
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
        STA streampointerlow, X 
        INY

        LDA (soundpointer), Y 
        STA streampointerhigh, X 
        
        LDA #$A0 
        STA streamtickertotal

        LDA #$01 
        STA streamnotelengthcount, X 

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

    JSR SoundSilence

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
        JSR SoundWriteBufferToAPU

    EndSoundPlayFrameLoop:
        INX 
        CPX #$06 
        BNE SoundPlayFrameLoop

        JSR SoundSetAPU
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
    CMP #$FF 
    BNE EndSoundGetByte
    LDA streamstatus, X 
    AND #%11111110
    STA streamstatus, X 

    LDA streamchannel, X 
    CMP #ChannelConst::Triangle 
    BEQ SoundSilenceTriangle
    LDA #$30 
    BNE SoundSilence 

SoundSilenceTriangle:
    LDA #$80 
SoundSilence:
    STA streamvolduty, X 
    JMP SoundUpdatePointer
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
    STY soundtemp1 
    ASL 
    TAY 
    LDA NoteTable, Y 
    STA streamnotelow, X 
    LDA NoteTable+1, Y 
    STA streamnotehigh, X 
    LDY soundtemp1
    JSR SoundCheckRest
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

SoundCheckRest: 
    LDA (soundpointer), Y 
    CMP #Octave::rest 
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

    LDA streamvolduty, X 
    STA ApuBuffer, Y

    LDA #$08 
    STA ApuBuffer+1, Y

    LDA streamnotelow, X 
    STA ApuBuffer+2, Y

    LDA streamnotehigh, X
    STA ApuBuffer+3, Y 

    LDA streamstatus, X 
    AND #%00000010
    BEQ EndSoundSetAPU

    LDA streamchannel, X 
    CMP #ChannelConst::Triangle
    BEQ tri  
    LDA #$30 
    BNE store 

    tri: 
        LDA #$80 
    store: 
        STA ApuBuffer, Y  
EndSoundSetAPU:
RTS 

SoundWriteBufferToAPU: 
    SoundWriteSquare1Buffer:
    LDA ApuBuffer
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

SongHeaders:
    .word song0header
    .word song1header


;;;;;;;;;;;;;;;;;;;;
; Tilebuffer
;;;;;;;;;;;;;;;;;;;;
WriteToTileBuffer:
    CheckTileDrawFlag:
        LDA DrawColumnFlag
        CMP #$01 
        BEQ :+
        JMP CheckTileDrawFlagLeft
        :
        JMP WriteColumnToBuffer

    CheckTileDrawFlagLeft:
        LDA DrawColumnLeftFlag
        CMP #$01 
        BEQ :+ 
        RTS 
        :
        JMP WriteColumnToBufferLeft    



    WriteColumnToBuffer:
        LDA world ; take the low byte
        STA columnaddress ; store low byte in z page
        LDA world+1 ; take the high byte
        STA columnaddress+1 ; store high into the world address +1 i.e the second byte of the address

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
        LDY #$00
    WriteTileLoop:
        LDA (columnaddress),Y 
        STA TileBuffer, X
        LDA columnaddress
        CLC 
        ADC #$20
        STA columnaddress
        LDA columnaddress+1
        ADC #$00
        STA columnaddress+1
        DEX
        BNE WriteTileLoop
RTS

    WriteColumnToBufferLeft:
        LDA #< WorldData
        STA columnaddress
        LDA #> WorldData
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
        LDY #$00
    WriteTileLoopLeft:
        LDA (columnaddress), Y 
        STA TileBuffer, X  
        LDA columnaddress
        CLC 
        ADC #$20
        STA columnaddress
        LDA columnaddress+1
        ADC #$00
        STA columnaddress+1
        DEX
        BNE WriteTileLoop
RTS

;;;;;;;;;;;;;;;;;;;

NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

;Variables

FrictionValue:
    .byte $09

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
    .byte $17,$00,$0C,$0F,  $0F,$07,$17,$0F,  $0F,$07,$10,$0F, $0F,$14,$15,$16  ;background palette data  
    .byte $17,$27,$14,$1A,  $22,$09,$1C,$0C,  $04,$16,$30,$27,$30,$0F,$36,$17  ;sprite palette data

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
    .byte $FF, $00, $01, $02, $03, $04, $05, $FF
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
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$43,$44,$24,$24,$47,$47,$47,$47,$47,$47,$47,$47,$24,$24,$43,$44,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$26,$26,$26,$26,$26,$26,$26,$26,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$41,$41,$41,$41,$41,$41,$41,$41,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$42,$42,$42,$42,$42,$42,$42,$42,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$4B,$25,$25,$25,$25,$25,$25,$25,$25,$4C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$4B,$45,$45,$45,$45,$45,$45,$45,$45,$4C,$24,$24,$24,$24,$24,$24,$24,$3B,$3C,$24,$24
    .byte $33,$24,$3D,$3E,$24,$24,$24,$24,$24,$24,$24,$4B,$46,$46,$46,$46,$46,$46,$46,$46,$4C,$24,$24,$24,$24,$24,$24,$24,$3D,$3E,$24,$24
    .byte $34,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$27,$27,$27,$27,$27,$27,$27,$27,$4C,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24

    .byte $33,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$27,$27,$27,$27,$27,$27,$27,$27,$4C,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24
    .byte $34,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$4B,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4D,$4C,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24
    .byte $33,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24

    .byte $34,$24,$43,$44,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$43,$44,$24,$24
    .byte $37,$38,$37,$38,$37,$38,$37,$37,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38

    .byte $39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32; Overscan blank line
 

WorldData2: ; Each row is 32
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$31 ; Overscan blank line
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F

    .byte $24,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$56,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$25,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36  
    .byte $37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38

    .byte $39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32; Overscan blank line


    WorldData3: ; Each row is 32
    .byte $33,$33,$34,$35,$36,$37,$38,$39,$3A,$3B,$3C,$3D,$3E,$33,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32 ; Overscan blank line
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$41

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$41,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36  
    .byte $37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38

    .byte $39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32; Overscan blank line

ColumnOne:
    .byte $32,$2C,$2E,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$25,$25,$37,$39,$32

ColumnTwo:
    .byte $32,$2D,$2F,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$25,$25,$38,$3A,$32

TileMap: ;1= solid
    
    .byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$00
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01

    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01

    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01
    .byte $01,$01,$00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

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
    .byte $77 
    .word song1square1
    .byte $40

    .byte Stream::MusicSquareTwo 
    .byte $01 
    .byte ChannelConst::SquareTwo
    .byte $B7 
    .word song1square2
    .byte $40

    .byte Stream::MusicTriangle 
    .byte $01 
    .byte ChannelConst::Triangle
    .byte $81 
    .word song1triangle
    .byte $40

    .byte Stream::MusicNoise
    .byte $00

;    .byte Stream::SfxOne
;    .byte $00

;    .byte Stream::SfxTwo
;    .byte $00


song1square1:
    .byte NoteLength::thirtysecond, Octave::B2, Octave::D3, Octave::F3, Octave::GS3, Octave::B3, Octave::D4, Octave::F4, Octave::GS4, Octave::B4, Octave::D5, Octave::F5, Octave::GS5, Octave::B5, Octave::D6, Octave::F6, Octave::GS6
    .byte Octave::BB2, Octave::DB3, Octave::E3, Octave::G3, Octave::BB3, Octave::DB4, Octave::E4, Octave::G4, Octave::BB4, Octave::DB5, Octave::E5, Octave::G5, Octave::BB5, Octave::DB6, Octave::E6, Octave::G6 
    .byte $FF 

song1square2:
    .byte NoteLength::thirtysecond, Octave::GS5, Octave::F5, Octave::D5, Octave::GS5, Octave::F5, Octave::D5, Octave::B4, Octave::F5, Octave::D5, Octave::B4, Octave::GS4, Octave::D5, Octave::B4, Octave::GS4, Octave::F4, Octave::B4 
    .byte Octave::G5, Octave::E5, Octave::DB5, Octave::G5, Octave::E5, Octave::DB5, Octave::BB4, Octave::E5, Octave::DB5, Octave::BB4, Octave::G4, Octave::DB5, Octave::BB4, Octave::G4, Octave::E4, Octave::BB4 
    .byte $FF 

song1triangle:
    .byte NoteLength::thirtysecond, Octave::F6, Octave::D6, Octave::B5, Octave::D6, Octave::B5, Octave::GS5, Octave::B5, Octave::GS5, Octave::F5, Octave::GS5, Octave::F5, Octave::D5, Octave::F5, Octave::D5, Octave::B4, Octave::GS4
    .byte Octave::E6, Octave::DB6, Octave::BB5, Octave::DB6, Octave::BB5, Octave::G5, Octave::BB5, Octave::G5, Octave::E5, Octave::G5, Octave::E5, Octave::DB5, Octave::E5, Octave::DB5, Octave::BB4, Octave::G4 
    .byte $FF

.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "Bank1.chr"
