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
.endscope

.struct Entity
    xpos .byte
    xposlow .byte
    ypos .byte
    yposlow .byte 
    type .byte
    spriteno .byte
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
    sineindex: .res 1 ; unused
   ;NB DO NOT DELETE ANY OF THESE EVEN IF THEY AREN'T BEING USED IT WILL MESS UP THE ENTITY HANDLER ATM

    MAXENTITIES = 5
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
    ScreenX: .res 1
    ScreenY: .res 1
    DrawColumnFlag: .res 1
    columnaddress: .res 2
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
    LDA #$FF 
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
    LDA #$80
    STA entities+Entity::xpos
    LDA #$60
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

;Set movespeed
LDA #$00
STA dxhigh
LDA #$00
STA dxlow  

;Set Zeropage variables
LDA #$00
STA ScreenX
STA ScreenY

JSR SpawnEurydice

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
    ;JSR PlaySound   ; does nothing atm except play an annoying noise
    JSR WaveFlip    ; This simply flips between 1 and 0. Used for directional variance
    JSR NoteIndex   ; This changes the note sprite that will spawn
    ;JSR SpawnFire   ; spawns a fireball as long as there's available mem
    ;JSR SpawnNote
    JSR ReadButtons ; Duh
    JSR CollideDown ; Think about moving this together with movement?
    JSR XMovement
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


    LDA #%10010000
    ORA currenttable
    ORA #%00000100
    STA $2000

    LDA #%00111110
    STA $2001

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
    RTS 
    :
    ; Turn off the flag 
    LDA DrawColumnFlag
    EOR #$01
    STA DrawColumnFlag

    ; Grab the coords for where thecolumn needs to be drawn
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

PlaySound: ; unused atm
    LDA #<279
    STA $4002

    LDA #<279
    STA $4003

    LDA #%10111111
    STA $4000
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
    JMP CheckB

    CheckARelease: ; If the button isn't pressed, check whether it was pressed last frame and released
        LDA ButtonFlag
        AND #$01
        BEQ CheckB
        LDA ButtonFlag
        EOR #$01 
        STA ButtonFlag
        JSR SpawnNote   ; Any behaviour can go here and will happen when the button is released

CheckB:

    LDA buttons 
    AND #%01000000
    BEQ CheckBRelease
    LDA ButtonFlag
    ORA #$02
    STA ButtonFlag
    JMP CheckSelect

    CheckBRelease:
        LDA ButtonFlag
        AND #$02
        BEQ CheckSelect
        LDA ButtonFlag
        EOR #$02 
        STA ButtonFlag
        JSR SpawnFire


CheckSelect:
    LDA buttons
    AND #%00100000
    BEQ CheckStart    

CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckUp

CheckUp:
        LDA buttons
        AND #%00001000
        BEQ  CheckDown  

        WalkUp:
        LDA entities+Entity::ypos
        SEC 
        SBC #$01
        STA entities+Entity::ypos
        LDA #$18
        STA entities+Entity::spriteno
        JSR CollideUp
        JMP EndButtons

CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckLeft

    WalkDown:
    LDA entities+Entity::ypos
    CLC
    ADC #$01 
    STA entities+Entity::ypos
    LDA #$10
    STA entities+Entity::spriteno

    JMP EndButtons

CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckRight

    WalkLeft:
    LDA dxlow
    CLC
    SBC SpeedValue
    STA dxlow

    LDA dxhigh
    SBC #$00
    STA dxhigh

    LDA #$00
    STA entities+Entity::spriteno

    ; set facing to 1 if moving left, does nothingatm
    LDA #$01
    STA facing

    JSR CollideLeft

    JMP EndButtons 

CheckRight:

    LDA buttons
    AND #%00000001
    BEQ EndButtons

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

    LDA #$00 
    STA facing

    JSR CollideRight

EndButtons:
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

SpawnFire:
    LDX #$00
FireLoop:
    CPX #TOTALENTITIES ; Check whether we're at the end of allowed entities
    BEQ EndNoteSpawn
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
        BEQ ProcessPlayer
        CMP #EntityType::Note ; note id
        BEQ ProcessNote
        CMP #EntityType::Fireball
        BEQ ProcessFire
        CMP #$04
        BEQ ProcessPlayerTwo
        CMP #EntityType::Eurydice
        BEQ ProcessEurydice
        JMP SkipEntity

    ProcessPlayer:
        ;JSR PlayerCollide
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
            ADC #$18
            STA entities+Entity::xpos, X
            LDA entities+Entity::ypos
            LDY framecount
            ADC VerticalWave, Y
            STA entities+Entity::ypos, X
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
    CMP #$1E   ; Max entities?
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
        JSR DoRightScroll
        JMP EndDoScroll
    ScrollLeft:
        JSR DoLeftScroll
    EndDoScroll: 
RTS  
    
    DoRightScroll:
        LDA entities+Entity::xpos
        SEC 
        SBC ScrollBoundary
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
        JSR CheckRightDraw
    RTS

    CheckRightDraw:
        LDA ScrollX
        AND #%00000111
        BEQ DrawRightColumn
        RTS
        DrawRightColumn: ; divide by 8 for each tile
            LDA columnnumber
            CLC 
            ADC #$01
            AND #%00011111
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

            

            LDA Columnflag
            EOR #$01
            STA Columnflag
            LDA DrawColumnFlag ; set a flag that a new coloumn needs to be drawn to the nametable
            EOR #$01
            STA DrawColumnFlag       
        RTS

    DoLeftScroll:
        LDA ScrollBoundaryLeft
        SEC  
        SBC entities+Entity::xpos
        STA temp
        LDA ScrollX
        SEC
        SBC temp  
        STA ScrollX
        BCS MovePlayerForward
        LDA currenttable
        EOR #$01
        STA currenttable
        MovePlayerForward:
        LDA ScrollBoundaryLeft
        STA entities+Entity::xpos
    EndDoLeftScroll:
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
        ADC #$01
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
    BPL LimitDXP   ; If its positive, branch

    LimitDXN:
        JSR FrictionN
        LDA dxhigh
        CMP #$FE 
        BCS AddDX
        LDA #$FE
        STA dxhigh
        LDA #$00
        STA dxlow
        JMP AddDX
    LimitDXP:
        JSR FrictionP
        LDA dxhigh
        CMP #$02
        BCC AddDX
        LDA #$02
        STA dxhigh
        LDA #$00
        STA dxlow


    AddDX:
    LDA entities+Entity::xposlow
    CLC 
    ADC dxlow 
    STA entities+Entity::xposlow
    LDA entities+Entity::xpos
    ADC dxhigh
    STA entities+Entity::xpos
 
EndXMovement:
RTS

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
        LDA #$03
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

;;;;;;;;;;;;;;;;;;;;
; Tilebuffer
;;;;;;;;;;;;;;;;;;;;
WriteToTileBuffer:

    LDA DrawColumnFlag
    CMP #$01 
    BEQ :+
    RTS 
    :
    
    LDA #< WorldData2 ; take the low byte
    STA columnaddress ; store low byte in z page
    LDA #> WorldData2 ; take the high byte
    STA columnaddress+1 ; store high into the world address +1 i.e the second byte of the address



    LDA columnnumber
   ; ASL 
  ;  ASL 
 ;   ASL 
 ;   ASL
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

    ;INY 
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
  .byte $17,$17,$27,$0F,  $0F,$16,$03,$04,  $0F,$06,$07,$08,$0F,$1A,$1B,$1C  ;background palette data
  .byte $17,$27,$14,$1A,  $22,$09,$1C,$0C,  $04,$16,$30,$27,$30,$0F,$36,$17  ;sprite palette data

AttributeData: 
    .byte %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000

Sine: ; unused, potential use for wave pattern
    .byte $01,$02,$03,$04,$05,$06,$07,$06,$05,$04,$03,$02,$FF

APURegs: ; sound, unused
    .byte $30,$08,$00,$00
    .byte $30,$08,$00,$00
    .byte $80,$00,$00,$00
    .byte $30,$00,$00,$00
    .byte $00,$00,$00,$00        

;Todo: Compression of worldata

WorldMap: ; test, unused
    .byte $1, $2, $3
    .byte $4, $5, $6
    .byte $7, $8, $9

WorldData: ; Each row is 32
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32 ; Overscan blank line
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$0E,$1D,$1A,$1D,$0D,$12,$13,$10,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24   
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24  
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$13,$0A,$12,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24 
    .byte $34,$20,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$18,$1A,$1F,$0E,$1D,$1B,$4A,$4B,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$20,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$20,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$44,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$46,$45,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3B,$3C,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $33,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $34,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $33,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $34,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$3F,$40,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38,$37,$38

    .byte $39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A,$39,$3A
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32; Overscan blank line
 

WorldData2: ; Each row is 32
    .byte $32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32,$32 ; Overscan blank line
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

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
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

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$35
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$36
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$25,$35

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
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00

    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
    .byte $00,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
    
.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS" ; sprite/tile data goes here
    .incbin "zelda.chr"
