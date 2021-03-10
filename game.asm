;;;; The header stuff is basically for the emulator

.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $01 ; 1 * 8KB CHR ROM
.byte %00000011 ; mapper and mirroring
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
.endscope

.struct Entity
    xpos .byte
    ypos .byte
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

    MAXENTITIES = 5 ; max allowed number of entities
    entities: .res .sizeof(Entity) * MAXENTITIES 
    TOTALENTITIES = .sizeof(Entity) * MAXENTITIES

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

    LDX #$04 ; add/sub 4 for each entity you want loaded initially
    LDA #$FF

CLEARENTITIES:
    STA entities+Entity::xpos, X
    STA entities+Entity::ypos, X
    STA entities+Entity::spriteno, X 
    LDA #$00
    STA entities+Entity::type, X
    LDA #$FF 
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

;SetMirroring: ; Doesn't work atm? Not sure if the mapper is incorrectly set up
   ; LDA #$80
   ; STA $8000
   ; LDA $00
   ; STA $8000


  ;  LDA #$0F
  ;  STA $8000
  ;  LSR
  ;  STA $8000
  ;  LSR 
  ;  STA $8000
  ;  LSR 
  ;  STA $8000
  ;  LSR 
  ;  STA $8000



; map some memory baabbbby!
; All sprite data is to be stored here and retrieved every frame
SpriteBuffer = $0200
LDA #$20 ; put this somewhere else? 
STA nextnote


;JSR SpawnFire
;JSR SpawnNote
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
    JSR ProcessEntities ; entity behaviour is handled here, the player has some special stuff elsewhere
    JSR IncFrameCount   ; Counts to 59 then resets to 0
    JSR DoScroll    
    JSR OAMBuffer   ; Sprite data is written to the buffer here
    
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
    JSR ReadSprites ; Get the sprites from the sprite buffer and write them to the ppu  
    JSR ReadScroll  ; Send the current scroll to the ppu
    INC nmidone
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
    ;LDA ScrollX
    ;CMP #$FF
    ;BNE :+
    ;LDA #%10010001
    ;STA $2000
   ; :

    LDA currenttable
    CMP #$00
    BNE SetTableTwo
    SetTableOne:
    LDA #%10010000
    STA $2000
    JMP SetScroll
    SetTableTwo:
    LDA #%10010001
    STA $2000

    SetScroll:
    LDA $2002
    LDA ScrollX
    STA $2005
    LDA ScrollY
    STA $2005
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
        CPX #$A0
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
    LDA entities+Entity::xpos
    SEC 
    SBC #$01
    STA entities+Entity::xpos


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

    LDA entities+Entity::xpos
    CLC
    ADC #$01
    STA entities+Entity::xpos

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

SpawnNote:
    LDX #$00
NoteLoop:
    CPX #TOTALENTITIES ; Check whether we're at the end of allowed entities
    BEQ EndNoteSpawn
; Checkif the current index has nothing in it   
    LDA entities+Entity::type, X 
    CMP #$00 ; NO TYPE
    BEQ AddNote
    TXA 
    CLC 
    ADC #.sizeof(Entity) ; This adds to the X index so that we can keep looping through all entitiy memory
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
    ADC #.sizeof(Entity) ; This adds to the X index so that we can keep looping through all entitiy memory
    TAX
    JMP FireLoop

AddFire:
    LDA #$0F
    STA entities+Entity::xpos, X
    LDA #$B1
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
;; entitiy processsing 
;;;;;;;;

ProcessEntities: ; TODO at some point there are going to be too many entity behaviours and BEQ wont branch far enough. Add a jmp index?
    LDX #$00
    ProcessEntitiesLoop:
        LDA entities+Entity::type, X 
        CMP #$01 ; player id
        BEQ ProcessPlayer
        CMP #$02 ; note id
        BEQ ProcessNote
        CMP #$03
        BEQ ProcessFire
        CMP #$04
        BEQ ProcessPlayerTwo
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

    ClearEntity:
        LDA #EntityType::NoEntity
        STA entities+Entity::type, X
        LDA #$00
        STA entities+Entity::xpos, X
        STA entities+Entity::ypos, X
 



    EntityComplete:
    SkipEntity:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #$28   ; Max entities?
    BNE ProcessEntitiesLoop

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
    CMP #$E0
    BCS ScrollRight
    CMP #$0F
    BCC ScrollLeft
    JMP EndDoScroll
    ScrollRight:
        JSR DoRightScroll
        JMP EndDoScroll
    ScrollLeft:
       ; JSR DoLeftScroll
    EndDoScroll: 
RTS  
    
DoRightScroll:
    LDA ScrollX
    CLC 
    ADC #$01 ;Scroll X needs to change by a factor of 256  
    STA ScrollX
    BCC MovePlayerBack
    LDA currenttable
    CMP #$00
    BEQ TableOneToTwo
    LDA #$00
    STA currenttable
    JMP MovePlayerBack
    TableOneToTwo:
    LDA #$01
    STA currenttable
    MovePlayerBack:
    LDA entities+Entity::xpos
    SEC 
    SBC #$01
    STA entities+Entity::xpos
EndDoRightScroll:
RTS

DoLeftScroll:
    LDA ScrollX
    CLC   
    SBC #$01 ;Scroll X needs to change by a factor of 256  
    STA ScrollX
    BVC MovePlayerForward
    LDA currenttable
    CMP #$00
    BEQ TableOneToTwo2
    LDA #$00
    STA currenttable
    JMP MovePlayerForward
    TableOneToTwo2:
    LDA #$01
    STA currenttable
    MovePlayerForward:
    LDA entities+Entity::xpos
    CLC 
    ADC #$01
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
        CMP #$00
        BEQ NoEntityJmp
        CMP #$01
        BEQ DrawPlayerJmp
        CMP #$02
        BEQ DrawNoteJmp
        CMP #$03
        BEQ DrawFireJmp
        CMP #$04
        BEQ DrawPlayerTwoJmp
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
    ;;;;   

    DrawPlayer:;
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


NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

PaletteData:
  .byte $0F,$27,$27,$0F,$01,$02,$03,$04,$05,$06,$07,$08,$09,$1A,$1B,$1C  ;background palette data
  .byte $22,$27,$14,$1A,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17  ;sprite palette data

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
    .byte $2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C ; Overscan blank line
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D  
    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F

    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25

    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$40,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25

    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25

    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25

    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25

    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $33,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25
    .byte $34,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25  
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D 

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F
    .byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29; Overscan blank line
 

WorldData2: ; Each row is 32
    .byte $2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C,$2C ; Overscan blank line
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D  
    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35

    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$35
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$36  
    .byte $2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D,$30,$2C,$2D 

    .byte $2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F,$31,$2E,$2F
    .byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29; Overscan blank line
 
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
