;;;; The header stuff is basically for the emulator

.segment "HEADER" 
.byte "NES"
.byte $1a
.byte $02 ; 2 * 16KB PRG ROM
.byte $01 ; 1 * 8KB CHR ROM
.byte %00000000 ; mapper and mirroring
.byte $0000
.byte $00
.byte $00
.byte $00
.byte $00, $00, $00, $00, $00 ; filler bytes


.scope EntityType
    NoEntity = 0
    PlayerType = 1
    Note = 2
.endscope

.struct Entity
    xpos .byte
    ypos .byte
    type .byte
   ; spriteno .byte 
.endstruct



.segment "ZEROPAGE" ; LSB 0 - FF
;; Reserve memory for some specific things we need not to be futzed with
    SpriteMem: .res 2
    world: .res 2  ; 1-2
    buttons: .res 1 ; 3
    nmidone: .res 1 ; ppu is done when this is 1 ;
    framecount: .res 1 ; 6. This will increment once per frame and reset over 59 (0 counts)
    twocount: .res 1
    ScrollX: .res 1 ;   7
    ScrollY: .res 1 ;   8
    FlipSprite: .res 1 ; 0 is right, 1 is left
    animation: .res 1
    XOffset: .res 1
    YOffset: .res 1
    moving: .res 1
    anioffset: .res 1
    facing: .res 1
    facingframe: .res 1
    playeraddress: .res 2
    sineindex: .res 1
   

    MAXENTITIES = 10
    entities: .res .sizeof(Entity) * MAXENTITIES
    TOTALENTITIES = .sizeof(Entity) * MAXENTITIES

    waveflip: .res 1
    nextnote: .res 1
    thirtyframe: .res 1
    fifteenframe: .res 1


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
    STA $0200, X ; Sprite data goes here
    LDA #$00
    INX
    BNE CLEARMEM    ; Keep incrementing so you clear out the whole thing

; Clear out all of the entities
; init 
INIT_ENTITIES:
    LDA #$70
    STA entities+Entity::xpos
    LDA #$B3
    STA entities+Entity::ypos
    LDA entities+Entity::type
    STA entities+Entity::type

    LDX #$03
    LDA #$FF

CLEARENTITIES:
    STA entities+Entity::xpos, X
    STA entities+Entity::ypos, X
    LDA #$00
    STA entities+Entity::type, X
    LDA #$FF 
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

InitSprite:
    LDA #<ZeldaSprite
    STA playeraddress
    LDA #>ZeldaSprite
    STA playeraddress+1 
    

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
    LDA #$55
    STA $2007
    INX
    CPX #$40
    BNE SetAttributes

    LDX #$00
    LDY #$00    

InitApu:
    LDY #$13
    InitApuLoop:
        LDA APURegs, Y 
        STA $4000, Y 
        DEY 
        BPL InitApuLoop

        LDA #$0F 
        STA $4015
        LDA #$40
        STA $4017




; map some memory baabbbby!
SpriteBuffer = $0200
LDA #$20
STA nextnote

; Enable interrupts
    CLI

    LDA #%10010000 ; enable NMI change background to use second chr set of tiles ($1000)
    STA $2000
    ; Enabling sprites and background for left-most 8 pixels
    ; Enable sprites and background
    LDA #%00011110
    STA $2001

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This is the forever loop, it goes here whenever its taken out of the NMI intterupt loop. Here is *ideally* where non draw stuff will happen...
; It runs through the whole game loop, then waits for the screen to be drawn then loops back to the beginning.
Loop:
    JSR PlaySound
    JSR WaveFlip 
    JSR NoteIndex
    JSR ReadButtons
    JSR ProcessEntities
    JSR ChangeFacing
    JSR IncFrameCount
    ;JSR Animate
    JSR OAMBuffer
    
    
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
    JSR ReadSprites
    JSR Scroll
    INC nmidone
    RTI

; Loading into 4014 automatically takes what you give as a high byte and writes 256 bytes to the PPU (so 02 == 0200 all thr way to FF)
; In a real nes this neads to be done every frame b/c dynamic ram degradation? 
ReadSprites:

    LDA #$00
    STA $2003
    LDA #$02 ; copy sprite data from $0200 => PPU memory for display.
    STA $4014
    LDX #$00
RTS

Scroll:
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


;;; TODO: Make buffer wiping dynamic based on what's in there.
ClearSpriteBuffer:
    LDX #$00
    LDA #$FF 
    ClearBufferLoop:
        STA $0200, X
        INX 
        CPX #$A0
        BNE ClearBufferLoop 
    RTS

PlaySound:  

RTS 

WaveFlip:
    LDA thirtyframe
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

NoteIndex:
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


NeutralValues:
    LDA #$00
    STA moving

; Music Note Spawning todo!
CheckA:
    
    LDA buttons
    AND #%10000000
    JSR SpawnNote    
    BEQ CheckB
    JSR SpawnNote    
    LDX #$00
   
  

CheckB:
    LDA buttons
    AND #%01000000
    BEQ CheckSelect
    LDX #$00
    

CheckSelect:
 LDA buttons
    AND #%00100000
    BEQ CheckStart

    LDX ScrollX
    INX
    STX ScrollX
    LDX #$00


CheckStart:
    LDA buttons
    AND #%00010000
    BEQ CheckUp

CheckUp:
    LDA buttons
    AND #$08
    BEQ CheckDown

    LDA entities+Entity::ypos
    CLC
    SBC #$01
    LDA entities+Entity::ypos

    DEC YOffset ; Y move
    LDA #$00
    STA facing
       
    JMP EndButtons


CheckDown:
    LDA buttons
    AND #%00000100
    BEQ CheckLeft

    LDA entities+Entity::ypos
    CLC
    ADC #$01
    LDA entities+Entity::ypos

    LDA #$01
    STA facing

    INC YOffset

    JMP EndButtons

CheckLeft:
    LDA buttons
    AND #%00000010
    BEQ CheckRight
    DEC XOffset

    LDA entities+Entity::xpos
    CLC
    SBC #$01
    LDA entities+Entity::xpos


    LDA #$02
    STA facing
    

    LDA #$01
    STA moving

    JMP EndButtons 

CheckRight:
    LDA buttons
    AND #%00000001
    BEQ EndButtons
    INC XOffset

    LDA entities+Entity::xpos
    CLC
    ADC #$01
    LDA entities+Entity::xpos


    LDA #$03
    STA facing

    LDA #$01

    STA moving
  

EndButtons:
RTS


SpawnNote:
    LDX #$00
    LDY thirtyframe
    CPY #$00 ; Check if frame count is 0
    BEQ NoteLoop
    JMP EndNoteSpawn
NoteLoop:
    CPX #TOTALENTITIES ; Check whether we're at the end of allowed entities
    BEQ EndNoteSpawn
    
    LDA entities+Entity::type, X 
    CMP #$00 ; NO TYPE
    BEQ AddNote
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX
    JMP NoteLoop

AddNote:
    LDA entities+Entity::xpos
    CLC 
    ADC #$01
    STA entities+Entity::xpos, X
    LDA entities+Entity::ypos
    SBC #$02
    STA entities+Entity::ypos, X
    LDA #$02 ; note type
    STA entities+Entity::type, X
    JMP EndNoteSpawn 



EndNoteSpawn:
    RTS

IncFrameCount:
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

ProcessEntities:
    LDX #$00
    ProcessEntitiesLoop:
        LDA entities+Entity::type, X 
        CMP #$01 ; player id
        BEQ ProcessPlayer
        CMP #$02 ; note id
        BEQ ProcessNote
        JMP SkipEntity
    ProcessPlayer:
        LDA entities+Entity::ypos, X
        CLC  
        ADC #$00
        STA entities+Entity::ypos, X
        LDA entities+Entity::xpos, X 
        CLC
        ADC #$00
        STA entities+Entity::xpos, X
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
    ClearEntity:
        LDA #EntityType::NoEntity
        STA entities+Entity::type, X
        LDA #$FF 
        STA entities+Entity::xpos, X
        STA entities+Entity::ypos, X



    EntityComplete:

    SkipEntity:
    TXA 
    CLC 
    ADC #.sizeof(Entity)
    TAX 
    CMP #$1E
    BNE ProcessEntitiesLoop

    DoneProcessingEntities:
    NOP
    NOP
    NOP
    NOP
RTS


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write to the OAM Buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

OAMBuffer:

    JSR ClearSpriteBuffer

    BlankOutMem:
        LDA #$01 ; PLAYER TYPE
        STA $17

        ;LDA #$02
        ;STA $1A


        LDX #$00
        LDY #$00
        LDA #$00

        STA SpriteMem
        LDA #$02
        STA SpriteMem+1
        
       
        
 
    DrawSprites:
        LDA entities+Entity::type, X 
        CMP #$00
        BEQ NoEntityJmp
        CMP #$01
        BEQ DrawPlayerJmp
        CMP #$02
        BEQ DrawNoteJmp
        JMP EndSpriteDraw


    ;;;; This branch is to get aroud the limited range of BEQ 
    NoEntityJmp:
        JMP CheckEndSpriteDraw
    DrawPlayerJmp:
        JMP DrawPlayer
    DrawNoteJmp:
        JMP DrawNote ;
    ;;;;   

    DrawPlayer:
        LDA entities+Entity::ypos, X 
        CLC
        ADC  YOffset
        STA SpriteBuffer, Y 
        INY 
        LDA #$10
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC
        ADC  XOffset
        STA SpriteBuffer, Y
        INY
        

        ;Sprite 2

        LDA entities+Entity::ypos, X
        CLC
        ADC  YOffset 
        STA SpriteBuffer, Y 
        INY 
        LDA #$11
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        ADC  XOffset
        STA SpriteBuffer, Y
        INY
         

        ;sprite 3
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        ADC YOffset
        STA SpriteBuffer, Y 
        INY 
        LDA #$12
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        ADC XOffset
        STA SpriteBuffer, Y
        INY
        

        ;sprite 4
        LDA entities+Entity::ypos, X
        CLC 
        ADC #$08 
        ADC YOffset
        STA SpriteBuffer, Y 
        INY 
        LDA #$13
        STA SpriteBuffer, Y 
        INY 
        LDA #$00
        STA SpriteBuffer, Y 
        INY
        LDA entities+Entity::xpos, X
        CLC 
        ADC #$08
        ADC XOffset
        STA SpriteBuffer, Y
        INY

        JMP CheckEndSpriteDraw

    DrawNote:
        LDA entities+Entity::ypos, X 
        STA SpriteBuffer, Y 
        INY 
        LDA #$20
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

ChangeFacing:

    LDA facing
    CMP #$00
    BEQ FaceUp
    CMP #$01
    BEQ FaceDown
    CMP #$03
    BEQ FaceRight
    CMP #$02
    BEQ FaceLeft
    
FaceUp:
    LDA #$18
    STA facingframe

    LDA #<ZeldaSpriteUp
    STA playeraddress
    LDA #>ZeldaSpriteUp
    STA playeraddress+1 

    JMP EndFacing

FaceDown:
    LDA #$10
    STA facingframe

    LDA #<ZeldaSpriteDown
    STA playeraddress
    LDA #>ZeldaSpriteDown
    STA playeraddress+1 
   
    JMP EndFacing

FaceRight:
    LDA #$00
    STA facingframe

    LDA #<ZeldaSprite
    STA playeraddress
    LDA #>ZeldaSprite
    STA playeraddress+1 
   
    JMP EndFacing

FaceLeft:
    

    LDA #<ZeldaSpriteLeft
    STA playeraddress
    LDA #>ZeldaSpriteLeft
    STA playeraddress+1 

    JMP EndFacing
EndFacing:


    RTS








NMI:            ; this happens once a frame when the draw arm mabob is returning to the top of the screen
    JMP MAINLOOP
    RTI

PaletteData:
  .byte $22,$29,$1A,$0F,$22,$36,$17,$0f,$22,$30,$21,$0f,$22,$27,$17,$0F  ;background palette data
  .byte $22,$27,$14,$1A,$22,$1A,$30,$27,$22,$16,$30,$27,$22,$0F,$36,$17  ;sprite palette data

Sine:
    .byte $01,$02,$03,$04,$05,$06,$07,$06,$05,$04,$03,$02,$FF

APURegs:
    .byte $30,$08,$00,$00
    .byte $30,$08,$00,$00
    .byte $80,$00,$00,$00
    .byte $30,$00,$00,$00
    .byte $00,$00,$00,$00        

;Todo: Compression of worldata

WorldData: ; Each row is 32
    .byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29 ; Overscan blank line
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25,$25

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24
    .byte $24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24,$24

    .byte $45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45,$45
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  
    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  
    

    .byte $47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47,$47  
    .byte $29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29,$29; Overscan blank line
 

ZeldaSprite:
  .byte $00, $00, $00, $08
  .byte $00, $01, $00, $10
  .byte $08, $02, $00, $08
  .byte $08, $03, $00, $10

ZeldaSpriteUp:
  .byte $00, $18, $00, $08
  .byte $00, $19, $00, $10
  .byte $08, $1A, $00, $08
  .byte $08, $1B, $00, $10

ZeldaSpriteDown:
  .byte $00, $10, $00, $08
  .byte $00, $11, $00, $10
  .byte $08, $12, $00, $08
  .byte $08, $13, $00, $10

ZeldaSpriteLeft:                ; This is sprite 1 but flipped and rearanged
  .byte $00, $00, %01000000, $10
  .byte $00, $01, %01000000, $08
  .byte $08, $02, %01000000, $10
  .byte $08, $03, %01000000, $08


Byte1:
        LDA (playeraddress), Y
        CLC
        ADC YOffset
        STA SpriteBuffer, Y
        INY

    Byte2:
        LDA (playeraddress), Y
        CLC
        ADC anioffset
        STA SpriteBuffer, Y
        INY

    Byte3:   
        LDA (playeraddress), Y
        STA SpriteBuffer, Y

    AttributeDone:
        INY

    Byte4:
        LDA (playeraddress), Y
        CLC
        ADC XOffset
        STA SpriteBuffer, Y
        INY

    CPY #$10
    BNE Byte1






.segment "VECTORS"      ; This part just defines what labels to go to whenever the nmi or reset is called 
    .word NMI           ; If you look at someone elses stuff they probably call this vblank or something
    .word Reset
     
.segment "CHARS"
    .incbin "zelda.chr"
