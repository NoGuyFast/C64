;Thanks to 
;*.Stephen.Judd.................*
;*.George.Taylor................*
;*.https://codebase64.org/doku.php?id=magazines:chacking8


;*. it took me 8 Days 
;*. to understand it and make my own Code out of that article 
;*. a week later my code is gone from 23 FPS to 31 FPS another week later I optimized everthing to get 50 FPS
;*. Additions-Rotations-Matrix .*
;*.  Code by CJoke 01/2022     .*

;C64Studio Syntax
;SPINLOAD = 1
!ifdef SPINLOAD {
*=$7a00
;------------------
jsr $1000
rts
} else {
*=$0801
!basic
;------------------
jsr $1000
rts
}

Z30 = multifacz         ;Table for Z-Movement, lots of memory
Z35 = multifacz+256
Z40 = multifacz+512
Z45 = multifacz+768
Z50 = multifacz+1024
Z55 = multifacz+1280
Z60 = multifacz+1536
Z65 = multifacz+1792
Z70 = multifacz+2048
Z75 = multifacz+2304
Z80 = multifacz+2560

sin = sintab
cos = sintab+128
fact_z = Z30 ;cos+128
TMATH =  cos+512 ;fact_z+384     ;Math table of f(x)=x*x/256

screen_1 = $0400      ;CharsetBitmap
Charset_1 = $3000
Charset_2 = $3800

ANGMAX=120      ;Bei uns hat ein Kreis nur 120 Grad

K=26                ;INT(64*2/Z0+0.5) | Z0=5 --- maybe for later use

;*******************************************************************************
;*** occupy the entiere ZeroPage

xcoord = $F3          ;Linien Koordinaten
ycoord = $F8
xcoord1 = $F9
ycoord1 = $FA

xdelta = $FB          ;für die Linienberechnung
ydelta = $FC 
delta_copy = $FD
            
dest = $FE   ;und $0a       ;Setpoint Zielzeiger (dest),y

;num1 = $FF   ;für normale mul 
;num2 = $14 

flip = $37   ;doublebuffer flipper

SX = $38    ;Rotationswinkel des Würfels
SY = $0f 
SZ = $10 

DSX = $11   ;Winkeländerung pro durchlauf
DSY = $12 
DSZ = $13 

AD2 = $15   ;zwischenmerker
AD3 = $16   
REM = $17   

R = $18 ;!byte 0,0,0,0,0,0,0,0    ;X-Coords
S = $26 ;!byte 0,0,0,0,0,0,0,0    ;Y-Coords

UX = $2e  ;A
UY = $2f  ;B
UZ = $30  ;C
VX = $31  ;D
VY = $32  ;E
VZ = $33  ;F
WX = $34  ;G
WY = $35  ;H
WZ = $36  ;I

X = $40 ;!byte 0,0,0,0,0,0,0,0     ;Arrays für die Punktdaten des Würfels
Y = $48 ;!byte 0,0,0,0,0,0,0,0
Z = $50 ;!byte 0,0,0,0,0,0,0,0

Z1 =$59  ;und $5a weil Zeiger SMULTV2 (Z1),y
Z2 =$5b  ;und $5c weil Zeiger (Z2),y

fps = $5d    ;wieviele zentel sek braucht ein durchlauf

T1 = $5e 
T2 = $5f 
T3 = $60 
T4 = $61 
T5 = $62 
T6 = $63 
T7 = $64 
T8 = $65 
T9 = $66 
T10 = $67 

;Sinus Cosinus for T - not in use anymore - precalc all SIN and COS from T is 10 cycles longer, so we get the values on the fly
;TC1 = $68 
;TC2 = $69 
;TC3 = $6a 
;TC4 = $6b 
;TC5 = $6c 
;TC6 = $6d 
;TC7 = $6e 
;TC8 = $6f 
;TC9 = $70 
;TC10 = $71 
;
;TS1 = $72 
;TS2 = $73 
;TS3 = $74 
;TS4 = $75 
;TS5 = $76 
;TS6 = $77 
;TS7 = $78 
;TS8 = $79 
;TS9 = $7a 
;TS10 = $7b 

DATA1_1zp = $7c  ;bis $88
DATA1_2zp = $02  ;bis $0e

tblZpt = $39    ;Zeiger auf die Zeigertabelle
factZ_zp =$3a  ;und 3B weil zeiger
ZchangeWait = $3c

;FREE = $3d-$3f
;FREE = $89-$8f

;*******************************************************************************
!macro mul2 ;signed bitshift
  ;well it is signed? anyway, works fine for this code
  asl
!end
!macro div2 ;signed bitshift
  ;looks more signed, but anyway, works fine for this code
  clc
  bpl +
  sec
+
  ror
!end

!macro SMULTV2
         STA Z1
         CLC
         EOR #$FF
         ADC #$01
         STA Z2
         LDA (Z1),Y
         SEC
         SBC (Z2),Y
!end
;*******************************************************************************

;Jede Bitmap hat eigene Drawlineroutine, das spart jede menge cycles
;**************************____________________ SET A SINGLE PIXEL/POINT CharBITMAP
!macro plotpixel_1
  lda tbl_8LoBig_1,x        ;special version for a Charbitmap
  sta dest                    ;= cell address
  lda tbl_8HiBig_1,x        ;
  sta dest+1
  
  lda (dest),y                ;
  ora tbl_orbit,x             ;isolate and set the point auch hier ne 256Byte Tabelle die immer die gleichen 8 BIT-Kombis enthält, spart 6 cycles
  sta (dest),y                ;
!end                 ;******* 29cycle Plotpixel
;----------------------
!macro plotpixel_2
  lda tbl_8LoBig_2,x        ;
  sta dest                    ;= cell address
  lda tbl_8HiBig_2,x        ;
  sta dest+1
  
  lda (dest),y                ;
  ora tbl_orbit,x             ;isolate and set the point auch hier ne 256Byte Tabelle die immer die gleichen 8 BIT-Kombis enthält, spart 6 cycles
  sta (dest),y     
!end                  ;******* 29cycle Plotpixel
;----------------------
!macro Turboplot              ;bei  Turboplot ist dest bereits gesetzt, daher gehts für beide Bitmaps
  lda (dest),y                ;
  ora tbl_orbit,x             ;isolate and set the point auch hier ne 256Byte Tabelle die immer die gleichen 8 BIT-Kombis enthält, spart 6 cycles
  sta (dest),y                ;
!end                  ;******* 11 Cycle plotpixel if only Ycoord changes while linedrawing

;**************************____________________ DRAWLINE
drawline ;nur bis X 255 - gebraucht wird aber in diem Fall nur Xmax=128, gibt das noch sparpotential?

;DrawlineMainloop_27cyc + Plotpixel_11|29cyc =  38|46Cycles/Pixel = ~0.6 rasterlines/pixel while drawline

; $e6 Opcode inc $ZP
; $c6 Opcode dec $ZP
opinx = $e8 ;Opcode inx 
opdex = $ca ;Opcode dex
opiny = $c8 ;Opcode iny
opdey = $88 ;Opcode dey

    lda #opinx ;$e6
    sta rh1 ;x
    sta rv2 ;x    ;erstmal inc-befehle einsetzen | default
    lda #opiny
    sta rh2 ;y
    sta rv1 ;y

    lda xcoord1
    sec
    sbc xcoord
    bcs plus
    eor #$FF    ;Ist ins Minus gelaufen, Zahl umdrehen / kein bit 7 löschen weil keine richtige vorzeichenzahl sondern 255-rest drinnsteht
    clc
    adc #1
    ldx #opdex ;$c6
    stx rh1
    stx rv2   ;x muss minus / dec Befehl einsetzen
  plus
    sta xdelta

    lda ycoord1
    sec
    sbc ycoord
    bcs plus1
    eor #$FF    ;Ist ins Minus gelaufen, Zahl umdrehen / kein bit 7 löschen weil keine richtige vorzeichenzahl sondern 255-rest drinnsteht
    clc
    adc #1
    ldx #opdey ;$c6
    stx rh2
    stx rv1     ;y muss minus / dec Befehl einsetzen
  plus1         
    sta ydelta

    cmp xdelta
    bcs drvertStart

;****************** Line is more Horz
;if xdelta > ydelta
    sta delta_copy  ;ydelta wird aufaddiert bei jedem durchgang daher ab in delta_copy
    ;asl ydelta
    ldx xcoord
    ldy ycoord
    +plotpixel_1
  drhorz            ;DrawLine MAINLOOP
rh1 inx 

    lda ydelta          ;ydelta ausaddieren und wenn es > xdelta ist muss ycoord einen hoch/runter
    ;clc
    adc delta_copy
    cmp xdelta
    bcs yisgroesser      ;in den meisten fällen ist y kleiner daher sprung bei größer
    
    sta ydelta
    +plotpixel_1
    cpx xcoord1       ;Sind ma schon da?
    bne drhorz        ;DrawLine Mainloop 30 Cycles or 42 Cycles if Y needs up/down one
    rts ;jmp drawlineend 

yisgroesser
rh2 iny           ;bei überlauf y einen hoch/runter
    ;sec
    sbc xdelta
    sta ydelta            ;ydelta zurücksetzen mit rest

    +plotpixel_1
    cpx xcoord1       ;Sind ma schon da?
    bne drhorz        ;DrawLine Mainloop 30 Cycles or 42 Cycles if Y needs up/down one
    rts ;jmp drawlineend 

;****************** Line is more Vert
;if xdelta < ydelta
  drvertStart
    lda xdelta
    sta delta_copy  ;xdelta wird aufaddiert bei jedem durchgang daher ab in delta_copy
    ;asl xdelta
    ldx xcoord
    ldy ycoord
    +plotpixel_1
  drvert            ;DrawLine MAINLOOP
rv1 iny 

    lda xdelta          ;xdelta aufaddieren und wenn es > ydelta ist muss xcoord einen hoch/runter
    ;clc
    adc delta_copy
    cmp ydelta
    bcs xisgroesser   ;in den meisten fällen ist x kleiner daher sprung bei größer
    
    sta xdelta
    +Turboplot        ;An dieser Stelle kann man Turboplot benutzen weil X gleichbleibt 11 Cycles anstatt 29
    cpy ycoord1       ;Sind ma schon da?
    bne drvert        ;DrawLine Mainloop 30 Cycles or 42 Cycles if X needs up/down one
    rts ;jmp drawlineend 

xisgroesser
rv2 inx           ;bei überlauf x einen hoch/runter
    ;sec
    sbc ydelta
    sta xdelta            ;xdelta zurücksetzen mit rest
    
    +plotpixel_1
    cpy ycoord1       ;Sind ma schon da?
    bne drvert        ;DrawLine Mainloop 30 Cycles or 42 Cycles if X needs up/down one
    ;jmp drawlineend 
    
;drawlineend  
rts  

;**************************____________________ DRAWLINE
drawline_1 ;nur bis X 255

;DrawlineMainloop_27cyc + Plotpixel_11/29cyc =  38/46Cycles/Pixel | ~0.6 rasterlines/pixel
    lda #opinx ;$e6
    sta rh1_1 ;x
    sta rv2_1 ;x    ;erstmal inc-befehle einsetzen | default
    lda #opiny
    sta rh2_1 ;y
    sta rv1_1 ;y


    lda xcoord1
    sec
    sbc xcoord
    bcs plus_1
    eor #$FF    ;Ist ins Minus gelaufen, Zahl umdrehen / kein bit 7 löschen weil keine richtige vorzeichenzahl sondern 255-rest drinnsteht
    clc
    adc #1
    ldx #opdex ;$c6
    stx rh1_1
    stx rv2_1   ;x muss minus / dec Befehl einsetzen
  plus_1
    sta xdelta

    lda ycoord1
    sec
    sbc ycoord
    bcs plus1_1
    eor #$FF    ;Ist ins Minus gelaufen, Zahl umdrehen / kein bit 7 löschen weil keine richtige vorzeichenzahl sondern 255-rest drinnsteht
    clc
    adc #1
    ldx #opdey ;$c6
    stx rh2_1
    stx rv1_1     ;y muss minus / dec Befehl einsetzen
  plus1_1         
    sta ydelta

    cmp xdelta
    bcs drvertStart_1

;****************** Line is more Horz
;if xdelta > ydelta
    sta delta_copy  ;ydelta wird aufaddiert bei jedem durchgang daher ab in delta_copy
    ;asl ydelta
    ldx xcoord
    ldy ycoord
    +plotpixel_2
  drhorz_1            ;DrawLine MAINLOOP
rh1_1 inx 

    lda ydelta          ;ydelta ausaddieren und wenn es > xdelta ist muss ycoord einen hoch/runter
    ;clc
    adc delta_copy
    cmp xdelta
    bcs yisgroesser_1      ;in den meisten fällen ist y kleiner daher sprung bei größer
    
    sta ydelta
    +plotpixel_2
    cpx xcoord1       ;Sind ma schon da?
    bne drhorz_1        ;DrawLine Mainloop 30 Cycles or 42 Cycles if Y needs up/down one
    rts ;jmp drawlineend_1 

yisgroesser_1
rh2_1 iny          ;bei überlauf y einen hoch/runter
    ;sec
    sbc xdelta
    sta ydelta            ;ydelta zurücksetzen mit rest

    +plotpixel_2
    cpx xcoord1       ;Sind ma schon da?
    bne drhorz_1        ;DrawLine Mainloop 30 Cycles or 42 Cycles if Y needs up/down one
    rts ;jmp drawlineend_1 

;****************** Line is more Vert
;if xdelta < ydelta
  drvertStart_1
    lda xdelta
    sta delta_copy  ;xdelta wird aufaddiert bei jedem durchgang daher ab in delta_copy
    ;asl xdelta
    ldx xcoord
    ldy ycoord
    +plotpixel_2
  drvert_1            ;DrawLine MAINLOOP
rv1_1 iny 

    lda xdelta          ;xdelta aufaddieren und wenn es > ydelta ist muss xcoord einen hoch/runter
    ;clc
    adc delta_copy
    cmp ydelta
    bcs xisgroesser_1   ;in den meisten fällen ist x kleiner daher sprung bei größer
    
    sta xdelta
    +Turboplot        ;An dieser Stelle kann man Turboplot benutzen weil X gleichbleibt 11 Cycles anstatt 29
    cpy ycoord1       ;Sind ma schon da?
    bne drvert_1        ;DrawLine Mainloop 30 Cycles or 42 Cycles if X needs up/down one
    rts ;jmp drawlineend_1 

xisgroesser_1
rv2_1 inx           ;bei überlauf x einen hoch/runter
    ;sec
    sbc ydelta
    sta xdelta            ;xdelta zurücksetzen mit rest
    
    +plotpixel_2
    cpy ycoord1       ;Sind ma schon da?
    bne drvert_1        ;DrawLine Mainloop 30 Cycles or 42 Cycles if X needs up/down one
    ;jmp drawlineend_1 
    
;drawlineend_1  
rts  

;mul               ;signed 8Bit Mul 
; stx num1
; sty num2
; lda #$00
; beq enterLoop
;
;doAdd
; clc
; adc num1
;
;loop
; asl num1
;enterLoop
; lsr num2
; bcs doAdd
; bne loop
; 
;rts             ;34 cycles + jsr/rts

;*********************************************************************************************
;******************************* HAUPTPROGRAMM ***********************************************
;*********************************************************************************************
*=$1000

;Setup Bitmap on Screen
jsr showscrn

-
    lda #$7f   ;Auf SPACE-Taste         
    sta $dc00  ; warten...              
    lda $dc01                           
    cmp #$ef                            
    bne - 

jsr clrscrn1
jsr clrscrn
ldx #0
!for x = 0 to 15
  !for y = 0 to 15
    txa
    sta screen_1 + (40-16)/2 + 1*40 + x + y*40  ;Mittig ab Zeile 1 soll das Feld stehen
    inx
  !end
!end

    sei   ;Interrupts kurz off

    lda #$7f   ;Alle CIA-IRQs abschalten
    sta $dc0d  ; (CIA1)                 
    sta $dd0d  ; (CIA2)                 
    bit $dc0d  ;CIA1-ICR löschen        
    bit $dd0d  ;CIA2-ICR löschen        
    ;lda #$00   ; VIC-Hintergrundstriche 
    ;sta $3fff  ; Emulator Borderglitch 
    
    lda #$35   ;ROMs abschalte          
    sta $01  

    lda #%00001000  ;multicolor aus
    sta $d016
    lda $d018 
    and #$f0
    ora #%00001110  ;Zeichensatz ab $3800
    sta $d018   
    lda $d018
    ora #%00001000    ;Bitmap bei $2000
    sta $d018
    lda #$00
    sta $d015

    lda #<RastUnten  ; IRQ-Vektor bei 
    sta $fffe      ;$ FFFE/$ FFFF 
    lda #>RastUnten  ; auf " BORDIRQ" verbiegen 
    sta $ffff 
    lda #%00011011      ; Wert für $ D011 mit gel.
    sta $d011      ; High-Bit f. Rasterpos.
    lda #180       ; Ersten Raster-IRQ bei
    sta $d012     ; Zeile 50 auslösen    
    lda #$81      ;VIC-Raster-IRQs        
    sta $d01a     ; erlauben              
    dec $d019     ;VIC-ICR ggf. löschen 

    cli
    
    lda #0    ;Rotation 0
    sta SX
    sta SY
    sta SZ
    
    sta tblZpt

    lda #3
    sta ZchangeWait
    
    lda #1    ;Bissl Drehung pro Durchlauf
    sta DSX
    lda #1
    sta DSY
    lda #2
    sta DSZ

    lda tblZlo
    sta factZ_zp
    lda tblZhi
    sta factZ_zp+1
    
    jsr MatrixInit

;*********************************************************************************************
          ;!for  ROW = 0 to 4
          ;  lda zahlenZehn+ROW
          ;  sta $2000+ROW
          ;!end
          !for  ROW = 0 to 4
            lda zahlen_zehn_1+ROW
            sta $a000+ROW
          !end
          lda #0     ;FPS Messung
          sta $dc09
          sta $dc08

;*********************************************************************************************
  infini      ;**************  The BIG MAIN LOOP ***********
  
  ;inc $d020  ;optischer Speedtest
  
    ;lda $d012  ;vsynch it, sinkt dann auf unter 20 FPS
    ;cmp #$FF
    ;bne infini  ;Warte erstmal auf zeile 255
    
  LDA $d018 
  EOR #%00000010  ;Flip the bit | double buffering CharsetBitmap
  STA $d018
  lda flip
  eor #%00000001
  sta flip
  beq dreitausend800
  
  jsr clrScreen_2
  jmp dreitausend

dreitausend800 ;wird grad gezeigt
    jsr clrScreen_1    
dreitausend
    jsr LABEL21   ;where the magic happens
    
          lda $dc09      ;Zeitmessung
          ;and #%00001111
          cmp #1
          beq +
          inc fps        ; bis zu 50 FPS atm
          jmp infini    ;Loop the Loop
          
        +
          lda fps   ;Breakpoint

;----- Print the Numbers --------- vordere Zahl
          cmp #50
          bcc +
          !for  ROW = 0 to 4
            ldx zahlen_zehn_5+ROW
            stx $a000+ROW
          !end
          sec
          sbc #50
          jmp hiza
        +
          cmp #40
          bcc +
          !for  ROW = 0 to 4
            ldx zahlen_zehn_4+ROW
            stx $a000+ROW
          !end
          sec
          sbc #40
          jmp hiza
        +
          cmp #30
          bcc +
          !for  ROW = 0 to 4
            ldx zahlen_zehn_3+ROW
            stx $a000+ROW
          !end
          sec
          sbc #30
          jmp hiza
        +
          cmp #20
          bcc +
          !for  ROW = 0 to 4
            ldx zahlen_zehn_2+ROW
            stx $a000+ROW
          !end
          sec
          sbc #20
          jmp hiza
        +
;-------------- hintere Zahl
hiza
          cmp #0
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_0+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #1
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_1+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #2
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_2+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #3
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_3+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #4
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_4+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #5
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_5+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #6
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_6+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #7
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_7+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #8
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_8+ROW
            stx $a008+ROW
          !end
          jmp ++    
        +
          cmp #9
          bne +
          !for  ROW = 0 to 4
            ldx zahlen_9+ROW
            stx $a008+ROW
          !end
          ;jmp ++    
        +
++
          dec ZchangeWait       
          ldy ZchangeWait
          bne ++
          ldy #2            ;alle 2 sec WürfelZ ändern
          sty ZchangeWait
          
          ldx tblZpt
          lda tblZlo,x
          sta factZ_zp
          lda tblZhi,x
          sta factZ_zp+1          
          inx
          cpx #22           ;länge der Zeigertabelle für FactZ-Tabelle
          bne +
          ldx #0
        +
          stx tblZpt
        ++
          lda #0
          sta fps
          sta $dc09
          sta $dc08
!ifdef SPINLOAD {
          lda #$7f   ;Auf SPACE-Taste         
          sta $dc00  ; warten...              
          lda $dc01                           
          cmp #$ef                            
          beq VecEnde
          jmp infini ;end           
} else  {
          jmp infini ;end           
}
VecEnde
          lda #$3c
          sta $dd02
          rts
;*********************************************************************************************
;*********************************************************************************************

RastOben
    pha        ;Prozessorregs. retten    
    txa                                  
    pha                                  
    tya                                  
    pha 
    lda $d011
    and #%11011111    ;Bitmap OFF
    sta $d011
    
;    inc $d020    
    
    lda #<RastUnten  ; IRQ-Vektor bei 
    sta $fffe      ;$ FFFE/$ FFFF 
    lda #>RastUnten  ; auf " BORDIRQ" verbiegen 
    sta $ffff 
    lda #202       ; Ersten Raster-IRQ bei
    sta $d012     ; Zeile 50 auslösen    
           
!ifdef SPINLOAD {
    lda $dd02
    eor #%00000010    ;Bank umschalten zwischen $8000 und $0000    
    sta $dd02    
} else {
    lda $dd00
    eor #%00000010    ;Bank umschalten zwischen $8000 und $0000    
    sta $dd00
}    
    dec $d019     ;VIC-ICR ggf. löschen  

    pla                ;Prozessorregs. zurückholen
    tay                ; und IRQ beenden0         
    pla                                  
    tax                                  
    pla                                  
    rti  

RastUnten
    pha        ;Prozessorregs. retten    
    txa                                  
    pha                                  
    tya                                  
    pha 
;    dec $d020
    lda #<RastOben  ; IRQ-Vektor bei 
    sta $fffe      ;$ FFFE/$ FFFF 
    lda #>RastOben  ; 
    sta $ffff 

    lda #57       ; Ersten Raster-IRQ bei
    sta $d012     ; Zeile 50 auslösen    

!ifdef SPINLOAD {
    lda $dd02
    eor #%00000010    ;Bank umschalten zwischen $8000 und $0000    
    sta $dd02    
} else {
    lda $dd00
    eor #%00000010    ;Bank umschalten zwischen $8000 und $0000   
    sta $dd00
}
    lda $d011
    ora #%00100000    ;Bitmap ON
    sta $d011
    ;lda $d018
    ;ora #%00001000    ;Bitmap bei $a000
    ;sta $d018

    dec $d019     ;VIC-ICR ggf. löschen   
    pla                ;Prozessorregs. zurückholen
    tay                ; und IRQ beenden0         
    pla                                  
    tax                                  
    pla                                  
    rti      

;********************************************************
MatrixInit    ; Würfel Punktdaten in XYZ Arrays schreiben
!for ROW = 0 to 7
  lda DATA+(ROW*3)
  sta X+ROW
  lda DATA+1+(ROW*3)
  sta Y+ROW
  lda DATA+2+(ROW*3)
  sta Z+ROW
!end
!For ROW = 0 to 11  ;Würfen Zeiger auf Linien Anfangs- und Endpunkte in die ZP kopieren
    lda DATA1_1+ROW
    sta DATA1_1zp+ROW
    lda DATA1_2+ROW
    sta DATA1_2zp+ROW
!end

  LDA #>TMATH ;Pointer auf TMath in der ZP setzen
  STA Z1+1
  STA Z2+1

rts

;********************************************************
;adds two angles
!macro adda .a
         clc
         adc .a
         cmp #ANGMAX
         bcc +
         sbc #ANGMAX
+
!end
;subtracts two angles
!macro suba .a
         sec
         sbc .a
         bcs +
         adc #ANGMAX
+
!end
 
;negates value - war in SMULT aber wird nicht gebraucht
;!macro NEG .MAC  ;Change.the.sign.of.a.two's.complement
; CLC
; LDA .MAC ;number.
; EOR #$FF
; ADC #$01
; 
;; sta .MAC ; ??? wegen fehler in smult?
;!end

;********************************************************************************
;********************************************************************************
;********************************************************************************

LABEL21     ;ROTATE IT WITH MATRIX

;********************************************************************************
;********************************************************************************
;********************************************************************************
; UPDATE Angles für kontinuierliche drehung
 CLC
 LDA SX
 ADC DSX
 CMP #ANGMAX ;Are.we.>=.maximum.angle?
 BCC upCONT1
 SBC #ANGMAX ;If so, reset
upCONT1 STA SX
 CLC
 LDA SY
 ADC DSY
 CMP #ANGMAX
 BCC upCONT2
 SBC #ANGMAX ;Same.deal
upCONT2 STA SY
 CLC
 LDA SZ
 ADC DSZ
 CMP #ANGMAX
 BCC upCONT3
 SBC #ANGMAX
upCONT3 STA SZ

;********************************************************************************
; https://www.forum64.de/index.php?thread/64503-rotierender-3d-w%C3%BCrfel-neujahrsgeschenk-f%C3%BCr-oobdoo-und-alle-interessierten/
;
; https://codebase64.org/doku.php?id=magazines:chacking8
;
;Daten für AdditionsMatrix vorbereiten T1 bis T10
    ldx SX  ;txa und tya benutzen anstatt lda - jeweils 1 cycle weniger
    ldy SY

;T1 = SY-SZ
    tya ;lda SY
    +suba SZ
    sta T1
    
;T2 = SY+SZ
    tya ;lda SY
    +adda SZ
    sta T2

;T3 = SX+SZ
    txa ;lda SX
    +adda SZ
    sta T3

;T4 = SX-SZ
    txa ;lda SX
    +suba SZ
    sta T4

;T5 = SX+SY+SZ = SX+T2
    txa ;lda SX
    +adda T2
    sta T5

;T6 = SX-SY+SZ = SX-T1
    txa ;lda SX
    +suba T1
    sta T6
    
;T7 = SX+SY-SZ = SX+T1
    txa ;lda SX
    +adda T1
    sta T7

;T8 = SY+SZ-SX = T2-SX
    lda T2
    +suba SX
    sta T8
    
;T9 = SY-SX
    tya ;lda SY
    +suba SX
    sta T9

;T10 = SY+SX
    tya ;lda SY
    +adda SX
    sta T10

;*******************************************************************************
;SinCos Macros 
!macro lda_C .t
    ldx .t
    lda cos,x
!end
!macro lda_S .t
    ldx .t
    lda sin,x
!end
!macro adc_C .t
    ldx .t
;    clc
    adc cos,x
!end
!macro sbc_C .t
    ldx .t
;    sec
    sbc cos,x
!end
!macro adc_S .t
    ldx .t
;    clc
    adc sin,x
!end
!macro sbc_S .t
    ldx .t
;    sec
    sbc sin,x
!end

;All things sortet - good to go with easy Code 
;*******************************************************************************
; unsere SinTab ist schon /2
;UX = (COS(T1)+COS(T2))/2
    +lda_C T1
    +adc_C T2
    sta UX

;UY = (SIN(T1)-SIN(T2))/2
    +lda_S T1
    +sbc_S T2
    sta UY

;UZ =  SIN(SY)      | wenn man *2 weglässt fängt der Würfel an zu morphen und sieht sogar gut aus
    ldx SY
    lda sin,x
    +mul2
    sta UZ
    
;VX = (SIN(T3)-SIN(T4))/2 + (COS(T6)-COS(T5)+COS(T8)-COS(T7))/4
    +lda_C T6
    +sbc_C T5
    +adc_C T8
    +sbc_C T7
    
    +div2

    +adc_S T3
    +sbc_S T4

    sta VX

;VY = (COS(T3)+COS(T4))/2 + (SIN(T5)-SIN(T6)-SIN(T7)-SIN(T8))/4
    +lda_S T5
    +sbc_S T6
    +sbc_S T7
    +sbc_S T8
    
    +div2

    +adc_C T3
    +adc_C T4

    sta VY

;VZ = (SIN(T9)-SIN(T10))/2
    +lda_S T9
    +sbc_S T10
    sta VZ

;WX = (COS(T4)-COS(T3))/2 + (SIN(T6)-SIN(T5)-SIN(T8)-SIN(T7))/4
    +lda_S T6
    +sbc_S T5
    +sbc_S T8
    +sbc_S T7
    
    +div2

    +adc_C T4
    +sbc_C T3

    sta WX

;WY = (SIN(T3)+SIN(T4))/2 + (COS(T6)-COS(T5)+COS(T7)-COS(T8))/4
    +lda_C T6
    +adc_C T7
    +sbc_C T5
    +sbc_C T8
    
    +div2

    +adc_S T3
    +adc_S T4

    sta WY

;WZ = (COS(T9)+COS(T10))/2
    +lda_C T9
    +adc_C T10
    sta WZ


!macro transferVorzeichen
  bpl + ;is postiv muss nix gemacht werden
  clc
  eor #$ff
  adc #1
+
!end
;**************************************** Einlesen der 3D daten und mit Rotationmatrix multiplizieren
;FORI=0TO7
;W(I)=WX*X(I)+WY*Y(I)+WZ*Z(I)
!For ROW = 0 TO 7

    lda WY
    ldx Y+ROW     ;Würfel koords sind immer 1 oder -1 also gehts ohne mul
    +transferVorzeichen ;vorzeichen transfer reicht in diesem Würfel-Spezialfall
    ;jsr mul      ;durch ein richtiges mul können die 3D-Daten geändert werden... feature
    sta AD2
    
    lda WZ
    ldx Z+ROW
    +transferVorzeichen
    ;jsr mul
    sta AD3
    
    lda WX
    ldx X+ROW
    +transferVorzeichen
    ;jsr mul

    clc
    adc AD2
;    clc
    adc AD3
    
;    clc
    adc #128
    tay ;tax
    lda (factZ_zp),y              ;durch ne Änderung der Tabelle kann der Würfel in richtung Z bewegt werden
    tay ;sta REM              ;der Z-Part - 64*D/(64*Z0-Z) precalc of the classic x' = d*x/(z-z0) without x

;U(I)=UX*X(I)+UY*Y(I)+UZ*Z(I)

    lda UY
    ldx Y+ROW
    +transferVorzeichen
    ;jsr mul             
    sta AD2
    
    lda UZ
    ldx Z+ROW
    +transferVorzeichen
    ;jsr mul
    sta AD3

    lda UX
    ldx X+ROW
    +transferVorzeichen
    ;jsr mul
    
    clc
    adc AD2
;    clc
    adc AD3

    ;ldy REM                  ;Ist immer noch in Y
    +SMULTV2
    
    clc
    adc #64    ;auf Bildschirm Zentrieren
    sta R+ROW   ;X-coord ready
    
;V(I)=VX*X(I)+VY*Y(I)+VZ*Z(I)

    lda VY
    ldx Y+ROW
    +transferVorzeichen
    ;jsr mul
    sta AD2
    
    lda VZ
    ldx Z+ROW
    +transferVorzeichen
    ;jsr mul
    sta AD3

    lda VX
    ldx X+ROW
    +transferVorzeichen
    ;jsr mul

    clc
    adc AD2
;    clc
    adc AD3
    
    ;ldy REM                  ;Ist immer noch in Y
    +SMULTV2

    clc
    adc #64   ;bei 64 bleibt manchmal ein Pixel unten stehen, also einen Pixel weiter oben darstellen
    sta S+ROW   ;Y-coord ready
    
!end
;NEXT
;************************************************* Linien malen
;FORI=0TO11
;DRAW 1,R(L(I,0)),S(L(I,0)) TO R(L(I,1)),S(L(I,1))
!For ROW = 0 TO 11
    ldx DATA1_1zp + ROW     ;Liniendata Zeiger auf Linenanfang
    lda R,x             ;X-Coord vom Linenanfang holen
    ;+div2
    sta xcoord
    lda S,x             ;Y-Coord vom Linenanfang holen
    ;+div2
    sta ycoord

    ldx DATA1_2zp + ROW     ;Liniendata Zeiger auf Linenende
    lda R,x             ;X-Coord vom Linenende holen
    ;+div2
    sta xcoord1
    lda S,x             ;Y-Coord vom Linenende holen
    ;+div2
    sta ycoord1     
    
    lda flip            ;DoubleBuffering
    cmp #1
    bne +
    
    jsr drawline_1        ;Coords ready -> Go
    jmp ++   

+
    jsr drawline      ;Coords ready -> Go
++
!end
;NEXT
rts

*=$2300
multifacz
!bin "facz30to80step5.prg"
;************************************************** Bitmap


*=$a000 
;!fill $2000,0
!bin "vectorc      pic.prg";"vector.bin"

;**************************____________________ sideourtines
*=$4000
clrScreen_1 ;no time for schnickschnack - floodfill the memory
  lda #00
  !for Zeichen = 1 to 254
      !for BYTS = 0 to 7
          sta Charset_1+ BYTS + Zeichen*8
      !end
  !end
  rts
  
clrScreen_2
  lda #00
  !for Zeichen = 1 to 254
      !for BYTS = 0 to 7
          sta Charset_2+ BYTS + Zeichen*8
      !end
  !end
rts

;******* Setpoint Tabellen für schnelles ermitteln der Zieposition für SetPoint
*=$7000
tbl_orbit ;special Table to save 6 cycles in Setpoint
!fill 32,[%10000000, %01000000, %00100000, %00010000, %00001000, %00000100, %00000010, %00000001]

tbl_8LoBig_1
!fill 16,[<(Charset_1+(i * 128)),<(Charset_1+(i * 128)),<(Charset_1+(i * 128)),<(Charset_1+(i * 128)),<(Charset_1+(i * 128)),<(Charset_1+(i * 128)),<(Charset_1+(i * 128)),<(Charset_1+(i * 128))]
tbl_8HiBig_1
!fill 16,[>(Charset_1+(i * 128)),>(Charset_1+(i * 128)),>(Charset_1+(i * 128)),>(Charset_1+(i * 128)),>(Charset_1+(i * 128)),>(Charset_1+(i * 128)),>(Charset_1+(i * 128)),>(Charset_1+(i * 128))]

tbl_8LoBig_2
!fill 16,[<(Charset_2+(i * 128)),<(Charset_2+(i * 128)),<(Charset_2+(i * 128)),<(Charset_2+(i * 128)),<(Charset_2+(i * 128)),<(Charset_2+(i * 128)),<(Charset_2+(i * 128)),<(Charset_2+(i * 128))]
tbl_8HiBig_2
!fill 16,[>(Charset_2+(i * 128)),>(Charset_2+(i * 128)),>(Charset_2+(i * 128)),>(Charset_2+(i * 128)),>(Charset_2+(i * 128)),>(Charset_2+(i * 128)),>(Charset_2+(i * 128)),>(Charset_2+(i * 128))]

zahlen_zehn_1
!byte %00000010
!byte %00000110
!byte %00000010
!byte %00000010
!byte %00000010
zahlen_zehn_2
!byte %00000110
!byte %00000001
!byte %00000010
!byte %00000100
!byte %00000111
zahlen_zehn_3
!byte %00000111
!byte %00000001
!byte %00000011
!byte %00000001
!byte %00000111
zahlen_zehn_4
!byte %00000100
!byte %00000101
!byte %00000111
!byte %00000001
!byte %00000001
zahlen_zehn_5
!byte %00000111
!byte %00000100
!byte %00000110
!byte %00000001
!byte %00000110
zahlen_0
!byte %00100000
!byte %01010000
!byte %01010000
!byte %01010000
!byte %00100000
zahlen_1
!byte %00100000
!byte %01100000
!byte %00100000
!byte %00100000
!byte %00100000
zahlen_2
!byte %01100000
!byte %00010000
!byte %00100000
!byte %01000000
!byte %01110000
zahlen_3
!byte %01110000
!byte %00010000
!byte %00110000
!byte %00010000
!byte %01110000
zahlen_4
!byte %01000000
!byte %01010000
!byte %01110000
!byte %00010000
!byte %00010000
zahlen_5
!byte %01110000
!byte %01000000
!byte %01100000
!byte %00010000
!byte %01100000
zahlen_6
!byte %00110000
!byte %01000000
!byte %01100000
!byte %01010000
!byte %01100000
zahlen_7
!byte %01110000
!byte %00010000
!byte %00100000
!byte %00100000
!byte %00100000
zahlen_8
!byte %00100000
!byte %01010000
!byte %00100000
!byte %01010000
!byte %00100000
zahlen_9
!byte %00100000
!byte %01010000
!byte %00110000
!byte %00010000
!byte %00010000

clrscrn   ;those stuff you know
  lda #$b
  sta $d021
  sta $d020
  
  ldx #0
  lda #$00      ;Zeichen Null weil es vom Vektorwürfel nicht überschrieben wird
wrstscra
  sta $0400,x
  sta $d800,x
  inx
  bne wrstscra
wrstscr1a
  sta $0500,x
  sta $d900,x
  inx
  bne wrstscr1a
wrstscr2a
  sta $0600,x
  sta $da00,x
  inx
  bne wrstscr2a
wrstscr3a
  sta $0700,x
  sta $db00,x
  inx
  cpx #$e8
  bne wrstscr3a
rts
clrscrn1      
  ldx #0
  lda #$0b    ;Farbe für HiresBitmap | hibyte pixelfarbe - lobyte hintergrund
wrstscra1
  sta $8400,x
  inx
  bne wrstscra1
wrstscr1a1
  sta $8500,x
  inx
  bne wrstscr1a1
wrstscr2a1
  sta $8600,x
  inx
  bne wrstscr2a1
wrstscr3a1
  sta $8700,x
  inx
  cpx #$e8
  bne wrstscr3a1
rts


*=$c080
sintab
!bin "cobatab120-70_math.prg"
           ;D-Wert (~SizeMultiplikator) gepimpte Table aus der Programmdemo für https://codebase64.org/doku.php?id=magazines:chacking8 UUdecode -> init3d.bas
           ;keine große Magie: sintab, costab kreis auf 120grad aufgeteilt *32 = 2x120byte plus 256Byte 64*D/(64*Z0-Z)
           ;plus 512bytes Mult-Factortabelle allerdings mit /64 um den 64er-MathOffset-Multiplikator (float-simulation) gleichzeitig zu dividieren

DATA ;Würfel Punkte - simple simple
!byte  1, 1, 1
!byte  1,-1, 1
!byte -1,-1, 1
!byte -1, 1, 1

!byte  1, 1,-1
!byte  1,-1,-1
!byte -1,-1,-1
!byte -1, 1,-1

DATA1_1 ;Würfel Punkteverbindungen
!byte 0,1,2,3, 4,5,6,7, 0,1,2,3         ;Linenanfang Zeilenposition im Dataarray
DATA1_2
!byte 1,2,3,0, 5,6,7,4, 4,5,6,7         ;Zeilenposition im Dataarray Linenende

tblZlo
!byte <Z30,<Z35,<Z40,<Z45,<Z50,<Z55,<Z60,<Z65,<Z70,<Z75,<Z80     ;Verschiedene Z-Tabellen
!byte <Z75,<Z70,<Z65,<Z60,<Z55,<Z50,<Z45,<Z40,<Z35,<Z30,<Z30
tblZhi
!byte >Z30,>Z35,>Z40,>Z45,>Z50,>Z55,>Z60,>Z65,>Z70,>Z75,>Z80     ;Verschiedene Z-Tabellen
!byte >Z75,>Z70,>Z65,>Z60,>Z55,>Z50,>Z45,>Z40,>Z35,>Z30,>Z30


showscrn   ;those stuff you know
  lda #$0
  sta $d021
  sta $d020
  
  ldx #0
wrstscra3
  lda startscr,x
  sta $0400,x
  lda startscrCol,x
  sta $d800,x
  inx
  bne wrstscra3
wrstscr1a3
  lda startscr+$0100,x
  sta $0500,x
  lda startscrCol+$0100,x
  sta $d900,x
  inx
  bne wrstscr1a3
wrstscr2a3
  lda startscr+$0200,x
  sta $0600,x
  lda startscrCol+$0200,x
  sta $da00,x
  inx
  bne wrstscr2a3
wrstscr3a3
  lda startscr+$0300,x
  sta $0700,x
  lda startscrCol+$0300,x
  sta $db00,x
  inx
  cpx #$e8
  bne wrstscr3a3
rts

startscr
;size 40,25
;screen char data
!byte $e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$13,$09,$0d,$10,$0c,$05,$20,$01,$12,$09,$14,$08,$0d,$05,$14,$09,$03,$20,$12,$0f,$14,$01,$14,$09,$0f,$0e,$20,$0d,$01,$14,$12,$09,$18,$20,$1a,$30,$2d,$1a,$20
!byte $20,$20,$20,$20,$20,$20,$04,$05,$13,$03,$12,$09,$02,$05,$04,$20,$09,$0e,$20,$03,$08,$01,$03,$0b,$09,$0e,$07,$20,$38,$2f,$39,$34,$20,$13,$2e,$0a,$15,$04,$04,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$0d,$01,$18,$06,$10,$13,$3a,$35,$30,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$10,$0c,$0f,$14,$10,$09,$18,$05,$0c,$3a,$32,$39,$20,$03,$19,$03,$0c,$05,$13,$20
!byte $20,$20,$16,$13,$19,$0e,$03,$3a,$0f,$06,$06,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$04,$12,$01,$17,$0c,$09,$0e,$05,$3a,$32,$37,$20,$03,$19,$03,$0c,$05,$13,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$0d,$01,$09,$0e,$0c,$0f,$0f,$10,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$24,$30,$38,$30,$31,$20,$14,$0f,$20,$24,$03,$06,$06,$06,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$06,$09,$0c,$0c,$05,$04,$20,$15,$10,$20,$17,$09,$14,$08,$20,$20,$20,$20,$20,$03,$0c,$05,$01,$12,$20,$02,$09,$14,$0d,$01,$10,$3a,$20,$20,$20,$20,$20,$20,$20
!byte $20,$03,$0f,$04,$05,$20,$01,$0e,$04,$20,$14,$01,$02,$0c,$05,$13,$20,$20,$20,$20,$20,$36,$0b,$02,$20,$13,$14,$01,$20,$03,$0f,$0d,$0d,$01,$0e,$04,$13,$20,$20,$20
!byte $20,$06,$0f,$12,$20,$06,$01,$13,$14,$0d,$01,$14,$08,$20,$20,$20,$20,$20,$20,$20,$20,$0e,$0f,$20,$0c,$0f,$0f,$10,$13,$20,$06,$0c,$0f,$0f,$04,$06,$09,$0c,$0c,$20
!byte $20,$01,$0e,$04,$20,$13,$10,$05,$05,$04,$03,$0f,$04,$05,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$55,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$49,$55,$43,$43,$43,$43,$49,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$42,$03,$15,$02,$09,$13,$0d,$20,$02,$19,$20,$03,$0a,$0f,$0b,$05,$6b,$73,$32,$30,$32,$32,$42,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$4a,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$43,$4b,$4a,$43,$43,$43,$43,$4b,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$01,$20,$12,$05,$01,$0c,$14,$09,$0d,$05,$20,$16,$05,$03,$14,$0f,$12,$03,$15,$02,$05,$20,$12,$0f,$14,$01,$14,$09,$0f,$0e,$20,$20,$20,$20,$20
!byte $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$15,$06,$0f
!byte $e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6,$e6

startscrCol
;screen color data
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0c,$0c,$0c,$0c,$01
!byte $01,$01,$01,$01,$01,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$0c,$0c,$0c,$0a,$0a,$0a,$0b,$0a,$0a,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$01
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0b,$01,$01,$01,$01,$01,$0b,$0b,$0b,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$0b,$0b,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$0b,$0b,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$04,$04,$04,$01,$01,$01,$01,$01,$01,$01
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$0b,$0b,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0c,$0c,$0c,$0c,$0c,$0c
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$0b,$0b,$0b,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$01
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01
!byte $01,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$0b,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$04,$01,$01,$04,$04,$04,$04,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01
!byte $01,$01,$01,$01,$01,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0a,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$0c,$01,$01,$01
!byte $01,$01,$01,$01,$01,$01,$01,$0c,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$0b,$0b,$0b
!byte $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01

; *** $0801 bis $CFFF komplett voll, nur um einen Würfel darzustellen und zu drehen
