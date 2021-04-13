; HIMEM-Erweiterung auf UMBs fr DOS 5 .. 7 ;
; Original (c) Peter Siering, erschienen in c't 8/91
; Erweiterte Version von as (Sch„pers), 15-AUG-91.
; Erweiterte Version von as (Stiller), c't 11/95
; Assemblieren/Linken mit  TASM + Tlink  + Exe2bin
; Fr Saturn, Mercury, Neptun, (C000h-EFFFh)
; Triton (mit DMA nur E000-EFFFh !)
; Aufruf mit:

; DEVICE=UMBPCI.SYS /I=aaaa-bbbb /I=cccc-dddd....

; Beispiel: DEVICE=UMBPCI.SYS /I=CC00-CFFF /I=E000-EFFF

          .MODEL SMALL
          .CODE
          .586

INITREQUEST STRUC   ; Parameterblock fr INIT
  irLength        db ?    ; Gesamtumfang des Parameterblocks
  irUnit          db ?    ; nicht verwendet
  irFunction      db ?    ; Funktion $00, d.h. Init
  irStatus        dw ?    ; Ergebniscode der Initialisierung
  irReserved      db 8 dup (?) ; "reserviert", nicht verwendet
  irUnits         db ?    ; Zahl der vom Treiber bedienten Ger„te
  irEndAddress    dd ?    ; h”chste verfgbare/belegte Speicheradr.
  irParamAddress  dd ?    ; Adresse der Kommandozeile (device=...)
  irDriveNumber   db ?    ; erste nicht belegte Laufwerks-Kennz.
  irMessageFlag   db ?    ; Flag fr Fehlermeldungen
INITREQUEST ENDS

; Ger„tetreiberkopf
DNext     dd -1           ; Adresse des n„chsten Treibers: FFFF:FFFF
DAttr     dw 0E000h       ; Attribute
DStrat    dw OFFSET Strat ; Offset der "Strategie"-Routine
DIntr     dw OFFSET Intr  ; Offset der "Interrupt"-Routine
DName     db 'UMBADDXX'   ; Name (oder Units)

ParamAddr    dd ?         ; Zwischenspeicher fr INIT-Parameterblock

; Strategie-Routine, speichert ES:BX (Parameterblock-Adresse)
Strat     PROC FAR
          mov Word Ptr [ParamAddr],bx
          mov Word Ptr [ParamAddr+2],es
          retf
Strat     ENDP

; Interrupt-Routine, definiert nur die INIT-Funktion
Intr      PROC FAR
          push ax         ; mehr als diese drei Register
          push bx         ; werden momentan nicht gebraucht
          push es
          pushf
          les  bx,[ParamAddr] ; ES:BX = Parameterblock
          mov  es:[bx+irStatus],8103h ; pessimistischer Ansatz
          mov  al,es:[bx+irFunction] ; Funktionsnummer
          or   al,al      ; Init?
          jnz  OtherCmd   ; Code "Befehl unbekannt" gesetzt
          call InitFunc   ; INIT - liefert Status in AX
          les  bx,[ParamAddr]
          mov  es:[bx+irStatus],ax
OtherCmd:
	  popf
          pop  es
          pop  bx
          pop  ax
          retf
Intr      ENDP

UMBEntry STRUC
Start     dw ?      ; Startadresse (Paragraphs)
Len       dw ?      ; L„nge Paragraphs
UMBEntry ENDS

UMBList  dw 22 dup(0) ; maximal 10 Eintr„ge

; wird von IntFunc in die Kette eingesetzt und von DOS
; zum Belegen der UMBs aufgerufen.
; Aufruf: DX = gewnschte Gr”áe (Bytes), Flags auf dem Stack
; Zurck: DX = Gr”áe gefundener Block, BX = Segment, AX = 1
;         bzw. AX = 0 und BL = $B1 fr "berhaupt kein Block"
;         bzw. AX = 0 und BL = $B0 fr "Block zu klein"
UMBRequest PROC NEAR
          push ds
          push si
          push cx
          push bx

          push cs
          pop  ds     ; DS aufs Codesegment
          mov  si,OFFSET UMBList
          xor  cx,cx  ; fr Gr”áenvergleich
          cld         ; aufsteigende Richtung
UMBSearch:
          lodsw       ; ein Eintrag der UMB-Liste
          or   ax,ax  ; Listenende?
          jz   UMBEnd ; -> ja, nicht gefunden
          mov  bx,ax  ; Startsegment festhalten
          lodsw
          cmp  bx,-1  ; bereits besetzt?
          jz   UMBSearch ; -> ja, n„chster Eintrag
          cmp  ax,cx  ; bis dato gr”áter Block?
          jb   ChkSize
          mov  cx,ax  ; ja, Gr”áe festhalten
ChkSize:  cmp  ax,dx  ; reicht die Gr”áe?
          jb   UMBSearch ; -> nein, n„chster Block
UMBFound: mov Word Ptr[si-4],-1 ; Block als vergeben markieren
          mov  dx,ax  ; Gr”áe in DX, Segment in BX
          pop  ax     ; altes BX vom Stack
          mov  ax,1   ; Signal fr Erfolg
          jmp  Short UMBDone
UMBEnd:   xor  ax,ax
          pop  bx     ; BX-Original (BH)
          mov  bl,0B0h ; Annahme: Block nur zu klein
          mov  dx,cx   ; zurckzuliefernde Blockgr”áe
          or   dx,dx  ; irgendetwas gefunden?
          jnz  UMBDone ; -> ja, nur zu klein
          inc  bl     ; gar nichts gefunden: BL = $B1
UMBDone:  pop  cx
          pop  si
          pop  ds
          popf
          retf
UMBRequest ENDP

; Kette von HIMEM.SYS
XMSOrgJmp dd ?       ; Originalwert
NewChain: jmp short NewCtrl
          nop        ; an dieser Stelle wrde der n„chste
          nop        ; Handler seinen Sprung einsetzen
          nop
NewCtrl:  pushf
          cmp  ah,10h  ; UMB request?
          jz  UMBRequest ; Flags auf dem Stack!
          popf
          jmp  [XMSOrgJmp] ; sonst Weitergabe

;
END_RESIDENT LABEL   BYTE   ; hier endet der residente Teil
;
; *** Beginn des nicht residenten Teils ***
CRLF      db 13,10,'$'

PrintString:
          push dx
          mov  dx,OFFSET CRLF
          call DoPrint
          pop  dx
          call DoPrint
          mov  dx,OFFSET CRLF
DoPrint:  mov  ah,09h
          int  21h
          ret

; Ausgabe einer Hex-Zahl mit vier Stellen (AX)
HDigits   db '0123456789ABCDEF'
PrHex:    mov  cx,0Ch   ; beim ersten Mal 12 Bit schieben
PrDig:    push ax
          shr  ax,cl
          and  ax,000Fh
          mov  bx,ax
          mov  dl,[HDigits+bx]
          mov  ah,2     ; "Display Output"
          int  21h
          pop  ax
          sub  cl,4
          jnb  PrDig
          ret

; Ausgabe einer Fehlermeldung ber die DOS-Funktion $09 und Abbruch
; Aufruf mit DS:DX = Text der Meldung
ErrOut:   call PrintString               ; Textausgabe
          mov  ax,0
          int  16h
          les  bx,[ParamAddr]
          mov  Word Ptr es:[bx+irEndAddress],0 ; 0 Bytes Platzbedarf
          mov  ax,8100h  ; Fehler (unspezifisch)
          jmp  InitEnd

; Initialisierung - wird mit ES:DI = Parameterblock und
; ES,BX,AX/Flags auf dem Stack aufgerufen, d.h. muá sich um
; diese Register nicht mehr kmmern
InitFunc  PROC NEAR
          push cx
          push dx
          push si
          push di
          push ds

          push cs
          pop  ds          ; DS aufs Codesegment

          mov  Word Ptr es:[bx+irEndAddress],OFFSET CRLF
          mov  Word Ptr es:[bx+irEndAddress+2],cs ; Endaddr eintragen
          mov  dx,OFFSET Copyright$  ; "Grá Gott"
          call PrintString

; HIMEM installiert?
chkHimem:
          mov  ax,4300h    ; Installationsprfung von HIMEM
          int  2Fh
          cmp  al,80h      ; vorhanden?
          jz   GetXMSCall  ; -> ja
          mov  dx,OFFSET NoXMS$
          jmp  ErrOut
; Kette ermitteln
GetXMSCall:
          mov  ax,4310h    ; HIMEM: Einsprungadresse
          int  2Fh
          mov  Word Ptr [XMSOrgJmp],bx
          mov  Word Ptr [XMSOrgJmp+2],es
; Existiert bereits ein UMB Server?
          mov  ah,16
          mov  dx,0FFFh    ; Anfordern eines Blocks mit 64 KByte
          call [XMSOrgJmp]
          cmp  bl,80h      ; nicht installiert?
          jz   GetCmdLine  ; -> OK
          mov  dx,OFFSET UMBPresent$
          jmp  ErrOut      ; da ist schon wer!
; Kommandozeile auswerten
GetCmdLine:
          les  di,[ParamAddr]  ; Parameterblock
          les  di,es:[di+irParamAddress] ; ES:DI auf Kommandozeile
          call SetList     ; Auswerten und Liste setzen
          jnc  TestAreas   ; -> OK
          mov  dx,OFFSET BadCmd$
          jmp  ErrOut      ; Kommandozeile ausgeben

TestAreas:CALL INITChipset
          JC   chipseterr

          mov  si,OFFSET UMBList
          cld
NextUMB:  lodsw            ; Startadresse
          or  ax,ax        ; Ende der Liste erreicht?
          jz  AreasTested
          mov es,ax        ; Startadresse in ES
          lodsw            ; Gr”áe (Paragraphs)
          mov  cl,10       ; Umrechnung in 16K-Blocks
          shr  ax,cl       ; => Anzahl der 16-K-Bl”cke
TstBlocks: xor  di,di
          mov  bx,es:[di]   ; momentanen Inhalt lesen
          mov  dx,bx
          xor  dx,0FFFFh    ; Umdrehen
          mov  es:[di],dx   ; und Schreiben
          mov  cx,80h       ; einen Moment warten, falls die
          loop $            ; Operation ins Leere gehen sollte
          cmp  es:[di],dx   ; haben wir einen konstanten Wert?
          mov  es:[di],bx   ; alten Inhalt zurck
          jnz  NoRAM        ; -> kein RAM an dieser Stelle!
          dec  ax
          jz   NextUMB      ; -> n„chster UMB-Bereich
          mov  di,es
          add  di,0400h     ; sonst ES 16K aufw„rts
          mov  es,di
          jmp  TstBlocks    ; und n„chsten 16K-Block prfen

NoRAM:    push es           ; Segmentadresse des Blocks
          mov  dx,OFFSET RAMFail1$
          call DoPrint      ; Ausgabe ohne CRLF
          pop  ax
          call PrHex        ; Segmentadresse ausgeben
          mov  dx,OFFSET RAMFail2$
          jmp  ErrOut

chipseterr:mov dx,OFFSET SetErr$
           jmp ErrOut

; Hier geht es nach einem erfolgreichen RAM-Test mit dem Einsetzen
; des eigenen "Hook" in die HIMEM.SYS-Sprungkette weiter
AreasTested:
           les di,[XMSOrgJmp]
ScanHooks: mov  al,Byte Ptr es:[di]
          cmp  al,0EBh      ; JMP SHORT?
          jz   HookFound    ; OK - momentanes Kettenende
          les  di,DWord Ptr es:[di+1] ; sonst n„chstes Element
          cmp  al,0EAh      ; war das berhaupt ein JMP FAR?
          jz   ScanHooks
          mov  dx,OFFSET HookErr$  ; Hook-Kette besch„digt
          jmp  ErrOut
HookFound: cli               ; *keine* St”rungen hier, bitte!
          dec  Byte Ptr es:[di] ; JMP SHORT -> JMP FAR
          mov  Word Ptr es:[di+1], OFFSET NewChain ; eigene Routine
          mov  Word Ptr es:[di+3], cs   ; einsetzen
          add  di,5         ; echte Originaladresse (hinter den NOPS)
          mov  Word Ptr [XMSOrgJmp],di
          mov  Word Ptr [XMSOrgJmp+2],es
          sti

showinst:
          mov  dx,OFFSET Installed$
          call PrintString
          mov  ax,0100h    ; Initialisierung fehlerfrei
InitEnd:  pop  ds
          pop  di
          pop  si
          pop  dx
          pop  cx
          ret
InitFunc  ENDP

Copyright$  db 'UMBADD c''t 11/95 - Siering/Sch„pers/Stiller - Modified for MiSTer AO486$'
NoXMS$      db 'Kein XMS-Treiber vorhanden!$'
UMBPresent$ db 'UMB Server bereits installiert!$'
BadCmd$    db 'Probleme mit den Bereichsangaben in CONFIG.SYS!',13,10
           db 'Format: DEVICE=UMBADD.SYS /Iaaaa-bbbb /Icccc-dddd ...$'
RAMFail1$   db 13,10,'Kein RAM im Segment (hex) $'
RAMFail2$   db '- Installation abgebrochen -$'
HookErr$    db 'Hook-Chain von HIMEM.SYS besch„digt bzw. unbekannt !$'
Installed$  db 'Programm installiert.$'
SetErr$     db 'Not a MiSTer AO486$'

; Aufruf mit ES:DI als Index in die Kommandozeile
; zurck mit DI+4, Wert bzw. 0 in DX und gel”schtem/gesetzten Z-Flag
HexToVal: push cx
          mov  cx,0404h     ; Z„hler, Shift-Z„hler
          xor  dx,dx        ; Wert
DoDigit:  shl  dx,cl        ; eine Stelle nach links
          mov  al,es:[di]   ; ein Zeichen
          inc  di
          cmp  al,'9'
          jbe  IsDigit
          and  al,0DFh      ; Groábuchstaben
          cmp  al,'A'
          jb   NoChar       ; Fehler
          cmp  al,'F'
          ja   NoChar       ; dito
          sub  al,55        ; 'A'..'F' -> 10..16
          jmp  short SetDig ; Einsetzen
IsDigit:  cmp  al,'0'
          jb   NoChar       ; Fehler
          sub  al,'0'       ; '0'..'9' -> 0..9
SetDig:   or   dl,al        ; Einsetzen
          dec  ch
          jnz  DoDigit
          jmp  Short DigDone
NoChar:   xor  dx,dx        ; Fehleranzeige: dx = 0
DigDone:  pop  cx
          or   dx,dx
          ret

; Auswertung der Kommandozeile. Aufruf mit ES:DI = Kommandozeile,
; zurck mit gesetzter UMBList und gesetztem/gel”schten Carry
SetList   PROC NEAR
          xor  bx,bx        ; Index in UMBList
DoElem:   mov  al,es:[di]
          inc  di
          cmp  al,13        ; CR?
          jz   ListSet      ; -> ja, Ende
          cmp  al,10        ; LF?
          jz   ListSet      ; -> dito
          cmp  al,'/'       ; Schr„gstrich von "/I=..."?
          jnz  DoElem
          mov  ax,es:[di]
          and  al,0DFh      ; Umsetzung 'i' -> 'I'
          cmp  ax,'=I'      ; ist "I="? (verkehrtherum)
          jnz  listerr       ; -> nein, TestC
          add  di,2         ; Ok, berspringen
          call HexToVal     ; Hexzahl auswerten -> DX
          jz   ListErr      ; -> war nichts
          mov  al,es:[di]
          cmp  al,'-'       ; Trennstrich?
          inc  di
          mov  cx,dx        ; Startadresse festhalten
          call HexToVal     ; Endadresse ermitteln
          jz   ListErr      ; -> war nichts
          mov  Word Ptr[UMBList+bx],cx  ; Eintrag Startadresse
          mov  ax,dx
          sub  ax,cx        ; Gr”áe in Paragraphs
          inc  ax           ; B000-B7FF sind $800 Paragraphs!
          mov  Word Ptr[UMBList+bx+2],ax
          add  bx,4
          cmp  bx,40        ; mehr als 10 Eintr„ge?
          jb   DoElem       ; -> nein, weiter

ListErr:  stc
          ret
ListSet:  sub  bx,1         ; setzt Carry, wenn BX = 0
          ret
SetList   ENDP

;************* MiSTer specific code *******

InitChipset PROC Near
	  push bp
	  mov bp, sp

; Check for 386
	  pushf
	  mov  cx, 7000h
	  push cx
	  popf
	  pushf
	  pop  cx
	  test ch, 70h
	  jz   @InitErr
	  popf
  
; Check for CPUID present
	  and  sp,0fffch
	  pushfd
	  mov  edx, ecx
	  xor  ecx, 200000h
	  push ecx
	  popfd
	  pushfd
	  pop  ecx
	  cmp  edx, ecx
	  jz   @InitErrPopfd

; Check for AO486
	  xor  eax, eax
	  cpuid
	  cmp  ebx, 5453694dh
	  jne  @InitErrPopfd
	  cmp  ecx, 3638344fh
	  jne  @InitErrPopfd
	  cmp  edx, 41207265h
	  jne  @InitErrPopfd
	  popfd
	  clc
	  jmp @Initexit

@InitErrPopfd:
	  popfd
@InitErr:
	  stc
@Initexit:
	  mov  sp,bp
	  pop  bp
	  ret

InitChipset Endp

  	  END

