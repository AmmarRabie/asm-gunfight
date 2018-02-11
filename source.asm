include drawingfighter.inc
include drawingline.inc
include welcomeScreen.inc
include drawingobst.inc

waitMicroSeconds macro highWord,lowWord
push cx
push dx
push ax
    mov ah,86h
    mov cx,highWord
    mov dx,lowWord
    int 15h
pop ax
pop dx
pop cx   
endm waitMicroSeconds
    

getEmptyBullet macro bullets
    
    local loopBullets
    local foundEmpty
    push cx
    pushf
    

    mov cx, 0   ; cx = offset of bullet
    loopBullets:
    
        mov bx,offset bullets
        add bx,cx
        
        ;add bx,4 ; get the direction
        ;cmp [bx],0FFFFh
        call isBulletFull
        jz foundEmpty 

    add cx,6d ; to the next bullet --> each bullet is byte
    cmp cx,maxNumBullets*6
    jnz loopBullets
    
    
    
foundEmpty:
  ;sub bx,4d     
  popf
  pop cx       
endm getEmptyBullet

; take the bullets that will fire from
; xPos,yPos, the dirction
Fire macro bullets,x,y,dir 
 push bx
 push dx
    
        ; get the first empty bullet of bullets in bx
        getEmptyBullet bullets
        
        mov dx,x
        mov [bx],dx
        add bx,2    ; y
        mov dx,y
        mov [bx],dx
        add bx,2    ; direction
        mov dl,dir
        mov dh,0
        mov [bx],dx
        
 pop dx
 pop bx
        
Fire endm        
       
.model small 
.stack 64
.data

;---------------------- ; parameters to macros and procedures

drXmin dw 0d    ; parameters used in drawrect procedure
drXmax dw 60d
drYmin dw 0d
drYmax dw 10d
drColor db 14d

temp1B db ?

;----------------------; Golbal variables for the game
roundStatus DW 0 ; {0:inPlay, 1:player 1 win, 2:player2 win}

roundNum equ 9   ; the number of rounds per game

roundCount db 0  ; the current round players play (0:8)

;----------------------- ;fighter 1 information
fighter1Xmin dw 11d
fighter1Xmax dw 10d
fighter1Ymin dw 100d
fighter1Ymax dw 300d
fighter1gunx  dw 11d     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; force
fighter1guny  dw 300d    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; force
fighter1Color db 14d
fighter1mode  db 0d     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; force  0,2,4
player1name db 16,?,17 dup('$')        ;; mafeesh data type 
player1Score db 0d;
fighter1newxmin dw 0d
fighter1newymin dw 0d   
fighter1hiddes  db Numofhiddes 
fighter1IsNowHidden db  0d   ;  0:not Hidden 1:isHidden  
fighter1HideTime    db  0d,0d       ; min and sec
;------------------------; fighter2 information
fighter2Xmin dw 0d
fighter2Xmax dw 309d
fighter2Ymin dw 100d
fighter2Ymax dw 300d
fighter2gunx  dw 0d      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; force
fighter2guny  dw 100d    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; force
fighter2Color db 14d
fighter2mode db  1d     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; force
player2name db  16,?,17 dup('$')
player2Score db 0d;
player2NumOfB db 7d;
fighter2newxmax dw 0d
fighter2newymin dw 0d   
fighter2hiddes  db numofhiddes 
fighter2IsNowHidden db  0d   ;  0:not Hidden 1:isHidden 
fighter2HideTime    db  0d,0d

;---------------------  scan codes
upsc   equ  48h
downsc equ  50h
rightsc equ  4dh
leftsc  equ  4bh
fighter1UpGunSC        equ  33h       ; scan code of  .<
fighter1NormalGunSc    equ  34h       ; scan code of  .>
fighter1DownGunSc      equ  35h       ; scan code of /?
fighter1HideSC           equ  32h   ; scan code of m
spcSc equ 39h                             ;sc of space key


Wsc   equ   11h
Asc   equ   1eh
Ssc   equ   1fh
Dsc   equ   20h
fighter2UpGunSC          equ   13h        ;sc of R
fighter2NormalGunSC      equ   14h        ;sc of T
fighter2DownGunSC        equ   15h        ;sc of Y
fighter2HideSc           equ   12h   ; scan code of E
ctrlSc equ 16h                            ;sc of controrl key



F1Sc equ 3bh
F2sc equ 3ch
ESCSc equ 01h 
IncDiff_sc equ 48h    ; up arrow
DecDiff_sc equ 50h    ; down arrow

ChngBulletMode_sc  equ 4Bh    ; right arrow or left arrow
ChngBulletMode_sc2 equ 4Dh    ; right arrow or left arrow
  

;---------------------  map information
fightmapxmin  equ  0d
fightmapymin  equ  9d
fightmapxmax  equ  320d
fightmapymax  equ  159d
fighter1part  equ  120d
fighter2part  equ  200d

fighterheight equ 51d
fighterwidth  equ 37d

backgroundColor    equ 0d
borderColor   equ  9d
;------------------------ general definitions
mesEnter db "Please enter your name: ",'$'
mesPress db "Press Enter key to continue ",'$'
mesChoice1 db "*To start chatting Press F1",'$'      
mesChoice2 db "*To start GunFight game Press F2",'$'
mesChoice3 db "*To end the Program Press ESC",'$'
mesChoiceDiff db "difficulty(",24d,',',25d,')','$'
mesChoiceMode db "Bullet Mode(",26d,",",27d,")",'$'


gameDifficltuy db 1+3      ; init with default value to be seen first in the screen(increase this value to speedFactor)
BulletMode db 0     ;0:translation      ;1:bounding
maxNumBullets   equ  3   ; maximum number of bullets in the screen of each player

obstColor   equ  0Ah    

bulletSpeed equ  1d
speedFactor db  4d+3     ; init with minimum speed (this value corresponds to 1 in gameDifficltuy),(at init should be greaterThean gameDifficltuy)

GameCurrentTime db 0d,0d,0d 
GameOldTime db 0d,0d,0d

HideColor equ 16d
NumOfHiddes equ 3d 
;------------------------ bullets info

P1bulletsFired dW 0 , 3*3 dup (0FFFFh)   ; 0 for exactly number of curren bullets on screen
P2bulletsFired dW 0 , 3*3 dup (0FFFFh)   ; 3 max buulets * 3 bytes for each bullet (x,y,direction){0FFFFh:empty,1:normal,2:top,3:bottom}

P1RemainBullets db 0FFh  ; start the game with seven bullets           ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;forced;;;;;;;;;;;;;;;;;;;;;;
P2RemainBullets db 0FFh                                                   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;forced;;;;;;;;;;;;;;;;;;;;;;

bulletColor   equ  4

;------------------------------------




.code
main proc far
     
     mov ax,@data
     mov ds,ax   
     
     ;------------------------   to write to video memory direct         
     mov ax,0A000h               ;row *320+column ; to write direct to video ram
     mov es,ax        
     
     mov ah,0
     mov al,13h
     int 10h
        
     call getusername   ; get users name in different screens
     
MainProgram:
  ; draw the main screen tp get a choice
  mainscreen meschoice1,meschoice2,meschoice3,mesChoiceDiff,gameDifficltuy,mesChoiceMode,BulletMode 
  mov ah,1
  int 16h
  jz MainProgram
     
  ;clear the buffer
  push ax
  mov ah,0
  int 16h
  pop ax
  
     cmp ah,F1Sc
     jz IsF1
     
     cmp ah,F2Sc 
     jz IsF2
     
     cmp ah,ESCSc
     jz IsESc
     
     cmp ah,IncDiff_sc
     jz IsIncDiff
     
     cmp ah,DecDiff_sc
     jz IsDecDiff
     
     cmp ah,ChngBulletMode_sc
     jz IsChngBulletMode
     cmp ah,ChngBulletMode_sc2
     jz IsChngBulletMode
     
     ; it is not a key of our action keys, so ignore it and ret    
     jmp MainProgram
     
      
      
     IsF1:
           ;for now, do nothing 
           jmp mainProgram  
           
     IsF2:
           ; run a new game 
           call GunFight
           jmp MainProgram
          
     
     IsIncDiff:      
           call increaseDifficluty           
           jmp Mainprogram
           
     IsDecDiff:      
           call decreaseDifficluty                      
           jmp Mainprogram
           
           
           
     IsChngBulletMode:                                 
           not BulletMode ; for now
           jmp Mainprogram
                      
                
     IsESc: 
           ; clear the screen
           chngeToVedioMode 
           ; return to dos                
           mov	ax,4c00h
	       int	21h					; Call DOS interrupt 21h  to return to operating system      
              
      HLT
main endp 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Draw Fighters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; to eliminate the code lentgh
drawfighter1WithGun proc
    ; call macros
drawfighter1     fighter1xmin,fighter1ymin,fighter1xmax,fighter1ymax,fighter1color
drawfighter1gun  fighter1xmin,fighter1ymin,fighter1mode,fighter1color,fighter1gunx,fighter1guny
ret
drawfighter1WithGun endp

; to eliminate the code lentgh
drawfighter2WithGun proc
    ; call macros
drawfighter2     fighter2xmax,fighter2ymin,fighter2xmin,fighter2ymax,fighter2color
drawfighter2gun  fighter2xmax,fighter2ymin,fighter2mode,fighter2color,fighter2gunx,fighter2guny
ret
drawfighter2WithGun endp
;///////////////////////////////////////////////////////////////////////////////////////////////////




; ------begin  :  DrawRect-----------------------------------------------------------
; input:
;   drxmax,drxmin,drymax,drymin datatype word
;  drcolor     datatype byte
; assume that the program video mode
; draws the rectangle from xmin ,ymin
drawRect    proc
push cx
push dx
push ax
push di
push bx

;  calculate the position of required byte
mov di,0d
mov bx,320d
mov ax,drymin
mul bx
mov di,ax
add di,drxmin   ; no di have the correct index wrt memory
mov al,drcolor ;Pixel color


mov cx,drxmax
sub cx,drxmin
mov drxmin,cx
inc drxmin

mov dx,drymax
sub dx,drymin
inc dx
DrawRectouterloop:

mov cx,drxmin   ; initilaze the column loop with xmax-xmin
rep stosb
sub di,drxmin
add di,320
dec dx
jnz drawrectouterloop

pop bx
pop di
pop ax
pop dx
pop cx

ret
endp
;------------------------------------------------------------------
;draw line with 4 info
; starting point[cx,dx],
; lentgh[bx],
; direction(H or V)[si 0H 1V],
; color[al]
;return the last point in cx,dx
drawIncreasingStraightLine proc

pushf
push ax
push di


mov ah,0ch
mov di,0

cmp si,1
jz VerticalLoop

horizontalLoop:
int 10h
inc cx

inc di
cmp di,bx
jnz horizontalLoop
pop di
pop ax
popf
ret


VerticalLoop:
int 10h
dec dx

inc di
cmp di,bx
jnz VerticalLoop


pop di
pop ax
popf
ret
endp


;-------------------------------------------------------------------------------------
;draw line with 4 info
; starting point[cx,dx],
; lentgh[bx],
; direction(RT RB LT LB according to the starting point)[si 0RT 1RB 2LT...],
; color[al]
; return the last point in cx,dx
draw45degreeLine proc

pushf
push ax
push di


mov ah,0ch
mov di,0

; detect the direction and go to the correct loop
cmp si,0d
jz RightTopLoop
cmp si,1d
jz RightBottomLoop
cmp si,2d
jz LeftTopLoop
jmp LeftBottomLoop

RightTopLoop:
int 10h
inc cx
dec dx

inc di
cmp di,bx
jnz RightTopLoop
jmp EndAndReturn


RightBottomLoop:
int 10h
inc cx
inc dx

inc di
cmp di,bx
jnz RightBottomLoop
jmp EndAndReturn


LeftTopLoop:
int 10h
dec cx
dec dx

inc di
cmp di,bx
jnz LeftTopLoop
jmp EndAndReturn


LeftBottomLoop:
int 10h
dec cx
inc dx

inc di
cmp di,bx
jnz LeftBottomLoop


EndAndReturn:
pop di
pop ax
popf
ret


endp
;--------------------------------------------------------------------------------------------
movplayer1 proc 
      
     push cx  
        
        ; first check if the new coordinates not valid
        mov cx,fighter1newxmin
        cmp cx,fightmapxmin
        jle player1NotMoving
         
        add cx,fighterwidth
        cmp cx,fighter1part
        jge player1NotMoving
        
        mov cx,fighter1newymin
        cmp cx,fightmapymin
        jle player1NotMoving
        
        add cx,fighterheight
        cmp cx,fightmapymax
        jge player1NotMoving
        
        
        
        mov cx,fighter1xmin
        mov drxmin,cx
       
        mov cx,fighter1ymin
        mov drymin,cx   
        
        mov cx,fighter1xmax
        mov drxmax,cx  
        
        mov cx,fighter1ymax 
        inc cx
        mov drymax,cx
        
        mov drcolor,backgroundColor;
        
        call drawrect 
        
       
        mov cx,fighter1newxmin
        mov fighter1xmin,cx  
                          
        mov cx,fighter1newymin
        mov fighter1ymin,cx
       
        cmp Fighter1isNowhidden,1
        jnz movPlayer1IsNothidden 
        
        mov cl,fighter1Color 
        mov ch,0d  
        push cx
        mov Fighter1color,HideColor
        movplayer1IsNothidden:
       
        call drawfighter1WithGun
       
        cmp Fighter1isNowhidden,1
        jnz  player1Moved 
        
        pop cx  
        mov Fighter1color,cl
        
       jmp player1Moved
     
       player1NotMoving:
       mov cx,fighter1xmin
       mov fighter1newxmin,cx  
                          
       mov cx,fighter1ymin
       mov fighter1newymin,cx
    
      player1Moved:
     
     pop cx  
     ret
endp    

;---------------------------------------------------------------------------------------------------------------------
movplayer2 proc 
      
     push cx  
        
        ; first check if the new coordinates not valid
        mov cx,fighter2newxmax
        cmp cx,fightmapxmax
        jge  Player2NotMoving
         
        sub cx,fighterwidth
        cmp cx,fighter2part
        jle  Player2NotMoving
        
        mov cx,fighter2newymin
        cmp cx,fightmapymin
        jle  Player2NotMoving
        
        add cx,fighterheight
        cmp cx,fightmapymax
        jge  Player2NotMoving
        
         
         ; if the new cooridantes is valid   first remove the fighter
        mov cx,fighter2xmin
        mov drxmin,cx
       
        mov cx,fighter2ymin
        mov drymin,cx   
        
        mov cx,fighter2xmax
        mov drxmax,cx  
        
        mov cx,fighter2ymax 
        inc cx
        mov drymax,cx
        
        mov drcolor,0d;
        
        call drawrect 
        
      ; then draw the new fighter 
      mov cx,fighter2newxmax
      mov fighter2xmax,cx  
                          
      mov cx,fighter2newymin
      mov fighter2ymin,cx 
      
      
        cmp fighter2isNowhidden,1
        jnz movPlayer2IsNothidden 
        
        mov cl,Fighter2Color 
        mov ch,0d  
        push cx
        mov Fighter2color,HideColor
        movplayer2isNothidden:
       
       
        call drawfighter2WithGun
       
        cmp Fighter2isNowhidden,1
        jnz  player2Moved 
        
        pop cx  
        mov Fighter2color,cl
        
       jmp player2Moved
       
      Player2NotMoving:
      mov cx,fighter2xmax
      mov fighter2newxmax,cx  
                          
      mov cx,fighter2ymin
      mov fighter2newymin,cx
      
      player2Moved:
     pop cx   
     ret
endp
 


;----------------------------------------------------------------------------------------------- 
changefighter1gun proc
 
 
 ;first remove the first gun  
 
        mov cx,fighter1xmin
        add cx,28d
        mov drxmin,cx
       
        add cx,9d
        mov drxmax,cx   
        
        mov cx,fighter1ymin 
        add cx,15d
        sub cx,9d
        mov drymin,cx  
        
        mov cx,fighter1ymin 
        add cx,15d
        add cx,9d
        mov drymax,cx  
        
        mov drcolor,backgroundColor;
        
        call drawrect 
  ;second draw new gun
        cmp Fighter1isNowhidden,1
        jnz ChangeFighter1GunIsNothidden 
       
        mov cl,fighter1Color 
        mov ch,0d  
        push cx
        mov Fighter1color,HideColor 
        
  ChangeFighter1GunIsNothidden:  
  drawfighter1gun fighter1xmin,fighter1ymin,fighter1mode,fighter1color,fighter1gunx,fighter1guny     
    
    cmp Fighter1isNowhidden,1
    jnz ChangeFighter1GunEnd:
        pop cx  
        mov Fighter1color,cl
        
    
    changefighter1GunEnd:   
  ret
    
endp

;--------------------------------------------------------------------------------------------------
changefighter2gun proc
 
 
 ;first remove the gun  
 
        mov cx,fighter2xmax
        sub cx,28d
        mov drxmax,cx
       
        sub cx,9d
        mov drxmin,cx   
        
        mov cx,fighter2ymin 
        add cx,15d
        sub cx,9d
        mov drymin,cx  
        
        mov cx,fighter2ymin 
        add cx,15d
        add cx,9d
        mov drymax,cx  
        
        mov drcolor,backGroundColor;
        
        call drawrect 
  ;second draw new gun
  
   cmp Fighter2isNowhidden,1
        jnz ChangeFighter2GunIsNothidden 
       
        mov cl,fighter2Color 
        mov ch,0d  
        push cx
        mov Fighter2color,hideColor
        
  ChangeFighter2GunIsNothidden:  
  drawfighter2gun fighter2xmax,fighter2ymin,fighter2mode,fighter2color,fighter2gunx,fighter2guny     
    
    cmp Fighter2isNowhidden,1
    jnz ChangeFighter2GunEnd:
        pop cx  
        mov Fighter2color,cl
        
    
    changefighter2GunEnd:     
       
  ret
    
endp   



;----------------------------------------------------
; changes fighter 1 parameter to hide mode
HideFighter1  proc 
           
 
  
  cmp fighter1hiddes,0d
  jz HideFighter1End
                     
  getMinSec fighter1HideTime[0],fighter1HideTime[1]
                     
  dec fighter1hiddes 
  mov fighter1IsNowHidden,1
   
   
   HideFighter1End:
ret                        
endp
     
;---------------------------------------------------     
; changes fighter 2 parameter to hide mode
HideFighter2  proc 
           
 
  
  cmp fighter2hiddes,0d
  jz HideFighter2End
                     
  getMinSec fighter2HideTime[0],fighter2HideTime[1] 
                   
  dec fighter2hiddes 
  mov fighter2IsNowHidden,1
   
   
   HideFighter2End:
ret                        
endp
     



 ;----------------------------------------- check on 

checkonHideTime proc 

push cx 
push dx
                        
cmp fighter1IsNowHidden,1d 
jnz chekonHideTime_fighter2 

       mov cl,GameCurrentTime[0]  ; minutes  
       mov dh,GameCurrentTime[1]   ; sec 

       cmp cl,fighter1HideTime[0]    ; check if they have the same  minutes
       jz checkonHideTime_sameminutes1
           
                  add dh,60d;    
      
       checkonHideTime_sameminutes1:; now in both cases we can compare the seconds
       
                  sub dh,3d;
                  
                  cmp dh,fighter1HideTime[1] 
                  jl   chekonHideTime_fighter2;; if less so fighter still in hide mode
                  
                  mov fighter1IsNowHidden,0
                  call MovPlayer1
                                      
chekonHideTime_fighter2:
cmp fighter2IsNowHidden,1d
jnz chekonHideTime_End 

       mov cl,GameCurrentTime[0]  ; minutes  
       mov dh,GameCurrentTime[1]   ; sec 

       cmp cl,fighter2HideTime[0]    ; check if they have the same  minutes
       jz checkonHideTime_sameminutes2
           
                  add dh,60d;    
      
       checkonHideTime_sameminutes2:; now in both cases we can compare the seconds
       
                  sub dh,3d;
                  
                  cmp dh,fighter2HideTime[1] 
                  jl   chekonHideTime_End    ; if less so fighter still in hide mode
                  
                  mov fighter2IsNowHidden,0
                  call MovPlayer2


chekonHideTime_End:
pop dx
pop cx  
    
ret    
endp        



;; [A] my opinion: make the main handle if there is an input, if there call this function and execute only the (sub or add) then retrun then call a function that redraw the players with these new info

;--------------------------------------------------------------------------------------------------
getAndExeUserAction proc

    mov ah,1
    int 16h
    jnz IsClicked
    ret
    
  IsClicked:
    ;clear the buffer
    push ax
    mov ah,0
    int 16h
    pop ax
    
    
    cmp ah,upsc
    jz IsUp
    
    cmp ah,downsc
    jz IsDown
    
    cmp ah,rightsc
    jz IsRight
    
    cmp ah,leftsc
    jz isLeft
    
    cmp ah,fighter1NormalGunSc
    jz isfighter1mod0
    
    cmp ah,fighter1DownGunSc
    jz isfighter1mod1
    
    cmp ah,fighter1UpGunSc
    jz isfighter1mod2
    
    cmp ah,spcSc
    jz isF1Fire   
    
    cmp ah,fighter1hidesc
    jz ishidefighter1 
    
    ;;;;;;; P2  ;;;;;;;;
    cmp ah,Wsc
    jz IsW
    
    cmp ah,Ssc
    jz IsS
    
    cmp ah,Dsc
    jz IsD
    
    cmp ah,Asc
    jz IsA
    
    cmp ah,fighter2NormalGunSc
    jz isfighter2mod0
    
    cmp ah,fighter2DownGunSc
    jz isfighter2mod1
    
    cmp ah,fighter2UpGunSc
    jz isfighter2mod2
    
    cmp ah,ctrlSc
    jz isF2Fire 
    
    
    cmp ah,fighter2hidesc
    jz ishidefighter2
    
    
;    cmp ah,ESCSc
;    jz mainprogram
    ; the cliciked button is not one of our action buttons, end
    jmp getAndExeUserAction_theEnd
    
    
    
    IsUp: 
    sub fighter1newymin,5d
    call movplayer1    
    jmp getAndExeUserAction_theEnd
    
    IsDown:
    add fighter1newymin,5d
    call movplayer1
    jmp getAndExeUserAction_theEnd
    
    IsRight:
    add fighter1newxmin,5d
    call movplayer1
    jmp getAndExeUserAction_theEnd
    
    isLeft:
    sub fighter1newxmin,5d
    call movplayer1
    jmp getAndExeUserAction_theEnd
    
    IsW:
    sub fighter2newymin,5d
    call movplayer2
    jmp getAndExeUserAction_theEnd
    
    IsS:
    add fighter2newymin,5d
    call movplayer2
    jmp getAndExeUserAction_theEnd
    
    IsD:
    add fighter2newxmax,5d
    call movplayer2
    jmp getAndExeUserAction_theEnd
    
    isA:
    sub fighter2newxmax,5d
    call movplayer2
    jmp getAndExeUserAction_theEnd
    
    isfighter1mod0:
    mov fighter1mode,0
    call changefighter1gun
    jmp getAndExeUserAction_theEnd
    
    isfighter1mod1:
    mov fighter1mode,2
    call changefighter1gun
    jmp getAndExeUserAction_theEnd
    
    isfighter1mod2:
    mov fighter1mode,4
    call changefighter1gun
    jmp getAndExeUserAction_theEnd
    
    isfighter2mod0:
    mov fighter2mode,1
    call changefighter2gun
    jmp getAndExeUserAction_theEnd
    
    isfighter2mod1:
    mov fighter2mode,3
    call changefighter2gun
    jmp getAndExeUserAction_theEnd
    
    isfighter2mod2:
    mov fighter2mode,5
    call changefighter2gun
    jmp getAndExeUserAction_theEnd
    
    
    isF1Fire:
        ; dont fire if his maxNumBullets bullets still on the screen
        cmp P1bulletsFired,maxNumBullets
        jz getAndExeUserAction_theEnd
        ; dont fire if he dont have remain bullets
        cmp P1RemainBullets,0
        jz getAndExeUserAction_theEnd
        ; dont fire if he is hide
        and fighter1IsNowHidden,1
        jnz getAndExeUserAction_theEnd
        
        ; execute the fire actino
        fire P1bulletsFired[2],fighter1gunx,fighter1guny,fighter1mode
        inc P1bulletsFired
        dec P1RemainBullets
    jmp getAndExeUserAction_theEnd
     
     
    isF2Fire:
        cmp P2bulletsFired,3d
        jz getAndExeUserAction_theEnd
        cmp P2RemainBullets,0
        jz getAndExeUserAction_theEnd
        and fighter2IsNowHidden,1
        jnz getAndExeUserAction_theEnd
                
        ; execute the fire actino
        fire P2bulletsFired[2],fighter2gunx,fighter2guny,fighter2mode
        inc P2bulletsFired
        dec P2RemainBullets
   jmp getAndExeUserAction_theEnd 
    
    

   IsHidefighter1:          ;
               call HideFighter1
               call movplayer1
               jmp getAndExeUserAction_theEnd 

   IsHideFighter2:
               call HideFighter2
               call movplayer2
               jmp getAndExeUserAction_theEnd 

    
getAndExeUserAction_theEnd:
;    mov ah,0
;    int 16h
ret
endp

;--------------------------------------------------------------------
getusername proc           ; to get user names in different screens


getUserNameScreen mesenter,mesPress,player1name

mov ah,0
mov al,13h  ; changing the video mode is faster than clearing the screen pixel pixel
int 10h

getUserNameScreen mesenter,mesPress,player2name

ret
endp


;-------------------------------------------------------------------
newround proc     ;initialize new game    

push cx
              
mov ah,0
mov al,13h  ;; changing the video mode is faster than clearing the screen pixel pixel
int 10h             
              
call drawscreen

 
mov fighter1Xmin, 11d
mov fighter1Xmax, 10d
mov fighter1Ymin, 100d
mov fighter1Ymax, 300d      
mov fighter1mode, 0d
mov fighter1newxmin, 0d
mov fighter1newymin, 0d
mov fighter1color,14d
mov fighter1IsNowHidden,0d

; two
mov fighter2Xmin ,0d
mov fighter2Xmax ,309d
mov fighter2Ymin ,100d
mov fighter2Ymax ,300d     
mov fighter2mode , 1d     
mov fighter2newxmax , 0d
mov fighter2newymin , 0d
mov fighter2color,14d
mov fighter2IsNowHidden,0d


mov P1RemainBullets, 7h  ; start the game with seven bullets
mov P2RemainBullets, 7h 
mov P1bulletsFired,0
mov P2bulletsFired,0

; empty all bullets
; and also set x,y to 0FFFFh --> we can delete it but to init the game as first
mov P1bulletsFired[2+0],0FFFFh
mov P1bulletsFired[2+2],0FFFFh
mov P1bulletsFired[2+4],0FFFFh
mov P1bulletsFired[2+6],0FFFFh
mov P1bulletsFired[2+8],0FFFFh
mov P1bulletsFired[2+10],0FFFFh
mov P1bulletsFired[2+12],0FFFFh
mov P1bulletsFired[2+14],0FFFFh
mov P1bulletsFired[2+16],0FFFFh
                               
mov P2bulletsFired[2+0],0FFFFh
mov P2bulletsFired[2+2],0FFFFh
mov P2bulletsFired[2+4],0FFFFh
mov P2bulletsFired[2+6],0FFFFh
mov P2bulletsFired[2+8],0FFFFh
mov P2bulletsFired[2+10],0FFFFh
mov P2bulletsFired[2+12],0FFFFh
mov P2bulletsFired[2+14],0FFFFh
mov P2bulletsFired[2+16],0FFFFh 

; draw fighters
call drawfighter1WithGun
call drawfighter2WithGun

; set new x as x,...
mov cx,fighter1xmin
mov fighter1newxmin,cx

mov cx,fighter1ymin
mov fighter1newymin,cx


mov cx,fighter2xmax
mov fighter2newxmax,cx

mov cx,fighter2ymin     
mov fighter2newymin,cx

mov roundStatus,0  


; print the init score 0:0
call printscore  

; print the round count as 1
call printRoundCount      

;cacl time
getCurrentTime GameoldTime[0],GameoldTime[1],GameoldTime[2] 

pop cx
ret
endp 

;--------------------------------------------------

newGame proc 
               
   mov player1Score, 0d;
   mov player2Score, 0d;
; two

   mov roundCount, 0 
   
   mov fighter2hiddes,numOfhiddes 
   mov fighter1hiddes,numOfhiddes
   
   ret            
endp


drawscreen proc
    drawmap player1name,player2name,bordercolor 

    draw_obstacle  160d-3*obstscale,50d,obstColor       ; 84d-60d
    draw_obstacle  160d-3*obstscale,90d,obstColor       ; 84d-60d        
    draw_obstacle  160d-3*obstscale,150d,obstColor      ;84d-60d+20d
    
ret    
drawscreen endp

;--------------------------------------------------------------------

; draw all bullets of two players
drawBullets proc
pusha
    
    mov drColor,bulletcolor
    ;drxmax,drxmin,drymin,drymax
 
 ; loop over all bullets, if it empty continue and dont draw, if not, draw   
    
    ;P1
    ;for each bullet
    mov cx, 0   ; cx = index of bullet
    loopBullets11:
    
        mov bx,offset P1bulletsFired[2]
        add bx,cx
        
        call isBulletFull
        jz continue11

        ; draw this bullet
        call drawbullet    ; take the bullet in bx
    
    continue11:
    add cx,6d ; to the next bullet --> each bullet is 3 words
    cmp cx,maxNumBullets*6
    jnz loopBullets11
    
   
    
    ;P2
    mov cx, 0   ; cx = offset of bullet
    loopBullets12:
    
        mov bx,offset P2bulletsFired[2]
        add bx,cx
        
        ;add bx,4 ; get the direction
        ;cmp [bx],0FFFFh
        call isBulletFull  ; make it macro, but take the input in bx(dont send it)
        jz continue12
        
        call drawbullet    ; take the bullet in bx
    
    continue12:
    add cx,6d ; to the next bullet --> each bullet is byte
    cmp cx,maxNumBullets*6
    jnz loopBullets12
popa
ret
endp


; draw bullet with color in drColor
drawbullet proc
push cx
push bx
push dx
        mov cx, [bx]  ; cx is x
        add bx,2
        mov dx, [bx]  ; dx = y
        
        mov drxmin,cx ;
                      ; set up for drawRect procedure
        mov drymax,dx ;
        
        add cx,2
        sub dx,2
        mov drxmax,cx 
        mov drymin,dx     
        call drawRect                 
pop dx
pop bx
pop cx
ret
drawbullet endp       



; move all bullets of the 2 players        
moveBullets proc

push bx
push cx
push si
  
  ; loop over each bullet, if it empty, continue, if not-> move the bullet
    ;drxmax,drxmin,drymin,drymax
    
    ;for each bullet
    
    mov cx, 0   ; cx = offset of bullet
    mov si,0 ; si = the player type
    loopBullets21:
        
        ;mov ch,0
        mov bx,offset P1bulletsFired[2]
        add bx,cx
        

        call isBulletFull
        jz continue21
        
        call moveBullet

    continue21:
    add cl,6d ; to the next bullet --> each bullet is byte
    cmp cl,maxNumBullets*6
    jnz loopBullets21
    
    

    mov cx, 0   ; cx = offset of bullet
    mov si,1 ; si = the player type    
    loopBullets22:
    

        mov bx,offset P2bulletsFired[2]
        add bx,cx
        

        call isBulletFull
        jz continue22
        
        call moveBullet

    
    continue22:
    add cl,6d ; to the next bullet --> each bullet is byte
    cmp cl,maxNumBullets*6
    jnz loopBullets22
    
    
pop si
pop cx
pop bx

ret
endp




; isEmpty bullet, return 0,1 in ZF
isBulletFull proc       ; 0:empty, 1:full ;ZF = 1 --> empty

    push bx
    
    add bx,4
    cmp [bx],0FFFFh
    
    pop bx
    
ret    
endp isBulletFull    
    

; move the bullet and validate it  
; take the player type in si              |
moveBullet proc ;{0:player1, 1:player2}   | take the bullet as offset in bx 
push bx                              ;    |
push di
push dx
    
    ; remvoe the old positino
    mov drColor,backgroundColor
    call drawbullet
    
    
    mov dx,bx
    ; x' is dependent on player
    ; y' is dependent on bullet direction {0:'do not move', 1:'increment',2:'decrement'}
    
    ; di = direction
    add bx,4
    mov di,[bx]
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; new in test --> shr
    shr di,1
    sub bx,2 ; y
    or di,0
    jz moveBullet_horizontal; if di = 0
    cmp di,1
    jz moveBullet_down      ; if di = 1
    cmp di,2 
    jz moveBullet_top       ; if di = 2
    jmp moveBulle_TheEnd
    
    moveBullet_horizontal:
        mov bx,dx
        or  si,0 ; doesn't change si
        jz moveBullet_P1X
       dec w.[bx]
        jmp moveBulle_TheEnd
        moveBullet_P1X:
        inc w.[bx]
        jmp moveBulle_TheEnd
        
    moveBullet_down:
        inc w.[bx]
        jmp moveBullet_horizontal
        
    moveBullet_top:  
       dec w.[bx]
        jmp moveBullet_horizontal
        
moveBulle_TheEnd:     
pop dx
pop di
pop bx
    
;ret
;endp moveBulle


                                  ;; Validation ;; 
    
    ; check bullet conditions {obtacle hit, player hit, translation between top and down}
    ; we should come here from moveBullet_horizontal, so the bx = dx = offset of the bullet, so call direct the proc
    
    
    ; si = player type         bx = offset of bullet
    ;call validateBullet
pusha
    
    mov di,bx ; store the offset to be used again
     
    mov dx,[di]
    ; first check if the bullet reach the end (xmax or xmin)
    cmp w.[di],fightmapxmax
    jge validateBullet_emptyBullet   ; if x >= xmax
    cmp w.[di],fightmapxmin
    jle validateBullet_emptyBullet   ; or if x <= xmin
    
    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;satrt validate translation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; snd check if reach the (ymax,ymin)
    add di,2
    cmp [di],fightmapymax
        jz validateBullet_ymax
    cmp [di],fightmapymin
        jz validateBullet_ymin
    jmp validateBullet_players
    
        validateBullet_ymin:
        cmp BulletMode,0
        jz validateBullet_ymin_translation
        ;handle the direction (buonding) ; if it hit yMin, it should be down
        add di,2
        sub [di],2 ; change to top (2->4 or 3->5)
        jmp validateBullet_players
            ;handle the translation
            validateBullet_ymin_translation:
            mov [di],fightmapymax
            jmp validateBullet_x
    
        validateBullet_ymax:
        cmp BulletMode,0
        jz validateBullet_ymax_translation
        ;handle the direction (buonding) ; if it hit yMin, it should be down
        add di,2
        add [di],2 ; change to top (2->0 or 3->1)
        jmp validateBullet_players
            ;handle the translation
            validateBullet_ymax_translation:        
            mov [di],fightmapymin
        
            validateBullet_x:
            or si,0
            jz validateBullet_shBulletRight; if player1
                
                ; if hit the bullet is for player2
                add [bx],fightmapxmax/2
                jmp validateBullet_players
                
                ; if hit the bullet is for player1
                validateBullet_shBulletRight:
                sub [bx],fightmapxmax/2        
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;End validate translation;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
            
    
        
    validateBullet_players:
    mov di,bx
    ;;;;;;;;;;;;;;;;;;;;; check all vertices of bullet (rectangle) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;
    ; +++
    ; +++
    ; o++
    ;
    mov cx, [di]
    add di,2
    mov dx, [di]
    mov al, fighter1Color
    call iscolored            ; iscolored check the color and retrun the real color in al
    jz validateBullet_hitPlayer
    
    mov al, hidecolor
    call iscolored
    jz validateBullet_hitPlayer
    
    cmp al, obstColor
    jz validateBullet_emptyBullet
    
    ;
    ; +++
    ; +++
    ; -+o
    ;    
    add cx,2
    mov al, fighter1Color
    call iscolored
    jz validateBullet_hitPlayer
    
    mov al, hidecolor
    call iscolored
    jz validateBullet_hitPlayer
    
    
    cmp al, obstColor
    jz validateBullet_emptyBullet
    
    ;
    ; ++o
    ; +++
    ; ++-
    ;    
    sub dx,2
    mov al, fighter1Color
    call iscolored
    jz validateBullet_hitPlayer
    
    mov al, hidecolor
    call iscolored
    jz validateBullet_hitPlayer
    
    cmp al, obstColor
    jz validateBullet_emptyBullet
    
    ;
    ; o+-
    ; +++
    ; +++
    ;    
    sub cx,2
    mov al, fighter1Color
    call iscolored
    jz validateBullet_hitPlayer
    mov al, hidecolor
    call iscolored
    jz validateBullet_hitPlayer
    cmp al, obstColor
    jz validateBullet_emptyBullet
    
    ;;;;;;;;;;;;;;;;;;;;; check all vertices of bullet (rectangle) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; retuern if there is no any special cases ( it will continue to be drawn)    
    popa
    ret 
    
    
    
    ; actions
    validateBullet_hitPlayer:
    mov roundStatus,si
    inc roundStatus    ; the status is the player type + 1
    
    validateBullet_emptyBullet:
    add bx,4
    mov [bx],0FFFFh
    sub bx,4
    cmp si,0
    jnz validateBullet_decP2B
    dec P1bulletsFired
    popa
    ret
    validateBullet_decP2B:
    dec P2bulletsFired
    
    
    
popa
ret        
endp moveBullet     



; check if a pixel colored with passed value or not
; return the actual color in al
; take cx,dx,al --> as the int illustrate
isColored proc near
    
    push bx
    
    mov bl,al
    
    mov bh,0
    mov ah,0Dh
    int 10h
    cmp al,bl 
    
    pop bx
ret
endp isColored




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Start Game And Rounds ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; isolate it in an procedure as logic of ending the game might differ in the future (we may consider time!)
isGameEnd proc
    
    cmp roundCount,8
 
ret    
endp isGameEnd




; isolate it in an procedure as logic of ending the round might differ in the future (we may consider time!)
isRoundRun proc   
push cx

    cmp roundStatus,0
    jz isRoundRun_chkDraw
    jmp isRoundRun_clearZF                 ;clear ZF and exit --> ZF = 0
    
    isRoundRun_chkDraw:
    mov cl,P1RemainBullets
    add cl,P2RemainBullets
    cmp cl,0
    jz isRoundRun_Draw
    jmp isRoundRun_setZF                   ;set ZF and exit --> ZF = 1
    
    isRoundRun_Draw:
    mov roundStatus,3
    jmp isRoundRun_clearZF                 ;clear ZF and exit --> ZF = 0
    
    
    isRoundRun_clearZF:
        or cl,1  ; result = 1, ZF = 0
        jmp isRoundRun_end
    
    
    isRoundRun_setZF:
        and cl,0 ; result = 0, ZF = 1
    
    
isRoundRun_end:   
pop cx
ret    
isRoundRun endp


updateScore proc
    
    ;local player1
    ;local player2
     
    cmp roundStatus,1
    jz player1
    cmp roundStatus,2
    jz player2
    
    jmp updateScore_theEnd  ;draw or still running
    
    player1:
    inc player1Score
    jmp updateScore_theEnd
    
    player2:
    inc player2Score

updateScore_theEnd:
ret    
endp



printScore proc
push ax
push dx
    
    mov ah,2
    
    movecursor 18d,20d
    mov dl,player1Score
    add dl,48d
    int 21h
    
    movecursor 38d,20d
    mov dl,player2Score
    add dl,48d   
    int 21h
    
    
    
pop dx
pop ax
ret    
endp





printRoundCount proc
    
    push ax
    push dx
    movecursor  20d,0d
    
    mov dl,roundcount
    inc dl              ; to set the range from(0:8) --> (1:9)
    ; to ascii
    add dl,48d
    mov ah,2
    int 21h


pop dx       
pop ax    
ret    
endp


printRemainBullets proc
    movecursor 1,0
    printnumber P1RemainBullets
    
    movecursor 38d,0d
    printnumber P2RemainBullets
    
    
ret    
printRemainBullets endp



printRemainHide proc
push ax
push bx
push cx    
push dx    
    mov ch,0   ; to use cx as one byte cl
    
    ;config:
    mov ah,9 ;Display
    mov bh,0 ;Page 0
    mov al,157d ;Letter '¥' --> this change in dos asckii to desired shape
    mov cl,NumOfHiddes ;display with max number of hides
    mov bl,00h ;to clear this position (black on black)
    
    ;P1:
    ;clear the drawings first
    movecursor 14d-NumOfHiddes,0
    int 10h ; print number of bullets at black
    
    ;print the char number of curren availabel hides
    mov temp1B,14d-NumOfHiddes
    add temp1B,NumOfHiddes
    mov dl,fighter1hiddes
    sub temp1B,dl ; temp1B is the starting x position
    movecursor temp1B,0
    mov bl,05h
    mov cl,fighter1hiddes   ;curren availabel hides
    int 10h
    
    
    ;P2:clear with max number then print the correct number
    mov bl,00h
    mov cl,NumOfHiddes
    movecursor 25d,0
    int 10h   ; clear the drawings
    mov bl,05h
    mov cl,fighter2hiddes
    int 10h

pop dx    
pop cx
pop bx
pop ax    
ret    
printRemainHide endp






; clear the screen and print the winner name, if it draw --> print nothing
; no input
; no output
printWinner proc    
push ax

    chngeToVedioMode   ; to clear the screen
    movecursor 8d,12d  ; set the cursor position before printing
     
    mov al,player1Score
    cmp player2Score,al
    
    ja printWinner_P2    ; if > print P2
    jl printWinner_P1    ; if < print P1
    
    ; if =
    ; draw, display here draw message in the future
    jmp printWinner_end
    
    printWinner_P1:
    displaystring player1name[2]
    jmp printWinner_end
    
    printWinner_P2:
    displaystring player2name[2]
      
printWinner_end:              
pop ax    
ret    
endp printWinner



;increase the difficlty level with one   
increaseDifficluty proc
    inc gameDifficltuy
    inc speedFactor
    cmp gameDifficltuy,10d
    jz increaseDifficluty_rveres
    ret
    increaseDifficluty_rveres:
    dec gameDifficltuy
    dec speedFactor
ret    
increaseDifficluty endp

;decrease the difficlty level with one
decreaseDifficluty proc
    dec gameDifficltuy
    dec speedFactor
    cmp gameDifficltuy,0d
    jz decreaseDifficluty_rveres
    ret
    decreaseDifficluty_rveres:
    inc gameDifficltuy
    inc speedFactor
ret    
decreaseDifficluty endp

;-------------------------------------


; simulate one game with whole process
GunFight Proc 
    call newgame  ;init new game
    
    ; game loop every loop of this mean new round
    GunFight_game:
         
        call newround   ; first initialize the new round 
        
        ; round loop              
        GunFight_round:
            call drawscreen
            call printRemainBullets
            call printRemainHide
            
            getCurrentTime GameCurrentTime[0],GameCurrentTime[1],GameCurrentTime[2]
            call getAndExeUserAction ; get user input and exute the actiom
            call checkonhidetime
            call drawBullets
            call checkOnMoveBulletTime   
            
            
        call isRoundRun
        jz GunFight_round   
        
        call updateScore       ; update the new value
        call printScore        ; print the score to the players
        
        
    inc roundCount    
    call isGameEnd   
    jnz GunFight_game    

; after game is end, print the winner name    
call printWinner
waitMicroSeconds 01Eh,8480h ; wait 2 seconds

; clear the screen and return to the main program
chngeToVedioMode      
ret 
endp GunFight  
       
       
       
       
checkOnMoveBulletTime proc 
                 
push cx 
push dx
                        
 

       mov dl,GameCurrentTime[2]  ; mill  
       mov dh,GameCurrentTime[1]   ; sec 

       cmp dh,GameOldTime[1]    ; check if they have the same  sec
       jz checkonMoveBulletTime_sameSec:
           
                  add dl,100d;    
      
       checkonMoveBulletTime_sameSec:; now in both cases we can compare the msec
       
                  sub dl,bulletSpeed;
                  
                  cmp dl,GameOldTime[2] 
                  jl   checkOnMoveBulletTime_End;; if less so fighter still in hide mode
                  
                  ; move bullets with the speed
                  MoveBulletsTime_spdFact:
                  call moveBullets
                  inc ch
                  cmp ch,speedFactor
                  jnz MoveBulletsTime_spdFact
                   
                  mov cl,GameCurrentTime[0]
                  mov GameOldTime[0],cl                  
                  mov cl,GameCurrentTime[1]
                  mov GameOldTime[1],cl
                  mov cl,GameCurrentTime[2]
                  mov GameOldTime[2],cl    
                                      

checkOnMoveBulletTime_End:
pop dx
pop cx  
  
ret 
endp checkOnMoveBulletTime


end main