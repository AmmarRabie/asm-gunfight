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


ingamechat db 0


;---------------------- ; parameters to macros and procedures

drXmin dw 0d    ; parameters used in drawrect procedure
drXmax dw 60d
drYmin dw 0d
drYmax dw 10d
drColor db 14d

temp1B db ?  

byte   db ?

;----------------------; Golbal variables for the game
roundStatus DW 0 ; {0:inPlay, 1:player 1 win, 2:player2 win}

roundNum equ 9   ; the number of rounds per game

roundCount db 0  ; the current round players play (0:8)


;-----------------------------------------------------------------------------------------hosting and client
PlayerRole DB 3 ; 0:host, 1:client
Player2CurrReauest DB 0 ; 0:no request, 1:request to play, 2:request to chat == 1:accept,2:refuse,3"requestPlay,4:requesChat
acceptPlay equ 1d  ; these equs is used directly outed from guest and received from host to know that the other is accept 
acceptChat equ 7d
refuse equ 2d
requestPlay equ 3d ; these variable moved to currRequest 
requestChat equ 4d
;------------------------------------------------------------------------------------------------------
     
; 80*25 chars
cursor1 Db 1,2
cursor2 Db 1,14d    
     
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
fighter1moves  db numofmoves
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
fighter2moves  db numofmoves
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
fightmapymax  equ  150d
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
wrongname db "name must begin by character",'$'
statusbar db "-- press escape to exit","$"  
level2msg db "level",'$'
sendGameinvationMs db "you send a game invitation",'$'
sendchatinvationMs db "you send a chat invitation",'$'
recGameinvationMs db "game invitation, f2 to accept$"
recChatinvationMs db "chat invitation, f1 to accept",'$'




gameDifficltuy db 1+3      ; init with default value to be seen first in the screen(increase this value to speedFactor)
BulletMode db 0     ;0:translation      ;1:bounding

level2     db 0       
level2sc   equ 42h ; f8 
numofmoves equ 5d

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
     
     call initPort   
     call getusername   ; get users name in different screens
     
MainProgram:
 
  ; draw the main screen to get a choice
  mainscreen meschoice1,meschoice2,meschoice3,mesChoiceDiff,gameDifficltuy,mesChoiceMode,BulletMode,level2,level2msg

  call ifRequestToPlay ; check if the other player request to play (the proc will handle every thing if yes)

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
     
     cmp ah,level2sc
     jz setlevel2
     
     ; it is not a key of our action keys, so ignore it and ret    
       jmp MainProgram
     
      
      
     IsF1:
           ; send a new request to chat, or accept if there is invitation
           cmp Player2CurrReauest,requestchat
           jz IsF1_confirmRequest
           jmp IsF1_invitePlay
           
           IsF1_invitePlay:
                      mov al,requestchat
                      IsF1_main_loopTillEmpty:
                      call isTransmitterRegHoldData
                      jz IsF1_main_loopTillEmpty 
                      call outValue
                      mov playerrole,0 ; request
                      movecursor 2,20d 
                      displaystring sendchatinvationMs
                      jmp MainProgram
                                                                                                     ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
           IsF1_confirmRequest:
                      mov al,acceptchat
                      IsF1_main_loopTillEmpty2:
                      call isTransmitterRegHoldData
                      jz IsF1_main_loopTillEmpty2   
                      call outValue
                      mov playerRole,1  ; reciever
                 
                      call chat
                                
             jmp MainProgram  
           
     IsF2:
           ; send a new request to play, or accept if there is invitation
           cmp Player2CurrReauest,requestPlay
           jz IsF2_confirmRequest
           jmp IsF2_invitePlay
           
           IsF2_invitePlay:
                      mov al,requestPlay
                      main_loopTillEmpty:
                      call isTransmitterRegHoldData
                      jz main_loopTillEmpty 
                      call outValue
                      mov playerrole,0 ; request
                      movecursor 2,20d
                      displaystring sendgameinvationMs
                      jmp MainProgram
                      
           IsF2_confirmRequest:
                      mov al,acceptplay
                      IsF2_main_loopTillEmpty2:
                      call isTransmitterRegHoldData
                      jz IsF2_main_loopTillEmpty2    
                      call outValue
                      mov playerRole,1  ; reciever
                 
                      call gunfight          
             jmp MainProgram
          
     
     IsIncDiff:      
           call increaseDifficluty           
            jmp MainProgram
           
     IsDecDiff:      
           call decreaseDifficluty                      
            jmp MainProgram
           
           
           
     IsChngBulletMode:                                 
           not BulletMode ; for now
            jmp MainProgram 
           
     setlevel2:
           xor level2,00000001b
            jmp MainProgram 
                      
                
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
          
        mov cl,level2
          
        cmp  cl,1
             jz   moveplayer1_level2
             jmp  moveplayer1_level1
          
        moveplayer1_level2:
         
               cmp fighter1moves,0
               jz  player1moved
               dec fighter1moves
             
               
       
        moveplayer1_level1:
        
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
             
        ; first check if we in level 2   (player have #number of moves)
        mov cl,level2
        
        cmp  cl,1
        jz   moveplayer2_level2
        jmp  moveplayer2_level1
          
        moveplayer2_level2:
               cmp fighter2moves,0
               jz  player2moved
               dec fighter2moves
               
               
       
        moveplayer2_level1:
        
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
    
    mov al,ah
    call outValue
    
    
    call ExecuteToPlayerRole
    
        
getAndExeUserAction_theEnd:
;    mov ah,0
;    int 16h
ret
endp

;--------------------------------------------------------------------
getusername proc           ; to get user names in different screens


getUserNameScreen mesenter,mesPress,player1name,wrongname

mov ah,0
mov al,13h  ; changing the video mode is faster than clearing the screen pixel pixel
int 10h

;getUserNameScreen mesenter,mesPress,player2name,wrongname

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
mov fighter1Ymin, 90d
mov fighter1Ymax, 300d      
mov fighter1mode, 0d
mov fighter1newxmin, 0d
mov fighter1newymin, 0d
mov fighter1color,14d
mov fighter1IsNowHidden,0d

; two
mov fighter2Xmin ,0d
mov fighter2Xmax ,309d
mov fighter2Ymin ,90d
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
   ; first set the game conditions (recieve it if guest, send it if host)
   cmp playerrole,0
   jz newGame_sendConditions1
   
   
   
   ; recieve conditions
   newGame_recConditions1:
   call isDataNotReady
   jz newGame_recConditions1
   call invalue
   mov gameDifficltuy,al
   
   newGame_recConditions2:
   call isDataNotReady
   jz newGame_recConditions2
   call invalue
   mov speedFactor,al
   
   newGame_recConditions3:
   call isDataNotReady
   jz newGame_recConditions3
   call invalue
   mov BulletMode,al
   
   newGame_recConditions4:
   call isDataNotReady
   jz newGame_recConditions4
   call invalue
   mov level2,al       
   
   jmp newGame_cont
   
   
   
   newGame_sendConditions1:
   call isTransmitterRegHoldData
   jz newGame_sendConditions1
   mov al,gameDifficltuy
   call outvalue
   
   newGame_sendConditions2:
   call isTransmitterRegHoldData
   jz newGame_sendConditions2
   mov al,speedFactor
   call outvalue

   newGame_sendConditions3:
   call isTransmitterRegHoldData
   jz newGame_sendConditions3
   mov al,BulletMode
   call outvalue
   
   newGame_sendConditions4:
   call isTransmitterRegHoldData
   jz newGame_sendConditions4
   mov al,level2
   call outvalue
   
   
         
   newGame_cont:
               
   mov player1Score, 0d;
   mov player2Score, 0d;
; two

   mov roundCount, 0 
   
   mov fighter2hiddes,numOfhiddes 
   mov fighter1hiddes,numOfhiddes   
   
   mov fighter1moves,numofmoves
   mov fighter2moves,numofmoves
   
   
   ret            
endp


drawscreen proc
    drawmap player1name,player2name,bordercolor,statusbar 

    draw_obstacle  160d-3*obstscale,50d,obstColor       ; 84d-60d
    draw_obstacle  160d-3*obstscale,90d,obstColor       ; 84d-60d        
    draw_obstacle  160d-3*obstscale,140d,obstColor      ;84d-60d+20d
    
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
    
    movecursor 18d,19d
    mov dl,player1Score
    add dl,48d
    int 21h
    
    movecursor 38d,19d
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
    
    pusha 
    
    
    movecursor 1,0
    printnumber P1RemainBullets
    
    movecursor 38d,0d
    printnumber P2RemainBullets
    
    mov al,fighter1moves 
    mov ah,0d
    mov bl,5
    mov byte,bl
     
    call printByte  
    
    mov al,fighter2moves 
    mov ah,0d
    mov bl,32
    mov byte,bl
     
    call printByte  
    
   
   popa 
    
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
     
    cmp playerrole,0
    jnz p1_role1
               displaystring player1name[2]
               jmp printWinner_end
     p1_role1:
               displaystring player2name[2]
               jmp printWinner_end
                   
                   
    printWinner_P2:
    cmp playerrole,0
    jnz p2_role1
          displaystring player2name[2]
          jmp printWinner_end
      p2_role1:
          displaystring player1name[2] 
                   
                   
                   
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
    call get_recieve_name
    
    ; game loop every loop of this mean new round
    GunFight_game:
         
        call newround   ; first initialize the new round 
        
        ; round loop              
        GunFight_round:
            call drawscreen
            call printRemainBullets
            call printRemainHide
            
            getCurrentTime GameCurrentTime[0],GameCurrentTime[1],GameCurrentTime[2]
            
            call getAndExeUserAction ; get user input and exute the action
            call recieveDataExecutable ; get other user input and execute it
            
            call checkonhidetime
            call drawBullets
            call checkOnMoveBulletTime  
        
        call isMovesEnd
        jz  Gunfight_end    
            
        call isRoundRun
        jz GunFight_round
        ;jmp GunFight_round   
        
        call updateScore       ; update the new value
        call printScore        ; print the score to the players
        
        
    inc roundCount    
    call isGameEnd   
    jnz GunFight_game  
      
Gunfight_end:
; clear the screen and return to the main program 
call printWinner
waitMicroSeconds 01Eh,8480h ; wait 2 seconds  
call inimain
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


















 
 
 
 
 
 
 







;######################################################################################################### 
ifRequestToPlay proc
push ax    
   call isDataNotReady 
   jz ifRequestToPlay_theend
   
   call inValue 
   
   mov Player2CurrReauest,al

;   cmp al,3d
;   jz  player1_sendReq
;   jmp ifRequestToPlay_theEnd
;   player1_sendReq:
;   mov playerrole,1
;   jmp ifRequestToPlay_theEnd:

   cmp al,acceptPlay
   jz callGame
   cmp al,acceptChat
   jz callChat
   jmp ifRequestToPlay_theEnd
   
   
   callGame:
   call gunfight
   jmp ifRequestToPlay_theEnd
   
   
   callChat:
   call chat   
   jmp ifRequestToPlay_theEnd   
 
   
;   ; set ZF
;   mov ah,0
;   cmp ah,0
;   pop ax
;   ret
    
ifRequestToPlay_theEnd:
;; clear the ZF
;mov ah,1
;cmp ah,1

pop ax    
ret   
ifRequestToPlay endp     
 
 
 
 
 
 
 
 
 
 
 
 
 

initPort proc   
push ax
push dx
    ;Set Divisor Latch Access Bit
        mov dx,3fbh 			; Line Control Register
        mov al,10000000b		;Set Divisor Latch Access Bit
        out dx,al			;Out it 
        
        
    ;Set LSB byte of the Baud Rate Divisor Latch register.
        mov dx,3f8h			
        mov al,0ch			
        out dx,al
        
        
    ;Set MSB byte of the Baud Rate Divisor Latch register.
        mov dx,3f9h
        mov al,00h
        out dx,al
        
    ;Set port configuration
        mov dx,3fbh
        mov al,00011011b
            ;0:Access to Receiver buffer, Transmitter buffer
            ;0:Set Break disabled
            ;011:Even Parity
            ;0:One Stop Bit
            ;11:8bits
    out dx,al
    
pop dx
pop ax
ret 
initPort endp    




isTransmitterRegHoldData proc
push ax
push dx    
    mov dx, 3FDH		; Line Status Register
    
    In al, dx 			;Read Line Status
  	AND al, 00100000b
pop dx
pop ax
ret 	    
isTransmitterRegHoldData endp    



outValue proc ; take from al
push dx
    mov dx , 3F8H		; Transmit data register
    out dx , al
pop dx
ret     
outValue endp



;Check that Data Ready
isDataNotReady proc near
push dx
push ax
    mov dx , 3FDH		; Line Status Register
    in al , dx 
    AND al , 1
    ;jz again
pop ax
pop dx
ret
isDataNotReady endp






inValue proc  ; ret in al
push dx
    mov dx , 03F8H
    in al , dx 
pop dx
ret     
inValue endp






recieveDataExecutable proc
push ax
     call isDataNotReady
     jz recieveDataExecutable_theEnd
     
    call inValue
    mov ah,al
    
    ; negate the playerRole with formula playerRole = 1 - PlayerRole
    mov al,1
    sub al,playerRole
    mov playerRole,al
     
    call ExecuteToPlayerRole
      
    ; return back the playerRole with formula playerRole = 1 - PlayerRole
    mov al,1
    sub al,playerRole
    mov playerRole,al
recieveDataExecutable_theEnd:    
pop ax   
ret    
recieveDataExecutable endp    





























ExecuteToPlayerRole proc
     
         
    cmp ah,upsc
    jz ExecuteToPlayerRole_IsUp
    
    cmp ah,downsc
    jz ExecuteToPlayerRole_IsDown
    
    cmp ah,rightsc
    jz ExecuteToPlayerRole_IsRight
    
    cmp ah,leftsc
    jz ExecuteToPlayerRole_isLeft
    
    cmp ah,fighter1NormalGunSc
    jz ExecuteToPlayerRole_isfighter1mod0
    
    cmp ah,fighter1DownGunSc
    jz ExecuteToPlayerRole_isfighter1mod1
    
    cmp ah,fighter1UpGunSc
    jz ExecuteToPlayerRole_isfighter1mod2
    

    cmp ah,spcSc
    jz ExecuteToPlayerRole_isF1Fire   
    
    cmp ah,fighter1hidesc
    jz ExecuteToPlayerRole_ishidefighter1
    
    
    cmp ah,ESCSc
    jz ExecuteToPlayerRole_isEsc
     ;the cliciked button is not one of our action buttons, end
    jmp recieveDataExecutable_theEnd
    
    
      ExecuteToPlayerRole_isEsc:
      chngeToVedioMode   
      call inimain
      jmp  MainProgram
    
    
    
    ExecuteToPlayerRole_IsUp:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_IsW
        sub fighter1newymin,5d
        call movplayer1    
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_IsDown:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_IsS    
        add fighter1newymin,5d
        call movplayer1
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_IsRight:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_IsD    
        add fighter1newxmin,5d
        call movplayer1
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isLeft:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_isA    
        sub fighter1newxmin,5d
        call movplayer1
    jmp ExecuteToPlayerRole_theEnd
    
    
    ExecuteToPlayerRole_isfighter1mod0:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_isfighter2mod0    
        mov  fighter1mode,0
        call changefighter1gun
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isfighter1mod1:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_isfighter2mod1    
        mov fighter1mode,2
        call changefighter1gun
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isfighter1mod2:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_isfighter2mod2    
        mov fighter1mode,4
        call changefighter1gun
    jmp ExecuteToPlayerRole_theEnd
 
    ExecuteToPlayerRole_isF1Fire:    
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_isF2Fire
        ; dont fire if his maxNumBullets bullets still on the screen
        cmp P1bulletsFired,maxNumBullets
        jz ExecuteToPlayerRole_theEnd
        ; dont fire if he dont have remain bullets
        cmp P1RemainBullets,0
        jz ExecuteToPlayerRole_theEnd
        ; dont fire if he is hide
        and fighter1IsNowHidden,1
        jnz ExecuteToPlayerRole_theEnd
        
        ; execute the fire actino
        fire P1bulletsFired[2],fighter1gunx,fighter1guny,fighter1mode
        inc P1bulletsFired
        dec P1RemainBullets
    jmp ExecuteToPlayerRole_theEnd
     

   ExecuteToPlayerRole_IsHidefighter1:
        cmp PlayerRole,1
        jz  ExecuteToPlayerRole_IsHidefighter2   
        call HideFighter1
        call movplayer1
   jmp ExecuteToPlayerRole_theEnd 
   
   
   
      
    ExecuteToPlayerRole_IsW:
        sub fighter2newymin,5d
        call movplayer2
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_IsS:
        add fighter2newymin,5d
        call movplayer2
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_IsD:
        add fighter2newxmax,5d
        call movplayer2
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isA:
        sub fighter2newxmax,5d
        call movplayer2
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isfighter2mod0:
        mov fighter2mode,1
        call changefighter2gun
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isfighter2mod1:
        mov fighter2mode,3
        call changefighter2gun
    jmp ExecuteToPlayerRole_theEnd
    
    ExecuteToPlayerRole_isfighter2mod2:
        mov fighter2mode,5
        call changefighter2gun
    jmp ExecuteToPlayerRole_theEnd
    
    
      
    ExecuteToPlayerRole_isF2Fire:
        cmp P2bulletsFired,3d
        jz ExecuteToPlayerRole_theEnd
        cmp P2RemainBullets,0
        jz ExecuteToPlayerRole_theEnd
        and fighter2IsNowHidden,1
        jnz ExecuteToPlayerRole_theEnd
                
        ; execute the fire actino
        fire P2bulletsFired[2],fighter2gunx,fighter2guny,fighter2mode
        inc P2bulletsFired
        dec P2RemainBullets
   jmp ExecuteToPlayerRole_theEnd 
   
   
    ExecuteToPlayerRole_IsHideFighter2:
        call HideFighter2
        call movplayer2
   jmp ExecuteToPlayerRole_theEnd
   
   
   
   
    
ExecuteToPlayerRole_theEnd:       
ret     
ExecuteToPlayerRole endp    








;//////////////////////////
 get_recieve_name proc  
    pusha
 
 
 ;cmp playerRole ,0
 ;jnz get_recieve_name_player2     
 	;sending and reciveing one by one
    mov cl,player1name[1] 
    mov byte,cl
    send byte  

	recive byte  ;first recive name length
    mov ch,byte 
	 
    mov bx,offset player1name+2
	mov si,offset player2Name+2
    ExchangeNames:	
		cmp cl,00h
		jz no_more_send
			mov al,[bx]
			mov byte,al
			send byte
			inc bx
			dec cl
		no_more_send:
		
		cmp ch,00h
		jz no_more_recive
			recive byte
			mov al,byte
			mov [si],al
			inc si
			dec ch
		no_more_recive:
		cmp cx,0000h
    jnz ExchangeNames
   jmp get_recieve_name_end
                            
                            
    ;cmp playerRole ,0
;    jz get_recieve_name_end 
;                          
;    mov cx,0d 
;    mov si ,offset player1name[2]
;    mov bx ,offset player2name[2]
;    get_recieve_name_lop:
;    
;    add si,cx
;    add bx,cx
;    
;    mov al,[si]    
;    mov ah,[bx]
;    
;    mov [si],ah
;    mov [bx],al
;              
;    inc cx
;              
;    cmp cx,16d
;    jnz get_recieve_name_lop
; 
 
 get_recieve_name_end:  
                        
popa                             
ret   
endp 











;serial macros
send macro byte ; sending with wating
	local again
	push ax
	push dx
	again:
		mov dx,3fdh
		in al,dx
		and al,00100000b
	jz again
	mov al,byte
	mov dx,3f8h
	out dx,al
	pop dx
	pop ax
endm send 
;-----------------

recive macro byte
	local again
	push ax
	push dx
	again:
		mov dx,3fdh
		in al,dx
		and al,00000001b
	jz again
	mov dx,3f8h
	in al,dx
	mov byte,al
	pop dx
	pop ax
endm send 





ismovesEnd proc
    
    push cx 
      
     mov cl,fighter1moves
     add cl,fighter2moves
     
     cmp cl,0
           
pop cx           
ret    
endp isGameEnd


   
   
   
printByte proc    ; input the number in ax onle 3digit number; cursor position in byte  
    
   
   pusha
   
   mov bl,10d
   
  ;; mov al,010d
  ;; mov ah,0d
  
   
   mov cx,3
   
   printByte_div:
   
   div bl
   
   push ax
   
   mov ah,0
   
   loop printByte_div
   
   mov cx,3
   
   mov bl,byte 
   
   printByte_print:
   
    
    movecursor bl,0d
   
      pop ax
      
      mov bh,ah
     
    printnumber bh 
    
    inc bl
    
   loop printByte_print
    
  popa
  
 ret    
    
endp 



      
      
inimain proc 
    
  mov  gameDifficltuy , 1+3      ; init with default value to be seen first in the screen(increase this value to speedFactor)
  mov speedFactor, 4d+3
  mov BulletMode , 0     ;0:translation      ;1:bounding
  mov level2,0       

   
  mov PlayerRole,3  ; 0:host, 1:client
  mov Player2CurrReauest , 0 ; 0:no request, 1:request to play, 2:request to chat == 1:accept,2:refuse,3"requestPlay,4:requesChat


ret    
endp    

   
   
   
;exhgGameSettings proc
;    
;    
;cmp playerrole,0  
;jz  exhgGameSettings_1
;jmp exhgGameSettings_2   
;
;
;ret    
;endp    
;





;************************************* chat **************************************************************
chat proc
pusha  
     
     mov temp1B,0
     mov ah,0
     mov al,3h
     int 10h 
     
     call get_recieve_name
     ;call initport
     movecursor 0,12d
     call drawLineTextMode
     
     movecursor 0,23d
     call drawLineTextMode
     
     movecursor 0,24d
     displaystring statusbar     
     
     movecursor 1,1
     displaystring player1name[2]
     
     
     movecursor 1,13d
     displaystring player2name[2]
     

     chatLoop:
        call updatescroll
        
        call isNotPressed
        jnz printAndSend
        jmp chkrecieve
        
        printAndSend:
           call getchar
           call sendchar
           call printchar1
           
           
        chkrecieve:
        call isDataNotReady
        jnz recAndPrint
        cmp temp1B,ESCSc
        jnz chatLoop    ;jnz
        recAndPrint:   
            call invalue
            cmp al,ESCsc
            jz chat_end 
            call printchar2
        
     
     cmp temp1B,ESCSc
     jnz chatLoop    ;jnz
     
chat_end:
chngeToVedioMode
call inimain    
popa    
ret
chat endp






isNotPressed proc
push ax    
    mov ah,1
    int 16h
pop ax
ret    
isNotPressed endp    


    
    
    

; from al
sendchar proc
push ax        
    sendchar_check:
    call isTransmitterRegHoldData
    jz sendchar_check
    call outValue
    
pop ax    
ret
sendchar endp



    


getchar proc
        
    mov ah,0
    int 16h
    
    cmp ah,ESCSc
    jnz getchar_end
    mov temp1B,ESCSc
    mov al,ESCSc
    
getchar_end:    
ret           
getchar endp    



    

; from  al
printchar1 proc
push ax
push dx
push bx
   
   
   movecursor cursor1[0],cursor1[1] 
   mov ah,2
   mov dl,al
   int 21h
   
   ; get cursor position
   mov ah,3h
   mov bh,0h
   int 10h
   
   mov cursor1[0],dl
   mov cursor1[1],dh

pop bx 
pop dx
pop ax    
ret    
printchar1 endp    




; from  al
printchar2 proc
push ax
push dx
push bx
   
   movecursor cursor2[0],cursor2[1] 
   mov ah,2
   mov dl,al
   int 21h
   
   mov ah,3h
   mov bh,0h
   int 10h
   
   mov cursor2[0],dl
   mov cursor2[1],dh

pop bx 
pop dx
pop ax    
ret    
printchar2 endp




updatescroll proc
pusha
    mov al,cursor1[0]
    mov ah,cursor1[1]    
    cmp al,79d
    jnz updatescroll_end1
    cmp ah,11d      ;11d
    jnz updatescroll_end1
    
    ; update scrolling
    
    
    mov ah,06
    mov al,01
    mov bh,07
    ;mov dx,184FH
    mov cl,2
    mov ch,2d
    
    ; D, max x, max y
    mov dl,79d
    mov dh,11d
    int 10h
    
    mov cursor1[0],0
    
updatescroll_end1:
    mov al,cursor2[0]
    mov ah,cursor2[1]    
    cmp al,79d
    jnz updatescroll_end
    cmp ah,22d   ;25d
    jnz updatescroll_end
    
    ; update scrolling 2
    mov ah,06
    mov al,01
    mov bh,07
    ;mov dx,184FH
    mov cl,2
    mov ch,14d
    
    ; D, max x, max y
    mov dl,79d
    mov dh,22d
    int 10h
    
    mov cursor2[0],0
    
updatescroll_end:    
popa    
ret    
updatescroll endp    




drawLineTextMode proc

pushf
push ax
push bx
push cx


mov ah,9 ;Display
mov bh,0 ;Page 0
mov al,'_' ;Letter _


mov cx,79d ;5 times
mov bl,0Ah ;Green (A) on white(F) background
int 10h




pop cx
pop bx
pop ax
popf
ret
drawLineTextMode endp

;************************************************************ chat ***************************************
end main