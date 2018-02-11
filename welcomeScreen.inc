;--------------------------------------
readstring macro indata

mov ah,0AH
mov dx,offset InDATA
int 21h

endm
;------------------------


displaystring macro mes

mov ah, 9h
mov dx, offset mes
int 21h

endm

;---------------------------
; input xposition,yposition   datatybe byte

movecursor macro xposition,yposition
push ax
push dx
mov ah,2
mov dl,xposition
mov dh,yposition
int 10h
pop dx
pop ax
endm

;---------------------------------------
getUserNameScreen  macro msgdisplayed1,msgdisplayed2,name,wrongname

push ax
local checkonletters
local checkonupcase
local error
local begin
local end
begin:
movecursor 8d,11d
displaystring msgdisplayed1
movecursor 8d,15d
displaystring msgdisplayed2
movecursor 8d,13d
readstring name

mov ah,name[2]

cmp ah,'a'
jge checkonupcase
jmp checkonletters

checkonupcase:
sub ah,32d

checkonletters:
mov al,'A'
cmp al,ah
jg error

mov al,'Z'
cmp al,ah
jl error


jmp end

error:
chngeToVedioMode
movecursor 8d,17d
displaystring wrongname
jmp begin

end:
;clear the screen
chngeToVedioMode

pop ax
endm

;---------------------------------------------------------------


;---------------------------------------------------------------
mainScreen macro choice1,choice2,choice3,choiceDiff,diffNum,mesChoiceMode,BulletMode,level2,level2msg

movecursor 8d,2d
displaystring choice1
movecursor 8d,4d
displaystring choice2
movecursor 8d,6d
displaystring choice3

;display the difficlty mes
movecursor 2d,8d
displaystring choiceDiff
;display the difficlty number
movecursor 2d,9d
printnumber diffNum

;display the Bullet mode mesage
movecursor 2d,12d
displaystring mesChoiceMode
;display the mode string depending on mode --> in the future
movecursor 2d,13d
printnumber BulletMode


movecursor 2d,15d
displaystring level2msg
;display the mode string depending on mode --> in the future
movecursor 2d,16d
printnumber level2


local endlabel
local printPlay
local printChat

cmp Player2CurrReauest,requestPlay
jz printPlay
cmp Player2CurrReauest,requestChat
jz printChat
jmp endlabel



printPlay:
 movecursor 2d,19d
 displaystring recGameinvationMs

jmp endlabel


printChat:
 movecursor 2d,19d
 displaystring recChatinvationMs

jmp endlabel

endlabel:

endm
;-----------------------------------------
printnumber macro num
                  ;byte

mov ah,2
mov dl,num
add dl,'0'
int 21h

endm

;--------------------------------------------------------------

getMinSec macro  min,sec
push ax
push cx
push dx

mov ah,2ch
int 21h

mov min,cl
mov sec,dh

pop dx
pop cx
pop ax
endm

;----------------------------------------------------------------


;--------------------------------------------------------------

getSecMsec macro  sec,Msec
push ax
push cx
push dx

mov ah,2ch
int 21h

mov Msec,dl
mov sec,dh

pop dx
pop cx
pop ax
endm

;----------------------------------------------------------------


;--------------------------------------------------------------

getCurrentTime macro  min,sec,msec
push ax
push cx
push dx

mov ah,2ch
int 21h

mov min,cl
mov Msec,dl
mov sec,dh

pop dx
pop cx
pop ax
endm

;----------------------------------------------------------------



;--------------------------------------------------------------

chngeToVedioMode macro
push ax

mov ah,0
mov al,13h
int 10h

pop ax
endm chngeToVedioMode 

;----------------------------------------------------------------