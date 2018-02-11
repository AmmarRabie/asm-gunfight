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
; 
getUserNameScreen  macro msgdisplayed1,msgdisplayed2,name


movecursor 8d,11d
displaystring msgdisplayed1
movecursor 8d,15d
displaystring msgdisplayed2
movecursor 8d,13d
readstring name

;clear the screen
chngeToVedioMode

endm

;---------------------------------------------------------------
mainScreen macro choice1,choice2,choice3,choiceDiff,diffNum,mesChoiceMode,BulletMode

movecursor 8d,7d
displaystring choice1
movecursor 8d,9d
displaystring choice2
movecursor 8d,11d
displaystring choice3

;display the difficlty mes
movecursor 2d,15d
displaystring choiceDiff
;display the difficlty number
movecursor 2d,16d
printnumber diffNum

;display the Bullet mode mesage
movecursor 2d,18d
displaystring mesChoiceMode
;display the mode string depending on mode --> in the future
movecursor 2d,19d
printnumber BulletMode

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