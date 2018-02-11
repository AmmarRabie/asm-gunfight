
;------------- begin : drawfighter ----
; input:
; <xmin,ymin > of fighter    data type: word
; color                      data type: byte
; output <xmax,ymax> of object  (xmin+37,ymin+51)
:  
drawFighter1 macro    xmin,ymin,xmax,ymax ,color
        
        push cx
        push dx
     mov cl,color
     mov drcolor,cl

     mov cx,xmin 
     mov dx,ymin 
    
     
  mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 0d
add drxmax, 15d
add drymin, 0d
add drymax, 12d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 8d
add drxmax, 10d
add drymin, 12d
add drymax, 16d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 2d
add drxmax, 27d
add drymin, 17d
add drymax, 21d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 2d
add drxmax, 15d
add drymin, 22d
add drymax, 27d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 2d
add drxmax, 17d
add drymin, 28d
add drymax, 38d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 2d
add drxmax, 7d
add drymin, 39d
add drymax, 46d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 13d
add drxmax, 18d
add drymin, 39d
add drymax, 45d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 2d
add drxmax, 5d
add drymin, 46d
add drymax, 51d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 13d
add drxmax, 16d
add drymin, 46d
add drymax, 50d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 2d
add drxmax, 8d
add drymin, 52d
add drymax, 52d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 13d
add drxmax, 18d
add drymin, 51d
add drymax, 52d
call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 28d
add drxmax, 37d
add drymin, 15d
add drymax, 16d
;call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 28d
add drxmax, 30d
add drymin, 17d
add drymax, 18d
;call drawRect 
 mov drxmin, cx
mov drymin, dx
mov drxmax, cx
mov drymax, dx
add drxmin, 10d
add drxmax, 13d
add drymin, 7d
add drymax, 8d
mov drcolor,backgroundColor
call drawRect 
mov xmin,cx
mov ymin,dx
mov xmax,cx
mov ymax,dx

add xmax,37d
add ymax,51d
        
pop dx
pop cx
     
endm  

;------end  :drawfighter -----------  


;------------- begin : drawfighter 2 ----
; input:
; <xmax,ymin> of fighter    data type: byte
; color   data type: byte
; output <xmin,ymax> of object  (xmax-60,ymin+80)
:  
drawFighter2 macro    xmax,ymin,xmin,ymax ,color
        
        push cx
        push dx

     mov cl,color
     mov drcolor,cl
     
     mov cx,xmax ;
     mov dx,ymin ; 
      
    
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 15d
sub drxmax, 0d
add drymin, 0d
add drymax, 12d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 10d
sub drxmax, 8d
add drymin, 12d
add drymax, 16d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 27d
sub drxmax, 2d
add drymin, 17d
add drymax, 21d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 15d
sub drxmax, 2d
add drymin, 22d
add drymax, 27d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 17d
sub drxmax, 2d
add drymin, 28d
add drymax, 38d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 7d
sub drxmax, 2d
add drymin, 39d
add drymax, 46d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 18d
sub drxmax, 13d
add drymin, 39d
add drymax, 45d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 5d
sub drxmax, 2d
add drymin, 46d
add drymax, 51d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 16d
sub drxmax, 13d
add drymin, 46d
add drymax, 50d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 8d
sub drxmax, 2d
add drymin, 52d
add drymax, 52d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 18d
sub drxmax, 13d
add drymin, 51d
add drymax, 52d
call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 37d
sub drxmax, 28d
add drymin, 15d
add drymax, 16d
;call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 30d
sub drxmax, 28d
add drymin, 17d
add drymax, 18d
;call drawrect  
 mov drxmax, cx
mov drymin, dx
mov drxmin, cx
mov drymax, dx
sub drxmin, 13d
sub drxmax, 10d
add drymin, 7d
add drymax, 8d
mov drcolor,backgroundColor
call drawrect 
   

mov xmax,cx
mov ymin,dx
mov xmin,cx
mov ymax,dx

sub xmin,37d
add ymax,51d

pop dx
pop cx
   endm
;------end  :drawfighter2 -----------  



;fighter 1 normal
drawgun10 macro xpos,ypos,fightercolor,endx,endy 
pushf 
pusha
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,15d
mov bx,9d 
mov al,fightercolor 
mov si,0d
mov di,2d
local looop
looop:
drawIncreasingStraightLine_noReturn 
inc dx
dec di 
jnz loooP: 
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,17d
mov bx,3d  
mov di,2d
local looopp
looopp:
drawIncreasingStraightLine_noReturn 
inc dx
dec di 
jnz loooPp:                                     
 
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,15d

add cx,9d
;add dx,1d

mov endx,cx
mov endy,dx 

popa                                     
popf    
endm
         
         
         
  ;fighter 1 down45   
 drawgun11 macro xpos,ypos,fightercolor,endx,endy 

pushf 
pusha
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,15d
mov bx,9d 
mov al,fightercolor 
mov si,1d
mov di,2d
local looop
looop:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooP: 
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,17d
mov bx,3d  
mov di,2d
local looopp
looopp:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooPp:                                     
 

mov cx,xpos
mov dx,ypos
add cx,28d
add dx,15d

add cx,9d
add dx,9d

mov endx,cx
mov endy,dx 

popa                                     
popf    
endm        
 
 ;fighter 1 up 45
 drawgun12 macro xpos,ypos,fightercolor,endx,endy 

pushf
pusha
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,15d
mov bx,9d 
mov al,fightercolor 
mov si,0d
mov di,3d
local looop
looop:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooP: 
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,17d
mov bx,3d  
mov di,2d
local looopp
looopp:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooPp:                                     
 
mov cx,xpos
mov dx,ypos
add cx,28d
add dx,15d

add cx,9d
sub dx,9d

mov endx,cx
mov endy,dx 

popa                                     
popf    
endm     
         
         
         

 ;fighter 2 normal
drawgun20 macro fighterx,fightery,fightercolor,endx,endy 

pushf 
pusha
mov cx,fighterx
mov dx,fightery
sub cx,36d
add dx,15d
mov bx,9d 
mov al,fightercolor 
mov si,0d
mov di,2d
local looop
looop:
drawIncreasingStraightLine_noReturn 
inc dx
dec di 
jnz loooP: 
mov cx,fighterx
mov dx,fightery
sub cx,30d
add dx,17d
mov bx,3d  
mov di,2d
local looopp
looopp:
drawIncreasingStraightLine_noReturn 
inc dx
dec di 
jnz loooPp:                                     
 
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,15d

sub cx,9d
;add dx,1d

mov endx,cx
mov endy,dx 

popa                                     
popf    
endm
         
         
         
  ;fighter 2 down45   
 drawgun21 macro fighterx,fightery,fightercolor,endx,endy 

pushf 
pusha
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,15d
mov bx,9d 
mov al,fightercolor 
mov si,3d
mov di,2d
local looop
looop:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooP: 
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,17d
mov bx,3d  
mov di,2d
local looopp
looopp:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooPp:                                     
 
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,15d

sub cx,9d
add dx,9d

mov endx,cx
mov endy,dx 

popa                                     
popf    
endm        
 
 ;fighter 2 up 45
 drawgun22 macro fighterx,fightery,fightercolor,endx,endy 

pushf
pusha
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,15d
mov bx,9d 
mov al,fightercolor 
mov si,2d
mov di,4d
local looop
looop:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooP: 
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,17d
mov bx,3d  
mov di,4d
local looopp
looopp:
draw45degreeLine_noReturn 
inc dx
dec di 
jnz loooPp:                                     
 
mov cx,fighterx
mov dx,fightery
sub cx,28d
add dx,15d

sub cx,9d
sub dx,9d
mov endx,cx
mov endy,dx 


popa                                     
popf    
endm     
         
;---------------------------------------------------------

;input
; <x,y>min of fighter1  each word
; mode of the gun       byte
; color of the gun      byte
;output 
; (endx,endy) of the gun 
                                               
drawfighter1gun macro fighter1xmin,fighter1ymin,fighter1mode,fighter1color,fighter1gunx,fighter1guny
  
    local isUP
    local isDown
    local isNormal
    local end
  
  isUp: cmp fighter1mode,2*2
        jnz  isDown 
        drawgun12 fighter1xmin,fighter1ymin,fighter1color,fighter1gunx,fighter1guny
        jmp end
  isDown:cmp fighter1mode,1*2
        jnz  isNormal 
        drawgun11 fighter1xmin,fighter1ymin,fighter1color,fighter1gunx,fighter1guny
        jmp end
  ;0*2  
  isNormal:
         drawgun10 fighter1xmin,fighter1ymin,fighter1color,fighter1gunx,fighter1guny
  
  end:

endm                                

;--------------------------------------------------------------------------------------------
;input
; <xmax,yin> of fighter1  each word
; mode of the gun       byte
; color of the gun      byte
;output 
; (endx,endy) of the gun 

drawfighter2gun macro fighter2xmax,fighter2ymin,fighter2mode,fighter2color,fighter2gunx,fighter2guny
  
    local isUP
    local isDown
    local isNormal
    local end
  
  isUp: cmp fighter2mode,5
        jnz  isDown 
        drawgun22 fighter2xmax,fighter2ymin,fighter2color,fighter2gunx,fighter2guny
        jmp end 
  isDown:cmp fighter2mode,3
        jnz  isNormal 
       drawgun21 fighter2xmax,fighter2ymin,fighter2color,fighter2gunx,fighter2guny
       jmp end
  isNormal:	        ; 1
         drawgun20 fighter2xmax,fighter2ymin,fighter2color,fighter2gunx,fighter2guny
    
   end:   
endm                    