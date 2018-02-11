obstScale equ 2
scaleOberator equ *



draw_obstacle MACRO obstX,obstY,obstacle_color
    
     
    pusha
    pushf
    
        ; initial point, the most bottom left point
    mov cx, obstX
    mov dx, obstY
    mov al, obstacle_color
     
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; the base
    mov si,0d
    mov bx,6d scaleOberator obstScale
    drawIncreasingStraightLine_noReturn
    
    ; move in x 10 points to render a vertical line
    add cx,1d scaleOberator obstScale
    ; change direction to vertical
    mov si,1d
    ; increase the lenght with 20 of the base
    mov bx,8d scaleOberator obstScale			; increase with the tolt (6/3)
    drawIncreasingStraightLine_noReturn
    ; the snd vertical line
    add cx,4d scaleOberator obstScale
    drawIncreasingStraightLine_noReturn
    
    sub dx,7d scaleOberator obstScale
    mov bx,2d scaleOberator obstScale			;2*1(1 is the zalloma of base)
    
    mov si,0d
    call draw45degreeLine
    
    add bx,2d scaleOberator obstScale
    mov si,2d
    call draw45degreeLine
    
    mov si,3d
    call draw45degreeLine
    
    sub bx,2d scaleOberator obstScale
    mov si,1d 
    call draw45degreeLine

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    popf
    popa
endm draw_obstacle     



;-------------------------------------------------------------------------
; draw the boeder of the mab and obstacles
;input player1name , player2name  as array of bytes  (includeing the max size and actual size as the first 2 bytes)

drawmap macro player1name,player2name,color

mov drxmin,0d
mov drxmax,320d
mov drymin,fightmapymax
mov drymax,fightmapymax
mov drcolor,color

call drawrect

mov cx,fightmapymax
add cx,9d
mov drxmin,0d
mov drxmax,320d
mov drymin,cx
mov drymax,cx
mov drcolor,color

call drawrect

mov drxmin,0d
mov drxmax,320d
mov drymin,fightmapymin
mov drymax,fightmapymin
mov drcolor,color

call drawrect

mov drxmin,fighter1part
mov drxmax,fighter1part
mov drymin,fightmapymin
mov drymax,fightmapymax
mov drcolor,color

call drawrect

mov drxmin,fighter2part
mov drxmax,fighter2part
mov drymin,fightmapymin
mov drymax,fightmapymax
mov drcolor,color

call drawrect

mov drxmin,159d
mov drxmax,159d
mov drymin,160d
mov drymax,200d
mov drcolor,color

call drawrect


movecursor 0d,20d
displaystring player1name+2
movecursor 20d,20d
displaystring player2name+2



endm




