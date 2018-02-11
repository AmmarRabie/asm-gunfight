drawIncreasingStraightLine_noReturn MACRO
    
    push cx
    push dx
    call drawIncreasingStraightLine
    pop dx
    pop cx
endm drawIncreasingStraightLine_noReturn



draw45degreeLine_noReturn MACRO
    
    push cx
    push dx
    call draw45degreeLine
    pop dx
    pop cx
endm draw45degreeLine_noReturn   
