triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = if((a+b >c) && (a+c >b) && (c+b >a)) == True
   then let s = (a+b+c)/2 in sqrt(s*(s-a)*(s-b)*(s-c))
   else error "Not a triangle!"
