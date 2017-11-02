data Circuit = Component Float | Serial Circuit Circuit | Parallel Circuit Circuit

resistance :: Circuit -> Float

resistance (Component r)    = r
resistance (Serial c1 c2)   = resistance c1 + resistance c2
resistance (Parallel c1 c2) = 1 / (1/resistance c1 + 1/resistance c2)

