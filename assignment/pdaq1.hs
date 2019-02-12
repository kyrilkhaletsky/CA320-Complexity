type PDA = (Int,[Int],[Transition])
type Transition = ((Int,String,String),(Int,String))
type Configuration = (Int,String,String)
data Result = Accept | Reject deriving (Show,Eq)

type Stack = String
type Input = String
 
run :: PDA -> String -> Result
run pda input = helper startState pda trans "$" input
                where 
                    trans = getTran pda startState
                    startState = getStart pda

getStart :: PDA -> Int
getStart (s,_,_) = s 

getAccept :: PDA -> [Int]
getAccept (_,list,_) = list

doPop :: String -> Stack -> Stack
doPop [] stack = stack
doPop c (s:stack) | c == [s] = stack
                  | otherwise = []

doPush :: String -> Stack -> Stack
doPush _ [] = []
doPush [] stack = stack
doPush c stack = c ++ stack

getTran :: PDA -> Int ->[Transition]
getTran (_,_,[]) _ = []
getTran (_,_,((state,char,poped),(newState,pushed)):trans) curr 
                    | state == curr = ((state,char,poped),(newState,pushed)) : getTran (0,[0],trans) curr
                    | otherwise = getTran (0,[0],trans) curr


helper :: Int -> PDA -> [Transition] -> Stack -> Input -> Result
helper curr pda [] (s:[]) [] 
                                    | elem curr (getAccept pda)= Accept
                                    | otherwise = Reject

helper curr pda (((_,char,poped),(newState,pushed)):trans) (s:[]) [] 
                                    | char == "" && ((helper newState pda (getTran pda newState) (doPush pushed (doPop poped (s:[]))) []) == Accept ) = Accept
                                    | otherwise = helper curr pda trans (s:[]) []

helper curr pda _ _ [] = Reject
helper _ _ [] _ _ = Reject
helper _ _ _ [] _ = Reject
helper curr pda (((_,char,poped),(newState,pushed)):trans) stack (i:input)
                    | ((char == "" ) && ((helper newState pda (getTran pda newState) (doPush pushed (doPop poped stack)) (i:input)) == Accept )) = Accept
                    | ((char == [i]) && ((helper newState pda (getTran pda newState) (doPush pushed (doPop poped stack))   input  ) == Accept )) = Accept
                    | otherwise = helper curr pda trans stack (i:input) 
