data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Enum,Show)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
             deriving (Enum,Read)

type Date = (Int,Month,Int)


leap :: Int -> Bool
leap y
   | y `mod` 100 == 0 = y `mod` 400 == 0
   | y `mod` 4 == 0   = True
   | otherwise        = False
   
mLengths :: Int -> [Int]
mLengths y = [31,feb,31,30,31,30,31,31,30,31,30,31]
             where
             feb = if leap y then 29 else 28
             
numDays :: Date -> Int
numDays (day,month,year)
      = day                                                             -- days this month
      + sum (take (fromEnum month) (mLengths year))                     -- days this year
      + (year-1753) * 365 + length [yr | yr <- [1753..year-1], leap yr] -- days since adoption of Gregorian calendar
      
dayOfWeek :: Date -> Day
dayOfWeek d = toEnum (((numDays d)-1) `mod` 7)
