{-
    Sudoku solver again
-}
module SudokuSolver where

import qualified Data.Set as S
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)

--data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
--deriving (Eq, Ord, Show, Read, Bounded, Enum)

dimension = 4 :: Int

data SlotIndex = I1 | I2 | I3 | I4
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

data SlotPos = SlotPos  { row :: SlotIndex
                  , col :: SlotIndex  
                  } 
                    deriving (Eq, Show, Read)

data SlotVal = SlotPotential (S.Set SlotIndex)
              | SlotFinal SlotIndex
              | Empty
                  deriving (Eq, Show, Read)

data SlotBox = SlotBox SlotPos SlotVal
                  deriving (Eq, Show, Read)

type SudokuInstance = [SlotBox]
                  --deriving (Eq, Show, Read)

getSudokuRow :: SudokuInstance -> SlotIndex -> [SlotBox]
getSudokuRow boxes i = [box | box@(SlotBox (SlotPos row col) val) <- boxes, row == i]

--getCol SudokuInstance SlotIndex
getSudokuCol :: SudokuInstance -> SlotIndex -> [SlotBox]
getSudokuCol boxes j = [box | box@(SlotBox (SlotPos row col) val) <- boxes, col == j]

--getCol SudokuInstance SlotIndex
getSudokuSquare :: SudokuInstance -> SlotIndex -> [SlotBox]
getSudokuSquare boxes k = [box | box@(SlotBox pos@(SlotPos row col) val) <- boxes, getSquareIndex pos == k]

getSquareIndex :: SlotPos ->  SlotIndex
getSquareIndex (SlotPos row col) = toSlotIndex2 row col

--devideBy2 :: (Fractional a) => SlotIndex -> a
--devideBy2 :: (Fractional a) => SlotIndex -> a
--devideBy2 :: SlotIndex -> Int
devideBy2 :: (Integral a) => SlotIndex -> a
devideBy2 x = floor $ fromIntegral (fromEnum x)/2

--toSlotIndex :: (RealFrac a) => a -> SlotIndex
toSlotIndex :: Int -> SlotIndex
toSlotIndex x = toEnum( x )


toSlotIndex2 :: SlotIndex -> SlotIndex -> SlotIndex
toSlotIndex2 i j = toSlotIndex ((devideBy2 i) * 2 + (devideBy2 j))

--getSlotVal SudokuInstance SlotPos = 
allSlotIndices = [(minBound :: SlotIndex) ..]



{---------------------------------------------
    Logic to solve the problem
------------------------------------------------}
--getSet :: [SlotBox] -> S.Set SlotIndex
--getSet boxes = foldr addToSet S.empty [j | (SlotBox j v) <- boxes]

addSlotBoxesToSet :: [SlotBox] -> S.Set SlotIndex -> S.Set SlotIndex
addSlotBoxesToSet xs = addSlotValsToSet [v | (SlotBox pos v) <- xs]

addSlotValsToSet :: [SlotVal] -> S.Set SlotIndex -> S.Set SlotIndex
addSlotValsToSet xs s = foldr addToSet S.empty xs


addToSet :: SlotVal -> S.Set SlotIndex -> S.Set SlotIndex
addToSet (SlotFinal j) s = S.insert j s
addToSet _ s = s

getUsedValueSet :: [SlotBox] -> S.Set SlotIndex
getUsedValueSet xs = addSlotBoxesToSet xs S.empty

allValuesSet :: S.Set SlotIndex
allValuesSet  = addSlotValsToSet [(SlotFinal j)| j <- allSlotIndices] S.empty

getAvailableValueSet :: [SlotBox] -> S.Set SlotIndex
getAvailableValueSet xs = S.difference allValuesSet used
                        where used = (addSlotBoxesToSet xs S.empty)


getAvailableValuesRow :: SudokuInstance -> SlotIndex -> S.Set SlotIndex
getAvailableValuesRow sudoku i = getAvailableValueSet rowbx 
                              where rowbx = getSudokuRow sudoku i


getAvailableValuesCol :: SudokuInstance -> SlotIndex -> S.Set SlotIndex
getAvailableValuesCol sudoku j = getAvailableValueSet colbx 
                              where colbx = getSudokuCol sudoku j


getAvailableValuesSqr :: SudokuInstance -> SlotIndex -> S.Set SlotIndex
getAvailableValuesSqr sudoku i = getAvailableValueSet sqrbx 
                              where sqrbx = getSudokuSquare sudoku i


getAvailableValuesPos :: SudokuInstance -> SlotPos -> S.Set SlotIndex
getAvailableValuesPos sudoku pos@(SlotPos i j) = S.intersection (S.intersection rs cs) ss 
                              where rs = getAvailableValuesRow sudoku i
                                    cs = getAvailableValuesCol sudoku j
                                    ss = getAvailableValuesSqr sudoku (getSquareIndex pos)

createNextVal :: SudokuInstance -> SlotPos -> SlotVal
createNextVal sudoku pos 
                          | S.size pvs == 1 = SlotFinal (S.elemAt 0 pvs)
                          | otherwise       = SlotPotential pvs
                            where pvs = getAvailableValuesPos sudoku pos

createNextBox :: SudokuInstance -> SlotBox -> SlotBox
createNextBox sudoku box@(SlotBox _ (SlotFinal _)) = box 
createNextBox sudoku box@(SlotBox p _)             =  SlotBox p v
                                                        where v = createNextVal sudoku p

createNextSudoku :: SudokuInstance -> SudokuInstance
createNextSudoku sudoku = [createNextBox sudoku bx | bx <- sudoku]

{--
    Read sudoku from text
--}
readSudoku :: String -> SudokuInstance
readSudoku str =  concat (readMatrix xss)
                  where xss = (read str) :: [[Int]]

readMatrix2 :: [Int] -> SudokuInstance
readMatrix2 xs = concat $
                [readRow ys (toEnum i) | i <- [0..3], let ys = take 4( drop (i* 4) xs)]

readMatrix :: [[Int]] -> [[SlotBox]]
readMatrix xss = [readRow (xss !! i) (toEnum i) | i <- [0..3]]

readRow :: [Int] -> SlotIndex -> [SlotBox]
readRow xs r = [SlotBox (SlotPos r (toEnum i)) (readSlotIndex( xs !! i)) | i <- [0..3] ] ---, c <- 

readSlotIndex :: Int -> SlotVal
readSlotIndex 1 = SlotFinal I1
readSlotIndex 2 = SlotFinal I2
readSlotIndex 3 = SlotFinal I3
readSlotIndex 4 = SlotFinal I4
readSlotIndex _ = Empty

toMatrix :: SudokuInstance -> [[Int]]
toMatrix sudoku = [toIntRow (getSudokuRow sudoku (toEnum i)) | i <- [0..3]] 

toIntRow :: [SlotBox] -> [Int]
toIntRow bxs = [toIntVal v | (SlotBox p v) <- bxs]

toIntVal :: SlotVal -> Int
toIntVal (SlotFinal p) = (fromEnum p) + 1
toIntVal Empty = 0
toIntVal (SlotPotential vs) = 50000 + (foldr (\x agg -> (10*agg) + x) 0 xs)
                                where xs = [ (1 + (fromEnum v))| v <- S.toList vs] :: [Int]

--[SlotPos {row = I1, col = I1},SlotPos {row = I1, col = I2},SlotPos {row = I1, col = I3},SlotPos {row = I1, col = I4},--
--SlotPos {row = I2, col = I1},SlotPos {row = I2, col = I2},SlotPos {row = I2, col = I3},SlotPos {row = I2, col = I4},SlotPos {row = I3, col = I1},SlotPos {row = I3, col = I2},SlotPos {row = I3, col = I3},
--SlotPos {row = I3, col = I4},SlotPos {row = I4, col = I1},SlotPos {row = I4, col = I2},SlotPos {row = I4, col = I3},SlotPos {row = I4, col = I4}]
{--
createNextSudoku [] = []
createNextSudoku box@(SlotBox (SlotPos row col) (SlotFinal _)):xs = [x : box@(SlotBox (SlotPos row col) (SlotFinal SlotIndex)) ]
createNextSudoku x:xs = [x : box@(SlotBox (SlotPos row col) (SlotFinal SlotIndex)) ]
--}


--bDisplayItem :: Behavior Bool
--bDisplayItem =  --maybe False --(const True) <$> bSelection

--(evFillList, doFillList) <- newEvent
--initialList <- valuesSupply ""
--behFillList <- stepper initialList evFillList

solveMySudoku:: SudokuInstance -> SudokuInstance
solveMySudoku sudoku = if sudoku2 == sudoku then sudoku else (solveMySudoku sudoku2)
                        where sudoku2  = createNextSudoku sudoku

main = do
    putStrLn "Hello World"
    putStrLn . show $ fmap toSlotIndex [0, 1, 2 ,3]
    putStrLn . show $ allSlotIndices
    putStrLn . show $ [SlotPos i j | i <- allSlotIndices, j <- allSlotIndices]
    putStrLn . show $ fmap getSquareIndex [SlotPos i j | i <- allSlotIndices, j <- allSlotIndices]
    --inpStr <- readFile "sudoku1.txt"     putStrLn . show $ inpStr

    putStrLn . show $ SlotBox (SlotPos I1 I2) (SlotFinal I3)
    inpStr <- readFile "sudoku6.txt"
    let sudoku1 = readSudoku inpStr --(read inpStr):: SudokuInstance
    putStrLn . show $ "printing read data:"
    putStrLn . show $ toMatrix sudoku1
    putStrLn . show $ "printing (getSudokuSquare sudoku1 I2) data:"
    putStrLn . show $ (getSudokuSquare sudoku1 I2) -- getSquareIndex
    putStrLn . show $ "printing allValuesSet"
    putStrLn . show $ allValuesSet

    putStrLn . show $ "printing addSlotBoxesToSet:"
    putStrLn . show $ addSlotBoxesToSet (getSudokuSquare sudoku1 I2) S.empty
    
    putStrLn . show $ "printing get sqr index"
    putStrLn . show $ getSquareIndex (SlotPos I2 I4)

    putStrLn . show $ "printing getAvailableValuesSqr I2:"
    putStrLn . show $ getAvailableValuesSqr sudoku1 (getSquareIndex (SlotPos I2 I4))
    putStrLn . show $ getAvailableValuesRow sudoku1 I2
    putStrLn . show $ getAvailableValuesCol sudoku1 I4
    putStrLn . show $ getSudokuCol sudoku1 I4

    putStrLn . show $ "printing getAvailableValuesPos: (SlotPos I2 I4)"
    putStrLn . show $ getAvailableValuesPos sudoku1 (SlotPos I2 I4)

    putStrLn . show $ "Creating next sudoku:"

    let sudoku2  = createNextSudoku sudoku1
    --sudoku1 <- read txt::SudokuInstance
    putStrLn . show $ toMatrix sudoku2

    putStrLn . show $ "Creating next sudoku:"

    let sudoku3  = createNextSudoku sudoku2
    --sudoku1 <- read txt::SudokuInstance
    putStrLn . show $ toMatrix sudoku3

    putStrLn . show $ "solveMySudoku sudoku1:"
    let sudokuSolved = solveMySudoku sudoku1
    putStrLn . show $ toMatrix sudokuSolved
