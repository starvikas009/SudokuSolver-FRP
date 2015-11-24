{-# LANGUAGE LambdaCase #-}
import Graphics.UI.Threepenny as UI
import Data.Maybe
import Text.Printf
import Safe          (readMay)
import Control.Monad

--createIntRow :: [Behavior String] -> [Int]
createIntRow xs = [(read s :: Int) | s <- xs]

createIntRow4 :: Int -> Int -> Int -> Int -> [Int]
createIntRow4 w x y z = [w, x, y, z]

concatFourRows :: [Int] -> [Int] -> [Int] -> [Int] -> [[Int]]
concatFourRows w x y z = [w, x, y, z]

createSudokuInstance :: [[Int]] -> [[Int]]
createSudokuInstance xxs = xxs


readNumber :: String -> Int
readNumber s = (read s :: Int)

--liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
--liftA3 f a b c = fmap f a <*> b <*> c

liftA4 :: Applicative f => (a -> b -> c -> d -> r) -> f a -> f b -> f c -> f d -> f r
liftA4 f a b c d = fmap f a <*> b <*> c <*> d 
--liftA4 :: (Monad m) => (a -> b -> c) -> (m a -> m b -> m c)
-- \a b c d -> do { a' <- a; b' <- b; c' <- c; d' <- d; return (f a' b' c' d') }

createBIn :: Element -> UI (Behavior String)
createBIn uiIn = stepper "0" $ UI.valueChange (uiIn)

main :: IO ()
main = do
  (evFillList, doFillList) <- newEvent
  initialList <- valuesSupply ""
  behFillList <- stepper initialList evFillList

  -- Initial setup
  -- start GUI with defaultConfig for UI element -- 'win'
  -- do these things: two UI elements we are putting ul and listBox
  -- -- list <- ul :: list
  -- -- sel <- listBox
  startGUI defaultConfig $ \win -> do
    list <- ul
    sel <- listBox
        behFillList
        (pure Nothing)
        (pure $ \it -> UI.span # set text it)

    ---tstInput <- input

    dollar <- UI.input :: UI Element
    euro   <- UI.input :: UI Element

    euroIn   <- createBIn  euro --stepper "0" $ UI.valueChange (euro)
    let
        rate = 0.7 :: Double
        withString f = maybe "-" (printf "%.2f") . fmap f . readMay
    
        dollarOut = withString (/ rate) <$> euroIn

    status <- UI.span
    
    element dollar # sink value dollarOut

    --tstInput2 <- input
    --element tstInput2 #
    -- setup one more grid?
    uiIn00 <- UI.input
    bIn00 <- stepper "0" $ UI.valueChange (uiIn00)
    let bOut00 = readNumber <$> bIn00

    uiIn01 <- UI.input
    bIn01 <- stepper "0" $ UI.valueChange (uiIn01)
    let bOut01 = readNumber <$> bIn01

    uiIn02 <- UI.input
    bIn02 <- stepper "0" $ UI.valueChange (uiIn02)
    let bOut02 = readNumber <$> bIn02

    uiIn03 <- UI.input
    bIn03 <- stepper "0" $ UI.valueChange (uiIn03)
    let bOut03 = readNumber <$> bIn03

    --let b4row0 = liftA (\x -> x+1) bOut03

    --let bRow0 = liftA4 (\w x y z-> [w, x, y, z] ) bOut00 bOut01 bOut02 bOut03
    let bRow0 = liftA4 (createIntRow4) bOut00 bOut01 bOut02 bOut03

    let uiIn1s = [UI.input, UI.input, UI.input, UI.input]

    -- bIn1s :: [UI (m0 (Behavior [Char]))]
    let bIn1s = [liftA createBIn uiIn | uiIn <- uiIn1s] :: [UI(UI (Behavior String))] 
      --[createBIn uiIn | uiIn <- uiIn1s]
      --fmap (\uiIn -> (stepper "0" $ UI.valueChange (uiIn)))  uiIn1s!!0 uiIn1s!!1 uiIn1s!!2 uiIn1s!!3 
    -- [(stepper "0" $ UI.valueChange (uiIn))| uiIn <- uiIn1s]

    --let bInRow0 = [bIn00, bIn01, bIn02, bIn03]
    --let bRow0 = [ readNumber <$> bin | bin <- bInRow0]
    --let bRow0Lifted = (sequence [bOut03])
    --let sudoku1 = createSudokuInstance ([ readNumber <$> bin | bin <- bRow0])

    txt <- string "Result?"
    let sudokuGrid1 = createSudokuGrid
    sudokuGrid <- grid sudokuGrid1
    --euroIn   <- stepper "0" $ UI.valueChange (element ((sudokuGrid1 1!!0) !!0))

    -- button
    solveBtn <- button
    element solveBtn # set text "Sudoku!"

    -- On Click
    on UI.click solveBtn $ \_ -> do
          element solveBtn # set text (" [pressed]")
          cells <- (getElementsByClassName win "table-cell")
          strs <- getValuesList cells
          let concatStrs = foldl ( ++ ) "" strs
          --mat <- getIntMat (getElementsByClassName win "table-cell")
          let rslt = "my result: " ++ concatStrs--(mat!!0)
          element txt # set text (rslt)
          element list #+ [li # set html rslt]
    --getBody win #+ [createSudokuGrid]

    getBody win #+ [column [element sudokuGrid, element solveBtn, element txt, element list, element euro, element dollar]] --[column [createSudokuGrid, element solveBtn]]

    setFocus $ getElement sudokuGrid


    {-

    getBody win #+ [grid [[element list, element sel]]] -- putting list and sel in a grid then grid added to root element win
    setFocus $ getElement sel

    on selectionChange (getElement sel) $ \case
      Nothing -> return ()
      Just ix -> do -- ix is the index of selection
        items <- currentValue behFillList
        let it = items !! ix -- 'it' is the selected item

        -- get list of items for selected item "it" :: String 
        -- valuesSupply2 it :: (IO [String]) 
        -- then liftIO will get you list of string :: [String]
        liftIO $ valuesSupply2 it >>= doFillList
        element list #+ [li # set html it]
        setFocus $ getElement sel

    -}

getIntMat (xs) = fmap getValuesList xs

createSudokuGrid = [
                            createRow, 
                            createRow, 
                            createRow, 
                            createRow
                  ]

createRow :: [UI Element]
createRow = [input, input, input, input]

valuesSupply :: String -> IO [String]
valuesSupply x = return [x ++ show i | i <- [0..9]]

valuesSupply2 :: String -> IO [String]
valuesSupply2 str = return [str ++ "/" ++ show i | i <- [0..9]]
