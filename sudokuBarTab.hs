{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import SudokuSolver hiding (row, main)


-- | Main entry point.
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    -- active elements
    return w # set title "BarTab"

    elSudoku <- UI.button # set UI.text "Sudoku!"
    elResult2 <- UI.span

    inputs2   <- liftIO $ newIORef []
    
    -- functionality
    let
        displayTotal2 = void $ do
            xs <- mapM (get value) =<< liftIO (readIORef inputs2)
            element elResult2 # set text (showNumber . sum $ map readNumber xs)
        
        redoLayout :: UI ()
        redoLayout = void $ do
            layout2 <- mkGridLayout =<< liftIO (readIORef inputs2)
            getBody w # set children [layout2, elResult2, elSudoku]

        solveSudoku :: UI ()
        solveSudoku = void $ do
            xs <- mapM (get value) =<< liftIO (readIORef inputs2)
            let ys = map readInteger xs
            let sudoku1 = getSudokuInstance ys
            let sudokuSolved = solveMySudoku sudoku1
            displaySudokuInstance sudokuSolved

        displaySudokuInstance :: SudokuInstance -> UI()
        displaySudokuInstance sudoku = void $ do
            sudokuLayout <- mkSudokuLayout sudoku
            getBody w #+ [ element sudokuLayout, UI.hr ]
            --getBody w # set children [layout, layout2, elResult2, ]
            --redoLayout

        mkSudokuLayout :: SudokuInstance -> UI Element
        mkSudokuLayout sudoku = mkSudokuLayoutFromMatrix (toMatrix sudoku)

        mkSudokuLayoutFromMatrix :: [[Int]] -> UI Element
        mkSudokuLayoutFromMatrix xss = column $
                [row (map displaySudokuSlot xs) | xs <- xss]


        displaySudokuSlot :: Int -> UI Element
        displaySudokuSlot x = row [
                                    UI.span # set text " [ ", 
                                    UI.span # set text (show x), 
                                    UI.span # set text " ] " # set style [("width", "18px")]
                                    ]

        mkGridLayout :: [Element] -> UI Element
        mkGridLayout xs = column $
            [
                mkRowLayout xs1,
                mkRowLayout xs2,
                mkRowLayout xs3,
                mkRowLayout xs4
            ]

            where   xs1 = (take 4 xs)
                    xs2 = take 4 (drop 4 xs)
                    xs3 = take 4 (drop 8 xs)
                    xs4 = take 4 (drop 12 xs)

        mkRowLayout :: [Element] -> UI Element
        mkRowLayout xs = row $
            [] ++ (map element xs)
        
        addInput16 :: UI()
        addInput16 = do
            addInput2
            addInput2
            addInput2
            addInput2

            addInput2
            addInput2
            addInput2
            addInput2

            addInput2
            addInput2
            addInput2
            addInput2

            addInput2
            addInput2
            addInput2
            addInput2

            --[addInput | i <- [1..16]]

        addInput2 :: UI ()
        addInput2 = do
            elInput <- UI.input # set value "0"
            on (domEvent "livechange") elInput $ \_ -> displayTotal2
            liftIO $ modifyIORef inputs2 (elInput:)
    
    on UI.click elSudoku $ \_ -> solveSudoku
    
    addInput16
    redoLayout

{-----------------------------------------------------------------------------
    Functionality
------------------------------------------------------------------------------}
type Number = Maybe Double

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber :: String -> Number
readNumber s = listToMaybe [x | (x,"") <- reads s]    
showNumber   = maybe "--" show

readInteger :: String -> Int
readInteger s = read s :: Int

getSudokuInstance :: [Int] -> SudokuInstance
getSudokuInstance xs = readMatrix2 xs