{-# Language OverloadedStrings #-}

module Rasa.Ext.Nano where

import Rasa.Ext
import qualified Yi.Rope as Y
import Rasa.Ext.Files
import Rasa.Ext.Cursors
import Rasa.Ext.Views

nano :: Action ()
nano = onEveryTrigger_ handleKeypress

handleKeypress :: Keypress -> Action ()
handleKeypress keypress =
  focusDo_ $ bufKP keypress

bufKP :: Keypress -> BufAction ()
-- | original nano keybindings, not complete
bufKP (Keypress 'x' [Ctrl]) = liftAction exit
bufKP (KLeft []) = moveRangesByN (-1)
bufKP (KRight []) = moveRangesByN 1
bufKP (KUp []) = moveRangesByC (Coord (-1) 0)
bufKP (KDown []) = moveRangesByC (Coord 1 0)
bufKP (KBS []) = moveRangesByN (-1) >> delete
bufKP (KEnter []) = insertText "\n" >> moveRangesByC (Coord 1 0) >> startOfLine
bufKP (Keypress 'd' [Ctrl]) = delete
bufKP (Keypress 'o' [Ctrl]) = save
bufKP (Keypress 'a' [Ctrl]) = startOfLine
bufKP (Keypress 'e' [Ctrl]) = endOfLine
bufKP (Keypress 'y' [Ctrl]) = liftAction $ scrollBy 7 -- Half-Page down
bufKP (Keypress 'v' [Ctrl]) = liftAction $ scrollBy (-7) -- Half-Page down

-- | keybindings for rasa, mostly from rasa-ext-vim
bufKP (KLeft [Ctrl]) = liftAction focusViewLeft
bufKP (KRight [Ctrl]) =  liftAction focusViewRight
bufKP (KUp [Ctrl]) = liftAction focusViewAbove
bufKP (KDown [Ctrl]) = liftAction focusViewBelow

bufKP (Keypress '+' [Ctrl]) = liftAction nextBuf
bufKP (Keypress '-' [Ctrl]) = liftAction prevBuf
bufKP (Keypress 'w' [Ctrl]) = liftAction hSplit
bufKP (Keypress 's' [Ctrl]) = liftAction vSplit
bufKP (Keypress 'k' [Ctrl]) = liftAction closeInactive
bufKP (Keypress 'r' [Ctrl]) = liftAction rotate

bufKP (Keypress c _) = insertText (Y.singleton c) >> moveRangesByN 1
bufKP _ = return ()

-- | Move cursors to end of the line
endOfLine :: BufAction ()
endOfLine = findNext "\n"

-- | Move cursors to start of the line
startOfLine :: BufAction ()
startOfLine = findPrev "\n"
