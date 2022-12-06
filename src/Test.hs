{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Test where

import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import System.Exit
import System.Console.Terminal.Size

import Data.Text (unpack)
import Data.Maybe
import qualified Data.List as L
import Cursor.Types
import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Brick.Widgets.Center
import Cursor.TextField
import Cursor.Brick.TextField
import qualified Brick.Widgets.Edit as E
import Control.Lens ( (^.), (%=), (&), (.~), makeLenses, Zoom(zoom) )


-- ui :: Widget ()
-- ui =
--     joinBorders $
--     withBorderStyle unicode $
--     borderWithLabel (str "Hello!") $
--     (center (str "Left") <+> vBorder <+> center (str "Right"))







data ResourceName = String
                    | ResourceName
                    deriving (Show, Eq, Ord)


data POC 
    = File FilePath
    | Directory FilePath
    deriving (Show, Eq)



data TestState = TestState {  _testStatePaths :: !(NonEmptyCursor POC)
                            , _stateCursor :: TextFieldCursor
                            , _searchingState :: !Bool
                            , _inputString :: String
                            , _windowH :: Int
                            } 

makeLenses ''TestState



buildInitState :: IO TestState
buildInitState = do
    h <- do 
        terminal <- size
        case terminal of 
            Just a -> return (height a :: Int)
            Nothing -> return 15
    
    cur <- getCurrentDirectory
    contents <- getDirectoryContents cur
    contents' <- forM contents $ \fp -> do 
        e <- doesFileExist fp
        pure $
            if e
                then File fp
                else Directory fp
    case NE.nonEmpty contents' of 
        Nothing -> die "There are no contents"
        Just ne -> pure TestState { _testStatePaths = makeNonEmptyCursor ne
                                  , _stateCursor = makeTextFieldCursor ""
                                  , _searchingState = False
                                  , _inputString = ""
                                  , _windowH = h}
                                    
    



drawSearch :: TestState -> Bool -> Widget ResourceName
drawSearch st flg = if not flg
                        then hCenter $ padRight Max (str "Input 1 (20 words): " <+> e1)
                        else hCenter $ withAttr "selected" $ (padRight Max (str "Input 1 (20 words): " <+> e1))
    where
        e1 = selectedTextFieldCursorWidget ResourceName  (st^.stateCursor)

            
             


-- drawResult :: TestState -> Widget ResourceName
-- drawResult st = ui
--     where 
--         ui = map (st^.inputString) <+>


lastN' n xs = L.foldl' (const . drop 1) xs (drop n xs)

firstN' 0 xs = xs
firstN' _ [] = []
firstN' n (x:xs) = x: firstN' (n-1) xs

padString c n xs 
    | n <= length xs = xs
    | otherwise      = padString c n (xs ++ [c])


--ts^.windowH-1
drawTest :: TestState -> [Widget ResourceName]
drawTest ts = 
    let nec = ts^.testStatePaths
        in  [   hCenter $ border $ 
                (
                hCenter $ 
                padBottom (Pad ((ts^.windowH-2) - (fileLen nec)))(
                vBox $ concat
                    [  map (drawPath False) $ reverse $ (prevFile nec)--take 3 $ (nonEmptyCursorPrev nec)
                    , [drawPath (not flg) $ nonEmptyCursorCurrent nec]
                    , map (drawPath False) $ nonEmptyCursorNext nec]
                ))
                <+> (drawSearch ts flg)
                <+> ((hCenter (padLeftRight 7 (str "Result")))
                <=> (hCenter (padLeftRight (div (20- length (ts^.inputString)) 2) (str (ts^.inputString)))))
            ]
    where 
        fileLen nec = length (nonEmptyCursorPrev nec) + length (nonEmptyCursorNext nec) + 1
        prevFile nec = take (ts^.windowH-3) $ (nonEmptyCursorPrev nec)
        flg = ts^.searchingState



drawPath :: Bool -> POC -> Widget n
drawPath b poc = 
        (if b
            then forceAttr "selected"
            else id) $ 
        case poc of
            File fp -> padRight (Pad (30 - length fp)) $ withAttr "file" $ str fp
            Directory fp -> padRight (Pad (30 - length fp)) $ withAttr "directory" $ str fp


handleEvent :: TestState -> BrickEvent n e -> EventM n (Next TestState)
handleEvent st ev = 
    case ev of
        VtyEvent (EvKey k ms) -> handleKeyPress st ev (k,ms)
        _ -> continue st

handleKeyPress :: TestState -> BrickEvent n e -> (Key, [Modifier]) -> EventM n (Next TestState)
handleKeyPress st ev (key, ms) = 
    if st ^. searchingState
        then handleSearch st ev
        else handleNormal st ev
    
    where 
        handleNormal st ev = 
            case ev of
                VtyEvent vtye ->
                    case vtye of 
                        EvKey (KChar 'q') [] -> halt st
                        EvKey KDown [] -> do
                            let nec = st ^. testStatePaths
                            case nonEmptyCursorSelectNext nec of
                                Nothing -> continue st
                                Just nec' -> continue $ st {_testStatePaths = nec'}
                        EvKey KUp [] -> do
                            let nec = st ^. testStatePaths
                            case nonEmptyCursorSelectPrev nec of
                                Nothing -> continue st
                                Just nec' -> continue $ st {_testStatePaths = nec'}
                        EvKey KEnter [] -> do
                            let fp = nonEmptyCursorCurrent $ st ^. testStatePaths
                            case fp of 
                                File _ -> continue st
                                Directory fp -> do
                                    liftIO $ setCurrentDirectory fp
                                    s' <- liftIO buildInitState
                                    continue s'
                        EvKey KRight [] -> do
                            let st' = st & searchingState .~ True
                            continue st'
                        _ -> continue st
                _ -> continue st
        handleSearch st ev =  
            let mDo :: (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n (Next TestState)  
                mDo func = do
                    let tfc = st^.stateCursor
                    let tfc' = fromMaybe tfc $ func tfc
                    let st' = st & stateCursor .~ tfc'
                    continue st'
            in case key of 
                KLeft  -> do 
                            let st' = st & searchingState .~ False  & stateCursor .~ (makeTextFieldCursor "")
                            continue st'
                KEnter -> do
                            let st' = st & inputString .~ (unpack (rebuildTextFieldCursor (st ^. stateCursor))) & stateCursor .~ (makeTextFieldCursor "")
                            continue st'
                KBS -> mDo $ dullMDelete . textFieldCursorRemove
                (KChar c) -> mDo $ textFieldCursorInsertChar c . Just
                _      -> continue st


-- handleTestEvent :: TestState -> BrickEvent n e -> EventM n (Next TestState)
-- handleTestEvent s e =
--     case e of
--         VtyEvent vtye ->
--             case vtye of 
--                 EvKey (KChar 'q') [] -> halt s
--                 EvKey KDown [] -> do
--                     let nec = testStatePaths s
--                     case nonEmptyCursorSelectNext nec of
--                         Nothing -> continue s
--                         Just nec' -> continue $ s {_testStatePaths = nec'}
--                 EvKey KUp [] -> do
--                     let nec = testStatePaths s
--                     case nonEmptyCursorSelectPrev nec of
--                         Nothing -> continue s
--                         Just nec' -> continue $ s {_testStatePaths = nec'}
--                 EvKey KEnter [] -> do
--                     let fp = nonEmptyCursorCurrent $ testStatePaths s
--                     case fp of 
--                         File _ -> continue s
--                         Directory fp -> do
--                             liftIO $ setCurrentDirectory fp
--                             s' <- liftIO buildInitState
--                             continue s'
--                 -- EvKey KRight [] -> do
--                 --      focusRing %= F.focusPrev
--                 _ -> continue s
--         _ -> continue s

mainApp :: App TestState e ResourceName
mainApp = 
    App
        {   appDraw = drawTest
        ,   appChooseCursor = showFirstCursor
        ,   appHandleEvent = handleEvent
        ,   appStartEvent = pure
        ,   appAttrMap = 
                const $ 
                attrMap 
                    defAttr 
                    [ ("selected", fg red)
                    , ("file", fg blue)
                    , ("directory", fg yellow)
                    ]
        }

test :: IO()
test = do 
    initState <- buildInitState
    endState <- defaultMain mainApp initState
    return ()
    --print endState


