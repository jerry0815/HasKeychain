{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Test where

import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
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
import Brick.Forms
import qualified Data.Text as T


-- ui :: Widget ()
-- ui =
--     joinBorders $
--     withBorderStyle unicode $
--     borderWithLabel (str "Hello!") $
--     (center (str "Left") <+> vBorder <+> center (str "Right"))





data ResourceName = String
                    | ResourceName
                    | NameField
                    | AccountField
                    | PasswordField
                    deriving (Show, Eq, Ord)
type PassWord = [Char]
-- type PassData = (String, String, PassWord)

data PassData =
    PassData { _name      :: T.Text
             , _account   :: T.Text
             , _password  :: T.Text
             }
makeLenses ''PassData

data POC
    = File FilePath
    | Directory FilePath
    deriving (Show, Eq)



data TestState = TestState {  _testStatePaths :: !(NonEmptyCursor POC)
                            , _stateCursor :: TextFieldCursor
                            , _searchingState :: !Int
                            , _inputString :: String
                            , _windowH :: Int
                            , _typingName :: T.Text
                            , _typingAccount :: T.Text
                            , _allPassData :: [PassData]
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
                                  , _searchingState = 2 -- 0: not search 1: search 2: typename 2: typeaccount
                                  , _inputString = ""
                                  , _windowH = h
                                  , _typingName = ""
                                  , _typingAccount = ""
                                  , _allPassData = []}





drawSearch :: TestState -> Int -> Widget ResourceName
drawSearch st flg = if flg == 0
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

drawPassData :: [PassData] -> Widget ResourceName
drawPassData  [] = emptyWidget
drawPassData  (item:items) =  border (padRight Max ((str "Name:    " <+> txt curname) <=> (str "Account: " <+> txt curaccount))) <=> drawPassData items
    where
        curname = item^.name
        curaccount = item^.account

drawFocusPassData :: PassData -> Widget ResourceName
drawFocusPassData item = padRight (Pad 5) (padBottom (Pad 1) ((str "Name: " <+> txt website)) <=> padBottom (Pad 1) ((str "Account: " <+> txt username)) <=> (str "Password: " <+> txt pass))
    where
        website = item^.name
        username = item^.account
        pass = item^.password

drawManual :: Int -> Widget ResourceName
drawManual 0 = str "search"
drawManual 1 = str "password"
drawManual _ = str "other"



drawAdd :: Form PassData e ResourceName -> [Widget ResourceName]
drawAdd f = []
    where
        form = border $ padTop (Pad 1) $ hLimit 50 $ renderForm f
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Age must be an integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Handedness selects from a list of options\n" <>
                     "- The last option is a checkbox\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"


drawTypingName :: TestState -> Widget ResourceName
drawTypingName st = vCenter $ padLeft (Pad 28) content <=> hCenter help
    where
        e1 = selectedTextFieldCursorWidget ResourceName  (st^.stateCursor)
        content =  str "This password is for: " <+> e1
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Age must be an integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Handedness selects from a list of options\n" <>
                     "- The last option is a checkbox\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"

drawTypingAccount :: TestState -> Widget ResourceName
drawTypingAccount st = vCenter $ padLeft (Pad 28) content <=> hCenter help
    where
        e1 = selectedTextFieldCursorWidget ResourceName  (st^.stateCursor)
        content = str "Account: " <+> e1
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "- Name is free-form text\n" <>
                     "- Age must be an integer (try entering an\n" <>
                     "  invalid age!)\n" <>
                     "- Handedness selects from a list of options\n" <>
                     "- The last option is a checkbox\n" <>
                     "- Enter/Esc quit, mouse interacts with fields"
drawTest :: TestState -> [Widget ResourceName]
drawTest ts = 
    case ts^.searchingState of
        0 -> [box]
        1 -> [box]
        2 -> [drawTypingName ts]
        3 -> [drawTypingAccount ts]
        -- 4 -> [receivePassword]

    where
        nec = ts^.testStatePaths
        fileLen nec = length (nonEmptyCursorPrev nec) + length (nonEmptyCursorNext nec) + 1
        prevFile nec = take (ts^.windowH-3) $ (nonEmptyCursorPrev nec)
        flg = ts^.searchingState
        sample = [ PassData { _name = "google"
                                  , _account = "123"
                                  , _password = "123"},
                   PassData { _name = "apple"
                                  , _account = "345"
                                  , _password = "345"}]
        ori = hCenter $ border $
                (
                hCenter $
                padBottom (Pad ((ts^.windowH-2) - (fileLen nec)))(
                vBox $ concat
                    [  map (drawPath 0) $ reverse $ (prevFile nec)--take 3 $ (nonEmptyCursorPrev nec)
                    , [drawPath (1) $ nonEmptyCursorCurrent nec]
                    , map (drawPath 0) $ nonEmptyCursorNext nec]
                ))
                <+> (drawSearch ts flg)
                <+> ((hCenter (padLeftRight 7 (str "Result")))
                <=> (hCenter (padLeftRight (div (20- length (ts^.inputString)) 2) (str (ts^.inputString)))))

        box = borderWithLabel (str "KeyChain")
         $ border (drawSearch ts flg) <=> drawPassData (ts^.allPassData)
         <+> vBorder
         <+> ((center (drawFocusPassData (head (ts^.allPassData))))  <=> hBorder <=> (center  (drawManual 0)))



drawPath :: Int -> POC -> Widget n
drawPath b poc =
        (if b == 1
            then forceAttr "selected"
            else id) $
        case poc of
            File fp -> padRight (Pad (30 - length fp)) $ withAttr "file" $ str fp
            Directory fp -> padRight (Pad (30 - length fp)) $ withAttr "directory" $ str fp


handleEvent :: TestState -> BrickEvent ResourceName e -> EventM ResourceName (Next TestState)
handleEvent st ev =
    case st ^. searchingState of
        0 -> case ev of
            VtyEvent (EvKey k ms) -> handleKeyPress st ev (k,ms)
            _ -> continue st
        1 -> case ev of
            VtyEvent (EvKey k ms) -> handleKeyPress st ev (k,ms)
            _ -> continue st
        2 -> case ev of
            VtyEvent (EvKey k ms) -> case k of
                KEsc -> do
                    let st' = st & stateCursor .~ (makeTextFieldCursor "") & searchingState .~ 0
                    continue st'
                KBS -> do
                    let tfc = st^.stateCursor
                    let tfc' = fromMaybe tfc $ (dullMDelete . textFieldCursorRemove) tfc
                    let st' = st & stateCursor .~ tfc'
                    continue st' 
                KEnter -> do
                    let st' = st & typingName .~ ( rebuildTextFieldCursor (st ^. stateCursor)) & stateCursor .~ (makeTextFieldCursor "") & searchingState .~ 3
                    continue st'
                (KChar c) -> do
                    let tfc = st^.stateCursor
                    let tfc' = fromMaybe tfc $ (textFieldCursorInsertChar c . Just) tfc
                    let st' = st & stateCursor .~ tfc'
                    continue st'
                _      -> continue st
            _ -> continue st
        3 -> case ev of
            VtyEvent (EvKey k ms) -> case k of
                KEsc -> do
                    let st' = st & stateCursor .~ (makeTextFieldCursor "") & searchingState .~ 0
                    continue st'
                KBS -> do
                    let tfc = st^.stateCursor
                    let tfc' = fromMaybe tfc $ (dullMDelete . textFieldCursorRemove) tfc
                    let st' = st & stateCursor .~ tfc'
                    continue st' 
                KEnter -> do
                    let st' = st & typingAccount .~ ( rebuildTextFieldCursor (st ^. stateCursor)) & stateCursor .~ (makeTextFieldCursor "") & searchingState .~ 0
                    let curAllPassData = st^.allPassData
                    let newData = PassData { _name = st'^.typingName
                                  , _account = st'^.typingAccount
                                  , _password = "TODO"}
                    let newAllPassData = curAllPassData ++ [newData]
                    let st'' = st' & allPassData .~ newAllPassData
                    continue st''
                (KChar c) -> do
                    let tfc = st^.stateCursor
                    let tfc' = fromMaybe tfc $ (textFieldCursorInsertChar c . Just) tfc
                    let st' = st & stateCursor .~ tfc'
                    continue st'
                _      -> continue st
            _ -> continue st
        _ -> continue st
        


handleKeyPress :: TestState -> BrickEvent n e -> (Key, [Modifier]) -> EventM n (Next TestState)
handleKeyPress st ev (key, ms) =
    if st ^. searchingState == 1
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
                            let st' = st & searchingState .~ 1
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
                            let st' = st & searchingState .~ 0  & stateCursor .~ (makeTextFieldCursor "")
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


