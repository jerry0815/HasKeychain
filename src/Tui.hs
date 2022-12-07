{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Tui where

import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
    ( makeNonEmptyCursor,
      nonEmptyCursorSelectNext,
      nonEmptyCursorSelectPrev,
      nonEmptyCursorPrev,
      nonEmptyCursorNext,
      nonEmptyCursorCurrent,
      NonEmptyCursor )
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import System.Exit
import System.Console.Terminal.Size

import Data.Text (unpack)
import Data.Maybe
import Data.Tuple.Select
import qualified Data.List as L
import Cursor.Types
import Brick.AttrMap ( attrMap )
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
import qualified Password as P
import qualified Data.Text as T







data ResourceName = String
                    | ResourceName
                    deriving (Show, Eq, Ord)




data PassData =
    PassData { _name      :: T.Text
             , _account   :: T.Text
             , _password  :: T.Text
             }

makeLenses ''PassData



data TuiState = TuiState {  _tuiStatePaths :: !(NonEmptyCursor PassData) -- cursor for each password
                            , _stateCursor :: !TextFieldCursor -- cursor for searching bar
                            , _eventState :: !Int -- 0 normal 1 search 2 insertName 3 insertAccount 4 browse
                            , _focusItem :: !(Maybe PassData) -- the password item user browsing
                            , _inputString :: String -- string searching bar input
                            , _typingName :: T.Text -- website name of new insert record
                            , _typingAccount :: T.Text -- account of new insert record
                            , _windowH :: !Int -- the height of terminal
                            } 

makeLenses ''TuiState



buildInitState :: IO TuiState
buildInitState = do
    h <- do 
        terminal <- size
        case terminal of 
            Just a -> return (height a :: Int)
            Nothing -> return 15
    contents <- P.searchPassWord ""
    contents' <- forM contents $ \fp -> pure PassData { _name = T.pack (sel1 fp), _account = T.pack (sel2 fp), _password = T.pack (sel3 fp)}
    case NE.nonEmpty contents' of 
        Nothing -> die "There are no contents"
        Just ne -> pure TuiState { _tuiStatePaths = makeNonEmptyCursor ne
                                  , _stateCursor = makeTextFieldCursor ""
                                  , _eventState = 0
                                  , _focusItem = Nothing
                                  , _inputString = ""
                                  , _windowH = h
                                  ,_typingName = ""
                                  , _typingAccount = ""
                                  }
                                    
    

-- drawing help cmd
drawHelpCmd :: TuiState -> Widget ResourceName
drawHelpCmd st = 
    let cmdState = st ^. eventState
        in case cmdState of 
            0   -> hBorder <=> center (vBox [
                    hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "Esc: quit program"
                    , hCenter $ txt "i: create password"
                    , hCenter $ txt "r: reset"
                    , hCenter $ txt "/: search, Esc: cancel search"
                    , hCenter $ txt "Enter: browse password of website"
                    ])
            1 -> hBorder <=> center (vBox [
                    hCenter $ txt "Enter: search password"
                    , hCenter $ txt "Esc: quit searching"])
            4 -> hBorder <=> center (vBox [
                    hCenter $ txt "Esc: quit viewing"
                    , hCenter $ txt "Del: delete password"
                    ])
            _ -> hBorder <=> center (vBox [
                    hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "Esc: quit program"
                    , hCenter $ txt "i: create password"
                    , hCenter $ txt "/: search, Esc: cancel search"
                    , hCenter $ txt "Enter: browse password of website"
                    ])
            
                    
-- drawing the searching bar
drawSearch :: TuiState -> Bool -> Widget ResourceName
drawSearch st flg = if not flg
                        then hLimit 50 $ border (padRight Max (str "Search: " <+> e1))
                        else hLimit 50 $ border  (padRight Max (withAttr "selected" $ ((str "Search: ") <+> e1)))
    where
        e1 = str (padString ' ' 30 input)
        input = unpack (rebuildTextFieldCursor (st ^. stateCursor))

            
-- drawing insert name page
drawTypingName :: TuiState -> Widget ResourceName
drawTypingName st = vCenter $ padLeft (Pad 28) content <=> hCenter help
    where
        e1 = selectedTextFieldCursorWidget ResourceName  (st^.stateCursor)
        content =  str "This password is for: " <+> e1
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "- Name cannot be empty                         \n" <>
                     "- Enter to proceed\n" <>
                     "- Esc to quit\n"

-- drawing insert account page
drawTypingAccount :: TuiState -> Widget ResourceName
drawTypingAccount st = vCenter $ padLeft (Pad 28) content <=> hCenter help
    where
        e1 = selectedTextFieldCursorWidget ResourceName  (st^.stateCursor)
        content = str "Account: " <+> e1
        help = padTop (Pad 1) $ borderWithLabel (str "Help") body
        body = str $ "- Name cannot be empty                         \n" <>
                     "- Enter to proceed\n" <>
                     "- Esc to quit\n"

-- drawing focus password user browse
drawFocusPassData :: T.Text -> T.Text -> T.Text  -> Widget ResourceName
drawFocusPassData website username pass =   padLeft (Pad 12) (padBottom (Pad 1) ((str "Name: " <+> txt website)) 
                                            <=> padBottom (Pad 1) ((str "Account: " <+> txt username)) 
                                            <=> (str "Password: " <+> txt pass))






padString c n xs 
    | n <= length xs = xs
    | otherwise      = padString c n (xs ++ [c])


-- drawing overall ui
drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = drawResult
        where 
            drawResult = case ts^.eventState of
                            2 -> [drawTypingName ts] 
                            3 -> [drawTypingAccount ts]
                            _ -> case ts^.focusItem of
                                    Just item   -> [borderWithLabel (str "KeyChain") $ box <+> ((vCenter (drawFocusPassData (item^.name) (item^.account) (item^.password))) <=> (center  (drawHelpCmd ts)))]
                                    Nothing     -> [borderWithLabel (str "KeyChain") $ box <+> ((vCenter (drawFocusPassData "" "" "")) <=> (center  (drawHelpCmd ts)))]
            nec = ts^.tuiStatePaths
            box = border (drawSearch ts (ts^.eventState == 1)) <=> pathData nec
                <+> vBorder
            fileLen nec = (length (nonEmptyCursorPrev nec) + length (nonEmptyCursorNext nec) + 1)*4
            prevFile nec = take (div (ts^.windowH-9) 4) $ (nonEmptyCursorPrev nec)
            pathData nec    = ( vBox $ concat
                            [  map (drawPath False) $ reverse $ (prevFile nec)
                            , [drawPath (ts^.eventState == 0) $ nonEmptyCursorCurrent nec]
                            , map (drawPath False) $ nonEmptyCursorNext nec])


        

-- drawing password list
drawPath :: Bool -> PassData -> Widget n
drawPath b poc = 
        (if b
            then forceAttr "selected"
            else id) $
        border (padRight Max ((str "Name:    " <+> txt curname) <=> (str "Account: "  <+> txt curaccount)))
        
        where 
            curname = poc^.name
            curaccount = poc^.account




handleEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleEvent st ev = 
    case ev of
        VtyEvent (EvKey k ms) -> handleKeyPress st ev (k,ms)
        _ -> continue st

-- handle each key press
handleKeyPress :: TuiState -> BrickEvent n e -> (Key, [Modifier]) -> EventM n (Next TuiState)
handleKeyPress st ev (key, ms) = resultEvent
    where 
        resultEvent = case st ^. eventState of
                            0 -> handleNormal st ev -- normal looking state
                            1 -> handleSearch st ev -- searching state
                            2 -> handleInsertName st ev -- inserting name state
                            3 -> handleInsertAccount st ev -- inserting account state
                            4 -> handleBrowse st ev -- browsing certain password state
                            _ -> continue st
        handleNormal st ev = 
            case ev of
                VtyEvent vtye ->
                    case vtye of 
                        EvKey KEsc  [] -> halt st
                        EvKey KDown [] -> do
                            let nec = st ^. tuiStatePaths
                            case nonEmptyCursorSelectNext nec of
                                Nothing -> continue st
                                Just nec' -> continue $ st {_tuiStatePaths = nec'}
                        EvKey KUp [] -> do
                            let nec = st ^. tuiStatePaths
                            case nonEmptyCursorSelectPrev nec of
                                Nothing -> continue st
                                Just nec' -> continue $ st {_tuiStatePaths = nec'}
                        EvKey KEnter [] -> do
                            let fp = nonEmptyCursorCurrent $ st ^. tuiStatePaths
                            let st' = st & focusItem .~ (Just fp) & eventState .~ 4
                            continue st'
                        EvKey (KChar '/') [] -> do
                            let st' = st & eventState .~ 1
                            continue st'
                        EvKey (KChar 'i') [] -> do
                            let st' = st & eventState .~ 2
                            continue st'
                        EvKey (KChar 'r') [] -> do 
                            st' <- liftIO buildInitState
                            continue st'
                        
                        _ -> continue st
                _ -> continue st
        handleSearch st ev =  
            let mDo :: (TextFieldCursor -> Maybe TextFieldCursor) -> EventM n (Next TuiState)  
                mDo func = do
                    let tfc = st^.stateCursor
                    let tfc' = fromMaybe tfc $ func tfc
                    let st' = st & stateCursor .~ tfc'
                    continue st'
            in case key of 
                KEsc   -> do 
                            let st' = st & eventState .~ 0  & stateCursor .~ (makeTextFieldCursor "")
                            continue st'
                KEnter -> do
                            let st' = st & inputString .~ (unpack (rebuildTextFieldCursor (st ^. stateCursor))) & stateCursor .~ (makeTextFieldCursor "") & eventState .~ 0
                            contents <- liftIO $ P.searchPassWord (st'^. inputString)
                            contents' <- forM contents $ \fp -> pure PassData { _name = T.pack (sel1 fp), _account = T.pack (sel2 fp), _password = T.pack (sel3 fp)}
                            case NE.nonEmpty contents' of
                                Nothing -> continue st'
                                Just ne -> continue (st' & tuiStatePaths .~ (makeNonEmptyCursor ne))
                                                     
                KBS -> mDo $ dullMDelete . textFieldCursorRemove
                (KChar c) -> mDo $ textFieldCursorInsertChar c . Just
                _      -> continue st
        handleBrowse st ev = 
                case ev of
                    VtyEvent vtye ->
                        case vtye of 
                            EvKey KEsc [] -> do 
                                    let st' = st & focusItem .~ Nothing & eventState .~ 0
                                    continue st'
                            EvKey KDel [] -> do 
                                let fp = nonEmptyCursorCurrent $ st ^. tuiStatePaths
                                let st' = st & focusItem .~ Nothing & eventState .~ 0
                                liftIO $ P.deletePassWord (unpack (fp^.name)) (unpack (fp^.account))
                                contents <- liftIO $ P.searchPassWord (st'^. inputString)
                                contents' <- forM contents $ \fp -> pure PassData { _name = T.pack (sel1 fp), _account = T.pack (sel2 fp), _password = T.pack (sel3 fp)}
                                case NE.nonEmpty contents' of
                                    Nothing -> continue st'
                                    Just ne -> continue (st' & tuiStatePaths .~ (makeNonEmptyCursor ne))
                            _ -> continue st
                    _ -> continue st

 
        handleInsertName st ev =
                case ev of
                    VtyEvent (EvKey k ms) -> case k of
                        KEsc -> do
                            let st' = st & stateCursor .~ (makeTextFieldCursor "") & eventState .~ 0
                            continue st'
                        KBS -> do
                            let tfc = st^.stateCursor
                            let tfc' = fromMaybe tfc $ (dullMDelete . textFieldCursorRemove) tfc
                            let st' = st & stateCursor .~ tfc'
                            continue st' 
                        KEnter -> do
                            case unpack (rebuildTextFieldCursor (st ^. stateCursor)) of
                                "" -> continue st
                                _ ->  do
                                    let st' = st & typingName .~ (rebuildTextFieldCursor (st ^. stateCursor)) & stateCursor .~ (makeTextFieldCursor "") & eventState .~ 3
                                    continue st'
                        (KChar c) -> do
                            let tfc = st^.stateCursor
                            let tfc' = fromMaybe tfc $ (textFieldCursorInsertChar c . Just) tfc
                            let st' = st & stateCursor .~ tfc'
                            continue st'
                        _  -> continue st
                    _ -> continue st
        handleInsertAccount st ev =
                case ev of
                    VtyEvent (EvKey k ms) -> case k of
                        KEsc -> do
                            let st' = st & stateCursor .~ (makeTextFieldCursor "") & eventState .~ 0
                            continue st'
                        KBS -> do
                            let tfc = st^.stateCursor
                            let tfc' = fromMaybe tfc $ (dullMDelete . textFieldCursorRemove) tfc
                            let st' = st & stateCursor .~ tfc'
                            continue st' 
                        KEnter -> do
                            case unpack (rebuildTextFieldCursor (st ^. stateCursor)) of
                                "" -> continue st
                                _ ->  do
                                    let st' = st & typingAccount .~ (rebuildTextFieldCursor (st ^. stateCursor)) & stateCursor .~ (makeTextFieldCursor "") & eventState .~ 0
                                    liftIO $ P.storeLocal (unpack (st'^.typingName)) (unpack (st'^.typingAccount))
                                    contents <- liftIO $ P.searchPassWord (st'^. inputString)
                                    contents' <- forM contents $ \fp -> pure PassData { _name = T.pack (sel1 fp), _account = T.pack (sel2 fp), _password = T.pack (sel3 fp)}
                                    case NE.nonEmpty contents' of
                                        Nothing -> continue st'
                                        Just ne -> continue (st' & tuiStatePaths .~ (makeNonEmptyCursor ne))
                        (KChar c) -> do
                            let tfc = st^.stateCursor
                            let tfc' = fromMaybe tfc $ (textFieldCursorInsertChar c . Just) tfc
                            let st' = st & stateCursor .~ tfc'
                            continue st'
                        _  -> continue st
                    _ -> continue st




mainApp :: App TuiState e ResourceName
mainApp = 
    App
        {   appDraw = drawTui
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

tui :: IO()
tui = do 

    initState <- buildInitState
    endState <- defaultMain mainApp initState
    return ()


