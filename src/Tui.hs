{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Tui where

import Lens.Micro
import Lens.Micro.TH
import Lens.Micro.Mtl
import qualified Graphics.Vty as V
import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.Center as C
import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Main
import Brick.Widgets.Core
import Graphics.Vty.Input.Events
import Brick.Widgets.Border




data TuiState = 
    TuiState {
        st :: St
    }


type ResourceName = String

data Name = ResourceName
          deriving (Ord, Show, Eq)

buildInitialState :: IO TuiState
buildInitialState = return (TuiState (St (F.focusRing [ResourceName])
       (E.editor ResourceName Nothing "")))


data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor String Name
       }
makeLenses ''St

drawTui :: TuiState -> [T.Widget Name]
drawTui ts = [ui]
    where 
        st' = st ts
        e1 = F.withFocusRing (st'^.focusRing) (E.renderEditor (str . unlines)) (st'^.edit1)
        ui = C.center $
            (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."
tuiApp :: App TuiState e Name
tuiApp = 
    App 
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty []
        }

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

-- drawTui _ts = [ , vCenter $ hCenter $ border $ hLimit 45 $ hCenter $ str "Hello, world!"]

-- drawTest :: TestState -> [Widget ResourceName]
-- drawTest ts = 
--     let nec = testStatePaths ts
--         in  [   hCenter $ border $
--                 hCenter $ vBox $ 
--                 concat
--                     [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
--                     , [drawPath True $ nonEmptyCursorCurrent nec]
--                     , map (drawPath False) $ nonEmptyCursorNext nec
--                     ]
--             ]

handleTuiEvent :: TuiState -> T.BrickEvent n e -> T.EventM n (T.Next TuiState)
handleTuiEvent s (T.VtyEvent vtye) = 
    case vtye of
        EvKey (KChar 'q') [] -> halt s
        _ -> continue s




appEvent :: TuiState -> T.BrickEvent Name e -> T.EventM Name (T.Next TuiState)
appEvent s (T.VtyEvent (V.EvKey V.KEsc [])) =
    halt s
appEvent s (T.VtyEvent (V.EvKey (V.KChar '\t') [])) =
    continue s
appEvent s ev = do
    r <- use focusRing
    case F.focusGetCurrent r of
      Just ResourceName -> zoom edit1 $ E.handleEditorEvent ev
      Nothing -> continue s

tui :: IO ()
tui = do
    initialState <- buildInitialState
    -- endState <- defaultMain tuiApp initialState
    ts <- defaultMain tuiApp initialState
    let st' = st ts
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st'^.edit1