{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}


module Tui where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Graphics.Vty.Input.Events


data TuiState = TuiState { tuiStatePaths :: [String]
        } deriving (Show, Eq)

type ResourceName = String


tui :: IO()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

tuiApp :: App TuiState e ResourceName
tuiApp = 
    App {appDraw = drawTui,
         appChooseCursor = showFirstCursor,
         appHandleEvent = handleTuiEvent,
         appStartEvent = pure,
         appAttrMap = const $ attrMap mempty []
        }

buildInitialState :: IO TuiState
buildInitialState = do 
    let content = getList
    pure TuiState {tuiStatePaths = content}


getList :: [String]
getList = [show i | i <- [1..10]]


drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = [vBox $ map str $ tuiStatePaths ts]


handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
    case e of 
        VtyEvent vtye -> 
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                _ -> continue s
        _ -> continue s










 
