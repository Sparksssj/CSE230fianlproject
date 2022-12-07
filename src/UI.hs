{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe (fromMaybe)

import Thief

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.))
import qualified Graphics.Vty as V
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))
import Test.QuickCheck
-- Types

-- | quickcheck
scorecheck :: Int -> Bool
scorecheck a = a <= 10


flagcheck :: Bool -> Bool
flagcheck a = a == False


-- | Ticks mark passing of time
--
-- This is our custom event that will be constantly fed into the app.
data Tick = Tick

-- | Named resources
--
-- Not currently used, but will be easier to refactor
-- if we call this "Name" now.
type Name = ()

data Cell = Thief | Gold | Golds | Enermy | Barry | Empty

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- decides how fast your game moves
  g <- initGame
  quickCheck(scorecheck(g ^.score))
  quickCheck(flagcheck(g ^.dead))
  quickCheck(flagcheck(g ^.win))
  

  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ turn North g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ turn South g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ turn East g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ turn West g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'r') [])) = liftIO (initGame) >>= continue
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.center $ padRight (Pad 2) (drawStats g) <+> drawGrid g ]

drawStats :: Game -> Widget Name
drawStats g = hLimit 11
  $ vBox [ padTop (Pad 2) $ drawGameOver (g ^. dead),
          padTop (Pad 2) $ drawWin (g ^. win)
         ]

drawScore :: Int -> Widget Name
drawScore n = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Score")
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Widget Name
drawGameOver dead =
  if dead
     then withAttr gameOverAttr $ C.hCenter $ str "GAME OVER"
     else emptyWidget


drawWin :: Bool -> Widget Name
drawWin win =
  if win
     then withAttr gameOverAttr $ C.hCenter $ str "YOU WIN"
     else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. thief = Thief
      | c == g ^. gold      = Gold
      | c `elem` g ^. golds = Golds
      | c == g ^. enermy      = Enermy
      | c == g ^. enermy1      = Enermy
      | c == g ^. enermy2      = Enermy
      | c == g ^. enermy3      = Enermy
      | c == g ^. enermy4      = Enermy
      | c `elem` g ^. barry   = Barry
      | otherwise           = Empty

drawCell :: Cell -> Widget Name
drawCell Thief = withAttr thiefAttr cw
drawCell Gold  = withAttr goldAttr cw
drawCell Golds  = withAttr goldsAttr cw
drawCell Enermy  = withAttr enermyAttr cw
drawCell Barry  = withAttr barryAttr cw
drawCell Empty = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (thiefAttr, V.yellow `on` V.yellow)
  , (goldAttr, V.green `on` V.green)
  , (goldsAttr, V.blue `on` V.blue)
  , (enermyAttr, V.red `on` V.red)
  , (barryAttr, V.white `on` V.white)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = "gameOver"

thiefAttr, goldAttr, goldsAttr,barryAttr,emptyAttr :: AttrName
thiefAttr = "thiefAttr"
goldAttr = "goldAttr"
goldsAttr = "goldsAttr"
enermyAttr = "enermyAttr"
barryAttr = "barryAttr"
emptyAttr = "emptyAttr"
