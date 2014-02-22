
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, TupleSections, 
  TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances         #-}

module Main where

import Diagrams.Prelude hiding (lineCap, lineJoin, option, value, (<>), trace)
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Offset
import Text.Printf
import Control.Monad
import Diagrams.Backend.CmdLine 
import Control.Lens (_1)
import Diagrams.TwoD.Vector     (perp)
import Data.Default.Class
import Options.Applicative hiding ((&))
import Text.Read
import qualified Text.Read.Lex as L

data Maze = Maze {image :: MazeOpts -> Diagram B R2, 
                     animation  :: MazeOpts -> Animation B R2}

data MazeStyle = Round | Square | SquareCut
   deriving (Read, Show)

data MazeOpts = MazeOpts { lineJoin :: LineJoin,   -- style used when offseting the arcs
                           lineCap :: MazeStyle,   -- style used when linking the arcs to the connecting point
                           seedStyle :: MazeStyle, -- style used for the seed
                           seedPos :: Int,         -- seed position (0 or 1)
                           anim :: Bool}           -- generated animation
                deriving (Show)
                  
instance Parseable MazeOpts where
   parser = MazeOpts 
      <$> option (value LineJoinRound <> short 'j' <> long "join" <> metavar "<Miter|Round|Bevel>" 
          <> help "Style of the joins (choose one)")
      <*> option (value Round <> short 'c' <> long "cap" <> metavar "<Round|Square|SquareCut>" 
          <> help "Style of the caps (choose one)")
      <*> option (value Round <> short 'd' <> long "seed" <> metavar "<Round|Square|SquareCut>" 
          <> help "Style of the seed (choose one)")
      <*> option (value 0 <> short 'p' <> long "seedPos" <> metavar "<0|1>" 
          <> help "Position of the seed (0 or 1)")
      <*> switch (short 'a' <> long "animation" 
          <> help "generate an animation")

instance Mainable Maze where
   type MainOpts Maze = (MainOpts (Diagram SVG R2), (DiagramAnimOpts, MazeOpts))
   mainRender (opts, (animOpts, mazeOpts@(MazeOpts _ _ _ _ True))) _ = defaultAnimMainRender (_1 . output) (opts, animOpts) (animMaze mazeOpts)
   mainRender (opts, (_, mazeOpts@(MazeOpts _ _ _ _ False))) _ = mainRender opts (maze mazeOpts) 


main = mainWith $ Maze maze animMaze

maze :: MazeOpts -> Diagram B R2
maze opts = pad 1.01 $ rotateBy (1/4) $ res opts 7

animMaze :: MazeOpts -> Animation B R2
animMaze opts = animEnvelope $ rotateBy (1/4) $ movie $ map (animArcN opts) [0..7]

-- animate the nth arc (leaving previous arcs)
animArcN :: MazeOpts -> Int -> Animation B R2
animArcN opts n = (res opts (n-1) <>) <$> (animLT $ arcN' opts n) # lc green # lw 0.1

--animate the drawing of a located trail
animLT :: Located (Trail R2) -> Animation B R2
animLT lt = strokeLocTrail . section lt 0 <$> ui

--result drawing at step n
res :: MazeOpts -> Int -> Diagram B R2   
res opts n = (initDraw (lineCap opts) # lc green # lw 0.1) <> (mconcat $ map strokeLocTrail (allArcs opts n)) # lc green # lw 0.1

--starting points
initpts = map p2 [(2,0),(2,1),(2,2),(1,2)]

rep4 :: (Transformable t, V t ~ R2) => t -> [t]
rep4 a = map (flip rotateBy a) [0, 1/4, 2/4, 3/4]

drawPts :: Diagram B R2
drawPts = position $ zip initpts $ repeat (circle 0.01 # fc red)

initLines, initRound, initSquare, initSquareCut :: Located (Trail R2) 
initLines = fromVertices $ map p2 [(0,0), (0,2)]
initRound = translate (2*unitX + 2*unitY) $ arc (0.5 @@ turn) (0.75 @@ turn)
initSquare = fromOffsets [(-1) ^& 0, 0 ^& 1] `at` (2 ^& 1)
initSquareCut = fromOffsets [(-0.5) ^& 0, (-0.5) ^& 0.5, 0 ^& 0.5] `at` (2 ^& 1)

--initial drawing
initDraw :: MazeStyle -> Diagram B R2
initDraw opts = mconcat $ rep4 $ mconcat $ strokeLocTrail <$> [initLines, initCorner opts]

--style of the initial drawing
initCorner :: MazeStyle -> Located (Trail R2)
initCorner Round     = initRound
initCorner Square    = initSquare
initCorner SquareCut = initSquareCut

--all points used to connect arcs
allpts = mconcat $ rep4 initpts

--different style of seeds
roundSeed, squareSeed, squareCutSeed :: Trail R2
roundSeed     = (arc' (0.5) (-0.25 @@ turn) (0.25 @@ turn)) 
squareSeed    = fromOffsets [1 ^& 0, 0 ^& 1, (-1) ^& 0]
squareCutSeed = fromOffsets [0.5 ^& 0, 0.5 ^& 0.5, (-0.5) ^& 0.5, (-0.5) ^& 0]

-- select style of seed
selectSeed :: MazeStyle -> Trail R2
selectSeed Round     = roundSeed  
selectSeed Square    = squareSeed
selectSeed SquareCut = squareCutSeed

--helper for arc + caps
arcCaps' opts arcP (p1:p2:_) (r1:r2:_) = arcCaps opts arcP r2 r1 p2 p1
arcCaps' _ _ _ _ = error "lists must contain at least 2 points each"

-- recursive function drawing the nth arc
arcN :: [P2] -> MazeOpts -> Int -> Located (Trail R2)
arcN _  opts 0 = posSeed (seedPos opts) $ selectSeed (seedStyle opts)
arcN ap opts n = arcCaps' opts (arcN ap opts (n-1)) (drop (n-1) ap) (drop (n-1) (reverse ap))

-- draw the nth arc
arcN' :: MazeOpts -> Int -> Located (Trail R2)
arcN' opts i = arcN (ptsList (seedPos opts)) opts i

-- two possibilities for the position of the seed
posSeed :: Int -> Trail R2 -> Located (Trail R2)
posSeed 0 tr = tr `at` (2 ^& 0)
posSeed 1 tr = tr `at` (2 ^& 1)
posSeed _ _ = error "seed position can be only 0 or 1"

-- the list of connector points must be shifted as well
ptsList :: Int -> [P2]
ptsList i = shiftList (i+1) allpts

shiftList n as = (drop n as) ++ (take n as)

-- draw all the arcs up to n
allArcs :: MazeOpts -> Int -> [Located (Trail R2)]
allArcs opts n = map (arcN' opts) [0..n]

--draw the initial cap if needed
capStart :: MazeStyle -> Located (Trail R2) -> P2 -> P2 -> Trail R2
capStart lc lt startPt center = if (close 0.0001 startPt (atStart lt)) 
   then mempty
   else fromLineCap lc center startPt (atStart lt) 

--draw the final cap if needed
capEnd :: MazeStyle -> Located (Trail R2) -> P2 -> P2 -> Trail R2
capEnd lc lt endPt center = if (close 0.0001 endPt (atEnd lt))
   then mempty
   else fromLineCap lc center (atEnd lt) endPt                    

--choose the cap style
fromLineCap :: MazeStyle -> P2 -> P2 -> P2 -> Trail R2
fromLineCap c = case c of
    SquareCut -> capCut 1 
    Round     -> capArc 1 
    Square    -> capSquare 1

--offset an arc and add the necessary caps
arcCaps :: MazeOpts -> Located (Trail R2) -> P2 -> P2 -> P2 -> P2 -> Located (Trail R2)
arcCaps opts arcP startP startR endP endR = 
   mconcat [capStart (lineCap opts) offs startP startR, 
            unLoc offs, 
            capEnd (lineCap opts) offs endP endR] `at` startP where
     offs = offsetTrail' (toOffsetOpts opts) 1 arcP

showParams :: Located (Trail R2) -> Diagram B R2
showParams lt = position $ map (\i -> (atParam lt i, dot i)) [0,0.025.. 1]

dot n = text (printf "%.2f\n" n) # scale 0.05 # centerXY <> circle 0.1 # fc yellow
dot' n = text (show n) # scale 0.1 # centerXY <> circle 0.1 # fc yellow

showVertices :: Path R2 -> Diagram B R2
showVertices lt = position $ (, circle 0.1 # fc red) <$> (join $ pathVertices lt)

close eps a b = a `distanceSq` b <= eps*eps

toOffsetOpts :: MazeOpts -> OffsetOpts
toOffsetOpts (MazeOpts lj _ _ _ _) = def & offsetJoin .~ lj

-- | Builds an arc to fit with a given radius, center, start, and end points.
-- --   A Negative r means a counter-clockwise arc
capArc :: Double -> P2 -> P2 -> P2 -> Trail R2
capArc r c a b = trailLike . moveTo c $ fs
   where
       fs | r < 0     = scale (-r) $ arcVCW (a .-. c) (b .-. c)
          | otherwise = scale r    $ arcV   (a .-. c) (b .-. c)

-- Arc helpers
arcV :: (TrailLike t, V t ~ R2) => R2 -> R2 -> t
arcV u v = arc (direction u) (direction v)

arcVCW :: (TrailLike t, V t ~ R2) => R2 -> R2 -> t
arcVCW u v = arcCW (direction u) (direction v)


-- | Builds a cap with a square centered on the end.
capSquare :: Double -> P2 -> P2 -> P2 -> Trail R2
capSquare _r c a b = unLoc $ fromVertices [ a, a .+^ v, b ] --, b .+^ v
   where
       v = perp (a .-. c)

-- | Builds a cap with a square centered on the end.
capCut :: Double -> P2 -> P2 -> P2 -> Trail R2
capCut _r c a b = unLoc $ fromVertices [ a, a .+^ (v/2), b .+^ (v2/2),  b ] --, b .+^ v
   where
       v = perp (a .-. c)
       v2 = perp (c .-. b)

-- | Builds a cap that directly connects the ends.
capCut' :: Double -> P2 -> P2 -> P2 -> Trail R2
capCut' _r _c a b = fromSegments [straight (b .-. a)]

instance Read LineJoin where
   readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "Miter"  -> return LineJoinMiter
           "Round" -> return LineJoinRound
           "Bevel" -> return LineJoinBevel
           _       -> pfail
    )

