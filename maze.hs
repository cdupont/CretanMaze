
{-# LANGUAGE NoMonomorphismRestriction, TypeFamilies, TupleSections, 
  TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances         #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Offset
import Text.Printf
import Control.Monad
import Diagrams.Backend.CmdLine
import Control.Lens (_1)

main = mainWith $ rotateBy (1/4) $ anim

frames :: [Diagram B R2]
frames = initdraw : (map res [0..7])

anim :: Animation B R2
anim = animEnvelope $ discrete $ frames

initpts = map p2 [(2,0),(2,1),(2,2),(1,2)]

rep4 :: (Transformable t, V t ~ R2) => t -> [t]
rep4 a = map (flip rotateBy a) [0, 1/4, 2/4, 3/4]

node, drawpts, drawlines, drawarcs :: Diagram B R2
node = circle 0.01
drawpts = position $ zip initpts (repeat node)
drawlines = fromVertices $ map p2 [(0,0), (0,2)]
drawarcs = translate (2*unitX + 2*unitY) $ arc (0.5 @@ turn) (0.75 @@ turn)

initdraw = lc red $ fc red $ lw 0.1 $ mconcat $ rep4 $ drawlines <> drawarcs <> drawpts

allpts = mconcat $ rep4 initpts

arc0 :: Located (Trail R2)
arc0 = translate (r2 (2, 1+1/2)) (arc' (0.5) (-0.25 @@ turn) (0.25 @@ turn)) 

res :: Int -> Diagram B R2   
res n = initdraw <> (mconcat $ map strokeLocTrail (allArcs n)) # lc green # lw 0.1 # fc red

arcCaps' arcP (p1:p2:_) (r1:r2:_) = arcCaps arcP r2 r1 p2 p1
arcCaps' _ _ _ = error "lists must contain at least 2 points each"

arcN :: [P2] -> Int -> Located (Trail R2)
arcN ap 0 = arc0
arcN ap n = arcCaps' (arcN ap (n-1)) (drop (n-1) ap) (drop (n-1) (reverse ap))

allArcs n = map (arcN $ shiftList 2 allpts) [0..n]

shiftList n as = (drop n as) ++ (take n as)

capStart :: Located (Trail R2) -> P2 -> P2 -> Trail R2
capStart lt startPt center = if (close 0.0001 startPt (atStart lt)) 
   then mempty
   else capArc 1 center startPt (atStart lt) 

capEnd :: Located (Trail R2) -> P2 -> P2 -> Trail R2
capEnd lt endPt center = if (close 0.0001 endPt (atEnd lt)) 
   then mempty
   else capArc 1 center (atEnd lt) endPt                    


arcCaps :: Located (Trail R2) -> P2 -> P2 -> P2 -> P2 -> Located (Trail R2)
arcCaps arcP startP startR endP endR = 
   mconcat [capStart offs startP startR, 
            unLoc offs, 
            capEnd offs endP endR] `at` startP where
     offs = offsetTrail 1 arcP



showParams :: Located (Trail R2) -> Diagram B R2
showParams lt = position $ map (\i -> (atParam lt i, dot i)) [0,0.025.. 1]

dot n = text (printf "%.2f\n" n) # scale 0.05 # centerXY <> circle 0.1 # fc yellow
dot' n = text (show n) # scale 0.1 # centerXY <> circle 0.1 # fc yellow

showVertices :: Path R2 -> Diagram B R2
showVertices lt = position $ (, circle 0.1 # fc red) <$> (join $ pathVertices lt)

close eps a b = a `distanceSq` b <= eps*eps


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

animMain :: Animation SVG R2 -> IO ()
animMain = mainWith

instance Mainable (Animation SVG R2) where
   type MainOpts (Animation SVG R2) = (MainOpts (Diagram SVG R2), DiagramAnimOpts) 
   mainRender = defaultAnimMainRender (_1 . output)
