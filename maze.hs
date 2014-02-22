
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
import Control.Lens (_1, (^.))
import Diagrams.TwoD.Vector     (perp)
import Data.Default.Class

--main = mainWith $ animEnvelope $ rotateBy (1/4) $ movie $ map animArcN [0..7]
main = mainWith $ pad 1.01 $ rotateBy (1/4) $ res (def & expandJoin .~ LineJoinRound & expandCap .~ LineCapSquare) 7 
-- LineJoin = LineJoinMiter    -- ^ Use a \"miter\" shape (whatever that is).
--              | LineJoinRound    -- ^ Use rounded join points.
--              | LineJoinBevel 
-- | What sort of shape should be placed at the endpoints of lines?
-- data LineCap = LineCapButt   -- ^ Lines end precisely at their endpoints.
--              | LineCapRound  -- ^ Lines are capped with semicircles
--              | LineCapSquare -- ^ Lines are capped with a squares

animArcN :: ExpandOpts -> Int -> Animation B R2
animArcN opts n = (res opts (n-1) <>) <$> (animLT $ arcN' opts n) # lc green # lw 0.1

animLT :: Located (Trail R2) -> Animation B R2
animLT lt = strokeLocTrail . section lt 0 <$> ui

res :: ExpandOpts -> Int -> Diagram B R2   
res opts n = (initDraw opts # lc green # lw 0.1) <> (mconcat $ map strokeLocTrail (allArcs opts n)) # lc green # lw 0.1

initpts = map p2 [(2,0),(2,1),(2,2),(1,2)]

rep4 :: (Transformable t, V t ~ R2) => t -> [t]
rep4 a = map (flip rotateBy a) [0, 1/4, 2/4, 3/4]

drawPts :: Diagram B R2
drawPts = position $ zip initpts $ repeat (circle 0.01 # fc red)

initLines, initRound, initMiter, initBevel :: Located (Trail R2) 
initLines = fromVertices $ map p2 [(0,0), (0,2)]
initRound = translate (2*unitX + 2*unitY) $ arc (0.5 @@ turn) (0.75 @@ turn)
initMiter = fromOffsets [(-1) ^& 0, 0 ^& 1] `at` (2 ^& 1)
initBevel = fromOffsets [(-0.5) ^& 0, (-0.5) ^& 0.5, 0 ^& 0.5] `at` (2 ^& 1)

initDraw :: ExpandOpts -> Diagram B R2
initDraw opts = mconcat $ rep4 $ mconcat $ strokeLocTrail <$> [initLines, initCorner opts]

initCorner (ExpandOpts LineJoinMiter _ _ _) = initMiter
initCorner (ExpandOpts LineJoinRound _ _ _) = initRound
initCorner (ExpandOpts LineJoinBevel _ _ _) = initBevel

allpts = mconcat $ rep4 initpts

roundSeed :: Located (Trail R2)
roundSeed = translate (r2 (2, 1+1/2)) (arc' (0.5) (-0.25 @@ turn) (0.25 @@ turn)) 

roundSeed' :: Located (Trail R2)
roundSeed' = translate (r2 (2, 1/2)) (arc' (0.5) (-0.25 @@ turn) (0.25 @@ turn)) 

squareSeed :: Located (Trail R2)
squareSeed = fromOffsets [(1) ^& 0, 0 ^& 1, (-1) ^& 0] `at` (2 ^& 0)

arcCaps' opts arcP (p1:p2:_) (r1:r2:_) = arcCaps opts arcP r2 r1 p2 p1
arcCaps' _ _ _ _ = error "lists must contain at least 2 points each"

arcN :: [P2] -> ExpandOpts -> Int -> Located (Trail R2)
arcN _  opts 0 = selectSeed opts
arcN ap opts n = arcCaps' opts (arcN ap opts (n-1)) (drop (n-1) ap) (drop (n-1) (reverse ap))

arcN' :: ExpandOpts -> Int -> Located (Trail R2)
arcN' opts i = arcN (shiftList 1 allpts) opts i

selectSeed :: ExpandOpts -> Located (Trail R2)
selectSeed (ExpandOpts LineJoinMiter _ _ _) = squareSeed
selectSeed (ExpandOpts LineJoinRound _ _ _) = roundSeed'
selectSeed (ExpandOpts LineJoinBevel _ _ _) = squareSeed

allArcs :: ExpandOpts -> Int -> [Located (Trail R2)]
allArcs opts n = map (arcN' opts) [0..n]

shiftList n as = (drop n as) ++ (take n as)

capStart :: LineCap -> Located (Trail R2) -> P2 -> P2 -> Trail R2
capStart lc lt startPt center = if (close 0.0001 startPt (atStart lt)) 
   then mempty
   else fromLineCap lc center startPt (atStart lt) 

capEnd :: LineCap -> Located (Trail R2) -> P2 -> P2 -> Trail R2
capEnd lc lt endPt center = if (close 0.0001 endPt (atEnd lt))
   then mempty
   else fromLineCap lc center (atEnd lt) endPt                    

fromLineCap :: LineCap -> P2 -> P2 -> P2 -> Trail R2
fromLineCap c = case c of
    LineCapButt   -> capCut 1 -- This cap does not allow to join the next point 
    LineCapRound  -> capArc 1 
    LineCapSquare -> capSquare 1

arcCaps :: ExpandOpts -> Located (Trail R2) -> P2 -> P2 -> P2 -> P2 -> Located (Trail R2)
arcCaps opts arcP startP startR endP endR = 
   mconcat [capStart (opts ^. expandCap) offs startP startR, 
            unLoc offs, 
            capEnd (opts ^. expandCap) offs endP endR] `at` startP where
     offs = offsetTrail' (toOffsetOpts opts) 1 arcP

showParams :: Located (Trail R2) -> Diagram B R2
showParams lt = position $ map (\i -> (atParam lt i, dot i)) [0,0.025.. 1]

dot n = text (printf "%.2f\n" n) # scale 0.05 # centerXY <> circle 0.1 # fc yellow
dot' n = text (show n) # scale 0.1 # centerXY <> circle 0.1 # fc yellow

showVertices :: Path R2 -> Diagram B R2
showVertices lt = position $ (, circle 0.1 # fc red) <$> (join $ pathVertices lt)

close eps a b = a `distanceSq` b <= eps*eps

toOffsetOpts :: ExpandOpts -> OffsetOpts
toOffsetOpts (ExpandOpts join miterLimit _ epsilon) = OffsetOpts join miterLimit epsilon

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

animMain :: Animation SVG R2 -> IO ()
animMain = mainWith

instance Mainable (Animation SVG R2) where
   type MainOpts (Animation SVG R2) = (MainOpts (Diagram SVG R2), DiagramAnimOpts) 
   mainRender = defaultAnimMainRender (_1 . output)
