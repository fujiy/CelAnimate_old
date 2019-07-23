{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Editor.Node.Vectorize where

import           Control.Arrow
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Control.Monad.State
import           Data.Complex
import           Data.Foldable
import           Data.KdMap.Static        (KdMap)
import qualified Data.KdMap.Static        as KdM
import           Data.KdTree.Static       (KdTree)
import qualified Data.KdTree.Static       as KdT
import           Data.List
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map
import           Data.Maybe
import           Data.Sequence            (Seq)
import qualified Data.Sequence            as Seq
import           Data.Set                 (Set)
import qualified Data.Set                 as Set
import qualified Data.UnionFind.ST        as UF
import qualified Data.Vector.Storable     as V
import           Debug.Trace
import           Geom2D                   hiding (magnitude)
import           Graphics.Image           (Image, MImage, Pixel, RGBA, VS, Y)
import qualified Graphics.Image           as I
import qualified Graphics.Image.Interface as I
import qualified Graphics.Image.IO        as I



import           Editor.Animate.Image


type FPoint   = Point Float
type FComplex = Complex Float
type FVec     = FPoint




blurRadius :: Double
blurRadius = 1.5

filledMTh, hideCoef :: Pixel Y Double
filledMTh = 0.5
hideCoef  = 1 / 16

deltaT = 0.5

-- getCenters :: Image VS Y Double -> _
getCenters image =
    let blured = I.applyFilter (I.gaussianLowPass 1 1 I.Edge)
                 $ I.toImageY image
        pts    = sobelParticles 0.1 deltaT blured
        mpts   = smooth 1.0 $ moveParticles deltaT pts
        sides  = getSides $ mpts

        -- image0 = edges blured
        -- image1 = runST $ do
        --     mimg <- I.thaw $ normalizeByNear image0
        --     -- normalizeByNear mimg
        --     -- normalize (1/8) mimg
        --     fills mimg
        --     I.freeze mimg
        -- image1' = I.applyFilter (I.gaussianLowPass 2 1 I.Edge) image0
        -- (points, ends) = centers image1
    in  (I.toImageRGBA blured, map fst mpts, sides
         -- togetherNears $ endpoints points
        )
  where

    -- clamp = fmap (\x -> x / 2 + 0.5)

    compHSI :: Pixel Y (Complex Double) -> Pixel I.HSI Double
    compHSI pix = I.fromComponents
        ( clamp (I.getPxC (I.phase pix) I.LumaY) / (2.01 * pi)
        , 1
        , I.getPxC (I.magnitude pix) I.LumaY / 2)

    clamp x | x < 0     = x + 2*pi
            | x >= 2*pi = x - 2*pi
            | otherwise = x

sobelParticles :: Float -> Float -> Image VS Y Double
              -> [(FPoint, (Complex Float, Float))]
sobelParticles th dt image = foldI particle [] image
  where
    vec = I.toVector image
    (h, w) = I.dims image

    particle (y, x) ps pix =
        let at dy dx = vec V.! I.fromIx w
                       (clamps (h, w) (y + dy, x + dx))
            (vx, vy) = foldr (\(dy, dx, cy, cx) (gx, gy) ->
                                  let g = realToFrac . getPx $ at dy dx
                                  in  (gx + g * cx, gy + g * cy))
                       (0, 0) sobelK
            v  = vx :+ vy
            m  = magnitude v
            vx' = dt * vx / m
            vy' = dt * vy / m
            px  = fromIntegral x + vx'
            py  = fromIntegral y + vy'
        in  if m >= th * 8
            then ( Point px py, (vx' :+ vy', 0) ) : ps
            else ps

    sobelK :: [(Int, Int, Float, Float)] -- ^ (y, x, cy, cx)
    sobelK = [ (-1, -1, -1, -1), (-1,  0, -2,  0), (-1,  1, -1,  1)
             , ( 0, -1,  0, -2), ( 0,  0,  0,  0), ( 0,  1,  0,  2)
             , ( 1, -1,  1, -1), ( 1,  0,  2,  0), ( 1,  1,  1,  1)]

-- movesParticles :: Float -> KdMap Float FPoint (Complex Float)
--                -> KdMap Float FPoint (Complex Float)
-- movesParticles dt kdm = let (kdm', moved) = moveParticles dt kdm
--                         in  if moved * 100 > KdM.size kdm'
--                             then movesParticles dt kdm'
--                             else kdm'

moveParticles :: Float
              -> [(FPoint, (Complex Float, Float))]
              -> [(FPoint, (Complex Float, Float))]
moveParticles dt ps = go initKdm [] ps

  where
    initKdm = KdM.build asList ps

    dt2 = dt * dt

    go :: KdMap Float FPoint (Complex Float, Float)
       -> [(FPoint, (Complex Float, Float))]
       -> [(FPoint, (Complex Float, Float))]
       -> [(FPoint, (Complex Float, Float))]
    go kdm spvs dpvs =
        let (pvs', dpvs', spvs', moved) =
                foldr (move kdm) (spvs, [], spvs, 0) dpvs
            kdm' = KdM.build asList pvs'
        in  if moved * 100 > KdM.size initKdm
            then go kdm' spvs' dpvs'
            else pvs'

    move :: KdMap Float FPoint (Complex Float, Float)
         -> (FPoint, (Complex Float, Float))
         -> ( [(FPoint, (Complex Float, Float))]
            , [(FPoint, (Complex Float, Float))]
            , [(FPoint, (Complex Float, Float))]
            , Int)
         -> ( [(FPoint, (Complex Float, Float))]
            , [(FPoint, (Complex Float, Float))]
            , [(FPoint, (Complex Float, Float))]
            , Int)
    move kdm (p, (v, t)) (pvs, dpvs, spvs, moved) =
        let (blocked, unite, len) = foldr
                (\(pa, (va, _)) (b, u, len) ->
                     let v_va = v `dotC` va
                         rp   = pa ^-^ p
                     in  ( (v_va < 0 && rp ^.^ cToP v < 0) || b
                         , (v_va > 0.8 * dt2 && vectorMagSquare rp < dt2
                            && realPart (v - va) < 0) || u
                         , len + 1))
                (False, False, 0) $ KdM.inRadius kdm 1.0 p
            vx :+ vy = v
            p'       = p ^+^ Point vx vy
            t'       = t + dt
        in if | unite ->
                ( pvs
                , dpvs
                , spvs
                , moved )
              | blocked ->
                ( (p, (v, t)) : pvs
                , dpvs
                , (p, (v, t)) : spvs
                , moved )
              | otherwise ->
                ( (p', (v, t')) : pvs
                , (p', (v, t')) : dpvs
                , spvs
                , moved + 1 )


smooth :: Float -> [(FPoint, x)] -> [(FPoint, x)]
smooth r points =
    let kdt = KdT.build asList $ map fst points
    in  map (\(p, x) ->
                 let nears = KdT.inRadius kdt r p
                 in  ( (p ^+^ (foldr (^+^) zeroV nears
                               ^/ fromIntegral (length nears))) ^/ 2
                     , x))
        points

-- getSides :: [(FPoint, (a, Float))]
--          -> [[(FPoint, Float)]]
getSides pvs = runST $ do
    -- sds <- foldM sidesGo Seq.empty pvs
    points <- forM pvs $ \(p, (v, t)) ->
        (p ,) <$> UF.fresh (Map.singleton p [])
    let kdm    = KdM.build asList points
        sides  = KdM.foldrWithKey (sidesGo kdm) Seq.empty kdm
        sides' = Seq.sortBy (\(_,_,_,_,x) (_,_,_,_,y) -> compare x y) sides
    forM_ sides' $ \(ufpa, ufpb, pa, pb, d) -> do
        eq <- UF.equivalent ufpa ufpb
        unless eq $ do
            -- UF.modifyDescriptor ufpa $ \ -> ((pb, d) : sds, t)
            -- UF.modifyDescriptor ufpb $ \(sds, t) -> ((pa, d) : sds, t)
            UF.union' ufpa ufpb $ \psa psb ->
                return $ Map.adjust ((pb, d) :) pa
                       $ Map.adjust ((pa, d) :) pb
                       $ psa <> psb

    (_, spms) <- foldM (\(rs, pps) (p, ufp) -> do
        ps <- UF.descriptor ufp
        let pr = fst $ Map.findMin ps
        return $ if pr `Set.member` rs
                 then (rs, pps)
                 else (Set.insert pr rs, fmap Map.fromList ps : pps)
        )
        (mempty, []) points

    let
        ss = filter (longerThan 1)
            $ map (map fst
                   . filter ((/= 2) . Map.size . snd)
                   . Map.toList
                   . cutBrunches 1.0)
             spms

        ss' = filter (longerThan 1)
            $ map (map fst
                   . filter ((> 1) . snd)
                   . Map.toList
                   . edgeDistances
                   )
             spms

        ssl = ss


    traceShow (length ssl) return ()

    return ssl

  where
    sidesGo :: KdMap Float FPoint (UF.Point s p)
            -> (FPoint, UF.Point s p)
            -> Seq (UF.Point s p, UF.Point s p,
                        FPoint, FPoint, Float)
            -> Seq (UF.Point s p, UF.Point s p,
                        FPoint, FPoint, Float)
    sidesGo kdm (p, ufp) sdss =
        let sds = Seq.fromList
                . map (\(pa, ufpa) -> (ufp, ufpa, p, pa, vectorDistance pa p))
                . filter ((/= p) . fst)
                $ KdM.inRadius kdm 1.5 p
        in  sds <> sdss


cutBrunches :: Float
            -> Map FPoint (Map FPoint Float)
            -> Map FPoint (Map FPoint Float)
cutBrunches d spm =
    let ls = Map.filter (< d) $ edgeDistances spm
    in (Map.\\ ls) <$> (spm Map.\\ ls)

edgeDistances :: Map FPoint (Map FPoint Float)
              -> Map FPoint Float
edgeDistances points =
    let edges = zeroV <$ Map.filter ((== 1) . Map.size) points
    in  go mempty edges
  where
    go :: Map FPoint Float -> Map FPoint Float -> Map FPoint Float
    go traced nexts | Map.null nexts = traced
    go traced nexts =
        let traced' = Map.union nexts traced
            nexts'  = Map.foldrWithKey'
                      (\p d m ->
                           let pm = Map.filterWithKey
                                    (\p _ ->
                                         let td = (points Map.! p
                                                  Map.\\ traced')
                                         in  Map.size td < 2)
                                  $ Map.difference
                                    ((d +) <$> points Map.! p) traced'
                           in  Map.unionWith max pm m)
                      mempty nexts
        in  go traced' nexts'

longerThan :: Int -> [a] -> Bool
longerThan n      _ | n < 0 = True
longerThan n     [] = False
longerThan n (_:xs) = longerThan (n - 1) xs


-- cutBrunches :: Float
--              -> KdMap Float FPoint p
--              -> Map FPoint (Map FPoint Float)
--              -> Map FPoint (Map FPoint Float)
-- cutBrunches radius kdm points =
--     let dists      = edgeDistances points
--         (rps, cns) =
--             Map.foldrWithKey'
--             (\p conns (rs, cs) ->
--                  let nears       = (\(pa, _) -> (pa ,) <$> dists Map.!? pa)
--                                    `mapMaybe` KdM.inRadius kdm radius p
--                      (target, _) = maximumBy (\(_,x) (_,y) -> compare x y) nears
--                  in  if null nears
--                      then (rs, cs)
--                      else ( (p, target)     : rs
--                           , (target, conns) : cs ))
--             ([], []) points
--         replaces = Map.fromList rps
--     in Map.mapKeysWith min (fromMaybe undefined . (replaces Map.!?))
--        <$> Map.fromListWith (<>) cns

-- cutBrunches' :: KdMap Float FPoint (UF.Point s p)
--             -> Map FPoint (Map FPoint x)
--             -> Map FPoint (Map FPoint x)
-- cutBrunches' kdm sides =
--     let points = Map.keysSet sides
--         ends   = Seq.sortBy (\(_,mx) (_,my) ->
--                                  compare (Map.size mx) (Map.size my))
--                  . Seq.fromList $ Map.toList sides
--         (dpm, dss) = Map.foldrWithKey'
--                      (\p pm (dpm, dss) ->
--                           let (dps, ds) = intersection sides points p pm
--                           in  (Map.insert p dps dpm, ds <> dss))
--                      (mempty, mempty)
--                      $ Map.filter ((>= 3) . Map.size) sides
--     in  Map.differenceWith
--         (\m dps -> Just $ m `Map.withoutKeys` dps)
--         (sides `Map.withoutKeys` dss)
--         dpm
--   where
--     intersection :: Map FPoint (Map FPoint x)
--                  -> Set.Set FPoint
--                  -> FPoint -> Map FPoint x
--                  -> (Set.Set FPoint, Set.Set FPoint)
--     intersection sides points p pm =
--         let ss         = Map.keys $ sides Map.! p
--             ls         = (\x -> let (ls, es) = lineFrom sides mempty p x
--                                 in  (x, es, ls))
--                          <$> ss
--             (dps, dss) = unzip
--                          $(\(pa, es, ls) ->
--                                if all (insignificant points ls) es
--                                then Just (pa, ls) else Nothing)
--                          `mapMaybe` ls
--         in  (Set.fromList dps, mconcat dss)

--     insignificant :: Set.Set FPoint -> Set.Set FPoint -> FPoint -> Bool
--     insignificant area locus p =
--         let nears = Set.fromList . map fst $ KdM.inRadius kdm 1.0 p
--         in  not . Set.null $ (nears `Set.intersection` area) Set.\\ locus


--     walk :: Map FPoint (Map FPoint x)
--          -> Set.Set FPoint -> Set.Set FPoint -> Seq FPoint -> Bool
--     walk sides area locus Seq.Empty      = True
--     walk sides area locus (p Seq.:<| ps) =
--         let nears = Set.fromList . map fst $ KdM.inRadius kdm 1.0 p
--         in  (not . Set.null $ (nears `Set.intersection` area) Set.\\ locus)
--             && walk sides area locus ps

-- lineFrom :: Map FPoint (Map FPoint x)
--          -> Set.Set FPoint
--          -> FPoint -> FPoint -> (Set.Set FPoint, Seq FPoint)
-- lineFrom sides locus pp pn =
--     let pm    = sides Map.! pn
--         nexts = filter (`Set.notMember` locus) $ Map.keys pm
--         locus' = Set.insert pn locus
--     in
--        case Map.size pm of
--         1 -> (locus', Seq.singleton pn)
--         -- _ -> (Seq.singleton pn, Seq.singleton pn)
--         _ -> foldMap (lineFrom sides locus' pn) nexts
--         -- _ -> (Seq.singleton pn,, Seq.singleton pn)

maxMag :: (Ord a, Floating a) => Point a -> Point a -> Point a
maxMag a b = if vectorMagSquare a > vectorMagSquare b then a else b

minMag :: (Ord a, Floating a) => Point a -> Point a -> Point a
minMag a b = if vectorMagSquare a < vectorMagSquare b then a else b

compMag :: (Ord a, Floating a) => Point a -> Point a -> Ordering
compMag a b = compare (vectorMagSquare a) (vectorMagSquare b)

mapToSnd :: (a -> b) -> a -> (a, b)
mapToSnd f a = (a, f a)
{-# INLINE mapToSnd #-}

mapMaybeS :: (a -> Maybe b) -> Seq a -> Seq b
mapMaybeS f = foldr (\a bs -> maybe bs (Seq.:<| bs) (f a)) mempty

dotC :: Num a => Complex a -> Complex a -> a
dotC (ax :+ ay) (bx :+ by) = ax * bx + ay * by

cToP :: Complex a -> Point a
cToP (x :+ y) = Point x y
asList :: FPoint -> [Float]
asList (Point x y) = [x, y]


foldIM :: (Monad m, PrimMonad m, I.MArray arr cs e)
       => ((Int, Int) -> a -> Pixel cs e -> m a)
       -> a -> MImage (PrimState m) arr cs e -> m a
foldIM f a img = go a 0 0
  where
    (height, width) = I.mdims img
    go a y x | y >= height = return a
             | x >= width  = go a (y + 1) 0
             | otherwise   = do
                   p <- I.read img (y, x)
                   a' <- f (y, x) a p
                   go a' y (x + 1)

mapIM_ :: (Monad m, PrimMonad m, I.MArray arr cs e)
       => ((Int, Int) -> Pixel cs e -> m ())
       -> MImage (PrimState m) arr cs e -> m ()
mapIM_ f = foldIM (\p _ -> f p) ()

foldI :: I.Array VS cs e
      => ((Int, Int) -> a -> Pixel cs e -> a)
      -> a -> Image VS cs e -> a
foldI f a img = go a 0 0
  where
    (height, width) = I.dims img
    vec = I.toVector img
    go a y x | y >= height = a
             | x >= width  = go a (y + 1) 0
             | otherwise   =
               let p  = vec V.! I.fromIx width (y, x)
                   a' = f (y, x) a p
               in  go a' y (x + 1)


readBorder :: (Functor m, PrimMonad m, I.MArray arr cs e)
           => MImage (PrimState m) arr cs e -> (Int, Int) -> m (Pixel cs e)
readBorder image (y, x) =
    let (h, w) = I.mdims image
        y'     = min h $ max 0 y
        x'     = min w $ max 0 x
    in  I.read image (y', x')

phaseDiff :: (Ord a, Floating a, I.Elevator a)
          => Pixel Y a -> Pixel Y a -> Pixel Y a
phaseDiff x y = normPhase $ x - y

phaseDiff' :: (Ord a, Floating a) => a -> a -> a
phaseDiff' x y = normPhase $ x - y


normPhase :: (Ord a, Floating a) => a -> a
normPhase x | x > pi    = normPhase $ x - 2 * pi
            | x < -pi   = normPhase $ x + 2 * pi
            | otherwise = x

normPhasePi :: (Ord a, Floating a) => a -> a
normPhasePi x | x > pi    = normPhasePi $ x - pi
              | x < 0     = normPhasePi $ x + pi
              | otherwise = x

getPx :: I.Elevator a => Pixel Y a -> a
getPx p = I.getPxC p I.LumaY

clamps :: (Int, Int) -> (Int, Int) -> (Int, Int)
clamps (h, w) (y, x) = (max 0 $ min (h - 1) y, max 0 $ min (w - 1) x)

phase' :: (RealFloat a, I.Elevator a) => Complex a -> a
phase' c = getPx . I.phase $ I.promote c

polar' :: (RealFloat a, I.Elevator a) => Complex a -> (a, a)
polar' c = let (m, p) = I.polar $ I.promote c
           in (getPx m, getPx p)

polarP :: (RealFloat a, I.Elevator a) => Point a -> (a, a)
polarP (Point x y) = polar' $ x I.:+ y

phaseP :: (RealFloat a, I.Elevator a) => Point a -> a
phaseP (Point x y) = phase' $ x I.:+ y

around9csp :: [(Int, Int, Pixel Y Double)]
around9csp = zip3
    (replicate 3 (-1) <> replicate 3 0 <> replicate 3 1)
    (concat $ replicate 3 [-1, 0, 1])
    [-pi * 3 / 4, -pi / 2, -pi / 4,
              pi,       0,       0,
      pi * 3 / 4,  pi / 2,  pi / 4]

around9cs :: [(Int, Int)]
around9cs = zip
    (replicate 3 (-1) <> replicate 3 0 <> replicate 3 1)
    (concat $ replicate 3 [-1, 0, 1])


between :: Ord a => a -> a -> a -> Bool
between mi mx x = mi <= x && x < mx


