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


-- data Reach = End   FPoint
--            | Cont  FPoint
--            | Intxn FPoint

-- weave :: Map FPoint (Map FPoint x) -> Map FPoint [FPoint]
-- weave sides = let (p, ps) = Map.findMin sides
--                   ps'     = Seq.fromList $ Map.keys ps
--               in Map.fromList $ walk mempty p ps'
--   where
--     -- go :: Set FPoint -> FPoint ->

--     r_sq = 1.0

--     walk :: Set FPoint -> FPoint -> Seq FPoint -> [(FPoint, [FPoint])]
--     walk locus p ps =
--         let (rs, lss) = Seq.unzip $ reach zeroV p `mapMaybeS` ps
--             nexts = group rs
--             pm    = concatMap
--                     (\(pn, pns) ->
--                          let locus' = Set.insert p locus
--                              pns'   = Seq.fromList
--                                       . filter (`Set.notMember` locus')
--                                       . Map.keys $ sides Map.! pn
--                          in  walk locus' pn $ pns' <> pns)
--                     nexts
--         in  (p, map fst pm) : pm

--     reach :: FVec -> FPoint -> FPoint -> Maybe (FPoint, [FPoint])
--     reach d pp pn
--         | vectorMagSquare d > r_sq = Just (pn, [])
--         | otherwise =
--           let  pm   = sides Map.! pn
--                next = fromJust . find (/= pp) $ Map.keys pm
--           in case Map.size pm of
--                  1 -> Nothing
--                  2 -> second (pn :) <$> reach (d ^+^ next) pn next
--                  _ -> Just (pn, [])

--     group :: Seq FPoint -> [(FPoint, Seq FPoint)]
--     group Seq.Empty      = []
--     group (p Seq.:<| ps) =
--         let (ns, ps') = Seq.partition ((< r_sq) . vectorDistance p) ps
--         in  (p, ns) : group ps'


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

-- brunchLengths :: Map FPoint (Map FPoint FVec)
--               -> Map FPoint FVec
-- brunchLengths spm =
--     let ss = Seq.sortBy (\(_, ma) (_, mb) ->
--                              compare (Map.size ma) (Map.size mb))
--              . Seq.fromList $ Map.toList spm
--     in  go ss $ const zeroV <$> spm
--   where
--     go :: Seq (FPoint, Map FPoint FVec)
--        -> Map FPoint FVec
--        -> Map FPoint FVec
--     go Seq.Empty pm            = pm
--     go ((p, m) Seq.:<| ss') pm =
--         let x   = pm Map.! p
--             pm' = Map.unionWith
--                   (\y d -> if y ^+^ d == x then y else maxMag y (x ^+^ d))
--                   pm m
--         in  go ss' pm'

--     maxMag :: FVec -> FVec -> FVec
--     maxMag a b = if vectorMagSquare a > vectorMagSquare b then a else b



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

-- lineFromBegin :: Map FPoint (Map FPoint x)
--               -> FPoint -> Seq FPoint
-- lineFromBegin sides p = foldMap (lineFrom sides p)
--                         $ Map.keys $ sides Map.! p



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


-- cutBrunches :: KdMap Float FPoint (UF.Point s p)
--             -> Map FPoint (Map FPoint a)
--             -> Map FPoint (Map FPoint a)
-- cutBrunches kdm spm = spm
--   where
--     cut :: FPoint
--         -> State (Map FPoint (Map FPoint a)) ()
--     cut p = return ()

--     walk :: [FPoint] -> FPoint
--          -> State (Map FPoint (Map FPoint a))
--                   (Set.Set FPoint)
--     walk ps p = do
--         spm <- get
--         let pm    = spm Map.! p
--             nexts = filter (/= p) $ Map.keys pm
--             locus = Set.fromList ps
--             nears = Set.fromList . map fst $ KdM.inRadius kdm 1.0 p
--         case Map.size pm of
--             1 | Set.null $ nears Set.\\ locus
--                 -> return Set.empty
--             1   -> return locus
--             2   -> walk (p:ps) $ head nexts
--             _   -> foldM cut nexts

-- cutBrunches' :: Float
--             -> Map FPoint (Map FPoint (FVec))
--             -> Map FPoint (Map FPoint (FVec))
-- cutBrunches' len spm =
--     let spdm = fmap (, zeroV) spm
--     in  fst <$> go spdm
--   where
--     len2 = len * len
--     go :: Map FPoint
--           (Map FPoint (FVec), FVec)
--        -> Map FPoint
--           (Map FPoint (FVec), FVec)
--     go spm =
--         let dels = Map.filter
--                    (\(pm, d) -> Map.size pm <= 1
--                                 && vectorMagSquare d < len2)
--                    spm
--             rdus = Map.fromListWith max
--                    $ concatMap
--                    (\(pm, d) ->
--                         map (second (^+^ d)) $ Map.toList pm)
--                    $ Map.elems dels
--             spm' = Map.differenceWith
--                    (\(pm, d) d' -> Just (pm Map.\\ dels, d ^+^ d'))
--                    (spm Map.\\ dels) rdus
--         in if Map.null rdus then spm' else go spm'

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

edges :: Image VS Y Double -> Image VS Y (Complex Double)
edges image = I.traverse image id edge
  where
    edge :: ((Int, Int) -> Pixel Y Double)
         -> (Int, Int) -> Pixel Y (Complex Double)
    edge getter (y, x) =
        let at dy dx = getter $ clamps (I.dims image) (y + dy, x + dx)
            ry = (sum (at    1     <$> [-1,0,1]) -
                  sum (at    (-1)  <$> [-1,0,1])) / 3
            rx = (sum ((`at` 1   ) <$> [-1,0,1]) -
                  sum ((`at` (-1)) <$> [-1,0,1])) / 3
        in  rx I.+: r

normalizeByNear :: Image VS Y (Complex Double) -> Image VS Y (Complex Double)
normalizeByNear image = I.traverse image id norm
  where
    dims = I.dims image

    norm :: ((Int, Int) -> Pixel Y (Complex Double))
         -> (Int, Int) -> Pixel Y (Complex Double)
    norm getter (y, x) =
        let cp    = getter (y, x)
            nears = map (\(dy, dx) -> I.magnitude . getter
                            $ clamps dims (y + dy, x + dx))
                    around9cs
            (m, p) = I.polar cp
        in if m > sum nears / 9 then I.mkPolar 1.0 p else 0


fills :: forall s. MImage s VS Y (Complex Double) -> ST s ()
fills image = do
    upd <- foldIM fill False image
    normalize (filledMTh * hideCoef) image

    when upd $ fills image
  where
    (height, width) = I.mdims image

    fill :: (Int, Int) -> Bool -> Pixel Y (Complex Double) -> ST s Bool
    fill (y, x) upd cp = do
        let (mc, pc) = I.polar cp
        if mc > filledMTh
           && between 1 (width  - 1) x
           && between 1 (height - 1) y
        then foldM (\upd' (dy, dx, dp) -> do
            let i = (y + dy, x + dx)
            rp <- I.read image i
            let (am, ap) = I.polar rp
                rp'      = rp + cp * (hideCoef I.+: 0)
            if am < filledMTh
               && abs (phaseDiff pc dp) < pi/7
               && not (dx == 0 && dy == 0)
               && not (am > filledMTh * hideCoef
                      && abs (phaseDiff pc ap) > pi/3)
            then do I.write image (clamps (I.mdims image) i) rp'
                    return True
            else return upd')
            upd around9csp
        else return upd


normalize :: forall s. Pixel Y Double
          -> MImage s VS Y (Complex Double) -> ST s ()
normalize th image = mapIM_ (\i cp -> do
    let (m, p) = I.polar cp
        cp' = if m > th then I.mkPolar 1.0 p else 0
    I.write image i cp')
    image

centers :: Image VS Y (Complex Double)
            -> (KdMap Float FPoint (Complex Double), [FPoint])
centers image = let (points, ends) = foldI go ([], []) image
                in  (KdM.build asList points, ends)
  where
    (height, width) = I.dims image
    vec = I.toVector image

    go :: (Int, Int)
       -> ([(FPoint, Complex Double)], [FPoint])
       -> Pixel Y (Complex Double)
       -> ([(FPoint, Complex Double)], [FPoint])
    go (y, x) (points, ends) pix =
        let at dy dx   = vec V.! I.fromIx width
                         (clamps (I.dims image) (y + dy, x + dx))
            dx I.:+ dy = getPx pix
            op         = at (round $ 1*dy) (round $ 1*dx)
            (mc, pc)   = I.polar pix
            (mo, po)   = I.polar op
            pd         = phaseDiff pc po
            p'         = normPhase $ pc + pd/2
            x'         = fromIntegral x + realToFrac dx / 2
            y'         = fromIntegral y + realToFrac dy / 2
            near       = sum (map (normPhasePi . I.phase . uncurry at)
                         around9cs) / 9
            v          = getPx $ I.mkPolar 1 p'
            -- v          = getPx $ I.mkPolar 1 near
            nears      = map (minPhasePi . flip I.getPxC I.LumaY
                              . phaseDiff' pc . I.phase
                              . uncurry at)
                         -- around9cs
                         [ (  round dy,   round dx)
                         , (  round dx, - round dy)
                         , (- round dx,   round dy)]
            diffmax    = maximum $ map minPhasePi nears
        in  if mc > filledMTh && mo > filledMTh && pd > pi/3
            then
            if diffmax > pi/3
                 then ((Point x' y', v) : points, Point x' y' : ends)
                 else
                     ((Point x' y', v) : points, ends)
            else (points, ends)

    groups :: (Ord a, Num a) => a -> [a] -> Int
    groups r = go 0 []
      where
        go n ts []     = n
        go n ts (x:xs) = if any ((<= r) . diff x) ts
                         then go n ts xs
                         else go (n+1) (x:ts) xs

    diff :: Num a => a -> a -> a
    diff x y = abs (x - y)

    -- minPhasePi :: Double -> Double
    minPhasePi x = min x (abs $ pi - x)


endpoints :: KdMap Float FPoint (Complex Double) -> [FPoint]
endpoints kdm = KdM.foldrWithKey f [] kdm
  where
    f :: (FPoint, Complex Double)
      -> [FPoint] -> [FPoint]
    f (p, v) ps =
        let pv    = phase' v
            x I.:+ y = v
            nears = map (direction p pv)
                  $ KdM.inRadius kdm 2 p
            nlen  = fromIntegral $ length nears
            -- diff  = max $ map (abs . phaseDiff' (phase' v)) nears
            nexts  = map (phaseP . fst) nears
            isEnd = not
                  $ any (phaseD (> pi/2) pv) nexts
                 && any (phaseD (< pi/2) pv) nexts
            diffs  = sum $ map (minPhasePi . snd) nears
            p' = p ^+^ Point (realToFrac x / 2) (realToFrac y / 2)
        in if diffs / nlen > pi/6 || isEnd then p':ps else ps
        -- in (p ^+^ Point (realToFrac x / 2) (realToFrac y / 2)):ps
        -- in (p ^+^ Point (round $ x * 4) (round $  y * 4)) : ps

    phaseD f pv p = p /= 0
                    && (f . abs $ phaseDiff' (realToFrac pv) p)

    direction :: FPoint -> Double -> (FPoint, Complex Double)
              -> (FPoint, Double)
    direction cp pv (p, v) =
        let dp = p ^-^ cp
            ph = normPhasePi $ phase' v - pv
        in  (dp, ph)
        -- in  if pointY dp < 0 then ph - pi else ph
        -- ph * fromIntegral (signum $ pointY dp)

    minPhasePi :: Double -> Double
    minPhasePi x = min x (abs $ pi - x)

    groups :: RealFrac a => a -> [a] -> Int
    groups torelance = Set.size . Set.fromList . map (round . (/ torelance))

togetherNears :: [FPoint] -> [FPoint]
togetherNears ps = map (close . close . close) ps
  where
    points = KdT.build asList ps

    together :: FPoint -> Bool
    together p = let nears = KdT.kNearest points 4 p
                 in  not $ any (inRange p) nears

    close :: FPoint -> FPoint
    close p = let nears = filter (inRange p) $ KdT.kNearest points 4 p
              in  sumV nears ^/ fromIntegral (length nears)



    inRange :: FPoint -> FPoint -> Bool
    inRange cp p = let rp      = p ^-^ cp
                       (m, ph0) = polarP rp
                       (_, ph1) = polarP $ negateV rp
                   in  m + ph0 < pi && abs ph0 < pi/2
                    || m + ph1 < pi && abs ph1 < pi/2

lines :: KdMap Int FPoint (Complex Double)
      -> Set.Set FPoint -> Seq [FPoint]
lines points ends = foldMap line ends
  where
    f :: (FPoint, Complex Double)
      -> (Set.Set FPoint, [[FPoint]])
      -> (Set.Set FPoint, [[FPoint]])
    f (p, v) (pset, pss) = (pset, pss)

    line :: FPoint -> Seq [FPoint]
    line p = undefined

    walk :: FPoint -> Float -> [FPoint]
    walk p ph = undefined

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


