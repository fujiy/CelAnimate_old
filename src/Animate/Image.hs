{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Animate.Image where

import           Codec.Picture
import           Codec.Picture.Bitmap
import           Codec.Picture.Types
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.ST
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Internal           as BS
import           Data.Complex
import           Data.Int
import           Data.Maybe
import qualified Data.Sequence                      as S
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Vector.Storable               as V

import           Data.Vector.Storable               ((!), (!?))
import qualified Data.Vector.Storable.Mutable       as MV
import           Data.Word
import           Debug.Trace
import qualified Foreign                            as F
import qualified GHCJS.Buffer                       as JS
import           GHCJS.DOM
import           GHCJS.DOM.ImageData
import           GHCJS.DOM.Types                    hiding (Text)
import qualified JavaScript.TypedArray.ArrayBuffer  as JS
import           Language.Javascript.JSaddle.Object hiding ((!))
import           Language.Javascript.JSaddle.Types

import           Editor


instance Editable ImageData where
    open path = do
        e <- liftIO $ readImage path
        case e of
            Left s -> do
                liftIO $ putStrLn s
                return Nothing
            Right di -> do
                let image = convertRGBA8 di
                    image' = promoteImage $
                             getCenters 128 $ extractLumaPlane image
                             :: Image PixelRGBA8
                let bs    = toByteString $ imageData image'

                buf <- bsToArrayBuffer bs

                imageJS <-
                    liftDOM (new (jsg ("Uint8ClampedArray" :: Text)) [buf])
                    >>= unsafeCastTo Uint8ClampedArray

                -- imageJS <-
                --     liftDOM (new (jsg ("Uint8ClampedArray" :: Text))
                --              [V.toList $ imageData image])
                --     >>= unsafeCastTo Uint8ClampedArray


                Just <$> newImageData imageJS
                    (fromIntegral $ imageWidth image)
                    (Just . fromIntegral $ imageHeight image)

    save path imageData = do
        return ()

bsToArrayBuffer :: MonadJSM m => BS.ByteString -> m ArrayBuffer
bsToArrayBuffer bs = liftJSM $ do
  (b, off, len) <- ghcjsPure $ JS.fromByteString bs
  ArrayBuffer <$> do
      ghcjsPure (JS.getArrayBuffer b) >>= ghcjsPure . jsval

toByteString :: V.Vector Word8 -> BS.ByteString
toByteString v = BS.PS fp s l
  where (fp, s, l) = V.unsafeToForeignPtr v

-- toBitmap :: Image PixelRGBA8 -> [Word8]
-- toBitmap = foldr' (\(PixelRGBA8 r g b a) s -> r : g : b : a : s) [] . imageData

vectorize :: Image a -> Image a
vectorize = vectorize

getCenters :: Word8 -> Image Pixel8 -> Image Pixel8
getCenters th img =
    let v = V.create $ do
            centers <- MV.new $ width * height
        -- goV centers 0
        -- goH centers 0
            edges vec centers start1 1 1
            normalize' centers 0

            -- forM_ [0..5] $ const $ normalize' centers 0
                   -- fills centers (width + 1) 1 1 False
            -- aligns centers 0
            return centers
    in Image width height $ V.map (clamp' . realPart) v
  where
    width  = traceShowId $ imageWidth  img
    height = traceShowId $ imageHeight img
    vec    = imageData   img
    size   = traceShowId $ V.length vec
    size1  = size - width - 1
    start1 = width + 1
    thf    = fromIntegral th

    clamp' :: Float -> Word8
    clamp' x | x >  254  = 255
             | x < -255  = 0
             | otherwise = truncate (x / 2) - 127

    clamp :: Float -> Word8
    clamp x | x > 255.0 = 255
            | otherwise = truncate x

    edges :: V.Vector Word8 -> MV.MVector s (Complex Float)
          -> Int -> Int -> Int -> ST s ()
    edges vec mv i x y | i >= size1 = return ()
    edges vec mv i x y = do
        let v9   = around9 vec i
            v4   = around4 vec i
            p    = vec ! i
            at n = fromIntegral $ v9 ! n
            rx   = (at 2 + at 5 + at 8 - at 0 - at 3 - at 6) / 3
            ry   = (at 6 + at 7 + at 8 - at 0 - at 1 - at 2) / 3
            -- rx'  = if rx < 0 then -rx else rx
            -- ry'  = if rx < 0 then -ry else ry
            hd   = V.sum (V.map (fromIntegral . abs . (p -)) v4) / 512
            (x', y') = if x < width - 1
                       then (x + 1, y    )
                       else (0,     y + 1)

        MV.write mv i $ (rx * hd) :+ (ry * hd)
        edges vec mv (i + 1) x' y'


    fills :: MV.MVector s (Complex Float)
          -> Int -> Int -> Int -> Bool -> ST s ()
    fills mv i x y upd | i >= size1 = when upd $ normalize mv 0
    fills mv i x y upd = do
        r <- MV.read mv i
        let (rm, rp) = polar r
            rx :+ ry = r
            -- signx    = rx > 0
            -- signy    = ry > 0
            r'       = r / 256
        if rm > 12.8 then do
            upd' <- foldM (\upd' (dx, dy, dp) -> do
                let i' = i + dx + dy
                ra <- MV.read mv i'
                let (am, ap) = polar ra
                if am < 12.8
                   && abs (phaseDiff rp dp) < pi / 4
                   && not (dx == 0 && dy == 0)
                   -- && (signx == (dx > 0) || signy == (dy > 0))
                   && not (am > 0.2 && abs (phaseDiff rp ap) > pi / 2)
                then do MV.modify mv (+ r') i'
                        return True
                else return upd')
                upd around9csp
            next (fills mv) i x y upd'
        else next (fills mv) i x y upd

    next f i x y = if x < width - 1
                   then f (i + 1) (x + 1) y
                   else f (i + 1) 0      (y + 1)

    phaseDiff x y = let r = x - y
                    in if | r >  pi   -> r - 2 * pi
                          | r < -pi   -> r + 2 * pi
                          | otherwise -> r


    normalize :: MV.MVector s (Complex Float)
              -> Int -> ST s ()
    normalize mv i | i >= size = fills mv start1 1 1 False
    normalize mv i = do
        MV.modify mv (\r@(rx :+ ry) ->
            let rm = magnitude r / 256
            in if rm > 0.002 then (rx / rm) :+ (ry / rm) else 0)
                  i
        normalize mv $ i + 1

    normalize' :: MV.MVector s (Complex Float)
              -> Int -> ST s ()
    normalize' mv i | i >= size = fills mv start1 1 1 False
    normalize' mv i = do
        MV.modify mv (\r@(rx :+ ry) ->
            let rm = magnitude r / 256
            in if rm > 0.5 then (rx / rm) :+ (ry / rm) else 0)
                  i
        normalize' mv $ i + 1



    -- fills ::  MV.MVector s (Complex Float)
    --       -> Int -> Int -> Int -> Bool -> ST s ()
    -- fills mv i x y upd | i >= size1 = when upd $ fills mv (width + 1) 1 1 False
    -- fills mv i x y upd = do
    --     r@(rx :+ ry) <- MV.read mv i
    --     if filled r then do
    --         rl <- MV.read mv (i - 1)
    --         rr <- MV.read mv (i + 1)
    --         rt <- MV.read mv (i - width)
    --         rb <- MV.read mv (i + width)
    --         let fl = ry < 0 && not (filled rl)
    --             fr = ry > 0 && not (filled rr)
    --             ft = rx < 0 && not (filled rt)
    --             fb = rx > 0 && not (filled rb)
    --         when fl $ MV.write mv (i - 1)     r
    --         when fr $ MV.write mv (i + 1)     r
    --         when ft $ MV.write mv (i - width) r
    --         when fb $ MV.write mv (i + width) r
    --         next $ fl || fr || ft || fb || upd
    --     else next upd
    --       where
    --         filled :: Complex Float -> Bool
    --         filled r = magnitude r > thf

    --         next upd' = let (x', y') = if x < width - 1
    --                                    then (x + 1, y    )
    --                                    else (0,     y + 1)
    --                     in fills mv (i + 1) x' y' upd

    aligns :: MV.MVector s (Complex Float) -> Int -> ST s ()
    aligns mv i | i >= size = return ()
    aligns mv i = do
        r <- MV.read mv i
        let r' = if realPart r < 0 then -r else r
        MV.write mv i r'
        aligns mv $ i + 1


    around9 :: V.Storable a => V.Vector a -> Int -> V.Vector a
    around9 v i = V.backpermute v $ V.fromList (around9is i)

    around9m :: V.Storable a => MV.MVector s a -> Int -> ST s (V.Vector a)
    around9m mv i = V.unfoldrM (f mv) $ around9is i
      where
        f mv []     = return Nothing
        f mv (i:is) = do
            r <- MV.read mv i
            return $ Just (r, is)

    around9is i = [i + x + y | y <- [-width, 0, width],
                               x <- [    -1, 0,     1]]

    around9cs :: [(Int, Int)]
    around9cs = [(x, y) | y <- [-width, 0, width],
                          x <- [    -1, 0,     1]]

    around9csp :: [(Int, Int, Float)]
    around9csp =
        zip3 (concat $ replicate 3 [-1, 0, 1])
             (replicate 3 (-width) <> replicate 3 0 <> replicate 3 width)
             [-pi * 3 / 4, -pi / 2, -pi / 4,
                        pi,      0,       0,
               pi * 3 / 4,  pi / 2,  pi / 4]



    around4 :: V.Storable a => V.Vector a -> Int -> V.Vector a
    around4 v i = V.backpermute v is
      where
        is = V.fromList [i + x + y | y <- [-width, width],
                                     x <- [    -1, 1]]


    -- go :: MV.MVector s Word8 -> Int -> Int -> Int -> ST s ()
    -- go mv i x y | i >= size = return ()
    -- go mv i x y | vec ! i >= th = do
    --     let (vx, vy) = foldr (\(dx, dy) (rx, ry)->
    --                    let p = fromMaybe 0 $ vec !? (i + dx + dy * width)
    --                    in  if p >= th then (rx + dx, ry + dy) else (rx, ry)
    --                        -- (rx + fromIntegral (dx * fromIntegral p),
    --                        --  ry + fromIntegral (dy * fromIntegral p))
    --                    )
    --                    (0, 0) table
    --         norm     = sqrt (fromIntegral $ vx ^ 2 + vy ^ 2) / 4
    --         dx       = truncate $ fromIntegral vx / norm
    --         dy       = truncate $ fromIntegral vy / norm
    --         (rx, ry) = ray dx dy i x y
    --         cx       = (x + rx) `div` 2
    --         cy       = (y + ry) `div` 2
    --         ci       = cx + cy * width
    --     unless (abs dx < 2 && abs dy < 2) $ MV.write mv ci 255

    --     -- forM_ [-1,0,1] $ \dx ->
    --     --      forM_ [-1,0,1] $ \dy ->
    --     --           case vec !? (i + dx + dy * width) of
    --     --               Just p | p >= th -> return ()
    --     --               _ -> let (rx, ry) = ray (-dx) (-dy) i x y
    --     --                        cx       = (x + rx) `div` 2
    --     --                        cy       = (y + ry) `div` 2
    --     --                        ci       = cx + cy * width
    --     --                    in  MV.write mv ci 255
    --     next mv i x y
    -- go mv i x y = next mv i x y

    table :: [(Int, Int)]
    table = [(x, y) | x <- [-2, -1, 0, 1, 2], y <- [-2, -1, 0, 1, 2]]


    -- next mv i x y =
    --     let (x', y') = if x + 1 < width
    --                    then (x + 1, y    )
    --                    else (0,     y + 1)
    --     in  go mv (i + 1) x' y'


    ray :: Int -> Int -> Int -> Int -> Int -> (Int, Int)
    ray dx dy i x y =
        let x'  = x + dx
            y'  = y + dy
            i'  = i + dx + dy * width
            xh  = x + dx `div` 2
            yh  = y + dy `div` 2
            ih  = i + dx `div` 2 + dy `div` 2 * width
            (x'', xb) = if | x' < 0       -> (0,          True )
                           | x' >= width  -> (width - 1,  True )
                           | otherwise    -> (x',         False)
            (y'', yb) = if | y' < 0       -> (0,          True )
                           | y' >= height -> (height - 1, True )
                           | otherwise    -> (y',         False)
        in if | xb || yb        -> (x'', y'')
              | vec ! i' < th -> if vec ! ih < th
                                   then (xh, yh) else (x', y')
              | otherwise       -> ray dx dy i' x' y'

    goV :: MV.MVector s Word8 -> Int -> ST s ()
    goV mv i | i >= size = return ()
    goV mv i = do
        skipV mv (i + width) i
        goV mv (i + width)


    skipV :: MV.MVector s Word8 -> Int -> Int -> ST s ()
    skipV mv max i | i >= max        = return ()
    skipV mv max i | vec ! i >= th = fillV mv max i (i + 1)
    skipV mv max i = skipV mv max (i + 1)

    fillV :: MV.MVector s Word8 -> Int -> Int -> Int -> ST s ()
    fillV mv max left i | i >= max =
        MV.write mv ((left + i) `div` 2) 100
    fillV mv max left i | vec ! i < th = do
        MV.write mv ((left + i) `div` 2) 100
        skipV mv max (i + 1)
    fillV mv max left i =
        fillV mv max left (i + 1)

    goH :: MV.MVector s Word8 -> Int -> ST s ()
    goH mv i | i >= width = return ()
    goH mv i = do
        skipH mv size i
        goH mv (i + 1)

    skipH :: MV.MVector s Word8 -> Int -> Int -> ST s ()
    skipH mv max i | i >= max        = return ()
    skipH mv max i | vec ! i >= th = fillH mv max i (i + width)
    skipH mv max i = skipH mv max (i + width)

    fillH :: MV.MVector s Word8 -> Int -> Int -> Int -> ST s ()
    fillH mv max top i | i >= max =
        MV.write mv (top + (i - top) `div` (width * 2) * width) 255
    fillH mv max top i | vec ! i < th = do
        MV.write mv (top + (i - top) `div` (width * 2) * width) 255
        skipH mv max (i + width)
    fillH mv max top i =
        fillH mv max top (i + width)


    -- fillV :: MV.MVector s Word8 -> Int -> Int -> ST s ()

around9csp' :: [(Int, Int, Float)]
around9csp' =
        zip3 (concat $ replicate 3 [-1, 0, 1])
             (replicate 3 (-10) <> replicate 3 0 <> replicate 3 10)
             [ pi * 3 / 4,  pi / 2,  pi / 4,
                        pi,      0,       0,
              -pi * 3 / 4, -pi / 2, -pi / 4]
