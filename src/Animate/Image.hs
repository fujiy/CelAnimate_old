{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Animate.Image where

import           Codec.Picture                      (imageData)
import           Prelude                            hiding (id, (.))
-- import           Codec.Picture.Bitmap
import           Codec.Picture.Types                (promoteImage)
-- import           Control.Lens
import           Control.Category
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Internal           as BS
import           Data.Int
import           Data.Maybe
import qualified Data.Sequence                      as S
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Vector.Storable               ((!), (!?))
import qualified Data.Vector.Storable               as V
import qualified Data.Vector.Storable.Mutable       as MV
import           Data.Word
import           Debug.Trace
import qualified Foreign                            as F
import qualified GHCJS.Buffer                       as JS
import           GHCJS.DOM
import           GHCJS.DOM.ImageData
import           GHCJS.DOM.Types                    hiding (Text)
import           Graphics.Image                     (Complex, Image, MImage,
                                                     Pixel, RGBA, VS, Y)
import qualified Graphics.Image                     as I
import qualified Graphics.Image.Interface           as I
import qualified Graphics.Image.IO                  as I
import qualified JavaScript.TypedArray.ArrayBuffer  as JS
import           Language.Javascript.JSaddle.Object hiding ((!))
import           Language.Javascript.JSaddle.Types

import           Editor


instance Editable (Image VS RGBA Double) where
    open path = do
        e <- liftIO $ I.readImage path
        case e of
            Left s -> do
                liftIO $ putStrLn s
                return Nothing
            Right (image :: Image VS RGBA Double) ->
                let image' = getCenters $ I.toImageY image
                in  return $ Just $ I.toImageRGBA image'
                -- let image = convertRGBA8 di
                --     image' = promoteImage $
                --              getCenters 128 $ extractLumaPlane image
                --              :: Image PixelRGBA8

    save path image = do
        return ()

toImageData :: MonadJSM m => Image VS RGBA Double -> m ImageData
toImageData image = do
    let jp = I.toJPImageRGBA8 $ I.toWord8I image
        bs = toByteString $ imageData jp

    buf <- bsToArrayBuffer bs

    imageJS <-
        liftDOM (new (jsg ("Uint8ClampedArray" :: Text)) [buf])
        >>= unsafeCastTo Uint8ClampedArray

    newImageData imageJS
        (fromIntegral $ I.cols image)
        (Just . fromIntegral $ I.rows image)


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

-- type STImage arr cs e = MImage ST arr cs e

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

readBorder :: (Functor m, PrimMonad m, I.MArray arr cs e)
           => MImage (PrimState m) arr cs e -> (Int, Int) -> m (Pixel cs e)
readBorder image (y, x) =
    let (h, w) = I.mdims image
        y'     = min h $ max 0 y
        x'     = min w $ max 0 x
    in  I.read image (y', x')

getCenters :: Image VS Y Double -> Image VS I.HSI Double
getCenters image =
    let image0 =
            edges
            $ I.applyFilter (I.gaussianBlur 1.0)
            image
        image1 = runST $ do
            mimg <- I.thaw image0
            normalize mimg
            fills mimg
            I.freeze mimg
    in  I.map (compHSI) $ image1
  where

    -- clamp = fmap (\x -> x / 2 + 0.5)

    compHSI :: Pixel Y (Complex Double) -> Pixel I.HSI Double
    compHSI pix = I.fromComponents
        ( clamp (I.getPxC (I.phase pix) I.LumaY) / (2 * pi)
        , 1
        , I.getPxC (I.magnitude pix) I.LumaY / 2)

    clamp x | x < 0     = x + 2*pi
            | otherwise = x

filledMTh, hideCoef :: Pixel Y Double
filledMTh = 0.5
hideCoef  = 1 / 16

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
        in  rx I.+: ry

fills :: forall s. MImage s VS Y (Complex Double) -> ST s ()
fills image = do
    upd <- foldIM fill False image
    normalize image

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
               && abs (phaseDiff pc dp) < pi / 3
               && not (dx == 0 && dy == 0)
               && not (am > filledMTh * hideCoef
                       && abs (phaseDiff pc ap) > pi / 2)
            then do I.write image (clamps (I.mdims image) i) rp'
                    return True
            else return upd')
            upd around9csp
        else return upd

normalize :: forall s. MImage s VS Y (Complex Double) -> ST s ()
normalize image = mapIM_ (\i cp -> do
    let (m, p) = I.polar cp
        cp' = if m > filledMTh * hideCoef then I.mkPolar 1.0 p else 0
    I.write image i cp')
    image

phaseDiff :: Pixel Y Double -> Pixel Y Double -> Pixel Y Double
phaseDiff x y = let r = x - y
                in if | r >  pi   -> r - 2 * pi
                      | r < -pi   -> r + 2 * pi
                      | otherwise -> r

clamps :: (Int, Int) -> (Int, Int) -> (Int, Int)
clamps (h, w) (y, x) = (max 0 $ min (h - 1) y, max 0 $ min (w - 1) x)


around9csp :: [(Int, Int, Pixel Y Double)]
around9csp = zip3
    (replicate 3 (-1) <> replicate 3 0 <> replicate 3 1)
    (concat $ replicate 3 [-1, 0, 1])
    [-pi * 3 / 4, -pi / 2, -pi / 4,
      pi,      0,       0,
      pi * 3 / 4,  pi / 2,  pi / 4]

between :: Ord a => a -> a -> a -> Bool
between mi mx x = mi <= x && x < mx

    -- around9 :: V.Storable a => V.Vector a -> Int -> V.Vector a
    -- around9 v i = V.backpermute v $ V.fromList (around9is i)

    -- around9m :: V.Storable a => MV.MVector s a -> Int -> ST s (V.Vector a)
    -- around9m mv i = V.unfoldrM (f mv) $ around9is i
    --   where
    --     f mv []     = return Nothing
    --     f mv (i:is) = do
    --         r <- MV.read mv i
    --         return $ Just (r, is)

    -- around9is i = [i + x + y | y <- [-width, 0, width],
    --                            x <- [    -1, 0,     1]]

    -- around9cs :: [(Int, Int)]
    -- around9cs = [(x, y) | y <- [-width, 0, width],
    --                       x <- [    -1, 0,     1]]

    -- around9csp :: [(Int, Int, Float)]
    -- around9csp =
    --     zip3 (concat $ replicate 3 [-1, 0, 1])
    --          (replicate 3 (-width) <> replicate 3 0 <> replicate 3 width)
    --          [-pi * 3 / 4, -pi / 2, -pi / 4,
    --                     pi,      0,       0,
    --            pi * 3 / 4,  pi / 2,  pi / 4]



    -- around4 :: V.Storable a => V.Vector a -> Int -> V.Vector a
    -- around4 v i = V.backpermute v is
    --   where
    --     is = V.fromList [i + x + y | y <- [-width, width],
    --                                  x <- [    -1, 1]]
