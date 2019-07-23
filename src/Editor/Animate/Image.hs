{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Editor.Animate.Image where

import           Codec.Picture                      (imageData)
import           Codec.Picture.Types                (promoteImage)
import           Control.Category
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Primitive
import           Control.Monad.ST
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Internal           as BS
import           Data.Default
import           Data.Int
import qualified Data.KdMap.Static                  as KdM
import           Data.Maybe
import qualified Data.Sequence                      as S
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import           Data.Vector.Storable               ((!), (!?))
import qualified Data.Vector.Storable               as V
import qualified Data.Vector.Storable.Mutable       as MV
import           Data.Word
import           Debug.Trace
import           Geom2D
import qualified GHCJS.Buffer                       as JS
import           GHCJS.DOM.ImageData
import           GHCJS.DOM.Types                    hiding (Text)
import           Graphics.Image                     (Complex, Image, MImage,
                                                     Pixel, RGBA, VS, Y)
import qualified Graphics.Image                     as I
import qualified Graphics.Image.Interface           as I
import qualified Graphics.Image.IO                  as I
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types
import           Prelude                            hiding (id, map, (.))

import           Editor
import           Reflex.Dom.Three

type RGBAImage = Image VS RGBA Double

instance Editable RGBAImage where
    type Viewer RGBAImage = Object3D

    open path = do
        e <- liftIO $ I.readImage path
        case e of
            Left s -> do
                liftIO $ putStrLn s
                return Nothing
            Right (image :: Image VS RGBA Double) ->
                -- let image' = getCenters $ I.toImageY image
                -- in
                return $ Just $ I.toImageRGBA image
                -- let image = convertRGBA8 di
                --     image' = promoteImage $
                --              getCenters 128 $ extractLumaPlane image
                --              :: Image PixelRGBA8

    viewer image = do
        let (h, w) = I.dims image
        geo <- lift $ planeGeometory (fromIntegral w) (fromIntegral h)
        arr <- imageToArray image
        tex <- lift $ dataTexture arr w h
        mtr <- lift $ meshBasicMaterial (def & map .~ Just tex)
        mesh geo mtr

    save path image = do
        return ()

imageToArray :: MonadJSM m => RGBAImage -> m Uint8Array
imageToArray image = do
    let jp = I.toJPImageRGBA8 $ I.toWord8I image
        bs = toByteString $ imageData jp

    buf <- bsToArrayBuffer bs

    liftDOM (new (jsg "Uint8Array") [buf])
        >>= unsafeCastTo Uint8Array


toImageData :: MonadJSM m => Image VS RGBA Double -> m ImageData
toImageData image = do
    let jp = I.toJPImageRGBA8 $ I.toWord8I image
        bs = toByteString $ imageData jp

    buf <- bsToArrayBuffer bs

    imageJS <-
        liftDOM (new (jsg "Uint8ClampedArray") [buf])
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
