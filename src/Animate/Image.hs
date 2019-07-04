
{-# LANGUAGE OverloadedStrings #-}

module Animate.Image where

import           Codec.Picture
import           Codec.Picture.Bitmap
import           Control.Lens
import           Control.Monad.IO.Class
import qualified Data.ByteString                    as BS
import qualified Data.ByteString.Internal           as BS
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Vector.Storable               as V
import           Data.Word
import qualified Foreign                            as F
import qualified GHCJS.Buffer                       as JS
import           GHCJS.DOM
import           GHCJS.DOM.ImageData
import           GHCJS.DOM.Types                    hiding (Text)
import qualified JavaScript.TypedArray.ArrayBuffer  as JS
import           Language.Javascript.JSaddle.Object
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
                    bs    = toByteString $ imageData image

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
