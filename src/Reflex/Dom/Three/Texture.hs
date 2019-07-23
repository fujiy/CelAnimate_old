{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Texture where


import           Data.Text                          (Text)
import           GHCJS.DOM.Types                    hiding (Text)
import           Language.Javascript.JSaddle.Object

import           Reflex.Dom.Three.Monad

newtype Texture = Texture JSVal deriving (ToJSVal)

dataTexture :: MonadJSM m
            => Uint8Array -- ^ Data
            -> Int               -- ^ Width
            -> Int               -- ^ Height
            -> m Texture
dataTexture d w h = do
    typ <- liftJSM $ three ! "RGBAFormat"
    t <- liftJSM $ new (three ! "DataTexture") (d, w, h, typ)
    liftJSM $ (t <# "needsUpdate") True
    return $ Texture t
