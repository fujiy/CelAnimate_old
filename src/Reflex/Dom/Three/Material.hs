{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Material where


import           Data.Text                          (Text)
import           Language.Javascript.JSaddle.Object

import           Reflex.Dom.Three.Monad

newtype Material = Material JSVal deriving (ToJSVal)



meshBasicMaterial :: MonadJSM m
                  => Int -- ^ color
                  -> m Material
meshBasicMaterial c =
    Material <$> liftJSM (new (three ! ("MeshNormalMaterial" :: Text)) ())
