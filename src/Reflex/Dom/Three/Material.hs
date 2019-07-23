{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Reflex.Dom.Three.Material where

import           Control.Lens
import           Data.Default
import           Data.Text                          (Text)
import           GHC.Generics
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types
import           Prelude                            hiding (map)

import           Reflex.Dom.Three.Color
import           Reflex.Dom.Three.Monad
import           Reflex.Dom.Three.Texture

newtype Material = Material JSVal deriving (ToJSVal)

data MaterialParams = MaterialParams
    { _color :: Color
    , _map   :: Maybe Texture
    , _size  :: Float }
    deriving (Generic)

instance Default MaterialParams

makeLenses 'MaterialParams

paramToObject :: MaterialParams -> JSM Object
paramToObject mp = do
    o <- obj
    (o <# "color") $ mp ^. color
    (o <# "map")   $ mp ^. map
    (o <# "size")  $ mp ^. size
    return o


meshBasicMaterial :: MonadJSM m
                  => MaterialParams
                  -> m Material
meshBasicMaterial params =
    Material <$> liftJSM (new (three ! "MeshBasicMaterial")
                          [paramToObject params])

pointsMaterial :: MonadJSM m
               => MaterialParams
               -> m Material
pointsMaterial params =
    Material <$> liftJSM (new (three ! "PointsMaterial")
                          [paramToObject params])
