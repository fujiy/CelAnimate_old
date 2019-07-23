{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Geometory where

import           Data.Text                          (Text)
import           JSDOM.Types                        hiding (Text)
import           Language.Javascript.JSaddle.Object
import           Linear.V3

import           Reflex.Dom.Three.Monad

newtype Geometory = Geometory JSVal deriving (ToJSVal)

instance ToJSVal a => ToJSVal (V3 a) where
    toJSVal (V3 x y z) = new (three ! "Vector3") (x, y, z)

geometory :: MonadJSM m
          => [V3 Float] -- ^ Vertices
          -> m Geometory
geometory vs = do
    g <- liftJSM (new (three ! "Geometry") ())
    liftJSM $ (g <# "vertices") vs
    return $ Geometory g

boxGeometory :: MonadJSM m
             => Float -- ^ Width
             -> Float -- ^ Height
             -> Float -- ^ Depth
             -> m Geometory
boxGeometory w h d =
    Geometory <$> liftJSM (new (three ! "BoxGeometry") (w, h, d))

planeGeometory :: MonadJSM m
             => Float -- ^ Width
             -> Float -- ^ Height
             -> m Geometory
planeGeometory w h =
    Geometory <$> liftJSM (new (three ! "PlaneGeometry") (w, h))

