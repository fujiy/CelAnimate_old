{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Geometory where

import           Data.Text                          (Text)
import           JSDOM.Types                        hiding (Text)
import           Language.Javascript.JSaddle.Object

import           Reflex.Dom.Three.Monad

newtype Geometory = Geometory JSVal deriving (ToJSVal)



boxGeometory :: MonadJSM m
             => Float -- ^ Width
             -> Float -- ^ Height
             -> Float -- ^ Depth
             -> m Geometory
boxGeometory w h d =
    Geometory <$> liftJSM (new (three ! ("BoxGeometry" :: Text)) [w, h, d])
