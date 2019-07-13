{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Light where

import           Data.Text                          (Text)
import           JSDOM.Types                        hiding (Text)
import           Language.Javascript.JSaddle.Object

import           Reflex.Dom.Three.Monad

newtype Light = Light JSVal deriving (ToJSVal)

ambientLight :: MonadJSM m
             => Int -- ^ color
             -> Float -- ^ intensity
             -> ObjectBuilderT t m Light
ambientLight c i = do
    l <- liftJSM $ new (three ! ("AmbientLight" :: Text)) (c, i)
    addParent l
    return $ Light l
