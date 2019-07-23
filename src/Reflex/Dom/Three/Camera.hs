{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Camera where

import           Control.Lens                       hiding (( # ))
import           Data.Text                          (Text)
import           JSDOM.Types                        hiding (Text)
import           Language.Javascript.JSaddle.Object

import           Reflex.Dom.Three.Monad


newtype Camera = Camera JSVal deriving (ToJSVal)

orthographicCamera :: MonadJSM m
                   => Float -- ^ left
                   -> Float -- ^ right
                   -> Float -- ^ top
                   -> Float -- ^ bottom
                   -> Float -- ^ near
                   -> Float -- ^ far
                   -> ObjectBuilderT t m Camera
orthographicCamera l r t b n f = do
    c <- liftJSM $ new (three ! "OrthographicCamera")
        [l, r, t, b, n, f]
    addParent c
    return $ Camera c

perspectiveCamera :: MonadJSM m
                  => Float -- ^ fov
                  -> Float -- ^ aspect
                  -> Float -- ^ near
                  -> Float -- ^ far
                  -> ObjectBuilderT t m Camera
perspectiveCamera v a n f = do
    c <- liftJSM $ new (three ! "PerspectiveCamera")
        [v, a, n, f]
    addParent c
    liftJSM $ c ! "position"
        ^. js3 "set" (0 :: Float) (0 :: Float) (500 :: Float)
    return $ Camera c
