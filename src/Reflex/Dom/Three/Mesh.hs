
module Reflex.Dom.Three.Mesh where

import           Data.Text                          (Text)
import           Language.Javascript.JSaddle.Object
import           Reflex.Dom.Three.Geometory
import           Reflex.Dom.Three.Material
import           Reflex.Dom.Three.Monad

mesh :: MonadJSM m => Geometory -> Material -> ObjectBuilderT t m ()
mesh g m = do
    ms <- liftJSM $ new (three ! "Mesh") (g, m)
    addParent ms

points :: MonadJSM m => Geometory -> Material -> ObjectBuilderT t m ()
points g m = do
    ms <- liftJSM $ new (three ! "Points") (g, m)
    addParent ms

line :: MonadJSM m => Geometory -> Material -> ObjectBuilderT t m ()
line g m = do
    ms <- liftJSM $ new (three ! "Line") (g, m)
    addParent ms
