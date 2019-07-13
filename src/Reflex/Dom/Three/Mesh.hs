
module Reflex.Dom.Three.Mesh where

import           Data.Text                          (Text)
import           Language.Javascript.JSaddle.Object
import           Reflex.Dom.Three.Geometory
import           Reflex.Dom.Three.Material
import           Reflex.Dom.Three.Monad

mesh :: MonadJSM m => Geometory -> Material -> ObjectBuilderT t m ()
mesh g m = do
    ms <- liftJSM $ new (three ! ("Mesh" :: Text)) (g, m)
    addParent ms
