
module Reflex.Dom.Three.Color
    ( Color
    ) where

import           Clay.Color
import           Clay.Property
import           Data.Default
import           GHCJS.Marshal.Internal
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types

import           Reflex.Dom.Three.Monad

instance ToJSVal Color where
    toJSVal c = toJSVal . plain . unValue $ value c

instance Default Color where
    def = white
