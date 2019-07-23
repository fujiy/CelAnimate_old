{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}

module Reflex.Dom.Three.Renderer where

import           Control.Lens
import           Data.Text                          (Text)
import           JSDOM.Types                        hiding (Element, Text)
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate
import           Reflex.Dom.Widget.Basic

import           Reflex.Dom.Three.Camera
import           Reflex.Dom.Three.Monad
import           Reflex.Dom.Three.Scene


render :: (MonadJSM m, DomBuilder t m)
       => Element EventResult GhcjsDomSpace t -- ^ canvas element
       -> Camera -> Scene -> m ()
render elm cm sc = liftJSM $ do
    let elv = _element_raw elm :: RawElement GhcjsDomSpace

    options <- obj
    (options <# "canvas") elv
    renderer <- new (three ! "WebGLRenderer") [options]


    -- renderer ^. js2 "setSize" "1500" "1500"
    renderer ^. js2 "render" sc cm

    return ()
