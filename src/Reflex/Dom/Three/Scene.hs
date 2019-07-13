{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Scene where


import           Control.Monad.Reader
import           Data.Text                          (Text)
import           Language.Javascript.JSaddle.Object
import           Language.Javascript.JSaddle.Types
import           Reflex.Dom.Three.Camera
import           Reflex.Dom.Three.Monad

newtype Scene = Scene JSVal deriving (ToJSVal)

scene :: ObjectBuilderT t JSM a -> JSM (a, Scene)
scene builder = do
    s <- new (three ! ("Scene" :: Text)) ()
    let env = ObjectBuilderEnv s
    a <- runReaderT (unObjectBuilderT builder) env
    -- s ^. js1t "add" _
    return (a, Scene s)
