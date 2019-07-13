{-# LANGUAGE TemplateHaskell #-}

module Reflex.Dom.Three.Lib where

import           Control.Monad.IO.Class
import           Data.FileEmbed
import           Data.Text
import           Language.Javascript.JSaddle.Evaluate
import           Language.Javascript.JSaddle.Marshal.String
import           Language.Javascript.JSaddle.Types
import           Language.Javascript.JSaddle.Value

useThree :: JSM ()
useThree = do
    -- eval ($(embedStringFile "three.js/build/three.min.js") :: Text)
    liftIO (readFile "three.js/build/three.min.js") >>= eval
    return ()
