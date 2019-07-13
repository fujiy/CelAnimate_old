module Editor.Main where


import           JSDOM.Types
import           Reflex.Dom

import           Editor.Canvas
import           Reflex.Dom.Three

editor :: Widget x ()
editor = el "div" $ do
    liftJSM useThree
    el "h1" $ text "Cel"
    canvas



frame :: Widget x ()
frame = el "div" blank
