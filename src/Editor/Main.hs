module Editor.Main where


import           JSDOM.Types
import           Reflex.Dom

import           Editor.Canvas

editor :: Widget x ()
editor = el "div" $ do
    el "h1" $ text "Cel"
    canvas


frame :: Widget x ()
frame = el "div" blank
