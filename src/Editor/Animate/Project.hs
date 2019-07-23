
module Editor.Animate.Project where

data Project

data Process = Process
    { layers :: [Layer] }

data Layer = Layer
    { timeline :: Timeline }

type Timeline = [KeyFrame]

data KeyFrame

type FrameTime = Int
