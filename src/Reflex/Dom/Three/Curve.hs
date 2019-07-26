{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Reflex.Dom.Three.Curve where


import           Data.Text                          (Text)
import           Geom2D
import           Geom2D.CubicBezier
import           JSDOM.Types                        hiding (Text)
import           Language.Javascript.JSaddle.Object

import           Reflex.Dom.Three.Monad

newtype Curve = Curve JSVal deriving (ToJSVal)

instance ToJSVal a => ToJSVal (Point a) where
    toJSVal (Point x y) = new (three ! "Vector2") (x, y)


cubicBezier :: MonadJSM m => CubicBezier Float -> m Curve
cubicBezier (CubicBezier c0 c1 c2 c3) =
    Curve <$> liftJSM (new (three ! "CubicBezierCurve") (c0, c1, c2, c3))
