{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Editor.Canvas where

import qualified Clay.Color                          as C
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Word
import           Debug.Trace
import           Geom2D
import           GHCJS.DOM                           hiding (Function)
import           GHCJS.DOM.CanvasRenderingContext2D
import           GHCJS.DOM.HTMLCanvasElement
import           GHCJS.DOM.ImageData
import           GHCJS.DOM.Types                     hiding (Function, Text)
import           GHCJS.DOM.WebGLRenderingContextBase
import qualified Graphics.Image                      as I
import           Language.Javascript.JSaddle.Object  hiding (Function (..))
import           Linear.V3
import           Reflex.Dom

import           Editor
import           Editor.Animate.Image
import           Editor.Node.Vectorize
import           Reflex.Dom.Three

canvas :: Int -- ^ width
       -> Int -- ^ height
       -> Widget x ()
canvas width height = do
    (e, _) <- elAttr' "canvas" ("width"  =: tshow width <>
                                "height" =: tshow height) blank
    (cm, sc) <- liftJSM $ scene $ do
            -- geo <- lift $ boxGeometory 100 100 100
            -- mtr <- lift $ meshBasicMaterial 0xFFFFFF
            -- mesh geo mtr

        -- mimage <- liftJSM $ open "./sample/thinning.jpg"
        -- mimage <- liftJSM $ open "./sample/thinning2.png"
        mimage <- liftJSM $ open "./sample/sample.png"
        let image = fromJust mimage :: RGBAImage
            (h, w) = I.dims image
            (im, pts, ends) = getCenters image
            ps    = (\(Point x y) ->
                         V3 (x - fromIntegral w/2)
                         (y - fromIntegral h/2) 1)
                    <$> pts

        forM (ends) $ \eps -> do
            let es = (\(Point x y) ->
                          V3 (x - fromIntegral w/2)
                         (y - fromIntegral h/2) 2)
                    <$> eps
                V3 a b c = head es
                col = C.toRgba $
                      C.hsl (abs (round (a * b * 5700)) `mod` 256) 1.0 0.5
                -- col = C.rgb (abs (round a * 100) `mod` 255)
                --       (abs (round b * 100) `mod` 255) 255
            gmt <- geometory es
            mtr <- pointsMaterial def { _color = col, _size = 1 }
            points gmt mtr




        gmt <- geometory ps
        mtr <- pointsMaterial def { _color = C.gray, _size = 1 }
        points gmt mtr



        -- viewer im
        ambientLight 0xFFFFFF 1.0
        perspectiveCamera 45 1 1 1000
            -- orthographicCamera 10 10 10 10 1 100
    render e cm sc

tshow :: Show a => a -> Text
tshow = T.pack . show

webGLCanvas :: Widget x ()
webGLCanvas = do

    (e, _) <- elAttr' "canvas" ("width" =: "1000" <> "height" =: "1000") blank
    cv :: HTMLCanvasElement <- unsafeCastTo HTMLCanvasElement $ _element_raw e
    -- ctx <- getContextUnsafe cv ("2d" :: Text) ([] :: [()])
    --    >>= unsafeCastTo CanvasRenderingContext2D
    gl <- getContextUnsafe cv ("webgl" :: Text) ([] :: [()])
        >>= unsafeCastTo WebGLRenderingContext

    w <- getDrawingBufferWidth gl
    h <- getDrawingBufferHeight gl

    program <- createProgram gl

    vertexShader <- createShader gl VERTEX_SHADER
    shaderSource  gl (Just vertexShader) vertexShaderProgram
    compileShader gl (Just vertexShader)

    fragmentShader <- createShader gl FRAGMENT_SHADER
    shaderSource  gl (Just fragmentShader) fragmentShaderProgram
    compileShader gl (Just fragmentShader)

    attachShader gl (Just program) (Just vertexShader)
    attachShader gl (Just program) (Just fragmentShader)
    linkProgram gl $ Just program


    positionLocation <-
        getAttribLocation gl (Just program) ("a_position" :: Text)
    texCoordLocation <-
        getAttribLocation gl (Just program) ("a_texCoord" :: Text)

    positionBuffer <- createBuffer gl
    bindBuffer gl ARRAY_BUFFER $ Just positionBuffer

    array <- fmap (uncheckedCastTo ArrayBuffer) $
        liftDOM (new (jsg ("Float32Array" :: Text))
            [[ -1.0,  -1.0,
               1.0,  -1.0,
               -1.0,  1.0,
               -1.0,  1.0,
               1.0,  -1.0,
               1.0,  1.0 :: Double]])
        >>= unsafeCastTo Float32Array
    bufferData gl ARRAY_BUFFER (Just array) STATIC_DRAW


    texCoordBuffer <- createBuffer gl
    bindBuffer gl ARRAY_BUFFER $ Just texCoordBuffer
    texCoord <- fmap (uncheckedCastTo ArrayBuffer) $
        liftDOM (new (jsg ("Float32Array" :: Text))
            [[ 0.0,  1.0,
               1.0,  1.0,
               0.0,  0.0,
               0.0,  0.0,
               1.0,  1.0,
               1.0,  0.0 :: Double]])
        >>= unsafeCastTo Float32Array
    bufferData gl ARRAY_BUFFER (Just texCoord) STATIC_DRAW

    texture <- createTexture gl
    bindTexture gl TEXTURE_2D $ Just texture
    texParameteri gl TEXTURE_2D TEXTURE_WRAP_S CLAMP_TO_EDGE
    texParameteri gl TEXTURE_2D TEXTURE_WRAP_T CLAMP_TO_EDGE
    texParameteri gl TEXTURE_2D TEXTURE_MIN_FILTER NEAREST
    texParameteri gl TEXTURE_2D TEXTURE_MAG_FILTER NEAREST
    bindTexture gl TEXTURE_2D $ Just texture

    image <- liftJSM $ open "./sample/thinning.jpg"
    -- image <- liftJSM $ open "./sample/thinning2.png"
    imageData :: ImageData <- toImageData $ fromJust image

    -- image <- liftDOM (new (jsg ("Uint8ClampedArray" :: Text))
    --     [[255,   0,   0, 255, --
    --         0, 255,   0, 255,
    --         0,   0, 255, 255,
    --         0,   0,   0, 255,
    --       255, 255, 255, 255, --
    --         0,   0, 255, 255,
    --         0, 255,   0, 255,
    --       255,   0,   0, 255,
    --         0,   0,   0, 255, --
    --         0, 255, 255, 255,
    --       255,   0, 255, 255,
    --       255, 255,   0, 255,
    --       255, 255,   0, 255, --
    --       255,   0, 255, 255,
    --         0, 255, 255, 255,
    --       255, 255, 255, 255 :: Word8]])
    --     >>= unsafeCastTo Uint8ClampedArray
    -- imageData <- newImageData image 4 (Just 4)

    texImage2D gl TEXTURE_2D 0 RGBA RGBA UNSIGNED_BYTE $ Just imageData


    clearColor gl 0 0 0 1.0
    clear gl COLOR_BUFFER_BIT


    useProgram gl $ Just program

    enableVertexAttribArray gl (fromIntegral positionLocation)
    bindBuffer gl ARRAY_BUFFER $ Just positionBuffer
    vertexAttribPointer gl (fromIntegral positionLocation) 2 FLOAT False 0 0


    enableVertexAttribArray gl (fromIntegral texCoordLocation)
    bindBuffer gl ARRAY_BUFFER $ Just texCoordBuffer
    vertexAttribPointer gl (fromIntegral texCoordLocation) 2 FLOAT False 0 0

    windowSizeLocation <-
        getUniformLocation gl (Just program) ("u_windowSize" :: Text)
    uniform2f gl (Just windowSizeLocation) (fromIntegral w) (fromIntegral h)



    drawArrays gl TRIANGLES 0 6

    blank


vertexShaderProgram :: Text
vertexShaderProgram =
    "attribute vec2 a_position;\
    \attribute vec2 a_texCoord;\
    \varying vec2 v_texCoord;\
    \void main() {\
    \  gl_Position = vec4(a_position, 0, 1);\
    \  v_texCoord = a_texCoord;\
    \}"

fragmentShaderProgram :: Text
fragmentShaderProgram =
    "precision mediump float;\
    \uniform sampler2D u_image;\
    \varying vec2 v_texCoord;\
    \void main(void) {\
    \  gl_FragColor = texture2D(u_image, v_texCoord);\
    \}"
