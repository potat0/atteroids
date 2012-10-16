import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))
import Data.IORef
import Control.Monad
import System.Environment (getArgs, getProgName)
import Data.Vector.Storable
import Data.Word

gen :: Int -> Word8
gen i = if mod i 3 == 0 then 255 else 0
tex = generate (256 * 256 * 3) gen

initialize = do
    [buffer] <- GL.genObjectNames 1
    GL.textureBinding GL.Texture2D $= Just buffer

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    generateMipmap GL.Texture2D $= GL.Enabled

    unsafeWith tex $ \ptr ->
        GL.texImage2D 
            Nothing 
            NoProxy 
            0 
            GL.RGB' 
            (GL.TextureSize2D 256 256) 
            0 
            (GL.PixelData GL.RGB GL.UnsignedByte ptr)

    errors <- get GL.errors
    when (errors /= []) $
        putStrLn $ show errors

    render buffer

render buffer = do
    GL.clear [GL.ColorBuffer]
    GL.textureBinding GL.Texture2D $= Just buffer
    GL.renderPrimitive GL.Quads $ do
        GL.texCoord $ texCoord2 0 0
        GL.vertex   $ vertex3 0 256 0
        GL.texCoord $ texCoord2 0 1
        GL.vertex   $ vertex3 (0) (0) 0
        GL.texCoord $ texCoord2 1 1
        GL.vertex   $ vertex3 256 (0) 0
        GL.texCoord $ texCoord2 1 0
        GL.vertex   $ vertex3 256 256 0

    errors <- get GL.errors
    when (errors /= []) $
        putStrLn $ show errors

    GLFW.swapBuffers

    windowOpen <- GLFW.getParam GLFW.Opened
    esc <- GLFW.getKey GLFW.ESC
    unless (esc == GLFW.Press || windowOpen == False) $
        render buffer


main =
    let windowWidth = 800
        windowHeight = 600
    in do
        GLFW.initialize
        GLFW.openWindowHint NoResize True
        GLFW.openWindow (GL.Size windowWidth windowHeight) [GLFW.DisplayAlphaBits 8] GLFW.Window
        GLFW.windowTitle    $= "GLFW Demo"
        GL.shadeModel       $= GL.Smooth
        GL.clearColor       $= GL.Color4 0 0 0 1
        GL.blend            $= GL.Enabled
        GL.blendFunc        $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)

        GL.texture GL.Texture2D $= GL.Enabled

        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (fromIntegral windowWidth) 0 (fromIntegral windowHeight)

        GL.matrixMode $= GL.Modelview 0
        GL.loadIdentity

        Main.initialize

        GLFW.terminate

-- type signatures to avoid ambiguity
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

texCoord2 :: GLfloat -> GLfloat -> GL.TexCoord2 GLfloat
texCoord2 = GL.TexCoord2

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3
