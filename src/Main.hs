import Codec.Picture
import Control.Monad
import Data.IORef
import Data.Vector.Storable
import Data.Array.Storable
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Foreign.Storable
import Graphics.Rendering.OpenGL.GL.VertexArrays as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.UI.GLFW as GLFW
import Paths_atteroids
import System.Environment (getArgs, getProgName)

gen :: Int -> Word8
gen i = if mod i 3 == 0 then 255 else 0
tex = generate (256 * 256 * 3) gen

updateTexture (Image width height imageData) = unsafeWith imageData $ \ptr ->
    GL.texImage2D 
        Nothing 
        NoProxy 
        0 
        GL.RGB' 
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 
        0 
        (GL.PixelData GL.RGB GL.UnsignedByte ptr)

--newtype VertexBufferObject = VertexBufferObject GL.BufferObject
--unVertexBufferObject (VertexBufferObject internalBuffer) =
--    internalBuffer
--
--type Sprite = (VertexBufferObject GL.BufferObject GL.BufferObject)

name = "256.png"

fillBuffer :: BufferObject -> [Float] -> IO ()
fillBuffer bufferObject vertexData = do
    GL.bindBuffer GL.ArrayBuffer $= Just bufferObject
    --GL.bufferData GL.ArrayBuffer $= (fromIntegral size, unsafeForeignPtrToPtr ptr, GL.StaticDraw)
    arr <- newListArray (0, Prelude.length vertexData) vertexData
    withStorableArray arr (\ptr -> 
        GL.bufferData GL.ArrayBuffer $= (fromIntegral $ (Prelude.length vertexData) * (sizeOf (0 :: Float)), ptr, GL.StaticDraw))

vertexCoords :: [Float]
vertexCoords = [
    0, 256,
    0, 0,
    256, 0,
    256, 256]

textureCoords :: [Float]
textureCoords = [
        0, 0,
        0, 1,
        1, 1,
        1, 0]

loadTexture = do
    [tex] <- GL.genObjectNames 1

    GL.textureBinding GL.Texture2D $= Just tex

    GL.textureWrapMode GL.Texture2D GL.S $= (GL.Repeated, GL.Repeat)
    GL.textureWrapMode GL.Texture2D GL.T $= (GL.Repeated, GL.Repeat)
    GL.textureFilter GL.Texture2D $= ((GL.Nearest, Nothing), GL.Nearest)

    generateMipmap GL.Texture2D $= GL.Enabled

    dataPath <- getDataFileName name
    readResult <- readImage dataPath
    case readResult of
        Left msg    -> putStrLn msg
        Right (ImageRGB8 image) -> updateTexture image
    return tex

initialize = do
    [vertexBuffer, textureCoordBuffer] <- GL.genObjectNames 2


    fillBuffer vertexBuffer vertexCoords
    fillBuffer textureCoordBuffer textureCoords

    tex <- loadTexture

    GL.textureBinding GL.Texture2D $= Just tex

    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    GL.arrayPointer GL.VertexArray $= VertexArrayDescriptor 2 GL.Float 0 nullPtr
    GL.clientState GL.VertexArray $= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer $= Just textureCoordBuffer
    GL.arrayPointer GL.TextureCoordArray $= VertexArrayDescriptor 2 GL.Float 0 nullPtr
    GL.clientState GL.TextureCoordArray $= GL.Enabled

    errors <- get GL.errors
    when (errors /= []) $
        putStrLn $ show errors

    render vertexBuffer tex

render vertexBuffer tex = do
    GL.clear [GL.ColorBuffer]

    GL.drawArrays GL.Quads 0 4
    --GL.renderPrimitive GL.Quads $ do
    --    GL.texCoord $ texCoord2 0 0
    --    GL.vertex   $ vertex3 0 256 0
    --    GL.texCoord $ texCoord2 0 1
    --    GL.vertex   $ vertex3 (0) (0) 0
    --    GL.texCoord $ texCoord2 1 1
    --    GL.vertex   $ vertex3 256 (0) 0
    --    GL.texCoord $ texCoord2 1 0
    --    GL.vertex   $ vertex3 256 256 0

    errors <- get GL.errors
    when (errors /= []) $
        putStrLn $ show errors

    GLFW.swapBuffers

    windowOpen <- GLFW.getParam GLFW.Opened
    esc <- GLFW.getKey GLFW.ESC
    unless (esc == GLFW.Press || windowOpen == False) $
        render vertexBuffer tex


main =
    let windowWidth = 800
        windowHeight = 600
    in do
        dataFile <- getDataFileName "foo"
        putStrLn dataFile

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
