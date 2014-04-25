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
import Foreign.Marshal.Array
import Graphics.Rendering.OpenGL.GL.VertexArrays as GL
import Graphics.Rendering.OpenGL (($=))
import Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL.Raw as GLRaw
import Graphics.UI.GLFW as GLFW
import Paths_atteroids
import System.Environment (getArgs, getProgName)
import Data.Time
import Data.Maybe
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import qualified Data.Text as Text
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import qualified System.IO as IO
import qualified Control.Exception as Exception
import qualified Sound.PortAudio as PortAudio

gen :: Int -> Word8
gen i = if mod i 3 == 0 then 255 else 0
tex = generate (256 * 256 * 3) gen

data FloatPoint = FloatPoint Float Float
data FloatSize = FloatSize Float Float
data FloatRect = FloatRect FloatPoint FloatSize
data Console = 
    -- Arguments:
    -- FloatPoint   The bottom left corner of the window.
    -- FloatSize    The height and width of the window.
    Console FloatPoint FloatSize

data ColorRect = ColorRect (GL.Color3 GLfloat) GL.BufferObject

colorRectDraw (ColorRect color vertexBuffer) = do
    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    GL.arrayPointer GL.VertexArray $= VertexArrayDescriptor 2 GL.Float 0 nullPtr
    --GL.clientState $= GL.VertexArray $= GL.Enabled

    -- Set the color.
    GL.color $ color

    GL.drawArrays GL.Quads 0 4

    -- Restore the color.
    GL.color $ ((GL.Color3 1 1 1) :: GL.Color3 GLfloat)

colorRectInit color (FloatRect (FloatPoint x y) (FloatSize width height)) = do
    [vertexBuffer] <- GL.genObjectNames 1
    fillBuffer vertexBuffer [x, y + height, x, y, x + width, y, x + width, y + height]
    return $ ColorRect color vertexBuffer

createTextureAndBind :: IO TextureObject
createTextureAndBind = do
    [tex] <- GL.genObjectNames 1

    GL.textureBinding GL.Texture2D $= Just tex
    return tex

updateTexture (Image width height imageData) = unsafeWith imageData $ \ptr ->
    GL.texImage2D 
        Nothing 
        NoProxy 
        0 
        GL.RGB' 
        (GL.TextureSize2D (fromIntegral width) (fromIntegral height)) 
        0 
        (GL.PixelData GL.RGB GL.UnsignedByte ptr)

name = "256.png"

fillBuffer :: BufferObject -> [Float] -> IO ()
fillBuffer bufferObject vertexData = do
    GL.bindBuffer GL.ArrayBuffer $= Just bufferObject
    arr <- newListArray (0, Prelude.length vertexData) vertexData
    withStorableArray arr (\ptr -> 
        GL.bufferData GL.ArrayBuffer $= (fromIntegral $ (Prelude.length vertexData) * (sizeOf (0 :: Float)), ptr, GL.StaticDraw))

vertexCoords :: [Float]
vertexCoords = [
    0, 256,
    0, 0,
    256, 0,
    256, 256]

vertexCoordsMoving offset = [
    0 + x', 256,
    0 + x', 0,
    256 + x', 0,
    256 + x', 256]
    where
        x' = fromInteger offset

textureCoords :: [Float]
textureCoords = [
        0, 0,
        0, 1,
        1, 1,
        1, 0]

loadTexture name = do
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

data Sprite = Sprite String
data StaticAnimation = StaticAnimation Int Int Int Int deriving (Show)

instance Aeson.FromJSON StaticAnimation where
    parseJSON (Aeson.Object v) = StaticAnimation <$>
        (v .: Text.pack "left") <*>
        (v .: Text.pack "top") <*>
        (v .: Text.pack "right") <*>
        (v .: Text.pack "bottom")
    parseJSON _ = mzero

jsonData = LazyByteString.pack "{\"left\":1,\"top\":2,\"right\":3,\"bottom\":4}"
decodedStaticAnim = Aeson.decode jsonData :: Maybe StaticAnimation

readJsonFile :: FilePath -> IO LazyByteString.ByteString
readJsonFile filename = do
    maybeHandle <- (Exception.try $ IO.openFile filename IO.ReadMode) :: IO (Either Exception.IOException IO.Handle)
    case maybeHandle of
        Left e -> return $ LazyByteString.pack $ "Exception while opening file: " Prelude.++ filename Prelude.++ ". Exception: " Prelude.++ show e
        Right handle -> do
            text <- LazyByteString.hGetContents handle
            IO.hClose handle
            return text

initialize = do

    shipFileName <- getDataFileName "ship.json"
    text <- readJsonFile shipFileName
    putStrLn $ show text

    [vertexBuffer, textureCoordBuffer] <- GL.genObjectNames 2

    fillBuffer vertexBuffer $ vertexCoordsMoving 10
    fillBuffer textureCoordBuffer textureCoords

    tex <- loadTexture name

    GL.textureBinding GL.Texture2D $= Just tex

    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    GL.arrayPointer GL.VertexArray $= VertexArrayDescriptor 2 GL.Float 0 nullPtr
    GL.clientState GL.VertexArray $= GL.Enabled

    GL.bindBuffer GL.ArrayBuffer $= Just textureCoordBuffer
    GL.arrayPointer GL.TextureCoordArray $= VertexArrayDescriptor 2 GL.Float 0 nullPtr

    colorRect <- colorRectInit (GL.Color3 255 0 0) (FloatRect (FloatPoint 128 128) (FloatSize 128 128))

    errors <- get GL.errors
    when (errors /= []) $
        putStrLn $ show errors

    render vertexBuffer tex colorRect


render vertexBuffer tex colorRect = do
    currentTime <- getCurrentTime

    GL.bindBuffer GL.ArrayBuffer $= Just vertexBuffer
    fillBuffer vertexBuffer $ vertexCoordsMoving $ mod (round $ utctDayTime currentTime) 50
    GL.arrayPointer GL.VertexArray $= VertexArrayDescriptor 2 GL.Float 0 nullPtr
    GL.clientState GL.TextureCoordArray $= GL.Enabled

    GL.clear [GL.ColorBuffer]

    GL.drawArrays GL.Quads 0 4
    GL.clientState GL.TextureCoordArray $= GL.Disabled
    --GL.renderPrimitive GL.Quads $ do
    --    GL.texCoord $ texCoord2 0 0
    --    GL.vertex   $ vertex3 0 256 0
    --    GL.texCoord $ texCoord2 0 1
    --    GL.vertex   $ vertex3 (0) (0) 0
    --    GL.texCoord $ texCoord2 1 1
    --    GL.vertex   $ vertex3 256 (0) 0
    --    GL.texCoord $ texCoord2 1 0
    --    GL.vertex   $ vertex3 256 256 0
    colorRectDraw colorRect

    errors <- get GL.errors
    when (errors /= []) $
        putStrLn $ show errors

    GLFW.swapBuffers

    windowOpen <- GLFW.getParam GLFW.Opened
    esc <- GLFW.getKey GLFW.ESC
    unless (esc == GLFW.Press || windowOpen == False) $
        render vertexBuffer tex colorRect


main = do
    portAudioResult <- PortAudio.withPortAudio portAudioMain
    case portAudioResult of
        Right result -> putStrLn $ "portAudioOk: " Prelude.++ (show result)
        Left error -> putStrLn $ "portAudio error: " Prelude.++ (show error)
    where
        portAudioMain = do

            --PortAudio.withDefaultStream 0 1 44100 (Just 256) Nothing Nothing withDefaultStream
            defaultStreamResult <- PortAudio.withDefaultStream 0 1 44100 (Just 256) Nothing Nothing withDefaultStream
            case defaultStreamResult of
                Right result -> putStrLn $ "withDefaultStream: " Prelude.++ (show result)
                Left error -> putStrLn $ "withDefaultStream error: " Prelude.++ (show error)
            dataFile <- getDataFileName "foo"

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
            return $ Right ()
            where
                windowWidth = 800
                windowHeight = 600
                withDefaultStream
                    :: PortAudio.Stream CFloat CFloat
                    -> IO (Either PortAudio.Error String)
                withDefaultStream s = do--return $ Right "Success"
                    arr <- newListArray (0, Prelude.length soundData) soundData
                    withStorableArray arr (\ptr -> PortAudio.writeStream s 256 ptr >>= \maybeErr -> return $ case maybeErr of
                        Just err -> Left err
                        otherwise -> Right "Okay")
                    where
                        soundData = Prelude.replicate 256 1.0

-- type signatures to avoid ambiguity
vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

texCoord2 :: GLfloat -> GLfloat -> GL.TexCoord2 GLfloat
texCoord2 = GL.TexCoord2

color3 :: GLfloat -> GLfloat -> GLfloat -> GL.Color3 GLfloat
color3 = GL.Color3
