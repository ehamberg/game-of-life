import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)
import Graphics.Rendering.GLU.Raw
import Data.IORef
import System.Random

-- convenience function for “casting” a double to a GLdouble
toGLdouble :: (Real a) => a -> GLdouble
toGLdouble = realToFrac

width  = 80 :: Int
height = 60 :: Int

drawQuad :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
drawQuad x y w h =
    renderPrimitive Quads $ do
        vertex (Vertex3  x     y    0)
        vertex (Vertex3 (x+w)  y    0)
        vertex (Vertex3 (x+w) (y-h) 0)
        vertex (Vertex3  x    (y-h) 0)

display :: IORef [Bool] -> IO ()
display c = do
    cells <- readIORef c

    let h = fromIntegral height
    let w = fromIntegral width

    mapM_ (\(n,b) -> do
        if b
          then currentColor $= Color4 0.9 0.4 0.0 1.0
          else currentColor $= Color4 0.3 0.1 0.7 1.0
        let x = 1/w*fromIntegral (n `mod` width)
        let y = 1-(1/h*fromIntegral (n `div` width))
        drawQuad x y (1/w) (1/h)
        ) (zip [0..] cells)

    swapBuffers

update :: IORef [Bool] -> IO ()
update c = do
    cells <- readIORef c
    writeIORef c $ map (\_ -> True) cells

main = do
    g <- newStdGen
    (_,_) <- getArgsAndInitialize

    cells <- newIORef []

    writeIORef cells ((take (width*height) . randoms) g :: [Bool])

    createWindow "Conway's Game of Life"
    initialDisplayMode    $= [DoubleBuffered]
    windowSize            $= Size 800 600
    displayCallback       $= (display cells)
    idleCallback          $= Just (update cells)
    gluOrtho2D 0 1 0 1                  -- orthogonal projection
    mainLoop                                          -- start main loop
