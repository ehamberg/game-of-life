import Graphics.Rendering.OpenGL hiding (get)
import Graphics.UI.GLUT hiding (get)
import Graphics.Rendering.GLU.Raw
import Data.IORef
import System.Random
import Control.Monad (when)

width  = 80 :: Int
height = 60 :: Int

drawQuad :: GLdouble -> GLdouble -> GLdouble -> GLdouble -> IO ()
drawQuad x y w h =
    renderPrimitive Quads $ do
        vertex (Vertex3  x     y    0)
        vertex (Vertex3 (x+w)  y    0)
        vertex (Vertex3 (x+w) (y-h) 0)
        vertex (Vertex3  x    (y-h) 0)

display :: IORef [[Bool]] -> IO ()
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
        ) (zip [0..] $ concat cells)

    swapBuffers

neighbours :: [[a]] -> (Int,Int) -> [a]
neighbours m (x,y) = map (\(x',y') -> m !! y' !! x') $ filter valid neighbours'
    where neighbours'   = [(x-1,y-1),(x,y-1),(x+1,y-1), -- over
                           (x-1,y),(x+1,y),             -- left/right
                           (x-1,y+1),(x,y+1),(x+1,y+1)] -- under
          h             = length m
          w             = length (head m)
          valid (x',y') = x' >= 0 && x' < w && y' >= 0 && y' < h

update :: IORef [[Bool]] -> IO ()
update c = do
    cells <- readIORef c

    let coords = [(x,y) | y <- [0..(height-1)], x <- [0..(width-1)]]

    f <- mapM (\(x,y) -> do
            let cell = cells !! y !! x
            let ns   = neighbours cells (x,y)
            return $ live cell ns
        ) coords

    writeIORef c (nLists width f)

    display c

live :: Bool -> [Bool] -> Bool
live c ns
    | c && live < 2      = False
    | c && live > 3      = False
    | c                  = True
    | not c && live == 3 = True
    | otherwise          = c
    where live = (length . filter id) ns


-- split a list into sublists of length n
nLists :: Int -> [a] -> [[a]]
nLists _ [] = []
nLists n ls = take n ls : nLists n (drop n ls)

keypress :: IORef [[Bool]] -> KeyboardMouseCallback
keypress c key state _ _ =
    when (key == Char ' ' && state == Down) (update c)

main :: IO ()
main = do
    g <- newStdGen
    (_,_) <- getArgsAndInitialize

    cells <- newIORef []

    -- random starting values
    writeIORef cells ((nLists width . take (width*height) . randoms) g)

    _ <- createWindow "Conway's Game of Life"
    initialDisplayMode    $= [DoubleBuffered]
    windowSize            $= Size 800 600
    displayCallback       $= display cells
    idleCallback          $= Just (update cells)
    keyboardMouseCallback $= Just (keypress cells)
    gluOrtho2D 0 1 0 1    -- orthogonal projection
    mainLoop              -- start main loop
