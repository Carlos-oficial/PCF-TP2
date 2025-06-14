import Adventurers

f :: Float -> Float -> Float -> Float
f x y z = 
    let r = (y / z)
        r' = x + r
    in r'


f' :: Float -> Float -> IO Float
f' x y =
  do
    z'::Float <- readLn
    let r = y / z'
    putStrLn . unwords $ [show y , "/", show z',"is", show r]
    let r' = x + r
    putStrLn . unwords $ [show x , "+", show r,"is", show r']
    return r'

g :: IO ()
g = do
  x :: Move <- readLn
  putStrLn $ show x