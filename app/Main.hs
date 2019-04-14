module Main where

import Data.List (intercalate)

width :: Integer
width = 1920

height :: Integer
height = 1080

colorCap :: Integer
colorCap = 255

interpolate :: Fractional a => a -> a -> a -> a
interpolate smax tmax = (* (tmax / smax))

colors :: Integer -> [Integer]
colors cells = truncate . interpolate upperBound fractionalCap <$> [0..upperBound] 
  where
    upperBound = fromIntegral cells - 1
    fractionalCap = fromIntegral colorCap - 1

pixels :: [[Integer]]
pixels = [[r, g, r * g `mod` colorCap] | g <- greens, r <- reds]
  where
    reds = colors width
    greens = colors height

showPixels :: [[Integer]] -> String
showPixels = intercalate "\n" . fmap (unwords . (show <$>)) 

main :: IO ()
main = do
  putStrLn "P3"
  putStrLn . unwords $ show <$> [width, height]
  putStrLn $ show colorCap
  putStrLn $ showPixels pixels
