{-# LANGUAGE TypeFamilies #-}
module Main where

import Codec.Picture
import Codec.Picture.Bitmap
import Codec.Picture.Types
import Control.Monad
import qualified Data.ByteString as L
import Data.List
import qualified Data.Map as Map
import qualified Data.Vector.Storable as V
import System.Exit
import System.FilePath

tileWidth = 16
tileHeight = 16

main :: IO ()
main = processImage "map.bmp"

processImage filename = do
  imageData <- decodeBitmapWithPaletteAndMetadata <$> L.readFile filename
  case imageData of
    Right (PalettedRGB8 image palette, meta) -> tileImage filename image palette
    Left err -> errout $ "failed to read " <> show filename <> ": " <> err
    _ -> errout "not in indexed 8 bit RGB"

errout message = do
  putStrLn message
  exitFailure

tileImage filename image palette =
  let xtiles = imageWidth image `div` tileWidth
      ytiles = imageHeight image `div` tileHeight
      tileCoordinates = [ (y,x) | x <- [0 .. xtiles - 1], y <- [0 .. ytiles - 1] ]

      tiledata (ty,tx) =
        let xstart = tx * tileWidth
            ystart = ty * tileHeight
            rect = [ (y,x) | x <- [0 .. tileWidth - 1], y <- [0 .. tileHeight - 1] ]
        in map (\(y,x) -> pixelAt image (x + xstart) (y + ystart)) rect

      tiles = map tiledata tileCoordinates
      table = Map.fromList $ zip (nub tiles) [0..]

      indexed = map index tiles
        where index tile = let Just n = Map.lookup tile table in n

      tabledata = map (L.pack . fst) (sortOn snd (Map.assocs table))

      paletteData = L.pack $ go (V.toList $ imageData $ palettedAsImage palette)
        where go (a:b:c:xs) = 0 : a : b : c : go xs
              go _ = []
    in do
      putStrLn $ "using  " <> show (Map.size table) <> " tiles"
      when (Map.size table > 255) exitFailure
      L.writeFile (replaceExtension filename ".index") (L.pack (map fromIntegral indexed))
      L.writeFile (replaceExtension filename ".tiledata") (mconcat tabledata)
      L.writeFile (replaceExtension filename ".palette") paletteData
