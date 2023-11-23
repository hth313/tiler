{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Codec.Picture
import Codec.Picture.Bitmap
import Codec.Picture.Types
import Control.Monad
import qualified Data.ByteString as L
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector.Storable as V
import Options.Applicative
import System.Exit
import System.FilePath

data Options = Options {
    tileWidth :: Int,
    tileHeight :: Int,
    maxTileCount :: Int,
    extractPalette :: Bool,
    sprites :: Bool,
    spriteCount :: Maybe Int,
    textBackgroundTile :: Maybe Int,
    noiseOnColor :: Maybe Int,
    noiseColors :: [Int],
    noiseLevel :: Int,
    filename :: FilePath
}

options :: Parser Options
options = Options
      <$> option auto
          ( long "width"
          <> showDefault
          <> value 16
          <> help "Tile or sprite width" )
      <*> option auto
          ( long "height"
          <> showDefault
          <> value 16
          <> help "Tile or sprite height" )
      <*> option auto
          ( long "max-tiles"
          <> showDefault
          <> value 256
          <> help "Maximum tiles that can be generated" )
      <*> switch
          ( long "extract-palette"
          <> help "Extract and emit a .palette file")
      <*> switch
          ( long "sprites"
          <> help "Generate sprites")
      <*> (optional $ option auto
           ( long "sprite-count"
           <> metavar "COUNT"
           <> help "Generate COUNT sprites, defaults to all in given file"))
      <*> (optional $ option auto
           ( long "text-background"
           <> metavar "COLOR"
           <> help "Use tile 0 as solid background (for text overlay), specify color used"))
      <*> (optional $ option auto
           ( long "noise-on-color"
           <> metavar "COLOR"
           <> help "The given color is 'background' on which we want to add noise"))
      <*> (many $ option auto
           ( long "noise-color"
           <> metavar "COLOR"
           <> help "Add given color to one of the used noise colors"))
      <*> (option auto
           ( long "noise-level"
           <> showDefault
           <> value 8
           <> metavar "percentage"
           <> help "The amount of noise to add"))
      <*> argument str (metavar "FILE")

main :: IO ()
main = processImage =<< execParser opts
  where
    opts = info (options <**> helper)
      ( fullDesc
      <> progDesc "Extract tiles or sprites from a .bmp"
      <> header "tiler - a tile and sprite extractor" )

processImage options = do
  imageData <- decodeBitmapWithPaletteAndMetadata <$> L.readFile (filename options)
  case imageData of
    Right (PalettedRGB8 image palette, meta) -> tileImage options image palette
    Left err -> errout $ "failed to read " <> show (filename options) <> ": " <> err
    _ -> errout "not in indexed 8 bit RGB"

errout message = do
  putStrLn message
  exitFailure

tileImage Options{..} image palette =
  let xtiles = imageWidth image `div` tileWidth
      ytiles = imageHeight image `div` tileHeight
      tileCoordinates = sequence [ [0 .. ytiles - 1], [0 .. xtiles - 1] ]

      tiledata [ty,tx] =
        let xstart = tx * tileWidth
            ystart = ty * tileHeight
            rect = sequence [ [0 .. tileHeight - 1], [0 .. tileWidth - 1] ]
        in map (\[y,x] -> pixelAt image (x + xstart) (y + ystart)) rect

      tiles = map tiledata tileCoordinates

      table = Map.fromList $ zip tilesWithNoise [0..]

      (tilesWithNoise, backgroundTilep, backgroundNoiseTileIndices)
        | isNothing noiseOnColor = useNoNoiseTiles
        | null noiseColors = useNoNoiseTiles
        | length tilesWithBackground < maxTileCount =
            ( tilesHead <> map addNoise tilesTail <> map addNoise noiseTiles
            , backgroundTilep
            , backgroundNoiseTileIndices)
        | otherwise = useNoNoiseTiles
        where
          useNoNoiseTiles = (tilesWithBackground, const False, [])
          Just background = noiseOnColor
          backgroundTile = makeTileOfColor background
          (tilesHead, tilesTail0)
            | isJust textBackgroundTile = splitAt 1 tilesWithBackground
            | otherwise = ([], tilesWithBackground)
          tilesTail = backgroundTile `delete` tilesTail0
          unused = maxTileCount - actualTileCount
          actualTileCount = length (tilesHead <> tilesTail)
          noiseTiles = replicate unused backgroundTile
          backgroundTilep tile = tile == backgroundTile
          backgroundNoiseTileIndices = [actualTileCount .. maxTileCount - 1]

      addNoise tile = tile

      tilesWithBackground
        | Just color <- textBackgroundTile =
            makeTileOfColor (fromIntegral color) : nub tiles
        | otherwise =
            nub tiles

      makeTileOfColor color = replicate (tileHeight * tileWidth) (fromIntegral color)

      indexed = map index tiles
        where index tile = let Just n = Map.lookup tile table in n

      tabledata = map (L.pack . fst) (sortOn snd (Map.assocs table))

      paletteData = L.pack $ go (V.toList $ imageData $ palettedAsImage palette)
        where go (a:b:c:xs) = 0 : a : b : c : go xs
              go _ = []

      generateTiles = do
        putStrLn $ "using  " <> show (Map.size table) <> " tiles"
        L.writeFile (replaceExtension filename ".index") (L.pack (map fromIntegral indexed))
        L.writeFile (replaceExtension filename ".tiledata") (mconcat tabledata)

      generateSprites =
        let count = fromMaybe (length tiles) spriteCount
            generate (n, spriteData) =
              let spritefile = dropExtension filename <> "-" <> show n -<.> ".sprite"
              in L.writeFile spritefile (L.pack spriteData)
        in do
          putStrLn $ "generating  " <> show count <> " sprites"
          mapM_ generate (take count (zip [0..] tiles))

    in do
      when (Map.size table > maxTileCount) exitFailure
      if sprites then generateSprites else generateTiles
      when extractPalette (L.writeFile (replaceExtension filename ".palette") paletteData)
