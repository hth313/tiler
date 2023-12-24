{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Codec.Picture
import Codec.Picture.Bitmap
import Codec.Picture.Types
import Control.Monad
import Data.Bits
import qualified Data.ByteString as L
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Vector.Storable as V
import Options.Applicative
import System.Exit
import System.FilePath
import System.Random
import System.FilePath.Posix (takeFileName)

data Options = Options {
    tileWidth :: Int,
    tileHeight :: Int,
    tileLUT :: Int,
    tileSet :: Int,
    maxTileCount :: Int,
    extractPalette :: Bool,
    bigEndian :: Bool,
    sprites :: Bool,
    spriteCount :: Maybe Int,
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
          ( long "tile-lut"
          <> showDefault
          <> value 0
          <> help "Tile color look up table (0-7)")
      <*> option auto
          ( long "tile-set"
          <> showDefault
          <> value 0
          <> help "Tile set number (0-7)")
      <*> option auto
          ( long "max-tiles"
          <> showDefault
          <> value 256
          <> help "Maximum tiles that can be generated" )
      <*> switch
          ( long "extract-palette"
          <> help "Extract and emit a .palette file")
      <*> switch
          ( long "big-endian"
          <> help "Target is big endian")
      <*> switch
          ( long "sprites"
          <> help "Generate sprites")
      <*> (optional $ option auto
           ( long "sprite-count"
           <> metavar "COUNT"
           <> help "Generate COUNT sprites, defaults to all in given file"))
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
           <> value 3
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
  rg <- getStdGen
  imageData <- decodeBitmapWithPaletteAndMetadata <$> L.readFile (filename options)
  case imageData of
    Right (PalettedRGB8 image palette, meta) -> tileImage options rg image palette
    Left err -> errout $ "failed to read " <> show (filename options) <> ": " <> err
    _ -> errout "not in indexed 8 bit RGB"

errout message = do
  putStrLn message
  exitFailure

tileImage Options{..} rg image palette =
  let xtiles = imageWidth image `div` tileWidth
      ytiles = imageHeight image `div` tileHeight
      tileCoordinates = sequence [ [0 .. ytiles - 1], [0 .. xtiles - 1] ]

      tiledata [ty,tx] =
        let xstart = tx * tileWidth
            ystart = ty * tileHeight
            rect = sequence [ [0 .. tileHeight - 1], [0 .. tileWidth - 1] ]
        in map (\[y,x] -> pixelAt image (x + xstart) (y + ystart)) rect

      tilemap = map tiledata tileCoordinates

      tiles = nub $ map tiledata tileCoordinates

      table = Map.fromList $ zip keyTiles (zip [0..] tilesWithNoise)

      noiseTiles = drop (Map.size table) tilesWithNoise
      noiseTileCount = length noiseTiles

      tilesWithNoise
        | isNothing mbackgroundNoiseTileRange = tilesWithBlanks
        | otherwise = map addNoise (zip3 tilesWithBlanks randoms randomNoise)
        where
          Just backgroundNoiseTileRange = mbackgroundNoiseTileRange
          randoms = go (randomRs (0, pixelCountTile - 1) rg)
            where
              go xs = let (g, gs) = splitAt noisePixels xs
                      in g : go gs
          randomNoise = go (randomRs (0, length noiseColors - 1) rg)
            where
              go xs = let (g, gs) = splitAt noisePixels xs
                      in g : go gs

          noisePixels = noiseLevel * pixelCountTile `div` 100

      pixelCountTile = tileHeight * tileWidth

      (keyTiles, tilesWithBlanks, backgroundTile, mbackgroundNoiseTileRange)
        | isNothing noiseOnColor = useNoNoiseTiles
        | null noiseColors = useNoNoiseTiles
        | length tiles < maxTileCount =
            ( tilesNoBackground <> tailedBackground
            , tilesNoBackground <> noiseTiles
            , backgroundTile
            , Just backgroundNoiseTileRange)
        | otherwise = useNoNoiseTiles
        where
          useNoNoiseTiles = (tiles, tiles, undefined, Nothing)
          Just background = noiseOnColor
          backgroundTile = makeTileOfColor background
          tilesNoBackground = backgroundTile `delete` tiles
          tailedBackground
            | backgroundTile `elem` tiles = [backgroundTile]
            | otherwise = []
          unused = maxTileCount - actualTileCount
          actualTileCount = length tilesNoBackground
          noiseTiles = replicate unused backgroundTile
          backgroundNoiseTileRange = (actualTileCount, maxTileCount - 1)

      addNoise (tile, randoms, noise) = go sortedRandoms (zip [0..] tile) noise
        where
          Just background = fromIntegral <$> noiseOnColor
          sortedRandoms = sort (nub randoms)
          go [] xs noise = map snd xs
          go nns@(n:ns) ((np,pixel):xs) nnoises@(noiseIndex : noises)
            | n == np, pixel == background =
                fromIntegral (noiseColors !! noiseIndex) : go ns xs noises
            | n == np =
                pixel : go ns xs nnoises
            | otherwise =
                pixel : go nns xs nnoises

      makeTileOfColor color = replicate pixelCountTile (fromIntegral color)

      indexedWithNoise
        | isJust mbackgroundNoiseTileRange =
            go indexed (randomRs backgroundNoiseTileRange rg)
        | otherwise = indexed
        where
          indexed = map index tilemap
          Just (blank, _noised) = Map.lookup backgroundTile table
          Just backgroundNoiseTileRange = mbackgroundNoiseTileRange
          index tile = let Just (n, _noised) = Map.lookup tile table in n
          go [] _ = []
          go (t:ts) (r:rs)
            | t == blank = r : go ts rs
            | otherwise = t : go ts rs

      addAttribute index
        | bigEndian = [ attribute, fromIntegral index ]
        | otherwise = [ fromIntegral index, attribute ]
      attribute = fromIntegral $ (tileLUT `shiftL` 3) .|. tileSet

      tabledata = map (L.pack . snd . snd) (sortOn (fst . snd) (Map.assocs table))

      paletteData = L.pack $ go (V.toList $ imageData $ palettedAsImage palette)
        where go (a:b:c:xs) = c : b : a : 0 : go xs
              go _ = []

      basefile = takeFileName $ dropExtension filename

      generateTiles = do
        putStrLn $ "using  " <> show (Map.size table + noiseTileCount) <> " tiles"
        when (noiseTileCount > 0) (putStrLn $ "(" <> show noiseTileCount <> " noise tiles)")
        L.writeFile (replaceExtension basefile ".index")
                    (L.pack (concatMap addAttribute indexedWithNoise))
        L.writeFile (replaceExtension basefile ".tiledata")
           (mconcat (tabledata <> map L.pack noiseTiles))

      generateSprites =
        let count = fromMaybe (length tiles) spriteCount
            generate (n, spriteData) =
              let spritefile = dropExtension basefile <> "-" <> show n -<.> ".sprite"
              in L.writeFile spritefile (L.pack spriteData)
        in do
          putStrLn $ "generating  " <> show count <> " sprites"
          mapM_ generate (take count (zip [0..] tiles))

    in do
      when (Map.size table > maxTileCount) exitFailure
      if sprites then generateSprites else generateTiles
      when extractPalette (L.writeFile (replaceExtension basefile ".palette") paletteData)
