module Neuralnet where

import Control.Concurrent (forkOS)
import Control.Monad (forM_, unless, void, when, zipWithM_)
import Data.IORef
import Data.IORef (IORef, atomicWriteIORef, newIORef, readIORef, writeIORef)
-- import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Vector qualified as V
import Data.Void
import Data.Word (Word8)
import Foreign.Store (Store (..))
import Foreign.Store qualified as FS
import Raylib.Core
import Raylib.Core.Shapes
import Raylib.Core.Text
import Raylib.Types
import Raylib.Util.Colors
import Raylib.Util.GUI
import Raylib.Util.Math
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lex
import Torch

type Parser = Parsec Void T.Text

type NeuralLayer = (Tensor, Tensor)

type NeuralNet = [NeuralLayer]

type TickAction = IO ()

data Digit = Digit
  { label :: Int,
    pixels :: V.Vector Int,
    filledPixels :: V.Vector RenderPixel
  }
  deriving (Show)

data RenderPixel = RenderPixel
  { pixelX :: Int,
    pixelY :: Int,
    pixelColor :: Color
  }
  deriving (Show)

data NeuralNetRenderData = NeuralNetRenderData
  { neuronPositions :: [(Int, Int)],
    neuronSizes :: [Float],
    weightPositions :: [((Int, Int), (Int, Int))],
    weightColors :: [Float],
    neuronValues :: [Float]
  }

data AppResources = AppResources
  { digits :: V.Vector Digit,
    digitCount :: Int
  }

{-# NOINLINE tickStore #-}
tickStore :: Store (IORef TickAction)
tickStore = Store 0

{-# NOINLINE appStateStore #-}
appStateStore :: Store (IORef AppState)
appStateStore = Store 1

{-# NOINLINE appResourcesStore #-}
appResourcesStore :: Store AppResources
appResourcesStore = Store 2

windowWidth :: Int
windowWidth = 1920

windowHeight :: Int
windowHeight = 1200

targetFramesPerSecond :: Int
targetFramesPerSecond = 60

sc = Lex.space hspace1 empty empty

lexeme :: Parser a -> Parser a
lexeme = Lex.lexeme sc

parseInteger = lexeme Lex.decimal

parseDigit :: Parser Digit
parseDigit = do
  digit_label <- parseInteger <* char ','
  pixels <- many (parseInteger <* optional (char ',')) <* space
  return (mkDigit digit_label (V.fromList pixels))

parseDigits :: Parser [Digit]
parseDigits = do
  _ <- count 4205 anySingle <* space1
  digits <- many parseDigit
  return digits

digitsFilePath :: FilePath
digitsFilePath = "resources/digits/mnist_debug.csv"

loadDigits :: FilePath -> IO (Maybe [Digit])
loadDigits fp = do
  text <- T.readFile fp
  let parse_result = parse parseDigits fp text
  case parse_result of
    Left err -> do
      putStrLn $ "Failed with error: " ++ errorBundlePretty err
      return Nothing
    Right digits -> return (Just digits)

neuralNetRenderX :: Int
neuralNetRenderX = 32

neuralNetRenderY :: Int
neuralNetRenderY = 128 + 32

neuralNetRenderWidth :: Int
neuralNetRenderWidth = 1100

neuralNetRenderHeight :: Int
neuralNetRenderHeight = 700

loadAppResources :: IO AppResources
loadAppResources = do
  digitsList <- loadDigits digitsFilePath
  let digitsVec = V.fromList (fromMaybe [] digitsList)
  pure
    AppResources
      { digits = digitsVec,
        digitCount = V.length digitsVec
      }

main :: IO ()
main = do
  resources <- loadAppResources
  initialState <- mkInitialAppState resources.digits
  stateRef <- newIORef initialState
  tickRef <- newIORef (tick resources stateRef)
  runWindow tickRef

mainDev :: IO ()
mainDev = do
  let Store storeId = tickStore
  existing <- FS.lookupStore storeId
  case existing of
    Nothing -> do
      putStrLn "booting"
      resources <- loadAppResources
      initialState <- mkInitialAppState resources.digits
      stateRef <- newIORef initialState
      tickRef <- newIORef (tick resources stateRef)
      FS.writeStore tickStore tickRef
      FS.writeStore appStateStore stateRef
      FS.writeStore appResourcesStore resources
      void $ forkOS $ do
        runWindow tickRef
        FS.deleteStore tickStore
        FS.deleteStore appStateStore
        FS.deleteStore appResourcesStore
        putStrLn "shutting down"
    Just _ -> do
      putStrLn "reloading"
      tickRef <- FS.readStore tickStore
      stateRef <- FS.readStore appStateStore
      resources <- do
        maybeRes <- FS.lookupStore (let Store rid = appResourcesStore in rid)
        case maybeRes of
          Nothing -> do
            loaded <- loadAppResources
            FS.writeStore appResourcesStore loaded
            pure loaded
          Just _ -> FS.readStore appResourcesStore
      atomicWriteIORef tickRef (tick resources stateRef)

runWindow :: IORef TickAction -> IO ()
runWindow tickRef = do
  window <- initWindow windowWidth windowHeight "neuralnet"
  setTargetFPS targetFramesPerSecond
  gameLoop tickRef
  closeWindow (Just window)

gameLoop :: IORef TickAction -> IO ()
gameLoop tickRef = do
  shouldClose <- windowShouldClose
  unless shouldClose $ do
    tickFn <- readIORef tickRef
    tickFn
    gameLoop tickRef

getNumNeurons :: NeuralLayer -> Int
getNumNeurons (weights, _) = case shape weights of
  [_, c] -> c
  _ -> error "Invalid layer"

getNumInputs :: NeuralLayer -> Int
getNumInputs (weights, _) = case shape weights of
  [r, _] -> r
  _ -> error "Invalid layer"

getNeuronLayerPositions :: [Int] -> Int -> Int -> Int -> Int -> Int -> [(Int, Int)]
getNeuronLayerPositions layer_neuron_counts i x y w h = zip xs ys
  where
    num_layers = length layer_neuron_counts
    num_neurons = (layer_neuron_counts !! i)
    offs_x :: Float = (fromIntegral w) / (fromIntegral num_layers)
    offs_y :: Float = (fromIntegral h) / (fromIntegral num_neurons)
    start_x = ((fromIntegral x) + (offs_x / 2.0) + offs_x * (fromIntegral i))
    start_y = ((fromIntegral y) + (offs_y / 2.0))
    xs = map Prelude.round $ Prelude.take num_neurons $ (Prelude.repeat start_x)
    ys = map Prelude.round $ [start_y, start_y + offs_y ..]

getNeuronPositions :: [Int] -> Int -> Int -> Int -> Int -> [(Int, Int)]
getNeuronPositions layer_neuron_counts x y w h = concatMap layerFn layer_indices
  where
    num_layers = length layer_neuron_counts
    layer_indices = [0 .. num_layers - 1]
    layerFn i = getNeuronLayerPositions layer_neuron_counts i x y w h

getWeightPositions :: [Int] -> Int -> Int -> Int -> Int -> [((Int, Int), (Int, Int))]
getWeightPositions layer_neuron_counts x y w h = concatMap layerFn [0 .. num_layers - 2]
  where
    num_layers = length layer_neuron_counts
    layerFn i = getLayerWeightPositions layer_neuron_counts i x y w h

getLayerWeightPositions :: [Int] -> Int -> Int -> Int -> Int -> Int -> [((Int, Int), (Int, Int))]
getLayerWeightPositions layer_neuron_counts i x y w h = weights
  where
    la = getNeuronLayerPositions layer_neuron_counts i x y w h
    lb = getNeuronLayerPositions layer_neuron_counts (i + 1) x y w h
    weights = [(wa, wb) | wa <- la, wb <- lb]

drawTextCentered :: String -> Int -> Int -> Int -> IO ()
drawTextCentered text x y font_size = do
  text_width <- measureText text font_size
  drawText text (x - (Prelude.div text_width 2)) (y - (Prelude.div font_size 2)) font_size rayWhite

drawNeuron :: Float -> (Int, Int) -> Float -> IO ()
drawNeuron s (x, y) value =
  when (value > 0.0) $ do
    drawCircle x y s color
    drawCircleLines x y s rayWhite
  where
    -- drawTextCentered (show value) x y 14
    strength = value
    intensity = Prelude.floor (clampValue 0 255 (strength * 255)) :: Word8
    color = Color intensity intensity intensity 255

drawWeight :: Float -> (Int, Int) -> (Int, Int) -> IO ()
drawWeight strength (sx, sy) (ex, ey) = when (strength > 0.0) $ do
  drawLine sx sy ex ey color
  where
    intensity = Prelude.floor (clampValue 0 255 (strength * 255)) :: Word8
    color = Color intensity intensity intensity 255

digitSidePixels :: Int
digitSidePixels = 28

digitScale :: Int
digitScale = 2

digitsPerRow :: Int
digitsPerRow = 5

digitSpacing :: Int
digitSpacing = 8

digitPanelMargin :: Int
digitPanelMargin = 18

digitPanelPadding :: Int
digitPanelPadding = 12

digitPanelHeaderHeight :: Int
digitPanelHeaderHeight = 0

digitPanelHeaderSpacing :: Int
digitPanelHeaderSpacing = 0

digitPanelVisibleRows :: Int
digitPanelVisibleRows = 12

digitPreviewScale :: Int
digitPreviewScale = 10

digitPreviewPadding :: Int
digitPreviewPadding = 14

digitPreviewHeaderHeight :: Int
digitPreviewHeaderHeight = 24

digitPreviewHeaderSpacing :: Int
digitPreviewHeaderSpacing = 8

digitPreviewSpacing :: Int
digitPreviewSpacing = 16

data DigitPanelLayout = DigitPanelLayout
  { panelX :: Int,
    panelY :: Int,
    panelWidth :: Int,
    panelHeight :: Int,
    gridOriginX :: Int,
    gridOriginY :: Int,
    gridWidth :: Int,
    visibleHeight :: Int,
    contentHeight :: Int,
    contentRows :: Int,
    panelColumns :: Int,
    tileSize :: Int
  }
  deriving (Show)

data DigitPreviewLayout = DigitPreviewLayout
  { previewX :: Int,
    previewY :: Int,
    previewWidth :: Int,
    previewHeight :: Int,
    previewTileX :: Int,
    previewTileY :: Int,
    previewTileSize :: Int
  }
  deriving (Show)

drawNeuralNet :: NeuralNetRenderData -> IO ()
drawNeuralNet renderData = do
  beginBlendMode BlendAdditive
  zipWithM_ (\str (p1, p2) -> drawWeight str p1 p2) renderData.weightColors renderData.weightPositions
  endBlendMode
  zipWith3M_ drawNeuron renderData.neuronSizes renderData.neuronPositions renderData.neuronValues

zipWith3M_ :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m ()
zipWith3M_ f as bs cs = sequence_ (zipWith3 f as bs cs)

-- Compute weight colors based on activation paths (source neuron × weight × target neuron)
-- Only shows weights where target neuron is activated (> 0)
computeWeightColors :: NeuralNet -> [Float] -> [Float]
computeWeightColors nn neuronValues = normalizedActivations
  where
    sourceNeuronValues = getNeuronValuesForWeights nn neuronValues getSourceNeuronIdx
    targetNeuronValues = getNeuronValuesForWeights nn neuronValues getTargetNeuronIdx
    weights = getWeightValues nn
    -- Only count activation if target neuron is activated (> 0)
    activations = zipWith3 (\s w t -> if t > 0 then Prelude.abs (s * w * t) else 0) sourceNeuronValues weights targetNeuronValues
    maxActivation = Prelude.maximum activations
    normalizedActivations =
      if maxActivation > 0
        then map (/ maxActivation) activations
        else activations

-- Get neuron values for each weight using an index function
-- indexFn determines whether to get source or target neuron for each weight
getNeuronValuesForWeights :: NeuralNet -> [Float] -> (Int -> Int -> Int -> Int -> Int) -> [Float]
getNeuronValuesForWeights nn neuronValues indexFn = concatMap layerValues (zip layerStartIndices nn)
  where
    layerNeuronCounts = getLayerNeuronCounts nn
    layerStartIndices = scanl (+) 0 layerNeuronCounts
    layerValues (startIdx, (weights, _)) =
      let (numInputs, numOutputs) = case shape weights of
            [i, o] -> (i, o)
            _ -> error "Invalid weight shape"
       in [neuronValues !! indexFn startIdx numInputs i o | i <- [0 .. numInputs - 1], o <- [0 .. numOutputs - 1]]

-- Index of source neuron for weight connecting input i to output o
getSourceNeuronIdx :: Int -> Int -> Int -> Int -> Int
getSourceNeuronIdx startIdx _ i _ = startIdx + i

-- Index of target neuron for weight connecting input i to output o
-- Target neurons start after the source layer (startIdx + numInputs)
getTargetNeuronIdx :: Int -> Int -> Int -> Int -> Int
getTargetNeuronIdx startIdx numInputs _ o = startIdx + numInputs + o

getLayerNeuronCounts :: NeuralNet -> [Int]
getLayerNeuronCounts [] = []
getLayerNeuronCounts nn@(firstLayer : _) = [getNumInputs firstLayer] ++ map getNumNeurons nn

getWeightValues :: NeuralNet -> [Float]
getWeightValues nn = concatMap getLayerWeights nn
  where
    getLayerWeights (weights, _) =
      let weightMatrix :: [[Float]] = asValue weights
       in [w | row <- weightMatrix, w <- row]

computeNeuralNetRenderData :: NeuralNet -> [Float] -> Int -> Int -> Int -> Int -> NeuralNetRenderData
computeNeuralNetRenderData [] _ _ _ _ _ = error "Cannot compute render data for empty neural network"
computeNeuralNetRenderData nn@(firstLayer : _) neuron_values x y w h =
  NeuralNetRenderData
    { neuronPositions = positions_neurons,
      neuronSizes = neuron_sizes,
      weightPositions = positions_weights,
      weightColors = weight_colors,
      neuronValues = neuron_values
    }
  where
    inputCount = getNumInputs firstLayer
    layer_neuron_counts = [inputCount] ++ map getNumNeurons nn
    positions_neurons = getNeuronPositions layer_neuron_counts x y w h
    positions_weights = getWeightPositions layer_neuron_counts x y w h
    layer_sizes = map (\lc -> (fromIntegral h) / (2.0 * (fromIntegral lc))) layer_neuron_counts
    neuron_sizes = concatMap (\(cnt, sz) -> Prelude.replicate cnt sz) (zip layer_neuron_counts layer_sizes)
    weight_colors = computeWeightColors nn neuron_values

forwardPass :: NeuralNet -> Tensor -> [Tensor]
forwardPass nn input = scanl layerFn input (zip indices nn)
  where
    num_layers = length nn
    indices = [0 .. num_layers]
    layerFn x (i, (weights, biases))
      | (i + 1 < num_layers) = relu $ (matmul x weights) + biases
      | otherwise = (matmul x weights) + biases

-- forwardPassResult :: NeuralNet -> Tensor -> Tensor
-- forwardPassResult nn input = List.foldl' layerFn input nn
--   where
--     layerFn i (weights, biases) = (matmul i weights) + biases

-- backprop :: NeuralNet -> Tensor -> Tensor -> NeuralNet
-- backprop nn input expected_output = scanr
--   where
--     loss = mseLoss input expected_output
--     fp = forwardPassResult nn input

getNeuronValues :: [Tensor] -> Tensor -> [Float]
getNeuronValues weights input = input_values ++ hidden_values
  where
    hidden_values = concatMap concat (map (asValue :: Tensor -> [[Float]]) weights)
    input_values = (asValue input) :: [Float]

drawDigitPixels :: V.Vector RenderPixel -> Int -> Int -> Int -> IO ()
drawDigitPixels pixels x y scale =
  V.forM_ pixels $ \RenderPixel {..} -> do
    let pixelSize = scale
    drawRectangle (x + pixelX * pixelSize) (y + pixelY * pixelSize) pixelSize pixelSize pixelColor

drawDigitTile :: Digit -> Int -> Int -> Int -> IO ()
drawDigitTile Digit {label = _, filledPixels} x y scale = do
  let tileSize = digitSidePixels * scale
  drawRectangleLines x y tileSize tileSize rayWhite
  drawDigitPixels filledPixels x y scale

digitPanelLayout :: Int -> Int -> Int -> DigitPanelLayout
digitPanelLayout screenWidth screenHeight digitCount =
  DigitPanelLayout
    { panelX,
      panelY,
      panelWidth,
      panelHeight,
      gridOriginX,
      gridOriginY,
      gridWidth,
      visibleHeight,
      contentHeight,
      contentRows,
      panelColumns,
      tileSize
    }
  where
    tileSize = digitSidePixels * digitScale
    panelColumns = Prelude.min digitsPerRow (Prelude.max 1 digitCount)
    contentRows = ceiling (fromIntegral digitCount / fromIntegral panelColumns :: Double)
    contentHeight =
      contentRows * tileSize
        + Prelude.max 0 (contentRows - 1) * digitSpacing
    visibleRowsHeight =
      digitPanelVisibleRows * tileSize
        + Prelude.max 0 (digitPanelVisibleRows - 1) * digitSpacing
    visibleHeight = Prelude.min contentHeight visibleRowsHeight
    gridWidth =
      panelColumns * tileSize
        + Prelude.max 0 (panelColumns - 1) * digitSpacing
    panelWidth = gridWidth + digitPanelPadding * 2
    panelHeight =
      digitPanelPadding
        + digitPanelHeaderHeight
        + digitPanelHeaderSpacing
        + visibleHeight
        + digitPanelPadding
    panelX = screenWidth - panelWidth - digitPanelMargin
    panelY = screenHeight - panelHeight - digitPanelMargin
    gridOriginX = panelX + digitPanelPadding
    gridOriginY = panelY + digitPanelPadding + digitPanelHeaderHeight + digitPanelHeaderSpacing

digitPreviewLayout :: DigitPanelLayout -> DigitPreviewLayout
digitPreviewLayout DigitPanelLayout {panelX, panelY, panelWidth} =
  DigitPreviewLayout
    { previewX,
      previewY,
      previewWidth,
      previewHeight,
      previewTileX,
      previewTileY,
      previewTileSize = digitPreviewScale
    }
  where
    tileSize = digitSidePixels * digitPreviewScale
    previewWidth = Prelude.max panelWidth (tileSize + digitPreviewPadding * 2)
    previewHeight =
      digitPreviewPadding
        + digitPreviewHeaderHeight
        + digitPreviewHeaderSpacing
        + tileSize
        + digitPreviewPadding
    previewX = panelX
    previewY = Prelude.max digitPanelMargin (panelY - digitPreviewSpacing - previewHeight)
    previewTileX = previewX + digitPreviewPadding
    previewTileY = previewY + digitPreviewPadding + digitPreviewHeaderHeight + digitPreviewHeaderSpacing

drawDigitsPanel :: V.Vector Digit -> DigitPanelLayout -> Int -> Int -> IO ()
drawDigitsPanel ds DigitPanelLayout {..} scrollOffset selection = do
  let panelColor = Color 18 18 24 230
      visibleBottom = gridOriginY + visibleHeight
      tileBlock = tileSize + digitSpacing
      startRow = scrollOffset `Prelude.div` tileBlock
      endRow =
        Prelude.min
          contentRows
          ( Prelude.ceiling
              (fromIntegral (scrollOffset + visibleHeight) / fromIntegral tileBlock :: Double)
          )
      startIdx = startRow * panelColumns
      endIdx = Prelude.min (V.length ds) (endRow * panelColumns)
  drawRectangle panelX panelY panelWidth panelHeight panelColor
  drawRectangleLines panelX panelY panelWidth panelHeight rayWhite
  forM_ [startIdx .. endIdx - 1] $ \idx -> do
    let digit = ds V.! idx
        row = idx `Prelude.div` panelColumns
        col = idx `mod` panelColumns
        tileY = gridOriginY + row * tileBlock - scrollOffset
        tileX = gridOriginX + col * tileBlock
        tileBottom = tileY + tileSize
    when (tileBottom >= gridOriginY && tileY <= visibleBottom) $ do
      drawDigitTile digit tileX tileY digitScale
      when (idx == selection) $ drawRectangleLines tileX tileY tileSize tileSize yellow

drawDigitPreview :: Maybe (Int, Digit) -> DigitPreviewLayout -> IO ()
drawDigitPreview Nothing _ = pure ()
drawDigitPreview (Just (idx, Digit {label = digitLabel, filledPixels})) DigitPreviewLayout {..} = do
  let panelColor = Color 18 18 24 230
      subtitle = "Label: " <> show digitLabel <> " (index " <> show idx <> ")"
  drawRectangle previewX previewY previewWidth previewHeight panelColor
  drawRectangleLines previewX previewY previewWidth previewHeight rayWhite
  drawText subtitle (previewX + digitPreviewPadding) (previewY + digitPreviewPadding + digitPreviewHeaderSpacing) 24 rayWhite
  drawDigitPixels filledPixels previewTileX previewTileY previewTileSize

handleDigitPanelInput :: V.Vector Digit -> DigitPanelLayout -> IORef AppState -> Vector2 -> Bool -> IO ()
handleDigitPanelInput digits layout stateRef mousePos hoveredPanel = do
  let digitCount = V.length digits
  let DigitPanelLayout
        { panelColumns,
          contentRows,
          gridWidth,
          visibleHeight,
          tileSize,
          gridOriginX,
          gridOriginY,
          contentHeight
        } = layout
  wheelMove <- if hoveredPanel then getMouseWheelMove else pure 0
  mousePressed <- isMouseButtonPressed MouseButtonLeft
  appState <- readIORef stateRef
  let digitSelection = appState.digitSelection
      digitScroll = appState.digitScroll
      scrollRange = Prelude.max 0 (contentHeight - visibleHeight)
      scrollStep = tileSize + digitSpacing
      scrollDelta = round ((-wheelMove) * fromIntegral scrollStep)
      newScroll =
        if hoveredPanel
          then clampValue 0 scrollRange (digitScroll + scrollDelta)
          else digitScroll
      tileBlock = tileSize + digitSpacing
      localX = Prelude.floor (vector2'x mousePos) - gridOriginX
      localY = Prelude.floor (vector2'y mousePos) - gridOriginY
      contentY = localY + newScroll
      col = if tileBlock == 0 then 0 else localX `Prelude.div` tileBlock
      row = if tileBlock == 0 then 0 else contentY `Prelude.div` tileBlock
      inTile =
        (localX `mod` tileBlock) < tileSize
          && (contentY `mod` tileBlock) < tileSize
      validClick =
        hoveredPanel
          && mousePressed
          && localX >= 0
          && localY >= 0
          && localX < gridWidth
          && localY < visibleHeight
          && inTile
      candidateIdx = row * panelColumns + col
      newSelection =
        if validClick
          && col < panelColumns
          && row < contentRows
          && candidateIdx < digitCount
          then candidateIdx
          else digitSelection
      selectionChanged = newSelection /= digitSelection
      newInput =
        if selectionChanged && newSelection >= 0 && newSelection < digitCount
          then digitToTensor (digits V.! newSelection)
          else appState.currentInput
      nn = appState.neuralNet
      newRenderData =
        if selectionChanged
          then
            let neuron_values = getNeuronValues (forwardPass nn newInput) newInput
             in computeNeuralNetRenderData nn neuron_values neuralNetRenderX neuralNetRenderY neuralNetRenderWidth neuralNetRenderHeight
          else appState.neuralNetRenderData
  writeIORef
    stateRef
    appState
      { digitSelection = newSelection,
        digitScroll = newScroll,
        currentInput = newInput,
        neuralNetRenderData = newRenderData
      }

pointInRect :: Vector2 -> Int -> Int -> Int -> Int -> Bool
pointInRect (Vector2 px py) x y w h =
  let ix = Prelude.floor px
      iy = Prelude.floor py
   in ix >= x && ix <= x + w && iy >= y && iy <= y + h

initNeuralNet :: [Int] -> IO NeuralNet
initNeuralNet layerSizes = mapM initLayer (zip layerSizes (drop 1 layerSizes))
  where
    initLayer (fanIn, fanOut) = do
      let stddev :: Float = Prelude.sqrt (2.0 / fromIntegral (fanIn + fanOut))
      weights <- randnIO' [fanIn, fanOut]
      let scaledWeights = weights * asTensor stddev
      let biases = zeros' [1, fanOut]
      pure (scaledWeights, biases)

testNeuralNet :: IO NeuralNet
testNeuralNet = initNeuralNet [784, 16, 16, 10]

digitToTensor :: Digit -> Tensor
digitToTensor digit = asTensor (V.toList (V.map normalizePixel digit.pixels) :: [Float])
  where
    normalizePixel p = fromIntegral p / 255.0

tick :: AppResources -> IORef AppState -> TickAction
tick AppResources {digits, digitCount} stateRef = do
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight
  let panelLayout = digitPanelLayout screenWidth screenHeight digitCount
      previewLayout = digitPreviewLayout panelLayout
      DigitPanelLayout {panelX = panelPosX, panelY = panelPosY, panelWidth = panelW, panelHeight = panelH} = panelLayout
      DigitPreviewLayout {previewX = previewPosX, previewY = previewPosY, previewWidth = previewW, previewHeight = previewH} = previewLayout
  mousePos <- getMousePosition
  let hoveredPanel =
        pointInRect
          mousePos
          panelPosX
          panelPosY
          panelW
          panelH
      hoveredPreview =
        pointInRect
          mousePos
          previewPosX
          previewPosY
          previewW
          previewH
  handleDigitPanelInput digits panelLayout stateRef mousePos hoveredPanel
  appState <- readIORef stateRef
  let digitSelection = appState.digitSelection
      digitScroll = appState.digitScroll
      nnRenderData = appState.neuralNetRenderData
      clampedSelection =
        if digitCount == 0
          then -1
          else clampValue 0 (digitCount - 1) digitSelection
      selectedDigit =
        if clampedSelection >= 0 && clampedSelection < digitCount
          then Just (clampedSelection, digits V.! clampedSelection)
          else Nothing
  when (clampedSelection /= digitSelection) $
    writeIORef stateRef appState {digitSelection = clampedSelection}
  updatedCamera <- updateCameraFromInput (not hoveredPanel && not hoveredPreview) stateRef
  beginDrawing
  clearBackground black
  beginMode2D updatedCamera
  drawNeuralNet nnRenderData
  endMode2D

  let buttonRect =
        Rectangle
          { rectangle'x = 20,
            rectangle'y = 20,
            rectangle'width = 140,
            rectangle'height = 30
          }
  buttonClicked <- guiButton buttonRect (Just "Reset camera")

  when buttonClicked $ do
    -- whatever you want the button to do; example: reset camera
    modifyIORef' stateRef $ \s ->
      s
        { camera =
            Camera2D
              { camera2D'offset = Vector2 0 0,
                camera2D'target = Vector2 0 0,
                camera2D'rotation = 0.0,
                camera2D'zoom = 1.0
              }
        }

  drawDigitPreview selectedDigit previewLayout
  drawDigitsPanel digits panelLayout digitScroll digitSelection
  endDrawing

drawLines :: [String] -> Int -> Int -> Int -> Int -> IO ()
drawLines linesToDraw x y fontSize lineHeight =
  zipWithM_ drawTextLine [0 ..] linesToDraw
  where
    drawTextLine idx lineText = drawText lineText x (y + idx * lineHeight) fontSize rayWhite

data AppState = AppState
  { camera :: Camera2D,
    digitSelection :: Int,
    digitScroll :: Int,
    neuralNet :: NeuralNet,
    neuralNetRenderData :: NeuralNetRenderData,
    currentInput :: Tensor
  }

mkInitialAppState :: V.Vector Digit -> IO AppState
mkInitialAppState digits = do
  nn <- testNeuralNet
  let input =
        if V.null digits
          then zeros' [784]
          else digitToTensor (digits V.! 0)
      neuron_values = getNeuronValues (forwardPass nn input) input
      renderData = computeNeuralNetRenderData nn neuron_values neuralNetRenderX neuralNetRenderY neuralNetRenderWidth neuralNetRenderHeight
  pure
    AppState
      { camera =
          Camera2D
            { camera2D'offset = Vector2 0 0,
              camera2D'target = Vector2 0 0,
              camera2D'rotation = 0.0,
              camera2D'zoom = 1.0
            },
        digitSelection = 0,
        digitScroll = 0,
        neuralNet = nn,
        neuralNetRenderData = renderData,
        currentInput = input
      }

updateCameraFromInput :: Bool -> IORef AppState -> IO Camera2D
updateCameraFromInput inputEnabled stateRef = do
  appState <- readIORef stateRef
  let camera = appState.camera
  if not inputEnabled
    then pure camera
    else do
      wheelMove <- getMouseWheelMove
      mouseDelta <- getMouseDelta
      mouseDown <- isMouseButtonDown MouseButtonLeft
      mousePos <- getMousePosition
      let Camera2D {camera2D'offset, camera2D'target, camera2D'rotation, camera2D'zoom} = camera
          zoomFactor = 1.0 + wheelMove * 0.1
          clampedZoom = clampValue 0.1 500.0 (camera2D'zoom * zoomFactor)
          zoomChanged = clampedZoom /= camera2D'zoom
          worldBeforeZoom = getScreenToWorld2D mousePos camera
          worldAfterZoom = getScreenToWorld2D mousePos camera {camera2D'zoom = clampedZoom}
          zoomOffset =
            if zoomChanged
              then worldBeforeZoom - worldAfterZoom
              else Vector2 0 0
          panDelta = if mouseDown then -(mouseDelta |* (1.0 / clampedZoom)) else Vector2 0 0
          newTarget = camera2D'target + zoomOffset + panDelta
          newCamera =
            Camera2D
              { camera2D'offset,
                camera2D'target = newTarget,
                camera2D'rotation,
                camera2D'zoom = clampedZoom
              }
      writeIORef stateRef appState {camera = newCamera}
      pure newCamera

clampValue :: (Ord a) => a -> a -> a -> a
clampValue low high = Prelude.max low . Prelude.min high

mkDigit :: Int -> V.Vector Int -> Digit
mkDigit digitLabel rawPixels =
  Digit
    { label = digitLabel,
      pixels = rawPixels,
      filledPixels = buildRenderPixels rawPixels
    }

buildRenderPixels :: V.Vector Int -> V.Vector RenderPixel
buildRenderPixels rawPixels = V.imapMaybe renderPixel rawPixels

renderPixel :: Int -> Int -> Maybe RenderPixel
renderPixel idx value =
  let clamped = clampValue 0 255 value
   in if clamped <= 0
        then Nothing
        else
          let (row, col) = idx `divMod` digitSidePixels
              intensity = fromIntegral clamped :: Word8
              color = Color intensity intensity intensity 255
           in Just RenderPixel {pixelX = col, pixelY = row, pixelColor = color}
