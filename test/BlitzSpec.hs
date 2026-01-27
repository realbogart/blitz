module BlitzSpec (spec) where

import Data.Int (Int32)
import Data.Word (Word32, Word64)
import Test.Hspec
import Test.QuickCheck

-- Mirror the encoding logic from Blitz to test at the Haskell level.
-- encodePixel packs (originalIndex, color) into Word64:
--   high 32 bits = index + 1, low 32 bits = color
encodePixelHS :: Int32 -> Word32 -> Word64
encodePixelHS idx col =
  fromIntegral (idx + 1) * 0x100000000 + fromIntegral col

-- decodeColor extracts low 32 bits (truncation via fromIntegral)
decodeColorHS :: Word64 -> Word32
decodeColorHS packed = fromIntegral packed

spec :: Spec
spec = do
  describe "Pixel encoding" $ do
    it "decodeColor recovers the color from encodePixel" $
      property $ \(idx :: Int32, col :: Word32) ->
        idx >= 0 ==> decodeColorHS (encodePixelHS idx col) == col

    it "background encodes with zero in high bits" $ do
      let bg = encodePixelHS (-1) 0xFF000000
      bg `shouldSatisfy` (< 0x100000000)

    it "real primitives encode with nonzero high bits" $
      property $ \(idx :: Int32) ->
        idx >= 0 ==>
          let encoded = encodePixelHS idx 0
           in encoded >= 0x100000000

  describe "Draw order via max" $ do
    it "higher original index wins when using max" $
      property $ \(lo :: Int32, hi :: Int32, colLo :: Word32, colHi :: Word32) ->
        lo >= 0 && hi > lo ==>
          let encLo = encodePixelHS lo colLo
              encHi = encodePixelHS hi colHi
           in max encLo encHi == encHi

    it "any primitive wins over background via max" $
      property $ \(idx :: Int32, col :: Word32) ->
        idx >= 0 ==>
          let bg = encodePixelHS (-1) 0xFF000000
              prim = encodePixelHS idx col
           in max bg prim == prim

  describe "Composite correctness" $ do
    it "compositing two passes selects highest original index" $ do
      let circleEnc = encodePixelHS 100 0xFFAABBCC
          lineEnc = encodePixelHS 101 0xFF112233
          result = decodeColorHS (max circleEnc lineEnc)
      result `shouldBe` 0xFF112233

    it "compositing with background preserves primitive color" $ do
      let bg = encodePixelHS (-1) 0xFF000000
          prim = encodePixelHS 42 0xFFDDEEFF
          result = decodeColorHS (max bg prim)
      result `shouldBe` 0xFFDDEEFF
