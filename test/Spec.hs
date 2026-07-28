module Main (main) where

import Refina.ParserProperties (propertySpec)
import Refina.ParserSpec (spec)
import Relude
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  spec
  propertySpec
