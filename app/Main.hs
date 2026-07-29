module Main (main) where

import Control.Exception (try)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Refina.AST.Dump (prettyModule)
import Refina.Parser (parseModule, parseRefina)
import Relude
import System.IO.Error (IOError, isDoesNotExistError)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      let src = "X := Str where _ != \"\"\nName := Str\nSchema := name : Str where _ !~ \"-\";"
      case parseModule src of
        Left err -> do
          putStrLn (errorBundlePretty err)
          exitFailure
        Right m -> do
          putStrLn "SUCCESS: parsed"
          putStrLn $ renderString $ layoutPretty defaultLayoutOptions $ prettyModule m
    (f : _) -> do
      result <- try @SomeException (readFileText f)
      case result of
        Left ex
          | Just ioErr <- fromException @IOError ex
          , isDoesNotExistError ioErr -> do
              putTextLn ("Error: file not found: " <> toText f)
              exitFailure
          | otherwise -> do
              putTextLn ("Error: " <> show ex)
              exitFailure
        Right src ->
          case parseRefina f src of
            Left err -> do
              putStrLn (errorBundlePretty err)
              exitFailure
            Right m -> do
              putStrLn "SUCCESS: parsed"
              putStrLn $ renderString $ layoutPretty defaultLayoutOptions $ prettyModule m
