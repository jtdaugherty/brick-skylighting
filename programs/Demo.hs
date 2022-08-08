{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Skylighting.Core as S
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Brick
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (borderWithLabel, hBorder)

import Brick.Widgets.Skylighting (highlight, attrMappingsForStyle)

haskellProgram :: T.Text
haskellProgram = T.unlines
  [ "module Main where"
  , ""
  , "data FooBar = Foo | Bar deriving (Eq, Read, Show)"
  , ""
  , "main :: IO ()"
  , "main = do"
  , "  let f x = x * x"
  , "  return ()"
  ]

pythonProgram :: T.Text
pythonProgram = T.unlines
  [ "import os.path"
  , ""
  , "if __name__ == \"__main__\":"
  , "    print('hello, world!')"
  ]

bashProgram :: T.Text
bashProgram = T.unlines
  [ "FOO=1"
  , ""
  , "function print_foo {"
  , "  echo $FOO"
  , "}"
  , ""
  , "print_foo"
  ]

ui :: [(T.Text, S.Syntax)] -> Int -> [Widget ()]
ui programs styleIndex =
    [vBox $ help : hBorder : header : progs]
    where
        help = hCenter $ txt "q/esc:quit   up/down:change theme"
        header = hCenter $ txt $ "Theme: " <> (fst $ styles !! styleIndex)
        progs = showProg <$> programs
        showProg (progSrc, syntax) =
            (borderWithLabel (txt $ S.sName syntax) $ highlight syntax progSrc)

styles :: [(T.Text, S.Style)]
styles =
    [ ("espresso", S.espresso)
    , ("kate", S.kate)
    , ("breezeDark", S.breezeDark)
    , ("pygments", S.pygments)
    , ("tango", S.tango)
    , ("haddock", S.haddock)
    , ("monochrome", S.monochrome)
    , ("zenburn", S.zenburn)
    ]

handleEvent :: BrickEvent () e -> EventM () Int ()
handleEvent (VtyEvent (V.EvKey V.KUp [])) = modify $ \i -> (i + 1) `mod` length styles
handleEvent (VtyEvent (V.EvKey V.KDown [])) = modify $ \i -> (i - 1) `mod` length styles
handleEvent (VtyEvent (V.EvKey V.KEsc [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = return ()

app :: [(T.Text, S.Syntax)] -> App Int e ()
app programs =
    App { appDraw = ui programs
        , appAttrMap = \i -> attrMap V.defAttr $
                             attrMappingsForStyle $ snd $ styles !! i
        , appHandleEvent = handleEvent
        , appChooseCursor = neverShowCursor
        , appStartEvent = return ()
        }

usage :: IO ()
usage = do
    pn <- getProgName
    putStrLn $ pn <> " <path to XML syntax definintion directory>"

main :: IO ()
main = do
    args <- getArgs
    path <- case args of
        [p] -> return p
        _ -> usage >> exitFailure

    syntaxMap <- do
        result <- S.loadSyntaxesFromDir path
        case result of
            Left e -> do
                putStrLn $ "Failed to load syntax map: " <> e
                exitFailure
            Right m -> return m

    let syntax = fromJust . S.syntaxByName syntaxMap
        programs = [ (haskellProgram, syntax "haskell")
                   , (pythonProgram, syntax "python")
                   , (bashProgram, syntax "bash")
                   ]

    void $ defaultMain (app programs) 0
