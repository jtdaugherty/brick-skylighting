{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Skylighting as S

import Brick
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (borderWithLabel, hBorder)

import Brick.Widgets.Skylighting (codeBlock, attrMappingsForStyle)

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

programs :: [(T.Text, T.Text)]
programs =
    [ (haskellProgram, "Haskell")
    , (pythonProgram, "Python")
    , (bashProgram, "Bash")
    ]

ui :: Int -> [Widget ()]
ui styleIndex =
    [vBox $ usage : hBorder : header : progs]
    where
        usage = hCenter $ txt "q/esc:quit   up/down:change theme"
        header = hCenter $ txt $ "Theme: " <> (fst $ styles !! styleIndex)
        progs = showProg <$> programs
        showProg (progSrc, langName) =
            let Just syntax = S.syntaxByName S.defaultSyntaxMap langName
            in (borderWithLabel (txt langName) $ codeBlock syntax progSrc)

styles :: [(T.Text, S.Style)]
styles =
    [ ("kate", S.kate)
    , ("breezeDark", S.breezeDark)
    , ("pygments", S.pygments)
    , ("espresso", S.espresso)
    , ("tango", S.tango)
    , ("haddock", S.haddock)
    , ("monochrome", S.monochrome)
    , ("zenburn", S.zenburn)
    ]

handleEvent :: Int -> BrickEvent () e -> EventM () (Next Int)
handleEvent i (VtyEvent (V.EvKey V.KUp [])) = continue $ (i + 1) `mod` length styles
handleEvent i (VtyEvent (V.EvKey V.KDown [])) = continue $ (i - 1) `mod` length styles
handleEvent i (VtyEvent (V.EvKey V.KEsc [])) = halt i
handleEvent i (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt i
handleEvent i _ = continue i

app :: App Int e ()
app =
    App { appDraw = ui
        , appAttrMap = \i -> attrMap V.defAttr $
                             attrMappingsForStyle $ snd $ styles !! i
        , appHandleEvent = handleEvent
        , appChooseCursor = neverShowCursor
        , appStartEvent = return
        }

main :: IO ()
main = void $ defaultMain app 0
