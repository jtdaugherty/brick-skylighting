{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Skylighting as S

import Brick
import Brick.Widgets.Border (borderWithLabel)

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

ui :: Widget ()
ui =
    vBox progs
    where
        progs = showProg <$> programs
        showProg (progSrc, langName) =
            let Just syntax = S.syntaxByName S.defaultSyntaxMap langName
            in (borderWithLabel (txt langName) $ codeBlock syntax progSrc)

styles :: [S.Style]
styles =
    [ S.kate
    , S.breezeDark
    , S.pygments
    , S.espresso
    , S.tango
    , S.haddock
    , S.monochrome
    , S.zenburn
    ]

handleEvent :: Int -> BrickEvent () e -> EventM () (Next Int)
handleEvent i (VtyEvent (V.EvKey V.KUp [])) = continue $ (i + 1) `mod` length styles
handleEvent i (VtyEvent (V.EvKey V.KDown [])) = continue $ (i - 1) `mod` length styles
handleEvent i (VtyEvent (V.EvKey V.KEsc [])) = halt i
handleEvent i (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt i
handleEvent i _ = continue i

app :: App Int e ()
app =
    (simpleApp ui) { appAttrMap =
                       \i -> attrMap V.defAttr $
                             attrMappingsForStyle $ styles !! i
                   , appHandleEvent = handleEvent
                   }

main :: IO ()
main = void $ defaultMain app 0
