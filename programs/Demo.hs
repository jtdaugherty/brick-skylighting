{-# LANGUAGE OverloadedStrings #-}
module Main where

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

programs :: [(T.Text, T.Text)]
programs =
    [ (haskellProgram, "Haskell")
    , (pythonProgram, "Python")
    ]

ui :: Widget ()
ui =
    vBox progs
    where
        progs = showProg <$> programs
        showProg (progSrc, langName) =
            let Just syntax = S.syntaxByName S.defaultSyntaxMap langName
            in (borderWithLabel (txt langName) $ codeBlock syntax progSrc)

app :: App s e ()
app =
    (simpleApp ui) { appAttrMap = const $ attrMap V.defAttr $
                                  attrMappingsForStyle S.espresso
                   }

main :: IO ()
main = defaultMain app ()
