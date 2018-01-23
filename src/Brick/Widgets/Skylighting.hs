{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.Skylighting
  ( codeBlock
  , codeBlock'

  -- * Attributes
  , attrMappingsForStyle
  , attrNameForTokenType
  , highlightedCodeBlockAttr
  )
where

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Graphics.Vty as V

import Brick

import qualified Skylighting as Sky
import Skylighting (TokenType(..))

codeBlock :: Sky.Syntax -> T.Text -> Widget n
codeBlock = codeBlock' txt

codeBlock' :: (T.Text -> Widget n) -> Sky.Syntax -> T.Text -> Widget n
codeBlock' renderToken syntax tx =
    let expandedTabs = T.replace "\t" (T.replicate 8 " ") tx
        cfg = Sky.TokenizerConfig (M.fromList [(Sky.sName syntax, syntax)]) False
        result = Sky.tokenize cfg syntax expandedTabs
    in case result of
        Left _ -> txt expandedTabs
        Right tokLines -> rawCodeBlock renderToken tokLines

rawCodeBlock :: (T.Text -> Widget n) -> [Sky.SourceLine] -> Widget n
rawCodeBlock renderToken ls =
    withDefAttr highlightedCodeBlockAttr $
    vBox $ renderTokenLine renderToken <$> ls

renderTokenLine :: (T.Text -> Widget n) -> Sky.SourceLine -> Widget n
renderTokenLine _ [] = str " "
renderTokenLine renderToken toks =
    let renderSingle (ty, tx) = withDefAttr (attrNameForTokenType ty) $ renderToken tx
    in hBox $ renderSingle <$> toks

highlightedCodeBlockAttr :: AttrName
highlightedCodeBlockAttr = "highlightedCodeBlock"

attrNameForTokenType :: TokenType -> AttrName
attrNameForTokenType ty = highlightedCodeBlockAttr <> attrName s
    where
        s = case ty of
          KeywordTok        -> "keyword"
          DataTypeTok       -> "dataType"
          DecValTok         -> "declaration"
          BaseNTok          -> "baseN"
          FloatTok          -> "float"
          ConstantTok       -> "constant"
          CharTok           -> "char"
          SpecialCharTok    -> "specialChar"
          StringTok         -> "string"
          VerbatimStringTok -> "verbatimString"
          SpecialStringTok  -> "specialString"
          ImportTok         -> "import"
          CommentTok        -> "comment"
          DocumentationTok  -> "documentation"
          AnnotationTok     -> "annotation"
          CommentVarTok     -> "comment"
          OtherTok          -> "other"
          FunctionTok       -> "function"
          VariableTok       -> "variable"
          ControlFlowTok    -> "controlFlow"
          OperatorTok       -> "operator"
          BuiltInTok        -> "builtIn"
          ExtensionTok      -> "extension"
          PreprocessorTok   -> "preprocessor"
          AttributeTok      -> "attribute"
          RegionMarkerTok   -> "regionMarker"
          InformationTok    -> "information"
          WarningTok        -> "warning"
          AlertTok          -> "alert"
          ErrorTok          -> "error"
          NormalTok         -> "normal"

attrMappingsForStyle :: Sky.Style -> [(AttrName, V.Attr)]
attrMappingsForStyle sty =
    (highlightedCodeBlockAttr, baseAttrFromPair (Sky.defaultColor sty, Sky.backgroundColor sty)) :
    (mkTokenTypeEntry <$> (M.toList $ Sky.tokenStyles sty))

baseAttrFromPair :: (Maybe Sky.Color, Maybe Sky.Color) -> V.Attr
baseAttrFromPair (mf, mb) =
    case (mf, mb) of
        (Nothing, Nothing) -> V.defAttr
        (Just f, Nothing)  -> fg (tokenColorToVtyColor f)
        (Nothing, Just b)  -> bg (tokenColorToVtyColor b)
        (Just f, Just b)   -> (tokenColorToVtyColor f) `on`
                              (tokenColorToVtyColor b)

tokenColorToVtyColor :: Sky.Color -> V.Color
tokenColorToVtyColor (Sky.RGB r g b) = V.rgbColor r g b

mkTokenTypeEntry :: (Sky.TokenType, Sky.TokenStyle) -> (AttrName, V.Attr)
mkTokenTypeEntry (ty, tSty) =
    let a = setStyle baseAttr
        baseAttr = baseAttrFromPair (Sky.tokenColor tSty, Sky.tokenBackground tSty)
        setStyle =
            if Sky.tokenBold tSty then flip V.withStyle V.bold else id .
            if Sky.tokenItalic tSty then flip V.withStyle V.standout else id .
            if Sky.tokenUnderline tSty then flip V.withStyle V.underline else id

    in (attrNameForTokenType ty, a)
