{-# LANGUAGE OverloadedStrings #-}
-- | This module provides an API for building Brick widgets to display
-- syntax-highlighted text using the Skylighting library.
--
-- To use this module, you'll need to:
--
--  * have some 'Text' you want to syntax-highlight.
--  * know the language in which the 'Text' is expressed.
--  * have a Skylighting 'Style' you'd like to use to determine the
--    colors, either from the Skylighting package or one of your own.
--
-- To highlight some text in your user interface, use one of
-- the (increasingly finer-grained) highlighting functions
-- 'simpleHighlight', 'highlight', 'highlight'', or 'renderRawSource'.
--
-- To actually see pretty colors, you'll need to update your
-- application's 'AttrMap' with name-to-color mappings. Those can be
-- built from a Skylighting 'Style' with 'attrMappingsForStyle' and then
-- appended to your 'attrMap' mapping list.
--
-- The highlighted code widget produced by this module uses
-- 'highlightedCodeBlockAttr' as its base attribute and then uses
-- a specific attribute for each kind of Skylighting token as per
-- 'attrNameForTokenType'.
--
-- See the 'Demo.hs' program in this package for an example of a
-- complete program that uses this module.
module Brick.Widgets.Skylighting
  ( simpleHighlight
  , highlight
  , highlight'
  , renderRawSource

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

-- | The simplest way to highlight some text.
simpleHighlight :: T.Text
                -- ^ The Skylighting name of the language in which the
                -- input text is written.
                -> T.Text
                -- ^ The text to be syntax-highlighted.
                -> Widget n
simpleHighlight langName body =
    case Sky.syntaxByName Sky.defaultSyntaxMap langName of
        Nothing -> txt $ expandTabs body
        Just syntax -> highlight syntax body

-- | If you already have a 'Syntax' handy, this provides more control
-- than 'simpleHighlight'.
highlight :: Sky.Syntax
          -- ^ The syntax to use to parse the input text.
          -> T.Text
          -- ^ The text to be syntax-highlighted.
          -> Widget n
highlight = highlight' txt

-- | If you already have a 'Syntax' handy and want to have control over
-- how each 'Text' token in the Skylighting AST gets converted to a
-- 'Widget', this provides more control than 'highlight', which just
-- defaults the text widget constructor to 'txt'.
highlight' :: (T.Text -> Widget n)
           -- ^ The token widget constructor.
           -> Sky.Syntax
           -- ^ The syntax to use to parse the input text.
           -> T.Text
           -- ^ The text to be syntax-highlighted.
           -> Widget n
highlight' renderToken syntax tx =
    let cfg = Sky.TokenizerConfig (M.fromList [(Sky.sName syntax, syntax)]) False
        expanded = expandTabs tx
        result = Sky.tokenize cfg syntax expanded
    in case result of
        Left _ -> txt expanded
        Right tokLines -> renderRawSource renderToken tokLines

expandTabs :: T.Text -> T.Text
expandTabs = T.replace "\t" (T.replicate 8 " ")

-- | If you have already parsed your input text into Skylighting tokens,
-- this function is the best one to use.
renderRawSource :: (T.Text -> Widget n)
                -- ^ The token widget constructor.
                -> [Sky.SourceLine]
                -- ^ The parsed input.
                -> Widget n
renderRawSource renderToken ls =
    withDefAttr highlightedCodeBlockAttr $
    vBox $ renderTokenLine renderToken <$> ls

renderTokenLine :: (T.Text -> Widget n) -> Sky.SourceLine -> Widget n
renderTokenLine _ [] = str " "
renderTokenLine renderToken toks =
    let renderSingle (ty, tx) = withDefAttr (attrNameForTokenType ty) $ renderToken tx
    in hBox $ renderSingle <$> toks

-- | The base attribute name for all syntax-highlighted renderings.
highlightedCodeBlockAttr :: AttrName
highlightedCodeBlockAttr = "highlightedCodeBlock"

-- | The constructor for attribute names for each 'TokenType' in
-- Skylighting.
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

-- | Given a Skylighting 'Style', build a reasonable list of
-- Brick-compatible 'AttrMap' entries. This may return 256-color
-- entries.
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
