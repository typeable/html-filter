module Text.Html.Filter
  ( filterTags
  , Attr(..)
  , noAttributes
  )where

import Text.HTML.Parser
import Data.Set qualified as Set
import Data.Set (Set)
import Data.Text qualified
import Data.Text.Lazy qualified
import Data.List (partition)
import Data.Bifunctor (bimap)
import Data.Text (Text)


noAttributes :: (Text,Attr) -> Bool
noAttributes = const False

-- | Filter out html tags. You can supply set of permitted tags and a predicate for accepting attributes.
-- Any non-permitted tags will escaped into plain text while comments and doctype declarations are
-- removed.
--
-- Returns the new html and set of tags and attributes that were discarded / escaped.
filterTags :: Set Text -> ((Text,Attr) -> Bool) -> Text -> (Data.Text.Lazy.Text, Set (Text,[Attr]))
filterTags permittedTags permittedAttributes rawHtml = let
  (discards, tokens) = unzip . map rewrite . canonicalizeTokens . parseTokens $ rawHtml
  in (renderTokens tokens, mconcat discards)
  where
    check name = name `Set.member` permittedTags
    encodeHtml = Data.Text.concatMap \case
      '>' -> "&gt;"
      '<' -> "&lt;"
      '\'' -> "&#39;"
      '"' -> "&quot;"
      '&' -> "&amp;"
      x -> Data.Text.singleton x
    filterAttrs :: Text -> [Attr] -> ([Attr], [Attr])
    filterAttrs name = bimap (map snd) (map snd) . partition permittedAttributes . fmap (name,)
    record name attrs = Set.singleton (name,attrs)
    recordAttrs _ [] = mempty
    recordAttrs name as = record name as
    asEscapedText = ContentText . encodeHtml . Data.Text.Lazy.toStrict . renderToken
    rewrite = \case
      t@(TagOpen name attrs)
        | check name
        , (goodAttrs, badAttrs) <- filterAttrs name attrs
        -> (recordAttrs name badAttrs, TagOpen name goodAttrs)
        | otherwise -> (record name attrs, asEscapedText t)
      t@(TagSelfClose name attrs)
        | check name
        , (goodAttrs, badAttrs) <- filterAttrs name attrs
        -> (recordAttrs name badAttrs, TagSelfClose name goodAttrs)
        | otherwise -> (record name attrs, asEscapedText t)
      t@(TagClose name)
        | check name -> (mempty, t)
        | otherwise -> (record name [], asEscapedText t)
      Comment _ -> (mempty, ContentText "")
      Doctype _ -> (mempty, ContentText "")
      t@ContentText{} -> (mempty, t)
      t@ContentChar{} -> (mempty, t)

