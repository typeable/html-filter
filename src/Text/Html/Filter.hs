module Text.Html.Filter
  ( filterHtml
  , filterTags
  , Attr(..)
  , noAttributes
  )where

import Text.HTML.Parser
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Text
import qualified Data.Text.Lazy
import Data.List (partition)
import Data.Bifunctor (bimap)
import Data.Text (Text)


-- | Drop any attributes
noAttributes :: (Text,Attr) -> Bool
noAttributes = const False

-- | 'filterHtml' by list of allowed tags.
filterTags
  :: Set Text
  -- ^ Set of allowed tags
  -> ((Text,Attr) -> Bool)
  -> Text
  -> (Data.Text.Lazy.Text, Set (Text,[Attr]))
filterTags tags = filterHtml (`Set.member` tags)

-- | Filter out html tags. You can supply predicate to test permitted tags and a predicate for accepting attributes.
-- Any non-permitted tags will be escaped into plain text while comments and doctype declarations are
-- removed.
--
-- Returns the new html and set of tags and attributes that were discarded / escaped.
filterHtml
  :: (Text -> Bool)
  -- ^ Tag predicate
  -> ((Text,Attr) -> Bool)
  -- ^ Attribute predicate
  -> Text
  -- ^ Raw HTML
  -> (Data.Text.Lazy.Text, Set (Text,[Attr]))
filterHtml permittedTags permittedAttributes rawHtml = let
  (discards, tokens) = unzip . map rewrite . canonicalizeTokens . parseTokens $ rawHtml
  in (renderTokens tokens, mconcat discards)
  where
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
        | permittedTags name
        , (goodAttrs, badAttrs) <- filterAttrs name attrs
        -> (recordAttrs name badAttrs, TagOpen name goodAttrs)
        | otherwise -> (record name attrs, asEscapedText t)
      t@(TagSelfClose name attrs)
        | permittedTags name
        , (goodAttrs, badAttrs) <- filterAttrs name attrs
        -> (recordAttrs name badAttrs, TagSelfClose name goodAttrs)
        | otherwise -> (record name attrs, asEscapedText t)
      t@(TagClose name)
        | permittedTags name -> (mempty, t)
        | otherwise -> (record name [], asEscapedText t)
      Comment _ -> (mempty, ContentText "")
      Doctype _ -> (mempty, ContentText "")
      t@ContentText{} -> (mempty, t)
      t@ContentChar{} -> (mempty, t)
