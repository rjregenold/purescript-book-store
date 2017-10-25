module HTML where

import Data.Either (either)
import Data.List (List, intercalate, mapMaybe)
import Data.Maybe (Maybe(..))
import LenientHtmlParser (Tag(..), parseTags)
import Prelude


-- | Given a string that contains HTML tags, this function will strip it 
-- | of all tags and return the contents.
stripTags :: String -> String
stripTags = either parseFail parsePass <<< parseTags
  where
    parseFail _ = ""

    parsePass :: List Tag -> String
    parsePass = intercalate " " <<< mapMaybe handleTag

    handleTag :: Tag -> Maybe String
    handleTag = 
      case _ of
        TNode x -> Just x
        _       -> Nothing

