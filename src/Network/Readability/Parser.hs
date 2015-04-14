{-# LANGUAGE TemplateHaskell #-}
module Network.Readability.Parser
    ( ParserResult(..)
    ) where

import Data.Text (Text)
import Data.Aeson.TH (deriveFromJSON, defaultOptions)

data ParserResult = ParserResult
    { content :: Text -- Html
    , domain :: Text -- Url
    , author :: Text
    , url :: Text -- Url
    , short_url :: Text -- Url
    , title :: Text
    , excerpt :: Text
    , direction :: Text -- make data type?
    , word_count :: Integer
    , total_pages :: Integer
    , date_published :: Maybe Text -- Date
    , dek :: Maybe Text -- or Html?
    , lead_image_url :: Maybe Text -- Url
    , next_page_id :: Maybe Int -- pageId?
    , rendered_pages :: Int
    } deriving (Show, Eq)

$(deriveFromJSON defaultOptions ''ParserResult)

-- parser :: ReadabilityToken -> Maybe Url -> Maybe ArticleId -> Maybe Int -> IO ParserResult
-- parser = undefined
