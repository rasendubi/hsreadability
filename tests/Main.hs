{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Data.FileEmbed (embedFile)
import Data.Aeson (decodeStrict)

import System.Exit (exitFailure)

import Network.Readability.Parser

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit

import Test.HUnit

main = defaultMain tests

tests =
    [ testCase "decode parser response 1" test_parser_json1
    ]

test_parser_json1 = assertEqual "decode example /parser response"
    (Just Article
        { content = "<div class=\"article-text\">\n<p>I'm idling outside Diamante's, [snip] ...</p></div>"
        , domain = "www.gq.com"
        , author = "Rafi Kohan"
        , url = "http://www.gq.com/sports/profiles/201202/david-diamante-interview-cigar-lounge-brooklyn-new-jersey-nets?currentPage=all"
        , short_url = "http://rdd.me/g3jcb1sr"
        , title = "Blowing Smoke with Boxing's Big Voice"
        , excerpt = "I'm idling outside Diamante's, a cigar lounge in Fort Greene, waiting for David Diamante, and soon I smell him coming. It's late January but warm. A motorcycle growls down the Brooklyn side street,&hellip;"
        , direction = "ltr"
        , word_count = 2892
        , total_pages = 1
        , date_published = Nothing
        , dek = Just "Announcer <strong>David Diamante</strong>, the new voice of the New Jersey (soon Brooklyn) Nets, has been calling boxing matches for years. On the side, he owns a cigar lounge in the heart of Brooklyn. We talk with Diamante about his new gig and the fine art of cigars"
        , lead_image_url = Just "http://www.gq.com/images/entertainment/2012/02/david-diamante/diamante-628.jpg"
        , next_page_id = Nothing
        , rendered_pages = 1
        })
    (decodeStrict $(embedFile "tests/files/parser_article.json"))
