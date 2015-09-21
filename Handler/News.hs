{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE RecordWildCards #-}

module Handler.News where

import Import
import Codec.MFO.API.Operator
import Codec.JSON.RPC
import Data.Time.Clock
import Data.Aeson

import Yesod.Auth
import Data.Conduit
import Data.Conduit.List as CL

import qualified Data.List as DL (head)
import Data.Text.Encoding as TE

postNewsR :: Handler Value
postNewsR = do
    authorid <- requireAuthId
    mjson <- getJSONRequest
    case mjson of
         Success ORNAddNews {..} -> do
             now <- liftIO $! getCurrentTime
             let newsitem = NewsItem
                     { newsItemPostdate = now
                     , newsItemAuthor = authorid
                     , newsItemTitle = ornTitle
                     , newsItemShortContent = ornShortContent
                     , newsItemContent  = ornContent
                     }
             runDB $! insert newsitem
             return $! toJSON $! OANAdded
         Success _ -> notFound
         Error _ -> notFound


getNewsR :: Handler TypedContent
getNewsR = do
    respondSourceDB typeJson $!do
        selectSource [] [Desc NewsItemPostdate]
            =$= awaitForever toNewsShort
            =$= awaitForever toBuilder
    where
    toNewsShort:: Monad m => Entity NewsItem -> Conduit (Entity NewsItem) m NewsShort
    toNewsShort entity@(Entity newsid (NewsItem {..}))= do
        let values:: [PersistValue]
            values = keyToValues newsid
            eid:: Either Text Text
            eid = fromPersistValueText $ DL.head values
            id = case eid of
                    Left _ -> error "not a key!"
                    Right some -> some
        yield $! NewsShort
             { nsId = id
             , nsPostDate = newsItemPostdate
             , nsTopic = newsItemTitle
             , nsTags = ""
             , nsShortContent = newsItemShortContent
             }
     --toBuilder entity@(Entity newid (NewsItem {..})) = do
    toBuilder ns@(NewsShort {..}) = do
        sendChunkLBS $ encode ns
        sendChunkText "\n"
        sendFlush


getGetNewsR:: NewsItemId -> Handler TypedContent
getGetNewsR id = do
    respondSourceDB typeJson $! do
        selectSource [NewsItemId ==. id] []
            =$= awaitForever toNewsFull
            =$= awaitForever toBuilder
    where
    toBuilder ns@(NewsFull {..}) = do
        sendChunkLBS $ encode ns
        sendChunkText "\n"
        sendFlush
    toNewsFull entity@(Entity newsid (NewsItem{..})) = do
        let values:: [PersistValue]
            values = keyToValues newsid
            eid:: Either Text Text
            eid = fromPersistValueText $ DL.head values
            nfid = case eid of
                    Left _ -> error "not a key!"
                    Right some -> some
        yield $! NewsFull
             { nfId = nfid
             , nfPostDate = newsItemPostdate
             , nfTopic = newsItemTitle
             , nfTags = ""
             , nfShortContent = newsItemShortContent
             , nfContent = newsItemContent
             }

postDeleteNewsR:: NewsItemId -> Handler Value
postDeleteNewsR id = do
    authid <- requireAuthId
    runDB $! delete id
    return $! toJSON $! OANDeleted
