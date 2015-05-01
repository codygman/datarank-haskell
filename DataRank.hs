{-# LANGUAGE OverloadedStrings #-}

module DataRank 
(
    DataRank(..)
    ,newClient
    ,fizzleValidate
    ,findTopics
    ,findTopic
    ,comments
    ,volume
    ,volumeDaily
    ,reach
    ,reachDaily
    ,sentiment
    ,sentimentDaily
    ,interaction
    ,interactionDaily
    ,wordcloud
    ,locationPins
    ,locationProvinces
    ,datasources
    ,datasourceTypes
) 
where

import Network (withSocketsDo)
import Network.HTTP.Conduit 
import Network.HTTP.Types
import Network.HTTP.Types.Header
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Char8 as C
import Data.List as D


type QueryParameter = (String, String)

apiUrl :: String
apiUrl = "https://api.datarank.com/"


data DataRank = DataRank { 
                    version :: String
                    ,accessToken :: ByteString 
                    } deriving Show


newClient :: String -> String -> DataRank
newClient accessToken version = DataRank {
                            accessToken = C.pack accessToken
                            ,version = version
                        }

convertParameters :: [QueryParameter] -> [(ByteString, Maybe ByteString)]
convertParameters queryParameters = D.map convertParameter queryParameters


convertParameter :: QueryParameter -> (ByteString, Maybe ByteString)
convertParameter (k,v) = (C.pack k, Just (C.pack v))


buildRequestUrl :: String -> IO(Request)
buildRequestUrl endpoint = parseUrl (apiUrl ++ endpoint)


buildRequestHeaders :: DataRank -> RequestHeaders
buildRequestHeaders config = [ ("Content-Type" , "application/json")
                             , ("Accept" , "application/vnd.datarank.v1+json")
                             , ( "Authorization" , (accessToken config))
                             ]


post :: String -> [QueryParameter] -> BS.ByteString -> DataRank -> IO(Response BS.ByteString)
post endpoint queryParameters postData config = do
                baseRequest <- buildRequestUrl endpoint
                let request' = setQueryString (convertParameters queryParameters) baseRequest
                let request = request' { method = methodPost
                   , requestHeaders = buildRequestHeaders config 
                   , requestBody = RequestBodyLBS postData
                   }
                (withManager $ httpLbs request)


get :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
get endpoint queryParameters config = do
                baseRequest <- buildRequestUrl endpoint
                let request' = setQueryString (convertParameters queryParameters) baseRequest
                let request = request' { method = methodGet
                   , requestHeaders = buildRequestHeaders config 
                   }
                (withManager $ httpLbs request)


fizzleValidate :: String -> DataRank -> IO(Response BS.ByteString)
fizzleValidate query config = do
        let postData = "{\"q\": \"" ++ query ++ "\"}"
        let postDataBS = BS.pack postData
        post "fizzle/validate" [] postDataBS config


 -- list all subscribed topics
findTopics :: DataRank -> IO(Response BS.ByteString)
findTopics config = get "topics" [] config


 -- list details of a specific topic
findTopic :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
findTopic slug searchFilters config = get ("topics/" ++ slug) searchFilters config


 -- list comments associated to a topic
comments :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
comments slug searchFilters config = get ("topics/" ++ slug ++ "/comments") searchFilters config


-- get topic volume data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
volume :: String -> String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
volume slug interval searchFilters config = get ("topics/" ++ slug ++ "/volume/" ++ interval) searchFilters config


volumeDaily :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
volumeDaily slug searchFilters config = get ("topics/" ++ slug ++ "/volume/daily") searchFilters config


-- get topic reach data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
reach :: String -> String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
reach slug interval searchFilters config = get ("topics/" ++ slug ++ "/reach/" ++ interval) searchFilters config


reachDaily :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
reachDaily slug searchFilters config = get ("topics/" ++ slug ++ "/reach/daily") searchFilters config


-- get topic interaction data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
interaction :: String -> String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
interaction slug interval searchFilters config = get ("topics/" ++ slug ++ "/interaction/" ++ interval) searchFilters config


interactionDaily :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
interactionDaily slug searchFilters config = get ("topics/" ++ slug ++ "/interaction/daily") searchFilters config


-- get topic sentiment data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
sentiment :: String -> String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
sentiment slug interval searchFilters config = get ("topics/" ++ slug ++ "/sentiment/" ++ interval) searchFilters config


sentimentDaily :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
sentimentDaily slug searchFilters config = get ("topics/" ++ slug ++ "/sentiment/daily") searchFilters config


-- get word cloud including term sentiment/frequency
wordcloud :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
wordcloud slug searchFilters config = get ("topics/" ++ slug ++ "/wordcloud") searchFilters config


-- get location pins, contains lat/lon coordinates of commenters within topic
locationPins :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
locationPins slug searchFilters config = get ("topics/" ++ slug ++ "/location/pins") searchFilters config


-- get demographic information around provinces related to topic
locationProvinces :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
locationProvinces slug searchFilters config = get ("topics/" ++ slug ++ "/location/provinces") searchFilters config


-- get datasource stats
datasources :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
datasources slug searchFilters config = get ("topics/" ++ slug ++ "/datasources") searchFilters config

-- get datasource type stats
datasourceTypes :: String -> [QueryParameter] -> DataRank -> IO(Response BS.ByteString)
datasourceTypes slug searchFilters config = get ("topics/" ++ slug ++ "/datasources/types") searchFilters config
