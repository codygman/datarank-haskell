{-# LANGUAGE OverloadedStrings #-}

module DataRank 
(
    DataRank(..)
    ,FilterKey(..)
    ,Age(..)
    ,DatasourceType(..)
    ,Gender(..)
    ,GeoDistanceUnits(..)
    ,Sentiment(..)
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

apiUrl :: String
apiUrl = "https://api.datarank.com/"

newClient :: String -> String -> DataRank
newClient accessToken version = DataRank {
                            accessToken = C.pack accessToken
                            ,version = version
                        }

data FilterKey
    = Filter String String -- generic filter
    | Page Int
    | Limit Int
    | Age Age
    | BioQuery String
    | Country String
    | Datasource String
    | DatasourceType DatasourceType
    | Gender Gender
    | GeoLat Float
    | GeoLon Float
    | GeoDistance Int
    | GeoDistanceUnits GeoDistanceUnits
    | HasGeo
    | HasBio
    | Province String
    | Query String
    | Sentiment Sentiment
    deriving (Eq, Show)

data DataRank = DataRank { 
                    version :: String
                    ,accessToken :: ByteString 
                    } deriving Show

data Age
    = Age18To24
    | Age25To34
    | Age35To44
    | Age45To54
    | Age55AndAbove
    deriving (Eq, Show, Bounded, Enum)

data DatasourceType
    = Other
    | Facebook
    | Twitter
    | Location
    | Forums
    | Photo
    | Video
    | ECommerce
    | Blog
    deriving (Eq, Show, Bounded, Enum)

data Gender
    = Male
    | Female
    | AnyGender
    deriving (Eq, Show, Bounded, Enum)

data GeoDistanceUnits
    = METERS
    | KILOMETERS
    | FEET
    | MILES
    deriving (Eq, Show, Bounded, Enum)

data HasGeo = True
    deriving (Eq, Show, Bounded, Enum)

data Sentiment
    = Positive
    | Negative
    | Neutral
    | AnySentiment
    | NoSentiment
    | Auto
    deriving (Eq, Show, Bounded, Enum)

filterKeyToPair :: FilterKey -> (String, String)
filterKeyToPair (Filter k v) = (k, v)
filterKeyToPair (Page p) = ("page", show p)
filterKeyToPair (Limit l) = ("limit", show l)
filterKeyToPair (Age age) = ("age", showAge age)
filterKeyToPair (BioQuery q) = ("q:bio", q)
filterKeyToPair (Country country) = ("country", country)
filterKeyToPair (Datasource datasource) = ("datasource", datasource)
filterKeyToPair (DatasourceType datasourceType) = ("datasource_type", showDatasourceType datasourceType)
filterKeyToPair (Gender gender) = ("gender", showGender gender)
filterKeyToPair (GeoLat lat) = ("lat", show lat)
filterKeyToPair (GeoLon lon) = ("lon", show lon)
filterKeyToPair (GeoDistance distance) = ("distance", show distance)
filterKeyToPair (GeoDistanceUnits units) = ("units", showGeoDistanceUnits units)
filterKeyToPair HasGeo = ("has:geo", "true")
filterKeyToPair HasBio = ("has:bio", "true")
filterKeyToPair (Province province) = ("province", province)
filterKeyToPair (Query q) = ("q", q)
filterKeyToPair (Sentiment sentiment) = ("sentiment", showSentiment sentiment)

showAge :: Age -> String
showAge Age18To24 = "18-24"
showAge Age25To34 = "25-34"
showAge Age35To44 = "35-44"
showAge Age45To54 = "45-54"
showAge Age55AndAbove = "55+"

showGender :: Gender -> String
showGender Male = "male"
showGender Female = "female"
showGender AnyGender = "male,female"

showGeoDistanceUnits :: GeoDistanceUnits -> String
showGeoDistanceUnits METERS = "m"
showGeoDistanceUnits KILOMETERS = "km"
showGeoDistanceUnits FEET = "ft"
showGeoDistanceUnits MILES = "mi"

showDatasourceType :: DatasourceType -> String
showDatasourceType Other = "other"
showDatasourceType Facebook = "facebook"
showDatasourceType Twitter = "twitter"
showDatasourceType Location = "location"
showDatasourceType Forums = "forums"
showDatasourceType Photo = "photo"
showDatasourceType Video = "video"
showDatasourceType ECommerce = "ecommerce"
showDatasourceType Blog = "blog"

showSentiment :: Sentiment -> String
showSentiment Positive = "positive"
showSentiment Negative = "negative"
showSentiment Neutral = "neutral"
showSentiment AnySentiment = "any"
showSentiment NoSentiment = "none"
showSentiment Auto = "auto"

convertSearchFilters :: [FilterKey] -> [(ByteString, Maybe ByteString)]
convertSearchFilters searchFilters = D.map (convertParameter.filterKeyToPair) searchFilters

convertParameter :: (String, String) -> (ByteString, Maybe ByteString)
convertParameter (k,v) = (C.pack k, Just (C.pack v))

buildRequestUrl :: String -> IO(Request)
buildRequestUrl endpoint = parseUrl (apiUrl ++ endpoint)

buildRequestHeaders :: DataRank -> RequestHeaders
buildRequestHeaders config = [ ("Content-Type" , "application/json")
                             , ("Accept" , "application/vnd.datarank.v1+json")
                             , ( "Authorization" , (accessToken config))
                             ]

post :: String -> [FilterKey] -> BS.ByteString -> DataRank -> IO(Response BS.ByteString)
post endpoint queryParameters postData config = do
                baseRequest <- buildRequestUrl endpoint
                let request' = setQueryString (convertSearchFilters queryParameters) baseRequest
                let request = request' { method = methodPost
                   , requestHeaders = buildRequestHeaders config 
                   , requestBody = RequestBodyLBS postData
                   }
                (withManager $ httpLbs request)

get :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
get endpoint queryParameters config = do
                baseRequest <- buildRequestUrl endpoint
                let request' = setQueryString (convertSearchFilters queryParameters) baseRequest
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
findTopic :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
findTopic slug searchFilters config = get ("topics/" ++ slug) searchFilters config

 -- list comments associated to a topic
comments :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
comments slug searchFilters config = get ("topics/" ++ slug ++ "/comments") searchFilters config

-- get topic volume data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
volume :: String -> String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
volume slug interval searchFilters config = get ("topics/" ++ slug ++ "/volume/" ++ interval) searchFilters config

volumeDaily :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
volumeDaily slug searchFilters config = get ("topics/" ++ slug ++ "/volume/daily") searchFilters config

-- get topic reach data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
reach :: String -> String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
reach slug interval searchFilters config = get ("topics/" ++ slug ++ "/reach/" ++ interval) searchFilters config

reachDaily :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
reachDaily slug searchFilters config = get ("topics/" ++ slug ++ "/reach/daily") searchFilters config

-- get topic interaction data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
interaction :: String -> String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
interaction slug interval searchFilters config = get ("topics/" ++ slug ++ "/interaction/" ++ interval) searchFilters config

interactionDaily :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
interactionDaily slug searchFilters config = get ("topics/" ++ slug ++ "/interaction/daily") searchFilters config

-- get topic sentiment data
-- valid intervals: secondly,minutely,hourly,daily,weekly,monthly,quarterly,yearly
sentiment :: String -> String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
sentiment slug interval searchFilters config = get ("topics/" ++ slug ++ "/sentiment/" ++ interval) searchFilters config

sentimentDaily :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
sentimentDaily slug searchFilters config = get ("topics/" ++ slug ++ "/sentiment/daily") searchFilters config

-- get word cloud including term sentiment/frequency
wordcloud :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
wordcloud slug searchFilters config = get ("topics/" ++ slug ++ "/wordcloud") searchFilters config

-- get location pins, contains lat/lon coordinates of commenters within topic
locationPins :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
locationPins slug searchFilters config = get ("topics/" ++ slug ++ "/location/pins") searchFilters config

-- get demographic information around provinces related to topic
locationProvinces :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
locationProvinces slug searchFilters config = get ("topics/" ++ slug ++ "/location/provinces") searchFilters config

-- get datasource stats
datasources :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
datasources slug searchFilters config = get ("topics/" ++ slug ++ "/datasources") searchFilters config

-- get datasource type stats
datasourceTypes :: String -> [FilterKey] -> DataRank -> IO(Response BS.ByteString)
datasourceTypes slug searchFilters config = get ("topics/" ++ slug ++ "/datasources/types") searchFilters config
