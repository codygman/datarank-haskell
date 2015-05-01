import DataRank
import Network.HTTP.Conduit 

buildConfig :: DataRank
buildConfig = newClient accessToken version
            where accessToken = "<ACCESS_TOKEN>"
                  version = "v1"

main :: IO ()
main = do
    let config = buildConfig  
    response <- fizzleValidate "fizzle: [Phrase (spacing=2) tide [AnyOf febreze febreeze]]" config
    print $ (responseBody response)

    -- list all subscribed topics
    response <- findTopics config
    print $ (responseBody response)

    -- list details of a specific topic
    response <- findTopic "tide-pods" [] config
    print $ (responseBody response)

    -- list details of a specific topic's themes
    response <- findThemes "tide-pods" config
    print $ (responseBody response)

    -- list details of a specific topic's theme correlations
    response <- findThemeCorrelations "tide-pods" [] config
    print $ (responseBody response)

    -- list details of a specific topic's retailers
    response <- findRetailers "tide-pods" config
    print $ (responseBody response)

    -- filter on male
    response <- findTopic "tide-pods" [Gender Male] config
    print $ (responseBody response)

    -- daily reach data
    response <- reachDaily "tide-pods" [] config
    print $ (responseBody response)

    -- yearly volume data
    response <- volume "tide-pods" "yearly" [] config
    print $ (responseBody response)

     -- show wordcloud
    response <- wordcloud "tide-pods" [Query "love", Limit 3] config
    print $ (responseBody response)

     -- location provinces
    response <- locationProvinces "tide-pods" [Age Age35To44] config
    print $ (responseBody response)

     -- datasource types
    response <- datasourceTypes "tide-pods" [] config
    print $ (responseBody response)

    -- list first 1 positive/negative comment
    response <- comments "tide-pods" [Sentiment Positive, Limit 1] config
    print $ (responseBody response)

    response <- comments "tide-pods" [Sentiment Negative, Limit 1] config
    print $ (responseBody response)

    -- list first 3 comments mentioning "love"
    response <- comments "tide-pods" [Query "love", Limit 3] config
    print $ (responseBody response)

    -- list first 3 comments form Arkansas
    response <- comments "tide-pods" [Province "US-AR", Limit 3] config
    print $ (responseBody response)

    -- list first 3 positive comments mentioning smell
    response <- comments "tide-pods" [Query "smell", Sentiment Positive, Limit 3] config
    print $ (responseBody response)

    -- list first 3 ecommerce comments
    response <- comments "tide-pods" [DatasourceType ECommerce, Limit 3] config
    print $ (responseBody response)

    -- list first 3 comments within 300 miles of lat/lon 
    response <- comments "tide-pods" [GeoLat 34.1, GeoLon (-94.1), GeoDistance 300, Limit 3] config
    print $ (responseBody response)

    -- list first 3 comments with hashtag #tide
    response <- comments "tide-pods" [Hashtag "tide", Limit 3] config
    print $ (responseBody response)    

    -- list first 3 comments with hashtag #tide OR #tidepods
    response <- comments "tide-pods" [Hashtags ["tide", "tidepods"], Limit 3] config
    print $ (responseBody response) 

    -- list first 3 comments matching theme 1 (packaging) or theme 2 (price)
    response <- comments "tide-pods" [Themes [1, 2], Limit 3] config
    print $ (responseBody response)    

    -- list first 3 comments within provided polygon
    response <- comments "tide-pods" [Polygon [(24.0,-84.0),(24.0,-104.0),(44.0,-104.0),(44.0,-84.0)], Limit 3] config
    print $ (responseBody response)  
