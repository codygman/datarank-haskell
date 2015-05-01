import DataRank
import Network.HTTP.Conduit 

buildConfig :: DataRank
buildConfig = newClient accessToken version
            where accessToken = "<<YOUR ACCESS TOKEN>>"
                  version = "v1"


main :: IO ()
main = do
    let config = buildConfig  
    response <- fizzleValidate "fizzle: [Phrase (spacing=2) tide [AnyOf febreze febreeze]]" config
    print $ (responseBody response)

    -- Uncomment any endpoint to test out

    -- list all subscribed topics
    response <- findTopics config
    print $ (responseBody response)

    -- list details of a specific topic
    response <- findTopic "tide-pods" [] config
    print $ (responseBody response)

    -- filter on male
    response <- findTopic "tide-pods" [("gender", "male")] config
    print $ (responseBody response)

    -- daily reach data
    response <- reachDaily "tide-pods" [] config
    print $ (responseBody response)

    -- yearly volume data
    response <- volume "tide-pods" "yearly" [] config
    print $ (responseBody response)

    -- list first 3 comments
    response <- comments "tide-pods" [("limit", "3")] config
    print $ (responseBody response)

    -- list first 3 comments mentioning "love"
    response <- comments "tide-pods" [("limit", "3"),("q", "love")] config
    print $ (responseBody response)

    -- list first 3 comments form Arkansas
    response <- comments "tide-pods" [("limit", "3"),("province", "US-AR")] config
    print $ (responseBody response)

     -- list first 3 positive comments mentioning smell
    response <- comments "tide-pods" [("limit", "3"),("sentiment", "positive"),("q", "smell")] config
    print $ (responseBody response)

    -- list first 3 ecommerce comments
    response <- comments "tide-pods" [("limit", "3"),("datasource_type", "ecommerce")] config
    print $ (responseBody response)

     -- show wordcloud
    response <- wordcloud "tide-pods" [("limit", "10"),("q", "love")] config
    print $ (responseBody response)

     -- location provinces
    response <- locationProvinces "tide-pods" [("limit", "10"),("q", "love")] config
    print $ (responseBody response)

     -- datasource types
    response <- datasourceTypes "tide-pods" [] config
    print $ (responseBody response)