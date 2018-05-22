{-# LANGUAGE OverloadedStrings #-}


module Main where 

import Mymoviedb
import Data.Aeson
import Data.Text as Text                    (unpack)
import Data.Time.Calendar                   (toGregorian)
import Data.Time.Clock                      (UTCTime (..))
import Database.PostgreSQL.Simple           (Only (..), query_, query, connectPostgreSQL)
import Database.PostgreSQL.Simple.Time      (LocalTimestamp)
import Control.Monad                        (forM_, forM)
import Data.Either                          (rights)

-- | Countries configuration file
countries_config :: FilePath
countries_config = "./countries.cfg"

main :: IO()
main = do
    
    -- 1. get target date to load
    target_date <- utctDay <$> get_target_date_to_load

    conn <- connectPostgreSQL "host='localhost' port=5432 dbname='moviesdb' user='postgres' password='postgres'"

    -- Check if target date has already been loaded
    [Only maxDate] <- query_ conn "select max(date_playing) maxd from public.\"MOVIE_PLAYING_FCT\""        

    if maxDate >= target_date
        then 
            putStrLn $ "\nThe requested target date (" ++ (show target_date) ++ ") has been already loaded.\nNothing to load."
        else
            do

                putStrLn $ "\n\n*** Target date to load: " ++ (show target_date) ++ " ***"

                -- 2. load date dim
                -- load_date_dim target_date

                -- 3. get countries from a configuration file
                countries <- get_countries_from_config_file countries_config

                putStrLn $ "\nCountries for which to load movies: " 
                forM (countries) (\c -> putStrLn $ (unpack c) ++ " ")
                putStrLn ""

                -- 4. load country dim
                -- load_country_dim countries

                -- 5. get fact table data for target date per country: 
                
                --      get movies currently playing in each country (e.g., United States, United Kingdom, Canada, Greece)
                --      returns a [Either String (Country, [MovieCore]] list of pairs
                eitherList_movies_core_per_country <- forM countries (\c -> get_now_playing_movies_in_country c )

                -- get rid of Eithers and get just the Rights:, i.e., return a [(Country, [MovieCore])] 
                let movies_core_per_country = rights eitherList_movies_core_per_country 
                -- We need to traverse the list of Either and get rid of the "Either Layer" and also discard the "error elements"
                -- At the end we want just this [(Country, [MovieCore])] list of (Country, [MovieCore]) pairs
                {-let movies_core_per_country = 
                                                -- we want to filter out the "API - Error" elements of the list inside IO
                                                filter (\(c,mvc_list) -> if c == "API - Error" then False else True) $ 
                                                    -- map returns an [(Country, [MovieCore])], where some Country values are "API - Error"
                                                    map (\eitherVal ->
                                                            case eitherVal of
                                                                Left err ->  ("API - Error",[]) -- signify errors with a special "Country value"
                                                                Right res -> res
                                                        ) eitherList_movies_core_per_country-}  
                -- debug
                print movies_core_per_country

                putStrLn "\nOK. Downloaded now-playing movies in countries."

                putStrLn "\nDownloading details for each movie..."

                -- enhance movie list with movie detail data
                --      returns a [[Either String (Country, Movie)]] list of pairs
                eitherList_ofLists  <- forM movies_core_per_country (\(c,mvcore_list) -> get_movie_details (c,mvcore_list))

                    
                let 
                    -- flatten the list of either and get a [Either String (Country, Movie)]
                    eitherList_movies_per_country = concat eitherList_ofLists

                    -- get rid of the Either layer and get just the rights, i.e.,return a [(Country, Movie)] list
                    movies_per_country = rights eitherList_movies_per_country

                -- debug
                print movies_per_country

                -- 6. load movies dim
                numrows <- load_movies_dim movies_per_country 

                putStrLn $ "\nSuccesfully loaded Movies Dimension"
                putStrLn $ (show numrows) ++ " movie records have been inserted in Movie _Dim"
                -- 7. load genres group dim
                -- load_genres_group_dim movies_per_country

                -- 8. load directors dim

                -- 9. load fact table of playing movies per country for the specific target date
                numrows2 <- load_movies_playing_fact_table movies_per_country $ target_date -- $ toGregorian $ utctDay target_date
                putStrLn $ "\nSuccesfully loaded MOVIE_PLAYING_FCT"
                putStrLn $ (show numrows2) ++ " movie records have been inserted in MOVIE_PLAYING_FCT"


                -- 10. If it the 1st of the month - load the monthly aggregate fact table
                case toGregorian $ target_date of
                    (y,m,1) -> do 
                                    numrows3 <- load_monthly_agg_fact_table (y,m) 
                                    putStrLn $ "\nSuccesfully loaded MOVIES_PER_COUNTRY_MON_AGG"
                                    putStrLn $ (show numrows3) ++ " movie records have been inserted in MOVIES_PER_COUNTRY_MON_AGG"

                    _       -> putStrLn "It is not the 1st of the month. No monthly aggregate data will be loaded" 

                -- 11. Print message that the loading process has been completed.
                putStrLn "ETL process completed succesfully!"




-------------------------- Example - ignore -------------------
{-
main_test :: IO ()
main_test = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String APIResult)
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
  Left err -> putStrLn err
  Right rs -> print rs

 putStrLn "DB query result = "
 res <- hello
 putStrLn (show res)
 -- res2 <- myquery
 -- putStrLn "actor_id\t\t\tfirst_name\t\t\tlast_name\t\t\tlast_update"
 -- forM_ res2 $ \(actor_id, first_name, last_name, last_update) -> 
 --                     putStrLn $ show(actor_id :: Int) ++ "\t\t\t" ++ Text.unpack first_name ++ "\t\t\t" ++ Text.unpack last_name ++ "\t\t\t" ++ show (last_update :: LocalTimestamp)
 res2 <- myinsert 
 -- putStrLn (show res2)
 putStrLn "id\t\t\tname\t\t\tmytime"
 forM_ res2 $ \(id, name, mytime) -> 
                    putStrLn $ show(id :: Int) ++ "\t\t\t" ++ Text.unpack name ++ "\t\t\t" ++ show (mytime :: LocalTimestamp)

-}