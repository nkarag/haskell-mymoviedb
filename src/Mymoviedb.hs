{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes  #-}

module Mymoviedb
    ( 
        Movie (..)
        ,MovieCore (..)
        ,MovieDetail (..)
        ,Genre (..)
        ,APIResult (..)
        ,Country
        ,MovieId
        ,NumRows
        ,getJSON
        ,get_target_date_to_load
        ,get_countries_from_config_file
        ,get_now_playing_movies_in_country
        ,get_movie_details
        ,load_movies_dim
        ,load_movies_playing_fact_table
        ,load_monthly_agg_fact_table
        ---------------------------------
        -- ,hello
        -- ,myquery
        -- ,myinsert
    ) where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class                       (liftIO)
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit                         (simpleHttp)
import GHC.Generics
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ             (sql)
import Data.Time.Calendar                           (Day, fromGregorianValid)
import Data.Time.Clock 
import Database.PostgreSQL.Simple.Time              (LocalTimestamp)
import GHC.Int
import Data.List                                    (map, nub)


-- | Basic Movie Data Type
-- It is split into two parts due to the way movie data are
-- retrieved via the IMdb API calls
data Movie = Movie MovieCore MovieDetail deriving (Eq,Show)

-- | Movie core attributes. 
-- Type of each JSON entry in record syntax.
data MovieCore = 
    MovieCore {
         id :: MovieId
        ,title :: !Text
        ,original_language :: !Text
    } deriving (Eq,Show,Generic)

-- | Movie detail attributes. 
-- Type of each JSON entry in record syntax.
data MovieDetail = 
    MovieDetail {
        -- id_MD :: MovieId
        original_title :: !Text
        ,production_countries :: [ProductionCountry]
        ,genres :: [Genre]
        ,release_date :: !Text -- "YYYY-MM-DD"
        ,popularity :: Double 
    } deriving (Eq,Show,Generic)

-- | The movie Genre data type
data Genre = Genre {
     --id_G :: Int
    name :: !Text  
}  deriving (Eq,Show,Generic)

-- | The Production Country of a movie
data ProductionCountry = ProductionCountry { 
                            iso_3166_1 :: !Text -- code name of the country

                        } deriving (Eq,Show, Generic)

-- | data type to accomodate retrieval of results from the IMdb API call
data APIResult = 
    APIResult {
        total_results :: Int
        ,results :: [MovieCore]
    } deriving (Show,Generic)

type MovieId = Int

type Country = Text

type NumRows = Int64

type D = Int
type M = Int
type Y = Integer

-- Instances to convert our type to/from JSON.
instance FromJSON MovieCore
instance ToJSON MovieCore

instance FromJSON MovieDetail
instance ToJSON MovieDetail

instance FromJSON Genre
instance ToJSON Genre

instance FromJSON ProductionCountry
instance ToJSON ProductionCountry


instance FromJSON APIResult
instance ToJSON APIResult


-- | URL that points to the remote JSON file, in case
--   you have it.
jsonURL :: String
jsonURL = "https://api.themoviedb.org/3/search/movie?api_key=bbb0e77b94b09193e6f32d5fac7a3b9c&query=Jack+Reacher" 
-- "http://daniel-diaz.github.io/misc/pizza.json"

-- | Read the remote copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

-- | API call to get now-playing movies per country
-- Returns a JSON bytestring
getJSON_nowPlayingMovies :: Country -> IO B.ByteString
getJSON_nowPlayingMovies c = simpleHttp (url_get_now_playing_movies_for_country $ unpack c)
    where
        url_get_now_playing_movies_for_country c = 
            --"https://api.themoviedb.org/3/movie/now_playing?api_key=bbb0e77b94b09193e6f32d5fac7a3b9c&language=en-UK&region=GR&page=1"
            "https://api.themoviedb.org/3/movie/now_playing?api_key=bbb0e77b94b09193e6f32d5fac7a3b9c&language=en-UK&region=" ++ c ++ "&page=1"

-- | API call to get movie details for a specific movie id
-- Returns a JSON bytestring
getJSON_movieDetails :: MovieId -> IO B.ByteString
getJSON_movieDetails id =  simpleHttp (url_get_movieDetails $ (show id))
    where
        url_get_movieDetails idstr = 
                "https://api.themoviedb.org/3/movie/"++idstr++"?api_key=bbb0e77b94b09193e6f32d5fac7a3b9c&language=en-US"
    

-- | Returns the "control date" to be loaded.
--  All data loaded in each daily load are driven by this date
-- This way, we can keep track of what data have been loaded into our data mart
get_target_date_to_load :: IO UTCTime
get_target_date_to_load = getCurrentTime

-- | Read list of countries from a configuration file
get_countries_from_config_file :: FilePath -> IO [Country]
get_countries_from_config_file f = do
    c <- readFile f
    return $ Data.Text.lines $ pack c

-- | Get (API call) now-playing movies for a specific country
-- If the API call returns an error, then the error message is returned and printed to screen.
get_now_playing_movies_in_country :: Country ->  IO (Either String (Country, [MovieCore]))
get_now_playing_movies_in_country c = do
     -- Get JSON data and decode it
     d <- (eitherDecode <$> (getJSON_nowPlayingMovies c)) :: IO (Either String APIResult)
     -- If d is Left, the JSON was malformed.
     -- In that case, we report the error.
     -- Otherwise, we perform the operation of
     -- our choice. In this case, just print it.
     case d of
      Left err -> do
                    putStrLn err  
                    return $ Left err 
      Right res -> return $ Right (c, results res)


-- | Get (API call) movie details for input list of movies
-- It returns a list of "Eithers". 
-- If the API call returns an error, then the error message is returned and printed to screen.
-- If the API call succeeds then a Movie data type including both core and detail info for a movie is returned,
-- in a pair with the corresponding country
get_movie_details :: (Country, [MovieCore]) -> IO [Either String (Country, Movie)]
get_movie_details (c, mvcore_list) = do
    -- for each movie id in the input list, go get the details
    forM mvcore_list (\mvcore -> do
                         -- Get JSON data and decode it
                         d <- (eitherDecode <$> (getJSON_movieDetails $ Mymoviedb.id mvcore)) :: IO (Either String MovieDetail)
                         -- If d is Left, the JSON was malformed.
                         -- In that case, we report the error.
                         -- Otherwise, we perform the operation of
                         -- our choice. In this case, just print it.
                         case d of
                          Left err -> do
                                        putStrLn err  
                                        return $ Left err 
                          Right mvdetail -> return $ Right (c, Movie mvcore mvdetail)
                     )

-- | Load the Movie dimension based on the input list of movies.
-- Only the movie delta will be inserted in the Movie dim.
-- Returns number of movies that have been inserted. 
load_movies_dim :: [(Country, Movie)] -> IO NumRows
load_movies_dim input_ls = do
        conn <- connectPostgreSQL "host='localhost' port=5432 dbname='moviesdb' user='postgres' password='postgres'"

        -- 1. get delta of movies to stored to be loaded

            -- 1.1 load all input movies into a staging table

            -- First truncate staging table
        let
            q1 = "TRUNCATE public.\"MOVIES_DIM_STAGE_TAB\""
        execute_ conn q1 
        
        let 
            -- isolate list of input movies from input and remove dublicates (remember there is a PK constraint on the table)
            input_movies_ls = nub $ Data.List.map (\(c,mv) -> mv) input_ls
            -- list of movie tuples to insert
            movie_tuples_ls = Data.List.map (\mv -> 
                case mv of
                    Movie (MovieCore {Mymoviedb.id = mvid, title = mvtitle, original_language = orlang}) (MovieDetail {original_title = ortitle, production_countries = prodc, genres = gnrs, release_date = rldate, popularity = pop})
                                                -> (mvid::Int, mvtitle::Text, orlang::Text, ortitle::Text, (textifyListOfText "," $ Data.List.map (\p -> iso_3166_1 p) prodc)::Text, (textifyListOfText "," $ Data.List.map (\g -> name g) gnrs)::Text, 
                                                    textdaytoDay rldate, pop :: Double)
                                            ) input_movies_ls 

            q2 = "INSERT INTO public.\"MOVIES_DIM_STAGE_TAB\" VALUES (?,?,?,?,?,?,?,?)"
        insres1 :: NumRows <- executeMany conn q2  movie_tuples_ls

        -- 2. Insert delta movies into MOVIES_DIM table
        insres2 :: NumRows <- execute_ conn [sql|
                                                    insert into public."MOVIES_DIM" 
                                                     select *
                                                     from public."MOVIES_DIM_STAGE_TAB"
                                                     where
                                                     movie_id in (
                                                                    select movie_id
                                                                    from public."MOVIES_DIM_STAGE_TAB"
                                                                    EXCEPT
                                                                    select movie_id
                                                                    from public."MOVIES_DIM")        
                                                |] 
        return insres2

-- | "YYYY-MM-DD" to a Time.Clock.Day data type 
textdaytoDay :: Text -> Maybe Day 
textdaytoDay textday = 
    case unpack textday of
        y1:y2:y3:y4 : '-' : m1:m2 : '-' : d1:d2 -> fromGregorianValid (read (y1:y2:y3:y4:[]) :: Integer) (read (m1:m2:[]) :: Int) (read (d1:d2) :: Int)
        _                     -> Nothing

-- | Loads the fact table of playing movies per country with the input movies for the specified event date.
-- It returns the number of rows that have been inserted
load_movies_playing_fact_table :: [(Country, Movie)] -> Day -> IO NumRows
-- -> (Y,M,Mymoviedb.D) -> IO NumRows
load_movies_playing_fact_table input_ls targetDate = do
        conn <- connectPostgreSQL "host='localhost' port=5432 dbname='moviesdb' user='postgres' password='postgres'"

        let 
            -- list of fact tuples to insert
            fact_tab_tuples_ls = Data.List.map (\(cnt, mv) -> 
                case mv of
                    Movie (MovieCore {Mymoviedb.id = mvid, title = mvtitle, original_language = orlang}) (MovieDetail {original_title = ortitle, production_countries = prodc, genres = gnrs, release_date = rldate, popularity = pop})
                                                   -> (mvid::Int, targetDate, cnt)
                                               ) input_ls         
        
            q = "INSERT INTO public.\"MOVIE_PLAYING_FCT\" VALUES (?,?,?)"
        insres :: NumRows <- executeMany conn q  fact_tab_tuples_ls
        return insres


-- | Loads the monthly aggregate fact table with the number of movies per country for the specified month
-- It returns the number of rows that have been inserted
load_monthly_agg_fact_table :: (Y,M) -> IO NumRows
load_monthly_agg_fact_table (y,m) = do
        conn <- connectPostgreSQL "host='localhost' port=5432 dbname='moviesdb' user='postgres' password='postgres'"

        -- Insert aggregated rows into MOVIES_PER_COUNTRY_MON_AGG table
        insres2 :: NumRows <- execute conn [sql|
                                                    insert into public."MOVIES_PER_COUNTRY_MON_AGG"
                                                        select to_char(date_playing,'YYYY-MM'), country_playing, count(distinct movie_id) num_movies 
                                                        from public."MOVIE_PLAYING_FCT"
                                                        where
                                                            to_char(date_playing,'YYYY-MM') = ?
                                                        group by to_char(date_playing,'YYYY-MM'), country_playing

                                                |] [(show y) ++ "-" ++ (if m < 10 then "0"++(show m) else (show m))]
        return insres2


-- | Takes a list of Text and produces a single Text value with all the Text elements concatanated with the input Text delimiter
textifyListOfText :: Text -> [Text] -> Text
textifyListOfText delimiter inptext = pack $ stringfyListOfStrings (unpack delimiter) $ Data.List.map (unpack) inptext

-- | Takes a list of String and produces a single String value with all the String elements concatanated with the input Text delimiter
stringfyListOfStrings :: String -> [String] -> String
stringfyListOfStrings delimiter [] = []
stringfyListOfStrings delimiter (s:rest) = s ++ delimiter ++ stringfyListOfStrings delimiter rest  


-------------------------------------------------------------
-- exmaple code - ignore
-------------------------------------------------------------

-- | Location of the local copy, in case you have it,
--   of the JSON file.
jsonFile :: FilePath
jsonFile = "pizza.json"


-- Move the right brace (}) from one comment to another
-- to switch from local to remote.

{--
-- Read the local copy of the JSON file.
getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile
--}


hello :: IO Int
hello = do
  conn <- connectPostgreSQL "host='localhost' port=5432 dbname='dvdrental' user='postgres' password='nikos'"
  [Only i] <- query_ conn "select 2 + 2"
  return i

myquery :: IO [(Int, Text, Text, LocalTimestamp)]
myquery = do
    conn <- connectPostgreSQL "host='localhost' port=5432 dbname='dvdrental' user='postgres' password='nikos'"
    result :: [(Int, Text, Text, LocalTimestamp)] <- query_ conn "select actor_id, first_name, last_name, last_update from actor"
    return result


myinsert :: IO [(Int, Text, LocalTimestamp)] -- IO Int64 
myinsert = do
    conn <- connectPostgreSQL "host='localhost' port=5432 dbname='moviesdb' user='postgres' password='nikos'"

    ctime <- getCurrentTime
    let q = "INSERT INTO test_Tab VALUES (?,?,?)"
    insres :: GHC.Int.Int64 <- executeMany conn q  [(5 :: Int , "hello" :: Text, ctime),(6 :: Int, "world" :: Text, ctime)]
    result :: [(Int, Text, LocalTimestamp)] <- query_ conn "select id, name, mytime from test_tab"
    return result
    -- return insres

