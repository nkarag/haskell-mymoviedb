---------------------------------------------------
-- MOVIE_PLAYING_FCT
--
-- The Movies playing Fact Table
-- Fact Table Grain Definition: 
--	One row = a movie playing in a specific country at date D
---------------------------------------------------
CREATE TABLE public."MOVIE_PLAYING_FCT"
(
    movie_id integer NOT NULL,
    date_playing date NOT NULL,
    country_playing character varying(20) NOT NULL
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public."MOVIE_PLAYING_FCT"
    OWNER to postgres;

ALTER TABLE public."MOVIE_PLAYING_FCT"
    OWNER to postgres;
COMMENT ON TABLE public."MOVIE_PLAYING_FCT"
    IS 'The Movies playing Fact Table';

ALTER TABLE public."MOVIE_PLAYING_FCT"
   ADD CONSTRAINT fk_movies_dim
   FOREIGN KEY (movie_id) 
   REFERENCES public."MOVIES_DIM"(movie_id);

---------------------------------------------------
-- MOVIES_DIM
--
-- The Movies Dimension Table
-- This tables holds all the details for each movie.
---------------------------------------------------

CREATE TABLE public."MOVIES_DIM"
(
    movie_id integer,
    title character varying(100) NOT NULL,
    original_language character varying(10) NOT NULL,
    original_title character varying(100) NOT NULL,
    production_countries character varying(100) NOT NULL,
    genres character varying(100) NOT NULL,
    release_date date NOT NULL,
    popularity integer NOT NULL,
    PRIMARY KEY (movie_id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public."MOVIES_DIM"
    OWNER to postgres;
COMMENT ON TABLE public."MOVIES_DIM"
    IS 'This tables holds all the details for each movie';

---------------------------------------------------
-- MOVIES_DIM_STAGE_TAB
--
-- A Staging table for loading the Movies Dimension Table
-- This tables is used in order to find the daily delta
---------------------------------------------------
CREATE TABLE public."MOVIES_DIM_STAGE_TAB"
(
    movie_id integer,
    title character varying(100) NOT NULL,
    original_language character varying(10) NOT NULL,
    original_title character varying(100) NOT NULL,
    production_countries character varying(100) NOT NULL,
    genres character varying(100) NOT NULL,
    release_date date NOT NULL,
    popularity integer NOT NULL,
    PRIMARY KEY (movie_id)
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public."MOVIES_DIM_STAGE_TAB"
    OWNER to postgres;




---------------------------------------------------
-- MOVIES_PER_COUNTRY_MON_AGG
--
-- The movies per country monthly aggregate
-- A monthly aggregate table holding totals of movies per country per month
---------------------------------------------------

CREATE TABLE public."MOVIES_PER_COUNTRY_MON_AGG"
(
    yearmonth character varying(20) NOT NULL,
    country character varying(20) NOT NULL,
    num_movies integer NOT NULL
)
WITH (
    OIDS = FALSE
);

ALTER TABLE public."MOVIES_PER_COUNTRY_MON_AGG"
    OWNER to postgres;
COMMENT ON TABLE public."MOVIES_PER_COUNTRY_MON_AGG"
    IS 'A monthly aggregate table holding totals of movies per country per month';
