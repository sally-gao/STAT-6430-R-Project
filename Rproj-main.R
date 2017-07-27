# Assignment:     R Project (STAT 6430)
# Names:          Sally Gao, Steven Mortensen, Kennan Grant
# Computing IDs:  sg2zv, sam8sp, khg3je

# load libraries
library(tidyverse)
library(stringr)



# Reviews -----------------------------------------------------------------

# read in reviews.  
reviews <- read_delim("reviews.txt"
                  , delim="\t"
                  , col_names=c("reviewer_id","movie_id","rating","timestamp")
                  )

# convert timestamp to POSIXct datetime format
reviews[["timestamp"]] <- as.POSIXct(reviews[["timestamp"]]
                                   , tz="GMT",origin="1970-01-01") 

# create primary key: review_id 
reviews <- rowid_to_column(reviews, "review_id")



# Movies ------------------------------------------------------------------

# reads in movies.
# primary key:  movie_id
movies <- read_delim("genres.txt"
                  , delim="|"
                  , col_names=c("movie_id","movie_title","release_date"
                                ,"DROP_THIS_COL","IMDb_URL"
                                ,"unknown","Action","Adventure","Animation"
                                ,"Children","Comedy","Crime","Documentary"
                                ,"Drama","Fantasy","Film-Noir","Horror"
                                ,"Musical","Mystery","Romance","Sci_Fi"
                                ,"Thriller","War","Western"))


# Reviewers ---------------------------------------------------------------

# reads in reviewers.  
# primary key:  reviewer_id
reviewers <- read_delim("reviewers.txt"
                        , delim="|"
                        , col_names=c("reviewer_id","age","gender"
                                      ,"occupation","zip_code"))



# Zip Codes ---------------------------------------------------------------

# reads in zip codes.
# primary key:  zip_code
zip_codes <- read_csv("zipcodes.csv"
                      , col_names=c("zip_code","zip_code_type","City","State"
                                    ,"location_type","Lat","Long"
                                    ,"Location","Decommisioned"))





