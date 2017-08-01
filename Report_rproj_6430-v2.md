Sally Gao, Stephen Mortensen, Kennan Grant

sg2zv, sam8sp, khg3je

Report: R Project (STAT 6430)
-----------------------------

### Executive Summary

For this project, we explored a dataset of over 100K movie reviews,
consisting of 1682 movies reviewed by 943 reviewers. Our goal was to
answer two questions: 1. What seems to be associated with a high rating?
2. Which groups are most likely to provide higher ratings?

### Some definitions: 

-   A “high rating” is a rating of 4 or 5 (out of 5).

-   Given an explanatory variable, if the probability of a 4 or 5 is
    higher than the probability of encountering a high rating when
    selecting a review at random random – 55.4% – then that explanatory
    variable value/level is predictive and can be said to be associated
    with a high rating.

Our analysis revealed several interesting associations with high
ratings, i.e. variables or levels of variables associated higher
probabilities of a rating of 4 or 5 than the baseline probability of
55.4%.

### Variables of particular interest:

-   mode rating of reviewer

-   hour of timestamp

-   release date

-   film-noir

-   war

-   state

-   city

### Methodology

We read the four data tables (‘reviews.txt’, ‘genres.txt’,
‘reviewers.txt’, ‘zipcodes.csv’) into R and joined all data to the
reviews table, creating a normalized table with each review as a single
observation. All other data were added as attributes of the
observations.

The primary keys for the tables were as follows:

‘review\_id’ (reviews table)

‘movie\_id’ (genres table – renamed ‘movies’ table in R)

‘reviewer\_id’ (reviewers table)

‘zip\_code’ (zipcodes table)

In order to join all four tables into a single, normalized table, we
joined reviews to movies by ‘movie\_id’, then to reviewers by
‘reviewer\_id’, and then to zipcodes by ‘zip\_code’. We called this
table ‘normalized\_base\_table’, and stored it as a tibble in R.

\[insert rating distribution overview graphic. Alter color of 4 and 5
bars to be distinct from 1,2,3\]

\[insert associated bar chart data in table\]

Interesting Variables

Mode Rating of Reviewer

\[insert exposition\]

\[insert graphic\]

Hour of Timestamp

\[insert exposition\]

\[insert graphic\]

Release Date

\[insert exposition\]

\[insert graphic\]

Film-Noir

\[insert exposition\]

\[insert graphic\]

War

\[insert exposition\]

\[insert graphic\]

State

\[insert exposition\]

\[insert graphic\]

City

\[insert exposition\]

\[insert graphic\]
