# fasola

An R package for exploring singing minutes from the Sacred Harp minutes data, including details on the songs themselves, leaders, locations we've sung at!

## Installation

```r
pak::pak("jakebharmon/fasola")
```

Or with devtools:

```r
devtools::install_github("jakebharmon/fasola")
```

## Usage

```r
library(fasola)

# Load the full dataset
# N.B. The database is downloaded automatically on first use and cached locally.
get_minutes()

# The result is assigned to fasola_df in your environment
head(fasola_df)
```

## Dataset

`get_minutes()` returns a tidy data frame with one row per lesson led:

| Column | Description |
|---|---|
| `leader_id` | Unique leader identifier |
| `leader_name` | Leader's name |
| `song_id` | Unique song identifier |
| `song_title` | Song title |
| `minutes_id` | Unique singing event identifier |
| `year` | Year of the event |
| `date_start` | Start date of the event |
| `date_end` | End date (differs from start for multi-day events) |

## Optional Detail Columns

```r
# Add location info (city, state, country, GPS coordinates)
get_minutes(include_location = TRUE)

# Add song detail (meter, music attribution, three-liner)
get_minutes(include_song_detail = TRUE)

# Add leader stats (lesson count, song entropy, top 20 count)
get_minutes(include_leader_detail = TRUE)

# Feel free to mix and match options, or get them all at once!
get_minutes(include_location = TRUE,
            include_song_detail = TRUE,
            include_leader_detail = TRUE)
```

## Bonus

```r
# Print the lyrics of a random Sacred Harp song!
feeling_lucky()
```

## Acknowledgements

Singing minutes data scraped and maintained by [Mark Godfrey](https://github.com/marktgodfrey/fasolaminutes_parsing). Lyrics from the [Sacred Harp Publishing Company 2025 edition](https://github.com/Sacred-Harp-Publishing-Company/2025-edition). 

Special thanks to the denizens of the Shapenote Data Research & Analytics Collective (ShaDRAC) for the inspiration and peer review!

## Roadmap

- Tie in metadata from Sacred Harp Publishing Company
- Add Christian Harmony dataset
