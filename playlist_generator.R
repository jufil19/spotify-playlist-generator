## Install and load the spotifyr library

# install.packages('spotifyr')
library(spotifyr)
library(tidyverse)

## Read in and wrangle the web-scraped data

data <- read.csv("content.csv", header = FALSE)
names <- t(data)[1, ]
lineup <- as.data.frame(t(data)[-1, ], index = FALSE)
names(lineup) <- names


tidy_data <- pivot_longer(lineup, cols = names(lineup), names_to = "date", values_to = "artist") %>%
  arrange("date") %>%
  filter(artist != "")

# Adding the year to the date and changing the format from 7 JUILLET 2022 to 2022-07-07
tidy_data$date <- paste(tidy_data$date, "2022")
tidy_data$date <- parse_datetime(tidy_data$date, "%d %B %Y", locale = readr::locale("fr"))
tidy_data <- tidy_data %>% arrange(date) # ordering by date


################################
########## SPOTIFYR ############
################################

# Authentification 

Sys.setenv(SPOTIFY_CLIENT_ID = '211a03c087564462bc01fe6c3d2054d9')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'b0358515e54346b6adc7ebf2eb26f77e')

access_token <- get_spotify_authorization_code()


## Using spotifyr to retrieve the artists id (add the id as a column)

artist_id <- numeric(0)
for (i in 1:nrow(tidy_data)) {
  artist_info <- search_spotify(tidy_data$artist[i], type = "artist")
  
  # If the artist is on spotify
  if (length(artist_info) != 0)
  {
    artist_id <- append(artist_id, artist_info$id[1])
  } 
  # If the artist is not on spotify (has no id)
  else
  {
    artist_id <- append(artist_id, "")
  }
}

# Adding column id
tidy_data["id"] <- artist_id

## Finding the top 10 tracks, for each artist (adding it as a column)
top_track_df <- data.frame()

for (i in 1:nrow(tidy_data)){
  # only if there actually is an id, if artist not found, there will be no songs either
  if (tidy_data$id[i] != "")
  {
    song_info <- get_artist_top_tracks(id = tidy_data$id[i])
    
    # Possible that the artist exists but that there is no info on the songs (so only check if there exists songs)
    if (length(song_info) != 0)
    {
      top_tracks_i <- song_info %>% 
          select('id', 'name', 'popularity') %>%
          rename(song.id = id, song.name = name, song.popularity = popularity) %>%
          arrange(desc(song.popularity)) %>%
          mutate(artist = tidy_data$artist[i])
      
      top_track_df<- rbind(top_track_df, top_tracks_i)
    }
  }
}

## Joining the song info and nesting it 

tidy_data <- left_join(tidy_data, top_track_df, by = 'artist')

tidy_data <- nest(tidy_data, song.info = c(song.id, song.name, song.popularity))



top_n_songs <- function(n, data){
  #' Creates lists of songs to an existing spotify playlist
  #'
  #' @param n The top 'n' songs from each artist will be added (n can take an integer value from 1 to 10 inclusively)
  #' @param data A tidy data frame where the 4th column is a column of nested dataframes. These nested dataframes have a column called song.id that 
  #' contains the spotify ids of songs
  #' 
  #' @return A list of songs to add to a spotify playlist
  
  songs_to_add <- numeric(0)

  for (i in 1:nrow(data))
  {
    songs_to_add <- append(songs_to_add, data[[4]][[i]]$song.id[1:n])
  }
  
  songs_to_add <- songs_to_add[!is.na(songs_to_add)]
  songs_to_add
}



add_songs_to_playlist <- function(dates_attending, number_songs_per_artist, name_playlist, spotify_id){
  
  #' Add songs to spotify playlist
  #'
  #' @param dates_attending Dates of the music festival the individual is interested in (format: %Y-%m-%d) (str)
  #' @param number_songs_per_artist Top n songs per artist to add (numeric)
  #' @param name_playlist Name of the new playlist to create
  #' @param spotify_id The users spotify id
  #'
  #' @return Creates a spotify playlist with the artist's 'n' most popular songs that are performing on the given dates 

  
  # Creating the playlist
  new_playlist <- create_playlist(user_id = spotify_id, name = name_playlist)
  new_playlist_id <- new_playlist[["id"]]
  
  # Filtering data according to which days the individual is attending the festival
  vector_dates <- as.Date(dates_attending)
  filtered_data <- tidy_data %>% filter(as.Date(date) %in% vector_dates)
  
  # Makes a list with the song id's of the n (given by `number_songs`) most popular songs for each artist playing on the `dates_attending` 
  list_songs_to_add <- top_n_songs(n = number_songs_per_artist, data = filtered_data)
  
  # Adds the songs to the playlist (can add a max of 100 songs at a time, so split list in batches of 50)

  while (length(list_songs_to_add) > 50){
    add_tracks_to_playlist(playlist_id = new_playlist_id, uris = list_songs_to_add[1:50])
    list_songs_to_add <- list_songs_to_add[-(1:50)]
  }
  add_tracks_to_playlist(playlist_id = new_playlist_id, uris = list_songs_to_add)
}

user_id = '31qx2rwdeju4kpeiqh4zyzqn6a3u'

add_songs_to_playlist(dates_attending = '2022-07-08', number_songs_per_artist = 3, name_playlist = 'yeah buddy2', spotify_id = user_id)








