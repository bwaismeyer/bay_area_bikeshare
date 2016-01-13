# scratchpad for exploring Bay Area Bikeshare data
# source: http://www.bayareabikeshare.com/open-data

# libraries
library(dplyr)
library(lubridate)
library(ggplot2)

# load the data pieces (assumes working directory set correctly)

# get the names of the files in each 6 month subfolder
file_names <- list.files(".", recursive = TRUE)
file_names <- file_names[grepl(".csv", file_names)]
file_names <- file_names[!grepl("status_data", file_names)]
file_names <- paste0("./", file_names)

# loop over the names and read in the csvs
data_tables <- lapply(file_names, function(x) {
    read.csv(x, stringsAsFactors = FALSE)
})

# name the list elements
names(data_tables) <- file_names

# rbind the complementary elements (e.g., station and station)
station_df <- do.call(rbind, data_tables[grepl("station", file_names)])

trip_list <- data_tables[grepl("trip", file_names)]
names(trip_list[[1]]) <- names(trip_list[[2]])
trip_df <- do.call(rbind, trip_list)

weather_list <- data_tables[grepl("weather", file_names)]
names(weather_list[[1]]) <- names(weather_list[[2]])
weather_df <- do.call(rbind, weather_list)

# linking the trip and weather data: joining by date and by zip code
# first we make the zipcode datatype consistent (character) on both datasets
weather_df <- weather_df %>%
    mutate(Zip = as.character(Zip))

# then we create POSIX date variables for both datasets
trip_df <- trip_df %>%
    mutate(Start.Date = mdy_hm(Start.Date),
           End.Date = mdy_hm(End.Date))

trip_df$start_year <- year(trip_df$Start.Date)
trip_df$start_month <- month(trip_df$Start.Date)
trip_df$start_day <- mday(trip_df$Start.Date)

weather_df <- weather_df %>%
    mutate(PDT = mdy(PDT))

weather_df$start_year <- year(weather_df$PDT)
weather_df$start_month <- month(weather_df$PDT)
weather_df$start_day <- mday(weather_df$PDT)

# join to get the weather for each trip start day
tw_df <- left_join(trip_df, weather_df,
                   by = c("Zip.Code" = "Zip",
                          "start_year" = "start_year",
                          "start_month" = "start_month",
                          "start_day" = "start_day")
)

tw_df %>%
    mutate(precip_in = ifelse(PrecipitationIn == "T",
                              0.005,
                              PrecipitationIn)) %>%
    mutate(precip_in = as.numeric(precip_in)) %>%
    group_by(Bike..) %>%
    summarise(use_count = n(),
              median_precip_in = mean(precip_in, na.rm = TRUE)) %>%
    arrange(desc(median_precip_in))