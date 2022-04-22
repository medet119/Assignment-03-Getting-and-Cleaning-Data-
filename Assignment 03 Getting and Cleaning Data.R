## Part 1
library(tidyverse)
details <- read_csv("StormEvents_details-ftp_v1.0_d1996_c20210803.csv")

## Part 2
df <- details %>%
  select(BEGIN_YEARMONTH, BEGIN_DAY, BEGIN_TIME, BEGIN_DATE_TIME,
         END_YEARMONTH, END_DAY, END_TIME, END_DATE_TIME,
         EPISODE_ID,
         EVENT_ID,
         STATE, STATE_FIPS,
         CZ_NAME,
         CZ_TYPE,
         CZ_FIPS,
         EVENT_TYPE,
         SOURCE,
         BEGIN_LAT, BEGIN_LON, END_LAT, END_LON)

## Part 3
df <- df %>%
  arrange(BEGIN_YEARMONTH)

## Part 4
df <- df %>%
  mutate(STATE = str_to_title(STATE),
         CZ_NAME = str_to_title(CZ_NAME))

## Part 5
df <- df %>%
  filter(CZ_TYPE == "C") %>%
  select(-CZ_TYPE)

## Part 6
df <- df %>%
  # pad the state and county FIPS 
  mutate(STATE_FIPS = str_pad(STATE_FIPS, width = 2, side = "left", pad = "0"),
         CZ_FIPS = str_pad(CZ_FIPS, width = 3, side = "left", pad = "0")) %>%
  # unite the two columns to make one 6-digit FIPS
  unite("fips", STATE_FIPS, CZ_FIPS, sep = "0")

## Part 7
df <- df %>%
  rename_all(tolower)

## Part 8
data("state")
states_df <- data.frame(state_name = state.name,
                        area = state.area,
                        region = state.region)

## Part 9
# create a dataframe with the number of events per state
states_events <- df %>%
  group_by(state) %>%
  summarise(num_events = n())

# merge in the state information dataframe
states_info <- states_df %>%
  left_join(states_events, by = c("state_name" = "state"))

## Part 10
ggplot(states_info, aes(x = area, y = num_events, col = region)) +
  geom_point() +
  labs(x = "Land area (square miles)",
       y = "# of storm events in 1996")
