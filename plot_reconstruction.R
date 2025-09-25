# reconstruct data for problematic plots, examine possibility of missed trees in
# pre visits
# load packages ####
library(tidyverse)
# load data ####
overstory <- read_csv("overstory_tidy.csv")

# most problematic plots ####
problem_plots <- c(
  "3A",
  "9A",
  "9B",
  "9D",
  "L7",
  "L12",
  "L27",
  "RA178",
  "RA281",
  "RA610",
  "RA1110",
  "RA1410",
  "RA2540"
)

overstory %>% filter(id %in% problem_plots) %>%
  arrange(id, date) %>%
  select(id, date, status, dbh) %>%
  group_by(id, date) %>%
  summarise(
    n = n(),
    min_dbh = min(dbh, na.rm = TRUE),
    max_dbh = max(dbh, na.rm = TRUE),
    mean_dbh = mean(dbh, na.rm = TRUE),
    median_dbh = median(dbh, na.rm = TRUE)
  )
# check site 3A
# 2024 reconstruction data not here
overstory %>%
  filter(id == "3A",
         status %in% c("L", "D", "S"),
         ) %>%
  ggplot() +
  geom_point(
    aes(x = x, y = y, color = as.character(date), size = dbh, pch = status),
    stroke = 1,
    alpha = 0.5
    ) +
  labs(
    title = "Plot 3A",
    x = "X Coordinate",
    y = "Y Coordinate",
    color = "Visit Index",
    size = "DBH (cm)",
    pch = "Status"
  ) +
  scale_shape_manual(values = c("L" = 19, "D" = 10, "S" = 4))

# maybe try a distance matrix between all stumps in POSTRX_2?
overstory %>%
  arrange(desc(date)) %>% View

# is there data missing from "Master"?
d <- read_excel("raw/RA_data_Master_20250224.xlsx", sheet = "Overstory")
d24 <- read_excel("raw/2024_RA_Data_Final.xlsx", sheet = "Overstory")
d24

# check for missing data using anti_join
missing_data <- anti_join(d24,
                          d %>% mutate(DBH = as.character(DBH)),
                          by = c("PLOT" = "PLOT",
                                         "DBH" = "DBH",
                                         "DATE" = "DATE"))

# rows present in d24 but not in d:
missing_data %>%
  arrange(PLOT, DATE) %>%
  View

# percent of d24 that is missing:
missing_percent <- nrow(missing_data) / nrow(d24) * 100
missing_percent


# TODO
# marin will fix master data to make it consistent with 2024 data

# anson:

# use shear transform to better align data across pre- and post-trt visits.
# optimize shear transform to minimize stress (discordance or squared distance)
# between closest neighbor trees across different visits.

# after shear transform, calculate distance matrix between trees in one visit
# and trees in successive (or prior) visits.

# then, use distance matrix (multidimensional, on location and DBH) to identify
# trees that have no close match in successive visits.

# pay special attention to plots with known issues, and 2024 stumps (status ==
# "S"). 2024 stumps that don't have a close neighbor in the "pre" visit were
# probably presence and not counted (by accident or negligence) in that "pre"
# visit.
