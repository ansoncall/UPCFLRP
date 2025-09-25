# investigate overstory plot data to check for accidental "half-plot" measures

library(tidyverse)
library(magrittr)
library(viridis)
library(crayon)
library(readxl)

# RECONSTRUCTION FLAG ####

# some "plot reconstruction" data was collected in 2024 in order to assess
# completeness/accuracy of potentially incomplete PRE_ plots. this additional
# 2024 data was collected without recording precise tree/stump locations.
# Typically, this data would be discarded, but for specific purpose of "plot
# reconstruction" it should be kept. set reconstruction <- TRUE to include this
# data.

reconstruction <- TRUE
if (reconstruction) {
  cat("Reconstruction flag set to", bold(underline(green(reconstruction))))
} else {
  cat("Reconstruction flag set to", bold(underline(red(reconstruction))))
}

# list of plots with reconstruction data (these are the ones that need special
# handling):
reconstruction_plots <- c(
  "L12", "L27", "L7", "L9", "RA178", "RA281", "9A", "9B", "9D", "3A", "RA2540",
  "RA1110", "RA1410", "RA610", "RA2010"
)

# read in raw data ####
d <- read_excel("raw/RA_data_Master_202500619.xlsx",
                sheet = "Overstory",
                guess_max = 10000
                ) %>%
  # tidy names
  set_names(c("date", "t_idx", "id", "plot_type", "midline_dist", "s_dist",
              "n_dist", "species", "status", "damage1", "damage2", "dbh",
              "cbh_class", "standing_class", "field_notes", "entry_notes",
              "changed"))

# check id ####
# some IDS were recorded incorrectly. Fix those here by recoding where
# necessary.
d %<>%
  mutate(id = case_when(
    id == "9D" ~ "RA9D", # fix missing prefix
    id == "RA 810" ~ "RA810", # fix inconsistent spacing
    id == "RA216" ~ "RA219", # NEW FIX based on examination of data and notes
    # id == "RA2160" ~ "RA216", # THESE ARE NOT THE SAME
    # id == "RA2510" ~ "RA251", # THESE ARE NOT THE SAME
    # id == "RA2610" ~ "RA261", # THESE ARE NOT THE SAME
    .default = id
  ))

# check dates ####
d %>% filter(is.na(date) | is.null(date)) # no na or null, good
# histogram shows no unusual values
d %>% ggplot(aes(x = date)) +
  geom_histogram() +
  # scale_x_date(date_labels = "%Y-%m-%d") +
  labs(title = "Overstory Plot Dates",
       x = "Date",
       y = "Count") +
  theme_minimal()

# check that all dates are the same for each t_idx and id
foo <- d %>%
  # count number of records in each group
  group_by(t_idx, id, date) %>%
  summarise(count = n(), .groups = "keep") %>%
  arrange(id, count)
print(foo, n = nrow(foo)) # multiple pre- and post- visits for some plots
print(foo %>% arrange(count), n = nrow(foo)) # a few very low tree counts

# low counts may be instances of one plot visit split across multiple days
d %>% filter(t_idx == "PRE", id == "RA2500", date == as.Date("2017-07-06"))
d %>% filter(t_idx == "PRE", id == "RA2500", date == as.Date("2017-07-07"))

# examine carefully to group dates where necessary:
for (i in seq_along(unique(foo$id))) {
  plot <- foo %>% filter(id == unique(foo$id)[i])
  cat(blue(paste(i, unique(foo$id)[i]), "\n"))
  print(plot %>% arrange(date), n = nrow(plot))
}
# issues:
date_issues <- c("L23", "RA1210", "RA219", "RA2300", "RA2500",
                 "RA271", "RA410")

for (i in seq_along(date_issues)) {
  plot <- foo %>% filter(id == date_issues[i])
  cat(blue(paste(i, date_issues[i]), "\n"))
  print(plot %>% arrange(date), n = nrow(plot))
}

# prep and plot all tree data by date
treemap <- function(data) {
  data %>%
    # drop unneeded columns
    select(date, id, t_idx, midline_dist, s_dist, n_dist, dbh) %>%
    mutate(# convert n_dist to -s_dist, merge s_dist and n_dist
      y = case_when(!is.na(as.numeric(s_dist)) ~ as.numeric(s_dist),
                    is.na(as.numeric(s_dist)) ~ -as.numeric(n_dist)),
      x = as.numeric(midline_dist),
      # clean up t_idx
      t_idx = case_when(t_idx %in% c("PRE", "PRE?") ~ "PRE",
                        t_idx == "POSTRX" ~ "POST",
                        t_idx == "POST" ~ "POST")
    ) %>%
    ggplot(aes(x = x, y = y, color = t_idx, fill = date, size = dbh)) +
    geom_point(pch = 21, stroke = 1) +
    theme_minimal()
}

# L23
treemap(d %>%
          filter(id == "L23", date > as.Date("2022-07-01")) %>%
          mutate(date = as.character(date)))
# appears to be a single visit, with N half recorded the day before the S half
d %<>% mutate(date_corrected = case_when(
  id == "L23" & date == as.Date("2022-07-11") ~ as.Date("2022-07-12"),
  .default = NA))

# RA1210
treemap(d %>%
          filter(id == "RA1210" & date < as.Date("2016-01-01")) %>%
          mutate(date = as.character(date)))
# same issue as before
d %<>% mutate(date_corrected = case_when(
  id == "RA1210" & date == as.Date("2015-06-21") ~ as.Date("2015-06-22"),
  .default = date_corrected))

# RA219
treemap(d %>%
          filter(id == "RA219" & date < as.Date("2016-01-01")) %>%
          mutate(date = as.character(date)))
# same issue as before
d %<>% mutate(date_corrected = case_when(
  id == "RA219" & date == as.Date("2014-06-10") ~ as.Date("2014-06-12"),
  .default = date_corrected))

# RA2300
treemap(d %>%
          filter(id == "RA2300") %>%
          mutate(date = as.character(date)))
# I'd guess the year is recorded incorrectly for the few "2017" trees.
d %<>% mutate(date_corrected = case_when(
  id == "RA2300" & date == as.Date("2017-06-13") ~ as.Date("2016-06-13"),
  .default = date_corrected))

# RA2500
treemap(d %>%
          filter(id == "RA2500") %>%
          mutate(date = as.character(date)))
# looks like they maybe started a plot but didn't finish it, since 07-06 only
# has 2 trees recorded.
d %<>% mutate(date_corrected = case_when(
  id == "RA2500" & date == as.Date("2017-07-06") ~ as.Date("2017-07-07"),
  .default = date_corrected))

# RA271
treemap(d %>%
          filter(id == "RA271" & date < as.Date("2015-01-01")) %>%
          mutate(date = as.character(date)))
# this also looks like they started a plot but didn't finish it.
d %<>% mutate(date_corrected = case_when(
  id == "RA271" & date == as.Date("2014-07-09") ~ as.Date("2014-07-10"),
  .default = date_corrected))

# RA410
treemap(d %>%
          filter(id == "RA410" & date < as.Date("2015-01-01")) %>%
          mutate(date = as.character(date)))
# another instance of N and S halves done on different days
d %<>% mutate(date_corrected = case_when(
  id == "RA410" & date == as.Date("2014-07-09") ~ as.Date("2014-07-10"),
  .default = date_corrected))

# now, fill in remaining date_corrected values with original values
d %<>% mutate(date_corrected = case_when(
  is.na(date_corrected) ~ date,
  .default = date_corrected))

# some other plots with two pre-treatment visit dates.
two_visit_sites <- c("L7", "L29", "L8", "L10", "L11")

d %>% filter(id %in% two_visit_sites) %>%
  group_by(id, t_idx, date_corrected) %>%
  summarise(count = n(), .groups = "keep") %>%
  arrange(id, t_idx, date_corrected)
# L11 2013 visit should probably be thrown out, as the data quality of that crew
# leader is suspect and the 2015 data is likely better.
treemap(d %>%
          filter(id == "L10" & date < as.Date("2016-01-01")) %>%
          mutate(date = as.character(date),
                 dbh = as.numeric(dbh)))


# L10 2013 visit should probably be thrown out, but there was never a treatment
# completed so it is a nothingburger.
treemap(d %>%
          filter(id == "L11" & date < as.Date("2016-01-01")) %>%
          mutate(date = as.character(date),
                 dbh = as.numeric(dbh)))


d %>% filter(id == "L29") %>%
  arrange(as.numeric(dbh)) %>% View


foo <- d %>%
  # (re)count number of records in each group
  group_by(t_idx, id, date_corrected) %>%
  summarise(count = n(), .groups = "keep") %>%
  arrange(id, count)

# examine carefully to make sure all date errors have been caught:
for (i in seq_along(unique(foo$id))) {
  plot <- foo %>% filter(id == unique(d$id)[i])
  cat(blue(paste(i, unique(foo$id)[i]), "\n"))
  print(plot %>% arrange(date_corrected), n = nrow(plot))
}
# looks good

# check to ensure all pre dates actually precede post dates
d %>%
  filter(t_idx %in% c("PRE", "PRE?")) %>%
  group_by(id) %>%
  summarise(pre_date = max(date_corrected)) %>%
  full_join(d %>%
              filter(t_idx %in% c("POST","POSTRX")) %>%
              group_by(id) %>%
              summarise(post_date = min(date_corrected)),
            by = "id") %>%
  filter(pre_date >= post_date) # no issues

# check t_idx (PRE/POST) ####
unique(d$t_idx) # no missing values, some weird values
d %<>%
  mutate(t_idx = case_when(t_idx == "PRE?" ~ "PRE",
                           .default = t_idx))

# check plot_type ####
unique(d$plot_type) # some NA
d %>% filter(is.na(plot_type)) %>% group_by(id, date) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
# not sure how to fix this or whether it matters

# check midline_dist ####
hist(as.numeric(d$midline_dist), breaks = 50) # some issues here
# some non_numeric values as well
d %>% filter(is.na(as.numeric(midline_dist))) %>% pull(midline_dist) %>% unique
if (reconstruction) {
  # save these values
  d %<>% mutate(midline_dist_notes = case_when(
    is.na(as.numeric(midline_dist)) ~ midline_dist,
    .default = NA
  ))
}
d %>% arrange(desc(midline_dist)) %>% filter(midline_dist > 164)
d %>% arrange(midline_dist)
d %>% filter(is.na(midline_dist))

# by plot visit
d %>% filter(midline_dist < 0 | midline_dist > 160 | is.na(midline_dist)) %>%
  group_by(date, t_idx, id) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

# check s_dist ####
hist(as.numeric(d$s_dist), breaks = 50) # some issues here
# no consequential non-numeric values
d %>% filter(is.na(as.numeric(s_dist))) %>% pull(s_dist) %>% unique
d %>% arrange(desc(s_dist)) %>% filter(s_dist > 82)
d %>% arrange(s_dist)

# check n_dist ####
hist(as.numeric(d$n_dist), breaks = 50) # some issues here
# no consequential non-numeric values
d %>% filter(is.na(as.numeric(n_dist))) %>% pull(n_dist) %>% unique
d %>% arrange(desc(n_dist)) %>% filter(n_dist > 82)
d %>% arrange(n_dist)

# ONE OF s_dist or n_dist should be NA
d %>% filter(!is.na(s_dist) & !is.na(n_dist)) # should be 0
d %>% filter(is.na(s_dist) & is.na(n_dist)) # should be 0, 19 found


d %>%
  filter(id %in% reconstruction_plots,
         date >= as.POSIXct("2024-01-01")) %>%
  View()

# correct coordinate errors ####
if (reconstruction) {
  # modified fix that doesn't discard any rows from reconstruction plots
  d %<>% mutate(
    midline_dist = as.numeric(midline_dist),
    s_dist = as.numeric(s_dist),
    n_dist = as.numeric(n_dist)
  ) %>%
    mutate(
      midline_dist = case_when(
        # exclude plots that have midline_dist just over 164 or NA
        # for one plot that's WAY over 160, assume missing decimal
        midline_dist > 400 ~ midline_dist / 10,
        .default = midline_dist),
      s_dist = case_when(
        # when both s_dist and n_dist are NA, set s_dist to 0
        is.na(s_dist) & is.na(n_dist) ~ 0,
        # assume a few extremely large values are instances of missing decimal
        s_dist > 5000 ~ s_dist / 100,
        s_dist > 100 & s_dist <= 5000 ~ s_dist / 10,
        .default = s_dist),
      n_dist = case_when(
        # assume a few extremely large values are instances of missing decimal
        n_dist > 100 & n_dist <= 5000 ~ n_dist / 10,
        .default = n_dist)
    ) %>%
    # exclude when any coordinate is slightly over the limit NOT INCLUDING
    # reconstruction plots
    filter(id %in% reconstruction_plots | midline_dist <= 160,
           id %in% reconstruction_plots | (s_dist <= 82 | is.na(s_dist)),
           id %in% reconstruction_plots | (n_dist <= 82 | is.na(n_dist)))
} else {
  # after discussion with Marin:
  d %<>% mutate(
    midline_dist = as.numeric(midline_dist),
    s_dist = as.numeric(s_dist),
    n_dist = as.numeric(n_dist)
  ) %>%
    mutate(
      midline_dist = case_when(
        # exclude plots that have midline_dist just over 164 or NA
        # for one plot that's WAY over 160, assume missing decimal
        midline_dist > 400 ~ midline_dist / 10,
        .default = midline_dist),
      s_dist = case_when(
        # when both s_dist and n_dist are NA, set s_dist to 0
        is.na(s_dist) & is.na(n_dist) ~ 0,
        # assume a few extremely large values are instances of missing decimal
        s_dist > 5000 ~ s_dist / 100,
        s_dist > 100 & s_dist <= 5000 ~ s_dist / 10,
        .default = s_dist),
      n_dist = case_when(
        # assume a few extremely large values are instances of missing decimal
        n_dist > 100 & n_dist <= 5000 ~ n_dist / 10,
        .default = n_dist)
    ) %>%
    # exclude when any coordinate is slightly over the limit
    filter(midline_dist <= 160,
           s_dist <= 82 | is.na(s_dist),
           n_dist <= 82 | is.na(n_dist))
}

# check species ####
d %>% filter(is.na(species)) # no NA, this is good
table(d$species) # some cleanup needed here

d %<>%
  mutate(species = case_when(
    species == "PIXXX" ~ "PIXX",
    species == "UNKFIR" | species == "UNKFl" | species == "UNKID" ~ "UNK",
    species == "Abla" ~ "ABLA",
    species == "psme" ~ "PSME",
    species == "ps,e" ~ "PSME",
    species == "POTR" ~ "POTR5",
    .default = species
  ))

# check status ####
table(d$status, useNA = "always") # some cleanup needed here
d %<>%
  mutate(status = case_when(
    status == "s" ~ "S",
    status == "." ~ NA_character_,
    .default = status
  ))

# check damage1 ####
table(d$damage1, useNA = "always") # some cleanup needed here
# TODO review this
d %<>%
  mutate(damage1 = case_when(
    damage1 == "." ~ NA_character_,
    .default = damage1
  ))

# check damage2 ####
table(d$damage2, useNA = "always") # some cleanup needed here
d %<>%
  mutate(damage2 = case_when(
    damage2 == "." ~ NA_character_,
    .default = damage2
  ))

# check dbh ####
# dbh should be numeric
# try converting to numeric and check the ones that fail conversion
d %>% mutate(dbhNA = is.na(as.numeric(dbh))) %>%
  filter(dbhNA)
# idk what to do about these, just dropping them. probably missing data.
d %<>% mutate(dbh = as.numeric(dbh)) %>% filter(!is.na(dbh))
hist(d$dbh, breaks = 50) # some issues here
d %>% arrange(desc(dbh)) %>% filter(dbh > 50)
d %>% arrange(dbh)
# leaving all this alone, though 121 dbh aspen is pretty much impossible.
# Perhaps wrong unit or missing decimals for some of these?

## fix dbh cutoff ####
# known issue: inconsistent minimum dbh thresholds across plot visits. fix:
# implement dbh cutoff of 8.
ifelse(reconstruction,
       {
         d %<>% filter(id %in% reconstruction_plots | dbh >= 8)
       },
       {
         d %<>% filter(dbh >= 8)
       }
)

# check cbh_class ####
table(d$cbh_class, useNA = "always") # some cleanup needed here
d %<>% mutate(
  cbh_class = as.integer(case_when(
    cbh_class == "." ~ NA_integer_,
    cbh_class == "UNK" ~ NA_integer_,
    cbh_class == "7" ~ NA_integer_, # class 7 not defined
    .default = as.integer(cbh_class)
  ))
)

# check standing_class ####
# Not using this anyway
table(d$standing_class, useNA = "always") # some cleanup needed here
d %<>% mutate(
  standing_class = case_when(
    standing_class == "UNK" ~ NA_character_,
    standing_class == "." ~ NA_character_,
    .default = standing_class
  ))

# some general cleanup and prep
d_tidy <- d %>%
  # drop unneeded columns
  select(id, t_idx, date = date_corrected, midline_dist, s_dist, n_dist,
         species, dbh, standing_class, status, midline_dist_notes) %>%
  mutate(# convert n_dist to -s_dist, merge s_dist and n_dist
    y = case_when(!is.na(s_dist) ~ s_dist,
                  is.na(s_dist) ~ -n_dist),
    x = midline_dist) %>%
  select(-s_dist, -n_dist)

if (!reconstruction) {
  # drop the midline_dist_notes col
  d_tidy %<>% select(-midline_dist_notes)
}

# make new t_idx to distinguish multiple pre- and post-treatment visits
temporal_index <- d_tidy %>%
  group_by(id, t_idx, date) %>%
  summarize(count = n(), .groups = "keep") %>%
  group_by(id, t_idx) %>%
  mutate(t_idxn = paste0(t_idx, "_", row_number())) %>%
  select(-count)

# add t_idxn to the original data
d_tidy %<>%
  left_join(temporal_index, by = c("id", "t_idx", "date")) %>%
  relocate(id, t_idx, t_idxn, date, x, y, species, dbh, standing_class, status)

d_tidy


# check t_idxn sequence ####

# plots should not revert from "post" -> "pre" or "postrx" -> "post" status.
# Once a plot visit is recorded as postrx, all subsequent plots should also be
# postrx (and so on).

# inspect to ensure datetimes are increasing from L to R across rows:
d_tidy %>%
  group_by(id, t_idxn) %>%
  summarize(date = first(date)) %>%
  ungroup() %>%
  pivot_wider(names_from = t_idxn, values_from = date) %>%
  select(id, PRE_1, PRE_2, POST_1, POST_2, POST_3, POSTRX_1, POSTRX_2) %>%
  View()





# export
if (reconstruction) {
  write_csv(d_tidy, "overstory_tidy_reconstruction.csv")
} else {
  write_csv(d_tidy, "overstory_tidy.csv")
}

# ntrees data
ntrees <- d_tidy %>%
  group_by(id, t_idxn) %>%
  summarise(ntrees = n(), .groups = "keep") %>%
  ungroup() %>%
  pivot_wider(names_from = t_idxn, values_from = ntrees) %>%
  rowwise %>%
  mutate(mean = mean(c_across(starts_with("P")), na.rm = TRUE),
         sd = sd(c_across(starts_with("P")), na.rm = TRUE),
         coef_variation = ifelse(mean == 0, NA, sd / mean)) %>%
  select(id, starts_with("PRE"), starts_with("POST_"), starts_with("POSTRX"),
         mean, coef_variation) %>%
  arrange(desc(coef_variation))

# write out ####
if (reconstruction) {
  write_csv(ntrees, "ntrees_reconstruction.csv")
} else {
  write_csv(ntrees, "ntrees.csv")
}
