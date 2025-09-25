# check plot ids
library(tidyverse)
# read in data
d <- read_csv("overstory_tidy.csv")
write_csv(d, "overstory_INCORRECT_IDS.csv")
d <- read_csv("overstory_INCORRECT_IDS.csv")

# filter to questionable plots only
df <- d %>%
  filter(id %in% c("RA2510", "RA251", "RA261", "RA2160", "RA2190", "RA219", "9D", "RA810", "RA2160", "RA216", "RA9D", "RA 810"))
df
# 2510~251
ggplot(df %>%
         filter(id %in% c("RA2510", "RA251")),
       aes(x = x, y = y, shape = t_idxn, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# not obviously the same plot

# 261~251
ggplot(df %>%
         filter(id %in% c("RA261", "RA251")),
       aes(x = x, y = y, shape = t_idxn, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# not obviously the same plot

# 216~261
ggplot(df %>%
         filter(id %in% c("RA216", "RA261")),
       aes(x = x, y = y, shape = t_idxn, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# maybe? 216 certainly seems like a partial plot, but 261 seems complete

# 216~2160
ggplot(df %>%
         filter(id %in% c("RA216", "RA2160")),
       aes(x = x, y = y, shape = t_idxn, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# maybe? 216 certainly seems like a partial plot, but 2160 also seems complete

# 216~2610
ggplot(df %>%
         filter(id %in% c("RA216", "RA2610")),
       aes(x = x, y = y, shape = t_idxn, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# there is no 2610

# 216~219
ggplot(df %>%
         filter(id %in% c("RA216", "RA219"),
                t_idxn == "PRE_1"),
       aes(x = x, y = y, shape = id, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# this is probably it

# 216~219
ggplot(d %>%
         filter(id %in% c("RA2040"),
                t_idxn == "POSTRX_1") %>%
         rbind(df %>% filter(id == "RA216")),
       aes(x = x, y = y, shape = id, color = id, size = dbh)) +
  geom_point() +
  labs(x = "Midline distance",
       y = "N/S distance") +
  theme_minimal() +
  coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))
# this is probably not it, but the fact remains that RA2040 is missing some data.

walk(c("RA2510", "RA251", "RA261", "RA2160", "RA2190", "RA219", "9D",
       "RA810", "RA2160", "RA216", "RA9D", "RA 810"), \(plot) {
         img <- ggplot(df %>%
                  filter(id == plot),
                aes(x = x, y = y, color = t_idxn, size = dbh)) +
           geom_point() +
           labs(title = paste0(plot),
                x = "Midline distance",
                y = "N/S distance") +
           theme_minimal() +
           coord_cartesian(xlim = c(-2, 166), ylim = c(-84, 84))

         print(img)

       }, .progress = TRUE)

nt <- read_csv("ntrees.csv")
ntf <- nt %>% filter(id %in% c("RA2510", "RA251", "RA261", "RA2160", "RA2190", "RA219", "9D", "RA810", "RA2160", "RA216", "RA9D", "RA 810"))
ntf


raw <- read_csv("RA_data_Master_202500619.xlsx")


# what I think:
# "RA2510" Unique.
# "RA251" Unique, though there is a RGN 251 in a different location.
# "RA261" Unique, though there is a SVD-1T6-261 near Hwy 67/Oxyoke.
# "RA2160" Unique, though there is a RGN 216 and an ERRONEOUS RA216

# "RA216" This is probably supposed to be RA219 (specifically PRE_1). Both 216
# and 219 PRE measures made of 06/12/2014. Notes in RA_Master doc say there is
# no data sheet to be found for RA216. ArcGIS says only RA2160 and RGN 216
# exist.

# "RA2190" A unique plot.
# "RA219" A unique plot. Should include RA216 as RA219 PRE_1 data. Note that
# there is also an RGN 219 that is in a different loc.

# "RA9D" Does not exist in AGOL map. Must be the same as 9D.
# "9D" alias: RA9D.
# "RA2160" A unique plot.
# "RA810" A unique plot, alias RA 810. Note that there is a different NHD-1T2-810 near Oxyoke.
# "RA 810" Does not exist in AGOL map. Must be the same as 9D.
