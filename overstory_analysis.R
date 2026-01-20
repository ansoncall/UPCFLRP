# analysis for final report, including BA, TPA, QMD, species composition

# load packages ####
library(tidyverse)
library(ggpubr)

# load data ####
# tidy tree-by-tree data, all statuses included
alltrees <- read_csv("overstory_tidy.csv")
glimpse(alltrees)
# info on which plots to use and how to categorize them
plotinfo <- readxl::read_excel(
  "raw/Final_tree_data_analysis_plots_to_use.xlsx"
  ) %>%
  select(trt_type = `trt type`, id = Plot) %>%
  # remove extra cells, weird duplicate id
  # "RA 191" has odd spacing and is duplicate anyway
  filter(grepl("L|R", id), !is.na(trt_type), id != "RA 191") %>%
  # fix capitalization in trt_type
  mutate(trt_type = case_when(trt_type == "RX" ~ "Rx",
                              trt_type == "mechanical" ~ "Mechanical",
                              .default = trt_type))
# info on forest types for each plot
typeinfo <- readxl::read_excel("raw/RA_visits_summary_allyears.xlsx") %>%
  select(id = Plot_name, forest_type = `Dominant forest type`)
# join info
info <- left_join(plotinfo, typeinfo, by = "id")

# tidy data ####
alltrees_tidy <- alltrees %>%
  inner_join(info, by = "id")


# BA: compare pre, post1, post5, post10(?) * treatment type in a grouped bar
# plot. also try * forest type and see what it looks like.

# Calculate basal area
# Calculate BA for each tree

# from former script: trees$BA_ft2<- pi * (trees$DBH/24)^2.
# plot size in acres: 162=plot dimensions in ft,  43560=ft^2 in an acres

# TODO check with marin: are we only using live trees and snags, and eliminating
# some species? See previous code:
# # Eliminate everything but live trees and snags from dataset
# trees <- trees[which(trees$Status != "S"),]
# trees <- trees[which(trees$Status != "X"),]
# trees <- trees[which(trees$Status != "Y"),]
#
# #look at the unique tree species in the dataset
# unique(trees$Spp)
#
# # Remove species which aren't of interest to analysis
# trees <- trees[which(trees$Spp !="UNK"),]
# trees <- trees[which(trees$Spp !="QUGA"),]

# also is this qmd calc correct?
## # Calculate QMD
## summary$QMD = sqrt((summary$BA_ac/summary$TPA)/0.005454154)

# std error func
se <- \(x) sqrt(var(x)/length(x))

# by plot ####
# prep data by plot
plot_df <- alltrees_tidy %>%
  mutate(
    ba_ft2 = pi * (dbh/24) ^ 2,
    ba_ac = ba_ft2 / (162 * 162 / 43560),
    Visit = fct_relevel(
      as_factor(case_when(visit_age == -1 ~ "Pre",
                          visit_age == 1 ~ "Post1",
                          visit_age >= 4 ~ "Post5",
                          .default = "ERR")),
      "Pre")
  ) %>%
  filter(status == "L") %>%
  # id and Visit are the key grouping vars. trt_type, forest_type are extra.
  group_by(id, Visit, trt_type, forest_type) %>%
  summarize(ntrees = n(),
            ba_ac = sum(ba_ac),
            .groups = "keep") %>%
  mutate(tpa = ntrees / (162 * 162 / 43560),
         qmd = sqrt(ba_ac / (tpa * 0.005454))) %>%
  ungroup() %>%
  select(id, Visit, trt_type, forest_type, ba_ac, tpa, qmd)

## by visit ####
df1 <- plot_df %>%
  group_by(Visit) %>%
  summarize(mean_ba_ac = mean(ba_ac),
            se_ba_ac = se(ba_ac),
            mean_tpa = mean(tpa),
            se_tpa = se(tpa),
            mean_qmd = mean(qmd),
            se_qmd = se(qmd))

# plots
exp = expression(paste('Basal Area (ft '^'2'*'ac'^'-1'*')'))
a <- ggplot(df1, aes(x = Visit, y = mean_ba_ac)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0) +
  labs(x = NULL, y = exp) +
  theme_classic()

b <- ggplot(df1, aes(x = Visit, y = mean_tpa)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0) +
  labs(x = NULL, y = "Trees per Acre") +
  theme_classic()

c <- ggplot(df1, aes(x = Visit, y = mean_qmd)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0) +
  labs(x = NULL, y = "Quadratic Mean Diameter") +
  theme_classic()

by_visit <- ggarrange(a, b, c, ncol = 3)

## by visit * trt_type ####
df2 <- plot_df %>%
  group_by(Visit, trt_type) %>%
  summarize(mean_ba_ac = mean(ba_ac),
            se_ba_ac = se(ba_ac),
            mean_tpa = mean(tpa),
            se_tpa = se(tpa),
            mean_qmd = mean(qmd),
            se_qmd = se(qmd))

# plots
a <- ggplot(df2, aes(x = trt_type, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

b <- ggplot(df2, aes(x = trt_type, y = mean_tpa, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Trees per Acre") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

c <- ggplot(df2, aes(x = trt_type, y = mean_qmd, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Quadratic Mean Diameter") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

by_visit_and_trt <- ggarrange(a, b, c,
                              ncol = 3,
                              legend = "right",
                              common.legend = TRUE)

## by visit * forest_type ####
df3 <- plot_df %>%
  group_by(Visit, forest_type) %>%
  summarize(mean_ba_ac = mean(ba_ac),
            se_ba_ac = se(ba_ac),
            mean_tpa = mean(tpa),
            se_tpa = se(tpa),
            mean_qmd = mean(qmd),
            se_qmd = se(qmd))

# plots
a <- ggplot(df3, aes(x = forest_type, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

b <- ggplot(df3, aes(x = forest_type, y = mean_tpa, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Trees per Acre") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

c <- ggplot(df3, aes(x = forest_type, y = mean_qmd, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Quadratic Mean Diameter") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

by_visit_and_forest <- ggarrange(a, b, c,
                                 ncol = 3,
                                 legend = "right",
                                 common.legend = TRUE)

# by plot * species ####
# prep data
spec_df <- alltrees_tidy %>%
  mutate(
    ba_ft2 = pi * (dbh/24) ^ 2,
    ba_ac = ba_ft2 / (162 * 162 / 43560),
    Visit = fct_relevel(
      as_factor(case_when(visit_age == -1 ~ "Pre",
                          visit_age == 1 ~ "Post1",
                          visit_age >= 4 ~ "Post5",
                          .default = "ERR")),
      "Pre")
  ) %>%
  filter(status == "L",
         # filter out unknown species
         species != "UNID",
         species != "UNK") %>%
  # also group by species this time
  group_by(id, Visit, species, trt_type, forest_type) %>%
  summarize(ntrees = n(),
            ba_ac = sum(ba_ac),
            .groups = "keep") %>%
  mutate(tpa = ntrees / (162 * 162 / 43560),
         qmd = sqrt(ba_ac / (tpa * 0.005454))) %>%
  ungroup()

## by visit ####
df4 <- spec_df %>%
  group_by(Visit, species) %>%
  summarize(mean_ba_ac = mean(ba_ac),
            se_ba_ac = se(ba_ac),
            mean_tpa = mean(tpa),
            se_tpa = se(tpa),
            mean_qmd = mean(qmd),
            se_qmd = se(qmd)
  ) %>%
  ungroup()

a <- ggplot(df4, aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

## by visit * trt_type ####
df5 <- spec_df %>%
  group_by(Visit, species, trt_type) %>%
  summarize(mean_ba_ac = mean(ba_ac),
            se_ba_ac = se(ba_ac),
            mean_tpa = mean(tpa),
            se_tpa = se(tpa),
            mean_qmd = mean(qmd),
            se_qmd = se(qmd)
  ) %>%
  ungroup()

b <- ggplot(df5, aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  facet_wrap(~trt_type)

## by visit * forest_type ####
df6 <- spec_df %>%
  group_by(Visit, species, forest_type) %>%
  summarize(mean_ba_ac = mean(ba_ac),
            se_ba_ac = se(ba_ac),
            mean_tpa = mean(tpa),
            se_tpa = se(tpa),
            mean_qmd = mean(qmd),
            se_qmd = se(qmd)
  ) %>%
  ungroup()

c <- ggplot(df6, aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  facet_wrap(~forest_type)

# output ####
## statistics ####
df1
df2
df3
df4
df5
df6

## plots ####
by_visit
by_visit_and_trt
by_visit_and_forest
a
b
c
