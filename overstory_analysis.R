# analysis for final report, including BA, TPA, QMD, species composition

# load packages ####
library(tidyverse)
library(ggpubr)

# load data ####
# tidy tree-by-tree data, all statuses included
alltrees <- read_csv("overstory_tidy.csv") %>%
  # remove gambel oak
  filter(species != "QUGA") %>%
  # fix two remaining weird values, rename POTR5 to POTR
  mutate(species = case_when(
    species == "PS,E" ~ "PSME",
    species == "UNID" ~ "UNK",
    species == "POTR5" ~ "POTR",
    .default = species
    ))
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
  mutate(trt_type = case_when(
    trt_type == "RX" | trt_type == "Rx" ~ "Prescribed burn",
    trt_type == "mechanical" ~ "Mechanical",
    .default = trt_type
  ))

# info on forest types for each plot
typeinfo <- readxl::read_excel("raw/RA_visits_summary_allyears.xlsx") %>%
  select(id = Plot_name, forest_type = `Dominant forest type`) %>%
  # change values for a few key "mixed" sites
  mutate(forest_type = case_when(
    id %in% c("RA181", "RA191") ~ "Mesic mixed conifer",
    id == "RA301" ~ "Dry mixed conifer",
    .default = forest_type
  ))

# join info
info <- left_join(plotinfo, typeinfo, by = "id")

# tidy data ####
alltrees_tidy <- alltrees %>%
  inner_join(info, by = "id")
glimpse(alltrees_tidy)

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
## no plot for this

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
# get count of plots by trt group
plot_df %>% group_by(trt_type, Visit) %>% summarize(n = n())
# L7 Post5 data is excluded. Only plot quadrants recorded.
# just going to put count as 22 for mech.
### mechanical ####

exp = expression(paste('Basal Area (ft '^'2'*'ac'^'-1'*')'))
a <- ggplot(df2 %>% filter(trt_type == "Mechanical"),
            aes(x = Visit, y = mean_ba_ac, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


a
b <- ggplot(df2 %>% filter(trt_type == "Mechanical"),
            aes(x = Visit, y = mean_tpa, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Trees per Acre") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
b
c <- ggplot(df2 %>% filter(trt_type == "Mechanical"),
            aes(x = Visit, y = mean_qmd, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Quadratic Mean Diameter (in)") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
c
by_visit_and_trt_mech <- ggarrange(a, b, c,
                              ncol = 3,
                              legend = "right",
                              common.legend = TRUE) +
  annotate("text", x = 1/7, y = 0.95, label = "a) Mechanical (n=22)", size =3, hjust = 0)

### rx burn ####
a <- ggplot(df2 %>% filter(trt_type == "Prescribed burn"),
            aes(x = Visit, y = mean_ba_ac, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

a
b <- ggplot(df2 %>% filter(trt_type == "Prescribed burn"),
            aes(x = Visit, y = mean_tpa, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Trees per Acre") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
b
c <- ggplot(df2 %>% filter(trt_type == "Prescribed burn"),
            aes(x = Visit, y = mean_qmd, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Quadratic Mean Diameter (in)") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
c

by_visit_and_trt_rx <- ggarrange(a, b, c,
                                   ncol = 3,
                                   legend = "right",
                                   common.legend = TRUE) +
  annotate("text", x = 1/7, y = 0.95, label = "b) Prescribed burn (n=12)", size = 3, hjust = 0)
ggarrange(by_visit_and_trt_mech, by_visit_and_trt_rx, nrow = 2)
ggsave("stats_by_trt_type.png", width = 7.5, height = 5, units = "in", dpi = 600, bg = "white")

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
plot_df %>% group_by(forest_type, Visit) %>% summarize(n = n())
### dry ####
a <- ggplot(df3 %>% filter(forest_type == "Dry mixed conifer"),
            aes(x = Visit, y = mean_ba_ac, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

b <- ggplot(df3 %>% filter(forest_type == "Dry mixed conifer"),
            aes(x = Visit, y = mean_tpa, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Trees per Acre") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

c <- ggplot(df3 %>% filter(forest_type == "Dry mixed conifer"),
            aes(x = Visit, y = mean_qmd, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Quadratic Mean Diameter") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

by_visit_and_forest_dry <- ggarrange(a, b, c,
                                 ncol = 3,
                                 legend = "right",
                                 common.legend = TRUE) +
  annotate("text", x = 1/8, y = 0.92, label = "a) Dry mixed conifer (n=16)", size = 2.8, hjust = 0, bg = "white")

### mesic ####
a <- ggplot(df3 %>% filter(forest_type == "Mesic mixed conifer"),
            aes(x = Visit, y = mean_ba_ac, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

b <- ggplot(df3 %>% filter(forest_type == "Mesic mixed conifer"),
            aes(x = Visit, y = mean_tpa, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_tpa - se_tpa,
                    ymax = mean_tpa + se_tpa),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Trees per Acre") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

c <- ggplot(df3 %>% filter(forest_type == "Mesic mixed conifer"),
            aes(x = Visit, y = mean_qmd, fill = Visit)) +
  geom_col() +
  geom_errorbar(aes(ymin = mean_qmd - se_qmd,
                    ymax = mean_qmd + se_qmd),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = "Quadratic Mean Diameter") +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()

by_visit_and_forest_mesic <- ggarrange(a, b, c,
                                     ncol = 3,
                                     legend = "right",
                                     common.legend = TRUE) +
  annotate("text", x = 1/8, y = 0.92, label = "b) Mesic mixed conifer (n=18)", size = 2.8, hjust = 0)

ggarrange(by_visit_and_forest_dry, by_visit_and_forest_mesic, nrow = 2)
ggsave("stats_by_forest_type.png", width = 7.5, height = 5, units = "in", dpi = 600, bg = "white")

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
## no plot for this

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
a <- ggplot(df5 %>% filter(trt_type == "Mechanical"),
            aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  annotate("text", x = 4, y = 38, hjust = 0, label = "a) Mechanical (n=22)", size = 4)

b <- ggplot(df5 %>% filter(trt_type == "Mechanical"), aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  annotate("text", x = 4, y = 38, hjust = 0, label = "b) Prescribed burn (n=12)", size = 4)
ggarrange(a, b, nrow = 2)
ggsave("spec_by_trt.png", width = 7, heigh = 5, units = "in", dpi = 600, bg = "white")
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

c <- ggplot(df6 %>% filter(forest_type == "Dry mixed conifer"),
            aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic() +
  annotate("text", x = 4, y = 60, hjust = 0, label = "a) Dry mixed conifer (n=16)", size = 4)

d <- ggplot(df6 %>% filter(forest_type == "Mesic mixed conifer"),
            aes(x = species, y = mean_ba_ac, fill = Visit)) +
  geom_col(position = position_dodge(0.9)) +
  geom_errorbar(aes(ymin = mean_ba_ac - se_ba_ac,
                    ymax = mean_ba_ac + se_ba_ac),
                width = 0,
                position = position_dodge(0.9)) +
  labs(x = NULL, y = exp) +
  scale_fill_manual(values = c("gray30", "gray50", "gray70")) +
  theme_classic()+
  annotate("text", x = 4, y = 40, hjust = 0, label = "b) Mesic mixed conifer (n=18)", size = 4)

ggarrange(c, d, nrow = 2)
ggsave("spec_by_forest.png", width = 7, heigh = 5, units = "in", dpi = 600, bg = "white")

# output ####
## statistics ####
df1
df2
df3
df4
df5
df6

## plots ####
# all stats by plot
by_visit_and_trt_mech
by_visit_and_trt_rx
by_visit_and_forest_dry
by_visit_and_forest_mesic
# ba by species
a
b
c
d
