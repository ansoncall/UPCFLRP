# analysis for final report, including BA, TPA, QMD, species composition

# load packages ####
library(tidyverse)

# load data ####
# tree counts per plot, live trees only
ntrees <- read_csv("ntrees_liveonly.csv")
glimpse(ntrees)
# tidy tree-by-tree data, all statuses included
alltrees <- read_csv("overstory_tidy.csv")
glimpse(alltrees)

# BA: compare pre, post1, post5, post10(?) * treatment type in a grouped bar
# plot. also try * forest type and see what it looks like.

# TPA: same as BA

# QMD: same as BA, TPA

# Species composition: BA * species across time periods (grouped barplot). also
# * trt type and * forest type? (multipanel?)
