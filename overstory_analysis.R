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
# info on which plots to use and how to categorize them
anson <- readxl::read_excel("raw/RA_data_Master_202500619.xlsx")
maggie <- readxl::read_excel("raw/RA_data_Master_202500619_Maggie.xlsx")
glimpse(plotinfo)

# tidy data ####
# tidy plotinfo
plotinfo %>% rename(trt_unit = `Trt Unit`,
                    trt_type = `trt type`,
                    id = plot,
                    PRE_1 = Pre1,
                    PRE_2 = Pre2,
                    POST_1 = Post1,
                    POST_2 = Post2,
                    POST_3 = Post3,
                    POSTRX_1 = PostRx1,
                    POSTRX_2 = `Post Rx2`,
                    post2_age = `2nd post trt`,
                    quantity = Quantity,
                    )


# join trt unit, trt type info to ntrees and alltrees

ntrees %>% left_join()



# BA: compare pre, post1, post5, post10(?) * treatment type in a grouped bar
# plot. also try * forest type and see what it looks like.

# Calculate basal area

# Treat DBH as numbers (this column was automatically being factored)
trees$DBH <- as.numeric(paste(trees$DBH,""))
# Calculate BA for each tree
trees$BA_ft2<- pi * (trees$DBH/24)^2
# Calculate the plot size in acres: 162=plot dimensions in ft,  43560=ft^2 in an acre
trees$plot_size = 162*162/43560
# Divide individual tree BA by plot size to get the BA each tree represents on a per-acre basis
trees$BA_ac = with(trees, BA_ft2 / plot_size)



# Calculate the number of trees per acre each individual tree represents
trees$TPA = with(trees, 1/plot_size)

# TPA: same as BA

# QMD: same as BA, TPA

# Species composition: BA * species across time periods (grouped barplot). also
# * trt type and * forest type? (multipanel?)
