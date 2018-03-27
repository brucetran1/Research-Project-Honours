# directory
setwd("C:/Users/rver4657/ownCloud/working/brucetran/Research-Project-Honours")

# libraries
library(tidyverse)
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
#library(ggplot2)

# read in the final data (from the analysis) dry period
used_st <- read_csv("data/obs.summary2_1990.csv")

st_df <- used_st %>%
  dplyr::select(Number,LAT,LON,Jurisdiction,Name,x1,x2,x3,x4,Area,CF,Viney,FDC.low,FDC.mid,FDC.high,AC,Peaks)

# Now use PCA (princomp)

pca <- prcomp(st_df[,6:ncol(st_df)], scale = T)

ggbiplot(pca, choices = 1:2, #first and second pcs
         obs.scale = 0.5, var.scale = 1, # scaling for visualisation
         ellipse = TRUE, circle = TRUE,# getting your ellipses
         labels.size = 6, varname.size=6,
         #groups = data$some_factor, # your ellipses
         alpha = 0.5) + # use 0 to remove points
  geom_point(colour="blue",size= 3) +
  theme_bw() + # removing gray background
  theme(panel.grid = element_blank()) + # removing background checkmarks
  theme(axis.text=element_text(size = rel(1.2)),
        axis.title=element_text(size=rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(2), face = "bold")) + 
  ggtitle("a) Wet Period") +
#  scale_x_continuous(limits=c(-8,8)) + # adjusting field of view
#  scale_y_continuous(limits=c(-3.5,5.5)) +
  scale_color_manual(values = c(#FFFF00", "#66CC00", "#FF6633",
                                "#3333CC")) # colouring the groups, html colour codes

ggsave("paper/wetperiodPCAplot.jpg", dpi = 600)

# read in the final data (from the analysis) dry period
used_st <- read_csv("data/obs.summary2_2000.csv")

st_df <- used_st %>%
  dplyr::select(Number,LAT,LON,Jurisdiction,Name,x1,x2,x3,x4,Area,CF,Viney,FDC.low,FDC.mid,FDC.high,AC,Peaks)

# Now use PCA (princomp)

pca <- prcomp(st_df[,6:ncol(st_df)], scale. = T)

ggbiplot(pca, choices = 1:2, #first and second pcs
         obs.scale = 0.5, var.scale = 1, # scaling for visualisation
         ellipse = TRUE, circle = TRUE,# getting your ellipses
         labels.size = 6, varname.size=6,
         #groups = data$some_factor, # your ellipses
         alpha = 0.5) + # use 0 to remove points
  geom_point(colour="blue",size= 3) +
  theme_bw() + # removing gray background
  theme(panel.grid = element_blank()) + # removing background checkmarks
  theme(axis.text=element_text(size = rel(1.2)),
        axis.title=element_text(size=rel(1.5), face = "bold"),
        plot.title = element_text(size = rel(2), face = "bold")) + 
  ggtitle("b) Dry Period") +
  #  scale_x_continuous(limits=c(-8,8)) + # adjusting field of view
    #scale_y_continuous(limits=c(-5,10)) +
  scale_color_manual(values = c(#FFFF00", "#66CC00", "#FF6633",
    "#3333CC")) # colouring the groups, html colour codes

ggsave("paper/dryperiodPCAplot.jpg", dpi = 600)



