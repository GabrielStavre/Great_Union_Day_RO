#########################
## Happy B-Day Romania ##
#########################


# Set working directory
#setwd("~/Documents/Plots/B-day Romania/GitHub version")

# Housekeeping
rm(list = ls())


# Define libraries
library(ggplot2)
library(readxl)


# Define variables
start_year = 1918
end_year = 2019


# Calculate additional variables
len1 <- (end_year-start_year)/0.1+1
len2 <- (6-1)/0.01+1


# Define & fill matrix
m <- matrix(nrow = len1*len2, ncol = 3)

nb <- 0
for (i in seq(1918,2019,0.1)) {
  for (j in seq(1,6,0.01)) {

    nb <- nb+1

    m[nb,1] = i
    m[nb,2] = j
    m[nb,3] = i*10
    # print(nb)
    
  }
  
}


# Make dataframe
DB <- as.data.frame(m)


# Build plot
ggplot(DB, aes(x = V1, y = V2, fill = V3)) +
  geom_tile() + 
  scale_fill_gradientn(colours = 
                         c("#002B7F", "#FCD116", "#CE1126")) +
  scale_x_continuous(breaks = c(seq(start_year, end_year-10, by = 10),end_year)) +
  scale_y_continuous(breaks = seq(1, 6, by = 0.5)) +
  annotate("text", x = start_year+round((end_year-start_year)/2,0), y = 4,
           label = "Happy Birthday Romania",color = 'white',size = 15, fontface='bold') +
  annotate("text", x = start_year+round((end_year-start_year)/2,0), y = 3.65,
           label = "___", color = "white", size = 15, fontface='bold') +
  annotate("text", x = start_year+round((end_year-start_year)/2,0), y = 3,
           label = paste(end_year-start_year,"years",sep = " "),color = 'white',size = 10, fontface='bold') +
  labs(title = "Figure 1. Great Union Day...",
       subtitle = "
... the unification of those Romanians and 
of all the territories inhabited by them with Romania",
       caption = "
Source: Alba Iulia National Assembly") +
  theme_classic() +
  theme(plot.title = element_text(size = 36),
        plot.subtitle = element_text(size = 32, face = "italic"),
        plot.caption = element_text(size = 28, face = "italic")) +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(size = 28))+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank()) +
        #axis.ticks.y=element_blank())+
  theme(legend.position = "none") +
  theme(panel.grid.major = element_line(colour = "grey", linetype = "dashed"))