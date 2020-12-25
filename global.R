library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(tidyverse) 
library(plotly)
library(glue)
library(scales) 
library(ggpubr)
library(readxl)
library(DT)
library(leaflet)

univ_rank_cat <- read_excel("Addresses_Geocoded 2.xlsx")
univ_rank <- univ_rank_cat %>% 
  mutate(Categoric = factor(Categoric, levels = c("1-10", "11-100", "101-200", "201-250", "251-300", 
                                                  "301-350", "351-400", "401-500", "501-600", "601-800",
                                                  "801-1000", "1001+")))

theme_CPDV <- theme(legend.key = element_rect(fill="black"),
                    legend.background = element_rect(color="white", fill="#263238"),
                    plot.subtitle = element_text(size=6, color="white"),
                    panel.background = element_rect(fill="#ead3f5"),
                    panel.border = element_rect(fill=NA),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color="#e3c7f0", linetype=2),
                    panel.grid.minor.y = element_blank(),
                    plot.background = element_rect(fill="#270d40"),
                    text = element_text(color="white"),
                    axis.text = element_text(color="white")
)

selectcategory <- unique(univ_rank$Categoric)
