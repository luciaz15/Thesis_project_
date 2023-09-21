#Snip of project

#clear= environment
rm(list=ls())
#Load Packages----
library(agricolae)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS) #Box-cox
library(tidyverse)
library(ggpubr)
library(flextable)
library(officer)
library(emmeans)
library(ggpmisc)
library(table1)
library(boot)
library(car) #Box-cox, 
library(rcompanion) #Box-cos
library(userfriendlyscience) # Gmaes-Howell test
library(RColorBrewer)
library(multcompView)
library(expss)
library(xtable)
library(FSA)
library(multcomp)
library(ggsignif)

#Import Dataset Metals-----
library(readxl)
Metals <- read_excel(".xlsx", 
                     sheet = "Metals_R", col_types = c("text", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "text", "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric","numeric",
                                                       "numeric", 
                                                       "numeric", "numeric","numeric"
                     ))


library(readxl)
carbon <- read_excel(".xlsx", 
                     sheet = "Carbon_R", col_types = c("text", 
                                                       "numeric", "numeric", "text", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric"))

#Upload EC, pH
library(readxl)
df2 <- read_excel(".xlsx", 
                  sheet = "Carbon_PH_CE_R", col_types = c("text", "numeric", "numeric", 
                                                          "text", "numeric", "numeric", "numeric", 
                                                          "skip", "numeric", "skip", "numeric", 
                                                          "skip", "numeric", "skip", "numeric", 
                                                          "numeric","numeric"), na = "NA")

#PREPARACION DE DATOS----
colnames(carbon)
#Comprimiendo los dataset en uno solo----
dc<- carbon %>% # carbon data
  group_by(Sample_name, Age, Row_position) %>%
  summarise_if(is.numeric,funs(mean(., na.rm = TRUE)))

dc$Age<-as.factor(dc$Age) #After to convert age to factor
df <- merge (dc, Metals, by=c("Sample_name")) # merge both datsets

df2_c <- df2 %>%
  group_by (Sample,Age, Row)%>%
  summarise_if(is.numeric, funs (mean(., na.rm=TRUE)))

df2_20 <- df2_c %>% filter(Depth=="20") # separarlo en 0-20
df2_20$Rep <-NULL
df2_20$Depth <-NULL

df2_40 <- df2_c %>% filter(Depth=="40") # separarlo en 20-40
df2_40$Rep <-NULL
df2_40$Depth <-NULL

#...BOXPLOTS----
#......Boxplot  tc vs age grouped by positions---
ggplot(df3, aes(x=df3$Age, y=df3$TC, fill=Row, na.rm=TRUE))+ 
  theme(axis.text=element_text(size=12, color="black"), 
        axis.title.x = element_text(size=12 ),
        axis.title.y = element_text(size=12),
        panel.background = element_rect(fill = "white", colour = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"),
        legend.title = element_text(colour="black", size=12 
        ),
        legend.text = element_text(colour="black", size=12 
        ))+
  geom_boxplot() + 
  scale_fill_brewer(palette="BuPu", labels = c("Between drip lines", "Under drip line"))+
  annotate("text",size=6, x=0.8, y=8, label= "a") + 
  annotate("text", size=6, x =1.2, y=10, label = "ab")+
  annotate("text", size=6, x =1.8, y=13, label = "ab")+
  annotate("text",size=6, x=2.2, y=11, label= "a") + 
  annotate("text", size=6, x =2.8, y=13, label ="b")+
  annotate("text", size=6, x =3.2, y=12, label = "a")+
  ylab(expression("Total Carbon (mg"~g^{-1}*")")) +
  labs(fill = "Position",title="Total Carbon accumulation in vineyard - 0 to 20 cm",x="Time of drip irrigation (years)")
