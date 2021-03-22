library(dplyr)
library(ggplot2)
library(tidyverse)
library(readr)

#Duomenu skaitymas ir filtravimas---- 

duomenys <- read.csv("https://github.com/adrkuc/KTU-duomenu-vizualizacija/raw/main/laboratorinis/data/lab_sodra.csv")

duom <-duomenys %>% filter(ecoActCode == 960900)

#1_uzduotis ----

ggplot(duom, aes(x = avgWage))+ geom_histogram(colour = "white", bins=60) +
  ggtitle("Histogram of average wage") + xlab("Average wage") 

#2_uzduotis ----

kompPav<-duom %>% 
  group_by(name) %>% 
  slice_max(avgWage, n=1) %>% 
  ungroup() %>%
  top_n(avgWage, n=5) %>% 
  select(name)

ggplot(duom %>% filter(name %in% kompPav$name), aes(x=month, y=avgWage, col=name))+
  geom_line()+
  geom_point()+
  theme_bw()+ 
  labs(title= "Average wage by month",
       x= "Month", y= "Average Wage") 


#3_uzduotis----

apdraustieji <- duom %>% filter(name %in% kompPav$name) %>%  group_by(name) %>% top_n(numInsured,n=1) %>% distinct(name,numInsured)

ggplot(apdraustieji, aes(x = (reorder(name,-numInsured)), y = numInsured, fill=name))+
  geom_col()+ labs(title= "Number of insured employees", x= "Name of company", y="Number of emplyees") +
  scale_fill_discrete(name= "Company name", labels= c("Biudzetine istaiga PARKAVIMAS KAUNE",  
                                                      "UAB INZINERIJOS SLENIS", "VSI PSICHIKOS SVEIKATOS PERSPEKTYVOS", "VSI GAMTOS ATEITIS", "VSI ILZENBERGO DVARAS IR UKIS")) +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

