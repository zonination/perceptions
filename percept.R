#Import files, load plot and data packages, fire up the number machine.
# setwd("~/Dropbox/R/Perceptions of Probability")
probly <- read.csv("probly.csv", stringsAsFactors=FALSE)
numberly <- read.csv("numberly.csv", stringsAsFactors=FALSE)
library(tidyverse)
library(ggjoy)
library(scales)

#Melt data into column format.
numberly <- gather(numberly, "variable", "value", 1:10)
numberly$variable <- gsub("[.]"," ",numberly$variable)
probly <- gather(probly, "variable", "value", 1:17)
probly$variable <- gsub("[.]"," ",probly$variable)
probly$value<-probly$value/100 # convert to %

#Order in the court!
probly$variable <- factor(probly$variable,
                          c("Chances Are Slight",
                            "Highly Unlikely",
                            "Almost No Chance",
                            "Little Chance",
                            "Probably Not",
                            "Unlikely",
                            "Improbable",
                            "We Doubt",
                            "About Even",
                            "Better Than Even",
                            "Probably",
                            "We Believe",
                            "Likely",
                            "Probable",
                            "Very Good Chance",
                            "Highly Likely",
                            "Almost Certainly"))
numberly$variable <- factor(numberly$variable, 
                            c("Hundreds of",
                              "Scores of",
                              "Dozens",
                              "Many",
                              "A lot",
                              "Several",
                              "Some",
                              "A few",
                              "A couple",
                              "Fractions of"))

#Modify Theme:
source("ztheme.R")

#Plot probability data
ggplot(probly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=.5)+
  geom_jitter(aes(color=variable),size=3,alpha=.2)+
  scale_y_continuous(breaks=seq(0,1,.1), labels=scales::percent)+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       x="Phrase",
       y="Assigned Probability",
       caption="created by /u/zonination")+
  coord_flip()+
  z_theme()
ggsave("plot1.png", height=8, width=8, dpi=120, type="cairo-png")

#Plot numberly data
ggplot(numberly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=0.5)+
  geom_jitter(aes(color=variable),size=3,alpha=.2)+
  scale_y_log10(labels=trans_format("log10",math_format(10^.x)),
                breaks=10^(-2:6))+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       x="Phrase",
       y="Assigned Number",
       caption="created by /u/zonination")+
  coord_flip()+
  z_theme()
ggsave("plot2.png", height=5, width=8, dpi=120, type="cairo-png")

# Joyplot for probly
ggplot(probly,aes(y=variable,x=value))+
  geom_joy(scale=4, aes(fill=variable), alpha=3/4)+
  scale_x_continuous(breaks=seq(0,1,.1), labels=scales::percent)+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       y="",
       x="Assigned Probability",
       caption="created by /u/zonination")+
  z_theme()
ggsave("joy1.png", height=8, width=8, dpi=120, type="cairo-png")

#Joyplot for numberly
ggplot(numberly,aes(y=variable,x=value))+
  geom_joy(aes(fill=variable, alpha=3/4))+
  scale_x_log10(labels=trans_format("log10",math_format(10^.x)),
                breaks=10^(-2:6))+
  guides(fill=FALSE,color=FALSE)+
  labs(title="Perceptions of Probability",
       x="Assigned Number",
       y="",
       caption="created by /u/zonination")+
  z_theme()
ggsave("joy2.png", height=5, width=8, dpi=120, type="cairo-png")