#Import files, load plot and data packages, fire up the number machine.
setwd("~/Dropbox/R/Perceptions of Probability")
probly <- read.csv("probly.csv", stringsAsFactors=FALSE)
numberly <- read.csv("numberly.csv", stringsAsFactors=FALSE)
library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)

#Melt data into column format.
numberly <- melt(numberly)
numberly$variable <- gsub("[.]"," ",numberly$variable)
probly <- melt(probly)
probly$variable <- gsub("[.]"," ",probly$variable)

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
z_theme <- function() {
    # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[5]
  color.axis.text = palette[7]
  color.axis.title = palette[7]
  color.title = palette[8]
    # Begin construction of chart
  theme_bw(base_size=9) +
        # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
        # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
        # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
        # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=20, vjust=1.25)) +
    theme(axis.text.x=element_text(size=14,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=14,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=16,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=16,color=color.axis.title, vjust=1.25))
}

#Plot probability data
png(file='plot1.png', width = 800, height = 800)
ggplot(probly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=.5)+
  geom_jitter(aes(color=variable),size=4,alpha=.2)+
  coord_flip()+
  guides(fill=FALSE,color=FALSE)+
  xlab("Phrase")+
  ylab("Assigned Probability (%)")+
  z_theme()+
  scale_y_continuous(breaks=seq(0,100,10))+
  ggtitle("Perceptions of Probability")
dev.off()

#Plot numberly data
png(file='plot2.png', width = 800, height = 500)
ggplot(numberly,aes(variable,value))+
  geom_boxplot(aes(fill=variable),alpha=0.5)+
  geom_jitter(aes(color=variable),size=4,alpha=.2)+
  scale_y_log10(labels=trans_format("log10",math_format(10^.x)),
                breaks=c(.01,.1,1,10,100,1000,10000,100000))+
  guides(fill=FALSE,color=FALSE)+
  xlab("Phrase")+
  z_theme()+
  ylab("Assigned Number")+
  coord_flip()+
  ggtitle("Perceptions of Numbers")
dev.off()