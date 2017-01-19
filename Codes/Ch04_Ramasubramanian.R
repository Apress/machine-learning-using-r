## ----  warning=FALSE,message=FALSE, fig.width=12, fig.height=9,fig.cap= ,fig.cap = "Image: 4.1: Line chart showing top 10 Countries based on their GDP"----

library(reshape)
library(ggplot2)


GDP <- read.csv("Dataset/WDI/Total GDP 2015 Top 10.csv")
names(GDP) <- c("Country", "2010","2011","2012","2013","2014","2015")

GDP_Long_Format <- melt(GDP, id="Country")
names(GDP_Long_Format) <- c("Country", "Year","GDP_USD_Trillion")

ggplot(GDP_Long_Format, aes(x=Year, y=GDP_USD_Trillion, group=Country)) + 
    geom_line(aes(colour=Country)) + 
    geom_point(aes(colour=Country),size = 5) +
    theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Year") +
    ylab("GDP (in trillion USD)") +
    ggtitle("Gross Domestic Product - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9,fig.cap = "Image: 4.2: Line chart showing top 10 countries based on % contribution to GDP from Agriculture"----

# Agriculture

Agri_GDP <- read.csv("Dataset/WDI/Agriculture - Top 10 Country.csv")

Agri_GDP_Long_Format <- melt(Agri_GDP, id = "Country")
names(Agri_GDP_Long_Format) <- c("Country", "Year", "Agri_Perc")
Agri_GDP_Long_Format$Year <- substr(Agri_GDP_Long_Format$Year, 2,length(Agri_GDP_Long_Format$Year))


ggplot(Agri_GDP_Long_Format, aes(x=Year, y=Agri_Perc, group=Country)) + 
    geom_line(aes(colour=Country)) + 
    geom_point(aes(colour=Country),size = 5) +
    theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Year") +
    ylab("Agriculture % Contribution to GDP") +
    ggtitle("% Contribution to GDP from Agriculture - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.3: Line chart showing top 10 countries based on % contribution to GDP from service sector"----

# Service

Service_GDP <- read.csv("Dataset/WDI/Services - Top 10 Country.csv")

Service_GDP_Long_Format <- melt(Service_GDP, id = "Country")
names(Service_GDP_Long_Format) <- c("Country", "Year", "Service_Perc")
Service_GDP_Long_Format$Year <- substr(Service_GDP_Long_Format$Year, 2,length(Service_GDP_Long_Format$Year))

ggplot(Service_GDP_Long_Format, aes(x=Year, y=Service_Perc, group=Country)) + 
    geom_line(aes(colour=Country)) + 
    geom_point(aes(colour=Country),size = 5) +
    theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Year") +
    ylab("Service sector % Contribution to GDP") +
    ggtitle("% Contribution to GDP from service sector - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.4: Line chart showing top 10 countries based on % contribution to GDP from industry"----
# Industry

Industry_GDP <- read.csv("Dataset/WDI/Industry - Top 10 Country.csv")

Industry_GDP_Long_Format <- melt(Industry_GDP, id = "Country")
names(Industry_GDP_Long_Format) <- c("Country", "Year", "Industry_Perc")
Industry_GDP_Long_Format$Year <- substr(Industry_GDP_Long_Format$Year, 2,length(Industry_GDP_Long_Format$Year))

ggplot(Industry_GDP_Long_Format, aes(x=Year, y=Industry_Perc, group=Country)) + 
    geom_line(aes(colour=Country)) + 
    geom_point(aes(colour=Country),size = 5) +
    theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Year") +
    ylab("Industry % Contribution to GDP") +
    ggtitle("% Contribution to GDP from Industry- Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.5: Stacked column chart showing contribution of various sector to the World GDP"----

library(plyr)

World_Comp_GDP <- read.csv("Dataset/WDI/Wrold GDP and Sector.csv")

World_Comp_GDP_Long_Format <- melt(World_Comp_GDP, id = "Sector")
names(World_Comp_GDP_Long_Format) <- c("Sector", "Year", "USD")

World_Comp_GDP_Long_Format$Year <- substr(World_Comp_GDP_Long_Format$Year, 2,length(World_Comp_GDP_Long_Format$Year))

# calculate midpoints of bars

World_Comp_GDP_Long_Format_Label <- ddply(World_Comp_GDP_Long_Format, .(Year), 
   transform, pos = cumsum(USD) - (0.5 * USD))

ggplot(World_Comp_GDP_Long_Format_Label, aes(x = Year, y = USD, fill = Sector)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = USD, y = pos), size = 3) +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Year") +
  ylab("% of GDP") +
  ggtitle("Contribution of various sector in the World GDP")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.6: Scatterplot showing the relationship Population and GDP for top 10 countries"----

library(reshape2)
library(ggplot2)

GDP_Pop <- read.csv("Dataset/WDI/GDP and Population 2015.csv")

ggplot(GDP_Pop, aes(x=Population_Billion, y=GDP_Trilion_USD))+
      geom_point(aes(color=Country),size = 5) + 
      theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Population ( in Billion)") +
    ylab("GDP (in Trillion US $)") +
    ggtitle("Population Vs GDP - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.7: Boxplot showing the GDP (in Trillion US $) for top 10 countries"----

# GDP

GDP_all <- read.csv("Dataset/WDI/GDP All Year.csv")
GDP_all_Long_Format <- melt(GDP_all, id = "Country")
names(GDP_all_Long_Format) <- c("Country", "Year", "GDP_USD_Trillion")
GDP_all_Long_Format$Year <- substr(GDP_all_Long_Format$Year, 2,length(GDP_all_Long_Format$Year))


ggplot(GDP_all_Long_Format, aes(factor(Country), GDP_USD_Trillion)) +
  geom_boxplot(aes(fill = factor(Country)))+
      theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Country") +
    ylab("GDP (in Trillion US $)") +
    ggtitle("GDP (in Trillion US $): Boxplot - Top 10 Countries")

## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.8: Boxplot showing the Population (in Billion) for top 10 countries"----

# Population

Population_all <- read.csv("Dataset/WDI/Population All Year.csv")
Population_all_Long_Format <- melt(Population_all, id = "Country")
names(Population_all_Long_Format) <- c("Country", "Year", "Pop_Billion")
Population_all_Long_Format$Year <- substr(Population_all_Long_Format$Year, 2,length(Population_all_Long_Format$Year))


ggplot(Population_all_Long_Format, aes(factor(Country), Pop_Billion)) +
  geom_boxplot(aes(fill = factor(Country))) +
      theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Country") +
    ylab("Population (in Billion)") +
    ggtitle("Population (in Billion): Boxplot - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.9: Histogram and Density Plot showing GDP and Population for developed and developing country"----

# Population

Population_all <- read.csv("Dataset/WDI/Population All Year.csv")
Population_all_Long_Format <- melt(Population_all, id = "Country")
names(Population_all_Long_Format) <- c("Country", "Year", "Pop_Billion")
Population_all_Long_Format$Year <- substr(Population_all_Long_Format$Year, 2,length(Population_all_Long_Format$Year))

#Developed Country

Population_Developed <- Population_all_Long_Format[!(Population_all_Long_Format$Country %in% c('India','China','Australia','Brazil','Canada','France','United States')),]

ggplot(Population_Developed, aes(Pop_Billion, fill = Country)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..),col="black") + 
  theme(legend.title=element_text(family="Times",size=20),
  legend.text=element_text(family="Times",face = "italic",size=15),
  plot.title=element_text(family="Times", face="bold", size=20),
  axis.title.x=element_text(family="Times", face="bold", size=12),
  axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population (in Billion)") +
  ylab("Frequency") +
  ggtitle("Population (in Billion): Histogram")


ggplot(Population_Developed, aes(Pop_Billion, fill = Country)) + 
  geom_density(alpha = 0.2, col="black") +
  theme(legend.title=element_text(family="Times",size=20),
  legend.text=element_text(family="Times",face = "italic",size=15),
  plot.title=element_text(family="Times", face="bold", size=20),
  axis.title.x=element_text(family="Times", face="bold", size=12),
  axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population (in Billion)") +
  ylab("Frequency") +
  ggtitle("Population (in Billion): Density")


#Developing Country

Population_Developing <- Population_all_Long_Format[Population_all_Long_Format$Country %in% c('India','China'),]

#Histogram

ggplot(Population_Developing, aes(Pop_Billion, fill = Country)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..),col="black") +
  theme(legend.title=element_text(family="Times",size=20),
  legend.text=element_text(family="Times",face = "italic",size=15),
  plot.title=element_text(family="Times", face="bold", size=20),
  axis.title.x=element_text(family="Times", face="bold", size=12),
  axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population (in Billion)") +
  ylab("Frequency") +
  ggtitle("Population (in Billion): Histogram")

#Density

ggplot(Population_Developing, aes(Pop_Billion, fill = Country)) + 
  geom_density(alpha = 0.2, col="black") +
  theme(legend.title=element_text(family="Times",size=20),
  legend.text=element_text(family="Times",face = "italic",size=15),
  plot.title=element_text(family="Times", face="bold", size=20),
  axis.title.x=element_text(family="Times", face="bold", size=12),
  axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Population (in Billion)") +
  ylab("Frequency") +
  ggtitle("Population (in Billion): Density Plot")



## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.10: Line chart showing top 10 Countries based on their Working Age Ratio"----

library(reshape2)
library(ggplot2)

Population_Working_Age <- read.csv("Dataset/WDI/Age dependency ratio - Top 10 Country.csv")

Population_Working_Age_Long_Format <- melt(Population_Working_Age, id = "Country")
names(Population_Working_Age_Long_Format) <- c("Country", "Year", "Wrk_Age_Ratio")
Population_Working_Age_Long_Format$Year <- substr(Population_Working_Age_Long_Format$Year, 2,length(Population_Working_Age_Long_Format$Year))


ggplot(Population_Working_Age_Long_Format, aes(x=Year, y=Wrk_Age_Ratio, group=Country)) + 
    geom_line(aes(colour=Country)) + 
    geom_point(aes(colour=Country),size = 5) +
    theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Year") +
    ylab("Working age Ratio") +
    ggtitle("Working age Ratio - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.11: Stacked bar chart showing constituent of different age group as a % of total population"----

library(reshape2)
library(ggplot2)
library(plyr)

Population_Age <- read.csv("Dataset/WDI/Population Ages - All Age - Top 10 Country.csv")

Population_Age_Long_Format <- melt(Population_Age, id = "Country")
names(Population_Age_Long_Format) <- c("Country", "Age_Group", "Age_Perc")
Population_Age_Long_Format$Age_Group <- substr(Population_Age_Long_Format$Age_Group, 2,length(Population_Age_Long_Format$Age_Group))


# calculate midpoints of bars

Population_Age_Long_Format_Label <- ddply(Population_Age_Long_Format, .(Country), 
   transform, pos = cumsum(Age_Perc) - (0.5 * Age_Perc))

ggplot(Population_Age_Long_Format_Label, aes(x = Country, y = Age_Perc, fill = Age_Group)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Age_Perc, y = pos), size = 3) +
  theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
  xlab("Country") +
  ylab("% of Total Population") +
  ggtitle("Age Group - % of Total Population - Top 10 Country")



## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.12: Line chart showing top 10 countries and their annual % population growth"----

library(reshape2)
library(ggplot2)

Population_Growth <- read.csv("Dataset/WDI/Population growth (annual %) - Top 10 Country.csv")

Population_Growth_Long_Format <- melt(Population_Growth, id = "Country")
names(Population_Growth_Long_Format) <- c("Country", "Year", "Annual_Pop_Growth")
Population_Growth_Long_Format$Year <- substr(Population_Growth_Long_Format$Year, 2,length(Population_Growth_Long_Format$Year))


ggplot(Population_Growth_Long_Format, aes(x=Year, y=Annual_Pop_Growth, group=Country)) + 
    geom_line(aes(colour=Country)) + 
    geom_point(aes(colour=Country),size = 5) +
    theme(legend.title=element_text(family="Times",size=20),
        legend.text=element_text(family="Times",face = "italic",size=15),
        plot.title=element_text(family="Times", face="bold", size=20),
        axis.title.x=element_text(family="Times", face="bold", size=12),
        axis.title.y=element_text(family="Times", face="bold", size=12)) +
    xlab("Year") +
    ylab("Annual % Population Growth") +
    ggtitle("Annual % Population Growth - Top 10 Countries")


## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=5, fig.cap = "Image: 4.13: Pie chart showing percentage share of each sector by consumption segment in India"----

# India

library(reshape2)
library(ggplot2)

GCD_India <- read.csv("Dataset/WDI/India - USD - Percentage.csv")

GCD_India_Long_Format <- melt(GCD_India, id = "Sector")
names(GCD_India_Long_Format) <- c("Sector", "Income_Group","Perc_Cont") 

ggplot(data=GCD_India_Long_Format, aes(x=factor(1), fill = factor(Sector))) +
geom_bar(aes(weight = Perc_Cont), width=1) +
  coord_polar(theta="y", start = 0) +
  facet_grid(facets=. ~ Income_Group) +
  scale_fill_brewer(palette="Set3") +
xlab('') +
ylab('') +
labs(fill='Sector') +
ggtitle("India - Percentage share of each sector by Consumption Segment")

## ----  warning=FALSE,message=FALSE, fig.width=15, fig.height=5, fig.cap = "Image: 4.14: Pie chart showing percentage share of each sector by consumption segment in China"----
# China

library(reshape2)
library(ggplot2)

GCD_China <- read.csv("Dataset/WDI/China - USD - Percentage.csv")

GCD_China_Long_Format <- melt(GCD_China, id = "Sector")
names(GCD_China_Long_Format) <- c("Sector", "Income_Group","Perc_Cont") 

ggplot(data=GCD_China_Long_Format, aes(x=factor(1), fill = factor(Sector))) +
geom_bar(aes(weight = Perc_Cont), width=1) +
  coord_polar(theta="y", start = 0) +
  facet_grid(facets=. ~ Income_Group) +
  scale_fill_brewer(palette="Set3") +
xlab('') +
ylab('') +
labs(fill='Sector') +
ggtitle("China - Percentage share of each sector by Consumption Segment")



## ----  warning=FALSE,message=FALSE, fig.cap = "Image: 4.15: Plot showing correlation between various world developement indicators"----

library(corrplot)
library(reshape2)
library(ggplot2)

correlation_world <- read.csv("Dataset/WDI/Correlation Data.csv")

corrplot(cor(correlation_world[,2:6],method = "pearson"),diag = FALSE,
         method = "ellipse",
         tl.cex = 0.7, tl.col = "black", cl.ratio = 0.2
         )

## ---- warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.16: Heatmap between region and there various world developement indicators"----

library(corrplot)
library(reshape2)
library(ggplot2)
library(scales)

#Heat Maps

bc <- read.csv("Dataset/WDI/Region Wise Data.csv")

bc_long_form <- melt(bc, id = c("Region","Indicator"))
names(bc_long_form) <- c("Region","Indicator","Year", "Inc_Value")
bc_long_form$Year <- substr(bc_long_form$Year, 2,length(bc_long_form$Year))


bc_long_form_rs <- ddply(bc_long_form, .(Indicator), transform ,rescale = rescale(Inc_Value))

ggplot(bc_long_form_rs, aes(Indicator, Region)) + geom_tile(aes(fill = rescale),colour = "white") + scale_fill_gradient(low = "white",high = "steelblue") +
  theme_grey(base_size = 11) + scale_x_discrete(expand = c(0, 0)) +
     scale_y_discrete(expand = c(0, 0)) + 
    theme( 
      axis.text.x = element_text(size = 15 * 0.8, angle = 330, hjust = 0, colour = "black",face="bold"),
      axis.text.y = element_text(size = 15 * 0.8, colour = "black",face="bold"))+
    ggtitle("Heatmap - Region Vs World Development Indicators") +
theme(text=element_text(size=12),
        title=element_text(size=14,face="bold"))

## ---- warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.17: Bubble chart showing GDP Per Captita Vs Life Expectency"----

library(corrplot)
library(reshape2)
library(ggplot2)
library(scales)

#Bubble chart

bc <- read.delim("Dataset/WDI/BubbleChart_GapMInderData.txt")
bc_clean <- droplevels(subset(bc, continent != "Oceania"))
str(bc_clean)

bc_clean_subset <- subset(bc_clean, year == 2007)
bc_clean_subset$year = as.factor(bc_clean_subset$year)

ggplot(bc_clean_subset, aes(x = gdpPercap, y = lifeExp)) + scale_x_log10() +
    geom_point(aes(size = sqrt(pop/pi)), pch = 21, show.legend = FALSE) +
    scale_size_continuous(range=c(1,40)) + 
    facet_wrap(~ continent) +
    aes(fill = continent) +
    scale_fill_manual(values = c("#FAB25B", "#276419", "#529624", "#C6E79C")) +
    xlab("GDP Per Capita(in US $)")+
    ylab("Life Expectancy(in years)")+
    ggtitle("Bubble Chart - GDP Per Captita Vs Life Expectency") +
    theme(text=element_text(size=12),
    title=element_text(size=14,face="bold"))


## ---- warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.17: Bubble chart showing GDP Per Captita Vs Life Expectency for four Countries"----

library(corrplot)
library(reshape2)
library(ggplot2)
library(scales)

bc <- read.csv("Dataset/WDI/Bubble Chart.csv")

ggplot(bc, aes(x = GDPPerCapita, y = LifeExpectancy)) + scale_x_log10() +
          geom_point(aes(size = sqrt(Population/pi)), pch = 21, show.legend = FALSE) +
            scale_size_continuous(range=c(1,40)) + 
              facet_wrap(~ Country) +
                aes(fill = Country) +
                xlab("GDP Per Capita(in US $)")+
                ylab("Life Expectancy(in years)")+
                ggtitle("Bubble Chart - GDP Per Captita Vs Life Expectency - Four Countries") +
theme(text=element_text(size=12),
        title=element_text(size=14,face="bold"))



## ---- warning=FALSE,message=FALSE, fig.width=15, fig.height=9, fig.cap = "Image: 4.18: Bubble chart showing Fertility rate Vs Life Expectency"----

library(corrplot)
library(reshape2)
library(ggplot2)
library("scales")

bc <- read.csv("Dataset/WDI/Bubble Chart.csv")

ggplot(bc, aes(y = FertilityRate, x = LifeExpectancy)) + scale_x_log10() +
          geom_point(aes(size = sqrt(Population/pi)), pch = 21, show.legend = FALSE) +
            scale_size_continuous(range=c(1,40)) + 
              facet_wrap(~ Country) +
                aes(fill = Country) +
                ylab("Fertility rate, total (births per woman)")+
                xlab("Life Expectancy(in years)")+
                ggtitle("Bubble Chart - Fertility rate Vs Life Expectency") +
theme(text=element_text(size=12),
        title=element_text(size=14,face="bold"))


## ----ch04_Waterfall------------------------------------------------------
#Read the Footfall Data
footfall <- read.csv("Dataset/Waterfall Shop Footfall Data.csv",header = T)

#Display the data for easy read
footfall

#Convert the Months into factors to retain the order in chart
footfall$Month <- factor(footfall$Month)
footfall$Time_Period <- factor(footfall$Time_Period)

#Load waterfall library
library(waterfall)
library(lattice)

#Plot using waterfall
waterfallplot(footfall$Net,names.arg=footfall$Month, xlab = "Time Period(Month)",ylab="Footfall",col = footfall$Type,main="Footfall by Month")

waterfallchart(Net~Time_Period, data=footfall,col = footfall$Type,xlab = "Time Period(Month)",ylab="Footfall",main="Footfall by Month")


waterfallchart(Month~Footfall_End_Percent, data=footfall)


## ----ch04_Dendograms-----------------------------------------------------
library(ggplot2)
data(iris)
# prepare hierarchical cluster on iris data
hc <- hclust(dist(iris[,1:2]))

# using dendrogram objects
hcd <- as.dendrogram(hc)

#Zoom Into it at level 1
plot(cut(hcd, h = 1)$upper, main = "Upper tree of cut at h=1")

#lets show how cluster looks looks like if we have cut the tree in three places
clusterCut <- cutree(hc, 3)

iris$clusterCut <- as.factor(clusterCut)

ggplot(iris, aes(Petal.Length, Petal.Width, color = iris$Species)) + 
  geom_point(alpha = 0.4, size = 3.5) + geom_point(col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green'))


## ----ch04_Wordcloud------------------------------------------------------
#Load the text file

job_desc <-readLines("Dataset/wordcloud.txt")

library(tm)
library(SnowballC)
library(wordcloud)

jeopCorpus <- Corpus(VectorSource(job_desc))

jeopCorpus <- tm_map(jeopCorpus, PlainTextDocument)
#jeopCorpus <- tm_map(jeopCorpus, content_transformer(tolower))
jeopCorpus <- tm_map(jeopCorpus, removePunctuation) 
jeopCorpus <- tm_map(jeopCorpus, removeWords,(c("Data","data","Experi","work","develop","use","will","can","you","busi", stopwords('english'))))
jeopCorpus <- tm_map(jeopCorpus, stemDocument)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(146)
wordcloud(words = jeopCorpus, scale=c(3,0.5), max.words=100, random.order=FALSE, 
          rot.per=0.10, use.r.layout=FALSE, colors=pal)

## ----ch04_sankeyChart----------------------------------------------------
#Load the data from sankey.csv


sankey_data <- read.csv("Dataset/sankey2.csv",header=T)

library(googleVis)
plot(
  gvisSankey(sankey_data, from="Start", 
             to="End", weight="Weight",
             options=list(
               height=250,
               sankey="{link:{color:{fill:'lightblue'}}}"
             ))
)


## ----ch04_time_series_plot-----------------------------------------------
library(reshape2)
library(ggplot2)
library(ggrepel)

time_series <- read.csv("Dataset/timeseries.csv",header=TRUE);

mdf <- melt(time_series,id.vars="Year");

mdf$Date <- as.Date(mdf$Year,format="%d/%m/%Y");

names(mdf)=c("Year","Country","GDP_Growth","Date");

ggplot(data=mdf,aes(x=Date,y=GDP_Growth)) + geom_line(aes(color=Country),size=1.5) 

#Now lest just see the growth rates for India,US and UK during recession years (2006,2007,2008,2009,2010)


mdf_2 <- mdf[mdf$Country %in% c("India","United.States","United.Kingdom") & (mdf$Date > as.Date("2005-01-01") & mdf$Date < as.Date("2011-01-01")),]

mdf_2$GDP_Growth <- round(mdf_2$GDP_Growth,2)

tp <- ggplot(data=mdf_2,aes(x=Date,y=GDP_Growth)) + geom_line(aes(color=Country),size=1.5)
tp + geom_text_repel(aes(label=GDP_Growth))


## ----ch04_lexis_cohort_plot----------------------------------------------
library(ggplot2)
require(plyr)

cohort <- read.csv("Dataset/cohort.csv",header=TRUE)

names(cohort)

#we need to melt data
cohort.chart <- melt(cohort, id.vars = "Credit_Issued")
colnames(cohort.chart) <- c('Credit_Issued', 'Year_Active', 'Active_Num')

cohort.chart$Credit_Issued <- factor(cohort.chart$Credit_Issued)

#define palette
blues <- colorRampPalette(c('lightblue', 'darkblue'))

#plot data
p <- ggplot(cohort.chart, aes(x=Year_Active, y=Active_Num, group=Credit_Issued))
p + geom_area(aes(fill = Credit_Issued)) +
 scale_fill_manual(values = blues(nrow(cohort))) +
 ggtitle('Active Credit Cards Volume')



## ----ch04_google_maps----------------------------------------------------

crime_data <- read.csv("Dataset/Case_reported_and_value_of_property_taken_away.csv",header=T)


#install.packages("ggmap")
library(ggmap)

#Example
qmap(location = "New Delhi, India")

crime_data$geo_location <- as.character(crime_data$geo_location)

crime_data$robbery = as.numeric(crime_data$robbery)

#lets just see the stats fpr 2010

mydata <- crime_data[crime_data$year == '2010',]

#Summarise the data by state
library(dplyr)
mydata <- summarise(group_by(mydata, geo_location),robbery_count=sum(robbery))

#get Geop code for all the cities

for (i in 1:nrow(mydata)) {
  latlon = geocode(mydata$geo_location[i])
  mydata$lon[i] = as.numeric(latlon[1])
  mydata$lat[i] = as.numeric(latlon[2])
}

head(mydata)
#write the data with geocode for future reference
mydata <- mydata[-8,]
row.names(mydata) <- NULL

write.csv(mydata,"Dataset/Crime Data for 2010 from NCRB with geocodes.csv",row.names = FALSE)

Robbery_By_State = data.frame(mydata$robbery_count, mydata$lon, mydata$lat)

colnames(Robbery_By_State) <- c('robbery','lon','lat')


India_center = as.numeric(geocode("India"))

IndiaMap <- ggmap(get_googlemap(center=India_center, scale=2, zoom=5,maptype = 'terrain'));

circle_scale_amt <- 0.005

IndiaMap + geom_point(data=Robbery_By_State,aes(x=lon,y=lat), col="orange", alpha=0.4, size=Robbery_By_State$robbery*circle_scale_amt) + scale_size_continuous(range=range(mv_num_collisions$robbery))


