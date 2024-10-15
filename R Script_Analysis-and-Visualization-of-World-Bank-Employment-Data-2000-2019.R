####### Install Tidyverse and Read Data.csv

install.packages("tidyverse")
library(tideverse)
employ<-read.csv("Data.csv", header = TRUE)

########### Wages and Salary 

ggplot(employ, aes(year, wst))  +  geom_bar(stat='identity', aes(fill=country)) + 
  facet_grid(. ~ country) +
  theme_dark() +
  labs(x="Year",y="Wage and Salary Percentage (%)",fill="Country",
       title="Wage and Salary Percentage")+
  scale_x_continuous(breaks = seq(2000, 2010, by = 10)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32)) +
  theme(strip.text.x = element_text(size =30))

######### Self-employmed 

ggplot(employ, aes(year, set))  +  geom_bar(stat='identity', aes(fill=country)) + 
  facet_grid(. ~ country) +
  theme_dark() +
  labs(x="Year",y="Self-employed Percentage (%)",fill="Country",
       title="Self-employed Percentage") +
  scale_x_continuous(breaks = seq(2000, 2010, by = 10)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32)) +
  theme(strip.text.x = element_text(size =30))

############ Employers 

ggplot(employ, aes(x=year, y=et, group=country)) +
  geom_line(aes(colour=country),size=4.5) +
  geom_point() +
  theme_dark() +
  labs(x="Year", y="Employers Percentage (%)",
       title="Employers Percentage",
       colour="Country") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 5)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32))

########## Vulnerable Employment 

ggplot(employ, aes(x=year, y=vet, group=country)) +
  geom_line(aes(colour=country),size=4.5) +
  geom_point() +
  theme_dark() +
  labs(x="Year", y="Vulnerable Employment Percentage (%)",
       title="Vulnerable Employment Percentage",
       colour="Country") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 5)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32))

######### Contributing family workers Devided BY Sex 

ggplot(employ, aes(x=year, y=cfwt, group=country)) +
  geom_line(aes(colour=country),size=4.5) +
  geom_point() +
  theme_dark() +
  labs(x="Year", y="Contributing family workers Total Percentage (%)",
       title="Contributing family workers Total Percentage",
       colour="Country") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 5)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32))


ggplot(employ, aes(x=year, y=cfwm, group=country)) +
  geom_line(aes(colour=country),size=4.5) +
  geom_point() +
  theme_dark() +
  labs(x="Year", y="Contributing family workers(Male) Percentage (%)",
       title="Contributing family workers(Male) Percentage",
       colour="Country") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 5)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32))

ggplot(employ, aes(x=year, y=cfwf, group=country)) +
  geom_line(aes(colour=country),size=4.5) +
  geom_point() +
  theme_dark() +
  labs(x="Year", y="Contributing family workers(Female) Percentage (%)",
       title="Contributing family workers(Female) Percentage",
       colour="Country") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 5)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=29))

######## Install Plotly 

install.packages("plotly")
library(plotly)


#####Install htmlwidgets 

install.packages("htmlwidgets")
library(htmlwidgets)

####### Animation - Employment in Agriculture 

agri <- ggplot(employ, aes(gdp, eiat, color = country,
                           frame = year, ids = country)) +
  geom_point(aes(size=10)) +
  theme_dark() +
  labs(x="GDP Per Person Employed", y="Employment Percentage in Agriculture (%)", 
       title="Employment Rate in Agriculture", fill="Country\n") +
  scale_x_log10()

ggplotly(agri)
agri.plot <-ggplotly(agri)
htmlwidgets::saveWidget(agri.plot , "agri-plot.html") 


###### Animation - Employement in Industry 

indus <- ggplot(employ, aes(gdp, eii, color = country,
                            frame = year, ids = country)) +
  geom_point(aes(size=10)) +
  theme_dark() +
  labs(x="GDP Per Person Employed", y="Employment Percentage in Industry (%)", 
       title="Employment Rate in Industry", fill="Country\n") +
  scale_x_log10()

ggplotly(indus)
indus.plot <-ggplotly(indus)
htmlwidgets::saveWidget(indus.plot , "indus-plot.html") 


###### Animation - Employement in Services 

ser <- ggplot(employ, aes(gdp, eis, color = country,
                          frame = year, ids = country)) +
  geom_point(aes(size=10)) +
  theme_dark() +
  labs(x="GDP Per Person Employed", y="Employment Percentage in Services (%)", 
       title="Employment Rate in Services", fill="Country\n") +
  scale_x_log10()

ggplotly(ser)
ser.plot <-ggplotly(ser)
htmlwidgets::saveWidget(ser.plot , "ser-plot.html") 

########## Read Data1.csv

employ1<-read.csv("Data1.csv", header = TRUE)

######### Employment to population Ratio by Sex and Country 

ggplot(employ1, aes(x=year , y=pro15, colour=x15sex)) +
  geom_line(size=2.5) +
  facet_grid(x15sex ~ country) +
  theme_dark() +
  labs(x="Year", y="Employment to population Ratio (%)", colour="Sex", title="Employment to Population Ratio by Country and Sex") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 10)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32)) +
  theme(strip.text.x = element_text(size =30),
        strip.text.y = element_text(size = 30))


############# Young Employment to population Ratio by Sex and Country 

ggplot(employ1, aes(x=year , y=pro24, colour=x24sex)) +
  geom_line(size=2.5) +
  facet_grid(x24sex ~ country) +
  theme_dark() +
  labs(x="Year", y="Young Employment to population Ratio (%)" , colour="Sex", title="15-24 Years Old Employment to Population Ratio by Country and Sex") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 10)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=30)) +
  theme(strip.text.x = element_text(size =30),
        strip.text.y = element_text(size = 30))

######## GDP Per Person Employed 

ggplot(employ, aes(x=year, y=gdp, group=country)) +
  geom_line(aes(colour=country),size=2.5) +
  geom_point() +
  theme_dark() +
  labs(x="Year", y="GDP Per Penson Employed (USD)",
       title="GDP Per Person Employed",
       colour="Country") +
  scale_x_continuous(breaks = seq(2000, 2019, by = 5)) +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30)) +
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=32))


######### Install Treemapify 

install.packages("treemapify")
library(treemapify)

####### Read Data2.csv & Data3.csv

employ2<-read.csv("Data2.csv", header = TRUE)
employ3<-read.csv("Data3.csv", header = TRUE)

########## Distribution of countries by GDP Per Person Employed in the year of 2000 

ggplot(employ2, aes(area = gdp, fill = epr15t, label=country)) +
  geom_treemap() + geom_treemap_text(fontface="bold", colour="white", place="centre" , size=60) +
  labs(title="Distribution of countries by GDP Per Person Employed in the year of 2000",
       fill="Employment\nto\nPopulation Ratio (%)") +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30))


###### Distribution of countries by GDP Per Person Employed in the year of 2019 

ggplot(employ3, aes(area = gdp, fill = epr15t, label=country)) +
  geom_treemap() + geom_treemap_text(fontface="bold", colour="white", place="centre" , size=60) +
  labs(title="Distribution of countries by GDP Per Person Employed in the year of 2019",
       fill="Employment\nto\nPopulation Ratio (%)") +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30))

####### PCA Analysis 

pca<-prcomp(employ[,3:33], scale=TRUE)
employ.pca.scaled<-data.frame(
  country=employ$country,
  year=employ$year,
  PC1=pca$x[,1],
  PC2=pca$x[,2]
)
ggplot(employ.pca.scaled, aes(PC1, PC2, label=country))+ geom_text(aes(colour=country),size=15)+
  labs(title="Scaled PCA of Employment Data") +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30))


pca<-prcomp(employ[,3:33], scale=TRUE)
employ.pca.loading<-data.frame(
  dimensions=colnames(employ)[3:33],
  PC1=pca$rotation[,1],
  PC2=pca$rotation[,2]
)
ggplot(employ.pca.loading, aes(PC1, PC2, label=dimensions))+ geom_text(size=15)+
  labs(title="Loading Plot of Employment Data") +
  theme(plot.title = element_text(size=43)) +
  theme(legend.text = element_text(size=30)) +
  theme(legend.title  = element_text(size=30))
