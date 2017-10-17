## ------------------------------------------------------------------------
library(ggplot2)
library(e1071)

Data_House_Worth <-read.csv("C:\\Users\\Karthik\\Dropbox\\Book Writing - Drafts\\Chapter Drafts\\Chap 6 29 Sep\\Dataset\\House Worth Data.csv",header=TRUE);

str(Data_House_Worth)

#remove the extra column that are not required for the model
Data_House_Worth$BasementArea <-NULL

## ------------------------------------------------------------------------

online_cmean <-cmeans(Data_House_Worth[,2:3],3,20,verbose=TRUE,method="ufcl",m=2)  
print(online_cmean)


## ------------------------------------------------------------------------
ggplot(Data_House_Worth, aes(StoreArea, LawnArea, color = HouseNetWorth)) +
geom_point(alpha =0.4, size =3.5) +geom_point(col = online_cmean$cluster) +
scale_color_manual(values =c('black', 'red', 'green'))


