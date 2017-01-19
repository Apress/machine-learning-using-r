## ---- echo = FALSE-------------------------------------------------------

## ------------------------------------------------------------------------
read.csv("employees.csv", header = TRUE, sep = ",")


## ---- message=FALSE, warning=FALSE---------------------------------------
library(xlsx)
read.xlsx("employees.xlsx",sheetName = "Sheet1")

## ---- eval=FALSE---------------------------------------------------------
## 
## <marathon>
##   <athletes>
##     <name>Mike</name>
##     <age>25</age>
##     <awards>
##       Two times world champion. Currently, worlds No. 3
##     </awards>
##     <titles>6</titles>
##   </athletes>
##   <athletes>
##     <name>Usain</name>
##     <age>29</age>
##     <awards>
##       Five times world champion. Currently, worlds No. 1
##     </awards>
##     <titles>17</titles>
##   </athletes>
## </marathon>
## 

## ---- message=FALSE, warning=FALSE---------------------------------------

library(XML)
library(plyr)

xml_data <- xmlToList("marathon.xml")

#Exluding "description" from print
ldply(xml_data, function(x) { data.frame(x[!names(x)=="description"]) } )


## ----header, eval=FALSE--------------------------------------------------
##     <head>
##       <title> Machine Learning with R </title>
##     </head>

## ----headings, eval=FALSE------------------------------------------------
##     <h1> Header </h1>
##     <h2> Headings </h2>
##     <h3> Tables </h3>
##     <h4> Anchors </h4>
##     <h5> Links </h5>

## ----para, eval=FALSE----------------------------------------------------
## 
##     <p> Paragraph 1 </p>
##     <p> Paragraph 2 </p>
## 

## ----table, eval=FALSE---------------------------------------------------
##     <table>
##         <tbody>
##           <thead>
##             <tr> Define a row </tr>
##           </thead>
##         </tbody>
##     </table>
## 
## 

## ----anchor, eval=FALSE--------------------------------------------------
## 
## 
##     <a href="https://www.apress.com/">__ Welcome to Machine Learning with R! </a>
## 

## ---- eval=FALSE---------------------------------------------------------
## 
## <!DOCTYPE html>
## <html>
##   <body>
##   	<h1>Machine Learning with R</h1>
## 		<p>Hope you having fun time reading this book !!</p>
## 		<h1>Chapter 2</h1>
## 		<h2>Data Exploration and Preparation</h2>
## 		<a href="https://www.apress.com/">Apress Website Link</a>
## </body>
## </html>
## 

## ------------------------------------------------------------------------
library(XML)
url <- "html_example.html"
doc <- htmlParse(url)
xpathSApply(doc, "//a/@href")


## ----json, eval=FALSE----------------------------------------------------
## {
##    "data": [
##       {
##          "id": "A1_B1",
##          "from": {
##             "name": "Jerry", "id": "G1"
##          },
##          "message": "Hey! Hope you like the book so far",
##          "actions": [
##             {
##                "name": "Comment",
##                "link": "http://www.facebook.com/A1/posts/B1"
##             },
##             {
##                "name": "Like",
##                "link": "http://www.facebook.com/A1/posts/B1"
##             }
##          ],
##          "type": "status",
##          "created_time": "2016-08-02T00:24:41+0000",
##          "updated_time": "2016-08-02T00:24:41+0000"
##       },
##       {
##          "id": "A2_B2",
##          "from": {
##             "name": "Tom", "id": "G2"
##          },
##          "message": "Yes. Easy to understand book",
##          "actions": [
##             {
##                "name": "Comment",
##                "link": "http://www.facebook.com/A2/posts/B2"
##             },
##             {
##                "name": "Like",
##                "link": "http://www.facebook.com/A2/posts/B2"
##             }
##          ],
##          "type": "status",
##          "created_time": "2016-08-03T21:27:44+0000",
##          "updated_time": "2016-08-03T21:27:44+0000"
##       }
##    ]
## }

## ------------------------------------------------------------------------
library(rjson)
url <- "json_fb.json"
document <- fromJSON(file=url, method='C')
as.data.frame(document)[,1:3]

## ------------------------------------------------------------------------
emp <- read.csv("employees.csv", header = TRUE, sep = ",")
str(emp)


## ------------------------------------------------------------------------

#Manually overriding the naming convention
names(emp) <- c('Code','First Name','Last Name', 'Salary(US Dollar)')

# Look at the data 
emp

# Now lets clean it up using make.names
names(emp) <- make.names(names(emp))

# Look at the data 
emp


## ------------------------------------------------------------------------
#Find duplicates
table(emp$Code)

#Find common names
table(emp$First.Name)


## ---- echo=FALSE---------------------------------------------------------
emp <- read.csv("employees.csv")
emp_qual <- read.csv("employees_qual.csv")

## ------------------------------------------------------------------------
merge(emp, emp_qual, by = "Code")

## ------------------------------------------------------------------------
merge(emp, emp_qual, by = "Code", all.x = TRUE)

## ------------------------------------------------------------------------
merge(emp, emp_qual, by = "Code", all.y = TRUE)

## ------------------------------------------------------------------------
merge(emp, emp_qual, by = "Code", all = TRUE)

## ---- warning=FALSE, message=FALSE---------------------------------------
library(dplyr)

## ------------------------------------------------------------------------
inner_join(emp, emp_qual, by = "Code")

## ------------------------------------------------------------------------
left_join(emp, emp_qual, by = "Code")

## ------------------------------------------------------------------------
right_join(emp, emp_qual, by = "Code")

## ------------------------------------------------------------------------
full_join(emp, emp_qual, by = "Code")

## ------------------------------------------------------------------------

employees_qual <- read.csv("employees_qual.csv")

#Inconsistent
employees_qual

employees_qual$Qual = as.character(employees_qual$Qual)
employees_qual$Qual <- ifelse(employees_qual$Qual %in% c("Phd","phd","PHd"), "PhD", employees_qual$Qual)

#Corrected
employees_qual


## ------------------------------------------------------------------------

emp <- read.csv("employees.csv")
employees_qual <- read.csv("employees_qual.csv")

#Correcting the inconsistency
employees_qual$Qual = as.character(employees_qual$Qual)
employees_qual$Qual <- ifelse(employees_qual$Qual %in% c("Phd","phd","PHd"), "PhD", employees_qual$Qual)

#Store the output from right_join in the variables impute_salary
impute_salary <- right_join(emp, employees_qual, by = "Code")

#Calculate the average salary for each Qualification
ave_age <- ave(impute_salary$Salary.US.Dollar., impute_salary$Qual,
                 FUN = function(x) mean(x, na.rm = TRUE))

#Fill the NAs with the average values
impute_salary$Salary.US.Dollar. <- ifelse(is.na(impute_salary$Salary.US.Dollar.), ave_age, impute_salary$Salary.US.Dollar.)

impute_salary


## ---- warning=FALSE,message=FALSE----------------------------------------

library("lubridate")
date <- as.POSIXct("2016-03-13 09:51:48")
date
with_tz(date, "UTC")


## ------------------------------------------------------------------------

dst_time <- ymd_hms("2010-03-14 01:59:59")
dst_time <- force_tz(dst_time, "America/Chicago")
dst_time


## ------------------------------------------------------------------------
dst_time + dseconds(1)


## ----warning=FALSE, message=FALSE----------------------------------------

library(data.table)
WDI_Data <- fread("WDI_Data.csv", header = TRUE, skip = 333555, select = c(3,40,43))
setnames(WDI_Data, c("Dev_Indicators", "1995","1998"))
WDI_Data <- WDI_Data[c(1,3),]


## ------------------------------------------------------------------------
WDI_Data[,"Dev_Indicators", with = FALSE]


## ------------------------------------------------------------------------
WDI_Data[,2:3, with = FALSE]


## ---- warning=FALSE, message=FALSE---------------------------------------
library(tidyr)
gather(WDI_Data,Year,Value, 2:3)


## ------------------------------------------------------------------------
marathon <- read.csv("marathon.csv")
summary(marathon)
quantile(marathon$Finish_Time, 0.25)


## ------------------------------------------------------------------------

quantile(marathon$Finish_Time, 0.25)


## ------------------------------------------------------------------------

quantile(marathon$Finish_Time, 0.5)

#Another function to calculate median

median(marathon$Finish_Time)


## ------------------------------------------------------------------------

quantile(marathon$Finish_Time, 0.75)


## ------------------------------------------------------------------------

quantile(marathon$Finish_Time, 0.75, names = FALSE) - quantile(marathon$Finish_Time, 0.25, names = FALSE)


## ------------------------------------------------------------------------

mean(marathon$Finish_Time)


## ----fig.width=10, fig.height=5, fig.cap='Frequency plot for Marathon Finish Time'----
plot(marathon$Type, xlab = "Marathoners Type", ylab = "Number of Marathoners")

## ----fig.width=10, fig.height=5, fig.cap='Boxplot of Marathon Finish Time'----
boxplot(Finish_Time ~ Type,data=marathon, main="Marathon Data", xlab="Type of Marathoner", ylab="Finish Time")

## ------------------------------------------------------------------------

mean(marathon$Finish_Time)
var(marathon$Finish_Time)
sd(marathon$Finish_Time)


## ------------------------------------------------------------------------
tapply(marathon$Finish_Time,marathon$Type, mean)
tapply(marathon$Finish_Time,marathon$Type, sd)


## ---- fig.width=10, fig.height=5, fig.cap='Skewness Plots'---------------

library("moments")

par(mfrow=c(1,3), mar=c(5.1,4.1,4.1,1))

# Negative skew
hist(rbeta(10000,2,6), main = "Negative Skew" )
skewness(rbeta(10000,2,6))

# Positive skew
hist(rbeta(10000,6,2), main = "Positive Skew")
skewness(rbeta(10000,6,2))

# Symmetrical
hist(rbeta(10000,6,6), main = "Symmetrical")
skewness(rbeta(10000,6,6))


## ---- fig.width=10, fig.height=5, fig.cap='Marathon Finish Time'---------

hist(marathon$Finish_Time, main = "Marathon Finish Time")
skewness(marathon$Finish_Time) 


## ----fig.width=10, fig.height=5, fig.cap='Kurtosis Plots'----------------

#leptokurtic
set.seed(2)
random_numbers <- rnorm(20000,0,0.5)
plot(density(random_numbers), col = "blue", main = "Kurtosis Plots", lwd=2.5, asp = 4)
kurtosis(random_numbers)


#platykurtic
set.seed(900)
random_numbers <- rnorm(20000,0,0.6)
lines(density(random_numbers), col = "red", lwd=2.5)
kurtosis(random_numbers)


#mesokurtic
set.seed(3000)
random_numbers <- rnorm(20000,0,1)
lines(density(random_numbers), col = "green", lwd=2.5)
kurtosis(random_numbers)

legend(1,0.7, c("leptokurtic", "platykurtic","mesokurtic" ), 
lty=c(1,1),
lwd=c(2.5,2.5),col=c("blue","red","green"))



## ----fig.width=10, fig.height=5, fig.cap='Marathon Finish Time'----------

plot(density(as.numeric(marathon$Finish_Time)), col = "blue", main = "Kurtosis Plots", lwd=2.5, asp = 4)
kurtosis(marathon$Finish_Time)


## ---- warning=FALSE, message=FALSE, cache=TRUE---------------------------

library(data.table)
data <- fread("ccFraud.csv",header=T, verbose = FALSE, showProgress = FALSE)
str(data)


## ---- warning=FALSE, cache = TRUE----------------------------------------

library(data.table)
US_state <- fread("US_State_Code_Mapping.csv",header=T, showProgress = FALSE)
data<-merge(data, US_state, by = 'state')


## ------------------------------------------------------------------------
library(data.table)
Gender_map<-fread("Gender Map.csv",header=T)
data<-merge(data, Gender_map, by = 'gender')


## ------------------------------------------------------------------------

library(data.table)
Credit_line<-fread("credit line map.csv",header=T)
data<-merge(data, Credit_line, by = 'creditLine')


## ---- echo=FALSE---------------------------------------------------------
data$gender<-NULL
data$state<-NULL
data$PostalCode<- NULL              

## ------------------------------------------------------------------------
setnames(data,"custID","CustomerID")
setnames(data,"code","Gender")
setnames(data,"numTrans","DomesTransc")
setnames(data,"numIntlTrans","IntTransc")
setnames(data,"fraudRisk","FraudFlag")
setnames(data,"cardholder","NumOfCards")
setnames(data,"balance","OutsBal")
setnames(data,"StateName","State")

str(data)


## ------------------------------------------------------------------------

summary(data[,c("NumOfCards","OutsBal","DomesTransc",
                "IntTransc"),with = FALSE])


## ------------------------------------------------------------------------
boxplot(I(DomesTransc + IntTransc )  ~ Gender, data = data)
title("Number of Domestic Transaction")

tapply(I(data$DomesTransc + data$IntTransc),data$Gender, median)

tapply(I(data$DomesTransc + data$IntTransc),data$Gender, mean)



## ------------------------------------------------------------------------

table(data$CardType,data$FraudFlag)


## ------------------------------------------------------------------------

table(data$Gender,data$FraudFlag)


