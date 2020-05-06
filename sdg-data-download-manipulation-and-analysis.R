#this is the code-only data to the SDG Analysis Tutorial

## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.data<-read.csv("data.csv") #load data 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(sdg.data)     #this command shows the number of rows and columns


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
names(sdg.data)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(sdg.data)      #data structure, such as integers, factors, numerical, or character data


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
 sdg.data$Value<-as.character(sdg.data$Value)
 
 sdg.data$Value<-as.numeric(sdg.data$Value)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(sdg.data$Goal)          # shows elements in top row and their frequency in bottom row


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(sdg.data$Indicator)     # here for each indicator


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(sdg.data$TimePeriod)    # across years


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(dplyr)               # this command is necessary to activate the toolkit

?dplyr                      # opens help file with basic description of package and commands



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.data<- subset(sdg.data, !is.na(Value)) # "!is.na()" translates to "is not not NA"


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.data<- subset(sdg.data, X.Sex.!= "FEMALE" & X.Sex. != "MALE") #we assume that the 4,375 unlabelled rows are for both sexes


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind.sum <- sdg.data %>% group_by(Indicator) %>% summarize_at(vars("TimePeriod", "Value"), list(n = length, mean=mean, min=min,max= max)) 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(ind.sum) #check out the first few rows


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(ind.sum) # down to 150 indicators


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary(ind.sum) #get an overview of the monster we created


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind.sum$dur <- ind.sum$TimePeriod_max - ind.sum$TimePeriod_min  #calculate difference between min and max year

ind.sum$dif<- ind.sum$dur - ind.sum$TimePeriod_n #for the indicators with a single value every year, this should be 0 or -1

table(ind.sum$dif) # for Uganda, 49 indicators are available for most of the covered period (dif == -1)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
ind.sum.cont<-subset(ind.sum, dif== -1) # we create a smaller dataframe with "clean" indicators

ind.sum.cont.5<- subset(ind.sum.cont, dur >= 5) # exclude indicators with less than 5 years of data


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
dim(ind.sum.cont.5)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.data2<- subset(sdg.data, Indicator %in% ind.sum.cont.5$Indicator)  # the command %in% stands for "member of the group"

write.csv(sdg.data2, "Uganda_sdg_clean") # having saved versions of data sets can be helpful, e.g. when sharing with collaborators


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(ind.sum.cont.5$Indicator)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
test<- subset(sdg.data2, Indicator=="3.1.1")   #the indicator number is in quotation marks, because the values are stored as factors, not numbers

test <- test[,c("Goal", "Target", "Indicator", "SeriesDescription", "GeoAreaName", "TimePeriod", "Value", "Units", "Source")]

print(test)



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(test$TimePeriod, test$Value, ylab="mortality per 100k births", xlab="year", main="Maternal mortality ratio in Uganda")
lines(test$TimePeriod, test$Value)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
test2<- subset(sdg.data2, Indicator=="5.5.1")   #the indicator number is in quotation marks, because the values are stored as factors, not numbers

test2 <- test2[,c("Goal", "Target", "Indicator", "SeriesDescription", "GeoAreaName", "TimePeriod", "Value", "Units", "Source")]

print(test2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
plot(test2$TimePeriod, test2$Value, ylab="number of women in parliament", xlab="year", main="Female members of parliament in Uganda")
lines(test2$TimePeriod, test2$Value)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
test<-test[,c("Value", "TimePeriod")] 
test$s.mat.mort <- scale (test$Value, center=T) #this standardizes the data by centering it on zero and dividing it by 2 standard deviations
test$s.mat.mort <- test$s.mat.mort * -1  #this inverts the standardized data
plot(s.mat.mort~Value, test, main="indicator 3.1.1 scaled") 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
test2<-test2[,c("Value", "TimePeriod")]
test2$s.women.parl <- scale (test2$Value, center=T)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
test.comp<-merge(test, test2, by="TimePeriod") #this combines the two data frames by matching years


plot(s.mat.mort~s.women.parl, test.comp, xlab="maternal mortality", ylab="women in parliament", main="SDG 5.5.1 vs SDG 3.1.1")
abline(lm(s.mat.mort~s.women.parl, test.comp))



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
cor.test(test.comp$s.mat.mort, test.comp$s.women.parl, method="pearson")



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(ncf)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
?cor2()


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
str(sdg.data2)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.data2$Indicator<-as.character(sdg.data2$Indicator) #we are converting the factor to character strings
sdg.data2$Indicator<-factor(sdg.data2$Indicator) #and back
table(sdg.data2$Indicator)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
table(sdg.data2$Indicator, sdg.data2$TimePeriod)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg3<-sdg.data2[-which(sdg.data2$Indicator=="6.b.1"),]
dim(sdg3) #down to 449 rows


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg3<-sdg3[,c("Goal", "Target", "Indicator", "SeriesDescription", "GeoAreaName", "TimePeriod", "Value", "Units")]
str(sdg3)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg3.sum<- sdg3 %>% group_by(Indicator) %>% filter(row_number()==1)
sdg3.sum #when you run the command in your own code, you will see all the data


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg3.sum$SeriesDescription


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg3.sum$direction<-c(1,-1, -1, -1, 1,1,1,1,1,1,1,1,1,1,1,1,1,-1,1,1,1,-1, -1, 1)#some are ambiguous, here your detective work is important!


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg3$s.value<-scale(sdg3$Value, center=T)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(tidyr)
sdg.long<-sdg3[,c("Indicator", "TimePeriod", "s.value")] #select key columns
sdg.wide<-spread(sdg.long, TimePeriod, s.value) #convert from long to wide format
head(sdg.wide) #check on the result


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.wide<-merge(sdg.wide, sdg3.sum, by="Indicator")
sdg.wide[,2:22]<-sdg.wide[,2:22]*sdg.wide$direction#this command tells R to take columns 2 to 22 and multiply the content by the "direction" column
head(sdg.wide)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.mat<-as.matrix(sdg.wide[,2:22])
str(sdg.mat) #looks like we almost have the right format, we only need to transpose the matrix
sdg.mat<-t(sdg.mat)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.correlations<-cor2(sdg.mat, y=NULL, circ=F) 


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
rownames(sdg.correlations)<-sdg.wide$Indicator
colnames(sdg.correlations)<-sdg.wide$Indicator
sdg.correlations<-round(sdg.correlations,2)
write.csv(sdg.correlations, "correlation_matrix_Uganda.csv")
sdg.correlations


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.wide[sdg.wide$Indicator=="9.b.1",]


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.correlations<-sdg.correlations[-c(18,25),]  #first get rid of rows
sdg.correlations<-sdg.correlations[,-c(18,25)]  #now columns



## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
sdg.correlations[is.na(sdg.correlations)]<-0


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
hist(sdg.correlations, freq=F, xlab="correlation coefficients", main="")


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(corrplot)
heatmap(sdg.correlations, Rowv=NULL, Colv=NULL)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
heatmap(sdg.correlations, Rowv=NA, Colv=NA)


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------
corrplot(sdg.correlations)

