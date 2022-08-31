### Section 1 ###
# a)
postcode <- read.csv('NIPostcodes.csv',header=F)
print(paste0('The postcode dataframe has ',nrow(postcode),' rows and ',ncol(postcode),' columns.'))
summary(postcode)
head(postcode,10)
# "The postcode data frame has 943034 rows and 15 columns."

# b)
colnames(postcode) <- c('Organisation Name',
                        'Sub-building Name',
                        'Building Name',
                        'Number',
                        'Primary Thorfare',
                        'Alt Thorfare',
                        'Secondary Thorfare',
                        'Locality',
                        'Townland',
                        'Town',
                        'County',
                        'Postcode',
                        'x-coordinates',
                        'y-coordinates',
                        'Primary Key')

head(postcode,10)

# c)
postcode[postcode==''] <- NA

install.packages('DescTools')

library(DescTools)
PlotMiss(postcode[runif(nrow(postcode))>0.95,])
# From the plot and the percentage of missing values listed on the right, we can conclude that 'Organisation Name', 'Sub-building Name', 'Building name', 'Alt Thorfare', 'Secondary Thorfare', and 'Locality' all has percentages of NAs over 90%. Therefore, these columns can be removed from our further analysis. For The rest of the columns, since the number of rows is large, we can take the subset of the dataset that contains no NAs in any of the rest columns, or in those that are important for our further analysis.

# d)
nas1 <- c()
for(i in 1:ncol(postcode)){
  nas1 <- c(nas1,sum(is.na(postcode[,i])))
}
# Based on the justification above, rows that contains NAs in 'Number', 'Town', 'Primary Thorfare', 'Postcode' are all removed. For now, the six columns that contain over 90% NAs will be kept.
postcode.narm <- subset(postcode,!is.na(Number) & !is.na(Town) & !is.na(Postcode) &!is.na(`Primary Thorfare`))
nas2 <- c()
for(i in 1:ncol(postcode.narm)){
  nas2 <- c(nas2,sum(is.na(postcode.narm[,i])))
}
nas <- data.frame(colnames(postcode),nas1,nas2)
nas
# According to the cleaning process above, the numbers of NAs before and after cleaning in all columns are displayed in the dataset above.

# e)
# For consistency, the analysis from here will use the original dataset.
postcode <- postcode[,c(15,1:14)]

# f)
Limavady_data <- postcode[grepl('LIMAVADY',postcode$Town),]
Limavady_data <- Limavady_data[grepl('LIMAVADY',Limavady_data$Townland),]
Limavady_data <- Limavady_data[grepl('LIMAVADY',Limavady_data$Locality),]
nrow(Limavady_data)
head(Limavady_data)
write.csv(Limavady_data,'Limavady.csv')

# g)
write.csv(postcode.narm,'CleanNIPostcodeData.csv')


rm(list=ls())


### Section 2 ###
# a)
for(i in list.files('NI Crime Data')){
  if(i=='2015-01'){
    data <- read.csv(paste0('NI Crime Data/',i,'/',list.files(paste0('NI Crime Data/',i))))
  }else{
    data <- rbind(data,read.csv(paste0('NI Crime Data/',i,'/',list.files(paste0('NI Crime Data/',i)))))
  }
}

write.csv(data,'AllNICrimeData.csv')
nrow(data)
head(data)

# b)
data2 <- data[,!(colnames(data) %in% c('Crime.ID', 'Reported.by', 'Falls.within', 'LSOA.code', 'LSOA.name', 
                 'Last.outcome.category', 'Context'))]
write.csv(data2,'AllNICrimeData.csv')
nrow(data2)
colnames(data2)
summary(data2)
head(data2)

# c)
data2$Crime.type[data2$Crime.type=="Anti-social behaviour"] <- 'ASBO'
data2$Crime.type[data2$Crime.type=="Bicycle theft"] <- 'BITH'
data2$Crime.type[data2$Crime.type=="Burglary"] <- 'BURG'
data2$Crime.type[data2$Crime.type=="Criminal damage and arson"] <- 'CDAR'
data2$Crime.type[data2$Crime.type=="Drugs"] <- 'DRUG'
data2$Crime.type[data2$Crime.type=="Other theft"] <- 'OTTH'
data2$Crime.type[data2$Crime.type=="Possession of weapons"] <- 'POWP' #This category has not been provided with a shortened description.
data2$Crime.type[data2$Crime.type=="Public order"] <- 'PUBO'
data2$Crime.type[data2$Crime.type=="Robbery"] <- 'ROBY'
data2$Crime.type[data2$Crime.type=="Shoplifting"] <- 'SHOP'
data2$Crime.type[data2$Crime.type=="Theft from the person"] <- 'THPR'
data2$Crime.type[data2$Crime.type=="Vehicle crime"] <- 'VECR'
data2$Crime.type[data2$Crime.type=="Violence and sexual offences"] <- 'VISO'
data2$Crime.type[data2$Crime.type=="Other crime"] <- 'OTCR'

write.csv(data2,'AllNICrimeData.csv')

# d)
crime.type <- unique(data2$Crime.type)
crime.count <- c()
for(i in crime.type){
  crime.count <- c(crime.count,sum(data2$Crime.type==i))
}
png('crimefreq.png',width=1600,height=1200,res=150)
par(mar=c(4,6,4,4),mfrow=c(1,1))
barplot(crime.count,names.arg=crime.type,main='Crime Frequency by Crime Type',ylab='Crime type\n',xlab='Frequency',col='darkblue',horiz=T,las=1)
dev.off()
# ASBO has the highest frequency, accounting for 38.1% of all the crimes, followed by VISO (22.2%), CDAR (12.0%) and OTTH (8.5%). THPR, ROBY, PUBO, POWP and BITH rarely occur. 

# e)
data2$Location <- gsub('On or near ','',data2$Location)
data2$Location[data2$Location==''] <- NA
head(data2)

# f)
random_crime_sample <- subset(data2,!is.na(Location))
set.seed(100)
random_crime_sample <- random_crime_sample[sample(1:nrow(random_crime_sample),5000),]

random_crime_sample$Town <- NA
random_crime_sample$Townland <- NA
CleanNIPostcodeData <- read.csv('CleanNIPostcodeData.csv')

CleanNIPostcodeData <- CleanNIPostcodeData[,c(6,10,11)]
CleanNIPostcodeData <- unique(CleanNIPostcodeData)

find_a_town <- function(location){
  town <- CleanNIPostcodeData$Town[tolower(CleanNIPostcodeData$Primary.Thorfare)==tolower(location)][1]
  townland <- CleanNIPostcodeData$Townland[tolower(CleanNIPostcodeData$Primary.Thorfare)==tolower(location)][1]
  return(c(town,townland))
}
# There are an exeption that some locations are matched with multiple towns. This may be due to the fact that street names are not unique. Since the requirement states to only use location variable, this function was designed to return the first matched town. Otherwise, correct coordinates can be used to increase matching precision.
for(i in 1:nrow(random_crime_sample)){
  result <- find_a_town(random_crime_sample$Location[i])
  random_crime_sample$Town[i] <- result[1]
  random_crime_sample$Townland[i] <- result[2]
}

# g)
random_crime_sample$Population <- NA
village <- read.csv('VillageList.csv')
colnames(village)[1] <- 'Name'

add_town_data <- function(town){
  return(village$POPULATION[tolower(village$Name)==tolower(town)][1])
}

for(i in 1:nrow(random_crime_sample)){
  result <- add_town_data(random_crime_sample$Town[i])
  if(length(result)==1){
    random_crime_sample$Population[i] <- result
  }else{
    random_crime_sample$Population[i] <- add_town_data(random_crime_sample$Townland[i])
  }
}

# h)
random_crime_sample <- random_crime_sample[,c(1:6,8)]
colnames(random_crime_sample) <- c('Month','Longitude','Latitude','Location','Crime type','City-Town-Village','Population')
write.csv(random_crime_sample,'random_crime_sample.csv')

# i)
random_crime_sample <- read.csv('random_crime_sample.csv')
random_crime_sample <- subset(random_crime_sample,City.Town.Village %in% c('BELFAST','LONDONDERRY'))

crime.type <- unique(random_crime_sample$Crime.type)
crime.count.Belfast <- c()
crime.count.Derry <- c()
for(i in crime.type){
  crime.count.Belfast <- c(crime.count.Belfast,sum(random_crime_sample$Crime.type==i & random_crime_sample$City.Town.Village=='BELFAST'))
  crime.count.Derry <- c(crime.count.Derry,sum(random_crime_sample$Crime.type==i & random_crime_sample$City.Town.Village=='LONDONDERRY'))
}

png('belfast_derry.png',width=1600,height=2400,res=150)
par(mfrow=c(2,1),mar=c(2,6,1,2))

barplot(crime.count.Belfast,names.arg=crime.type,main='Crime Frequency in Belfast and Derry',xlab='Frequency',ylab='Belfast\n',col='darkblue',horiz=T,las=1)
barplot(crime.count.Derry,names.arg=crime.type,main='',ylab='Derry\n',xlab='Frequency',col='darkblue',horiz=T,las=1)
dev.off()
# The distribution is similar between the two places regarding different types of crimes. Despite that Derry has a lower OTTH rate and a higher CDAR rate. And there are almost no ROBY or THPR in Derry. The result is susceptable to the lower population in Derry.