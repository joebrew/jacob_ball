# SET WORKING DIRECTORY ONLY IF OUTSIDE'S JAKE'S RPROJ
if(Sys.info()["user"] == "joebrew"){
  setwd("jacob_ball")
}

rm(list=ls())
require(gdata)
require(plyr)
require(stringr)
require(deSolve)
dat <- read.xls("transmissionHamstersPCRtab.xlsx",sheet="PCR data",header=T)
head(dat)

dat <- dat[,-1] # remove first column
head(dat)

# Look into duplicated rows
length(dat$X.1) == length(unique(dat$X.1)) # see if there are duplicated rows--there are
sum(duplicated(dat$X.1)) #there are 32 duplicated rows

subset(dat,duplicated(dat$X.1)) # duplicated rows are all empty.  The subset function creates a subset (duh)
# remove them
dat <- subset(dat,!duplicated(dat$X.1)) #subset of dat that does not have duplicated X.1
length(dat$X.1) == length(unique(dat$X.1))


names(dat)[1] <- "uniqueSample" # rename first column
head(dat)

# remove final (only NA) row
dat<-dat[-803,]
#dat


dat$transType[grep("A",dat$uniqueSample)]<-"Aer"
dat$transType[grep("F",dat$uniqueSample)]<-"Fom"
dat$transType[grep("C",dat$uniqueSample)]<-"Con"
levels(as.factor(dat$transType))



dat$swabType[grep("oral",dat$uniqueSample)]<-"oral"		
dat$swabType[grep("urine",dat$uniqueSample)]<-"urine"
dat$swabType[grep("fecal",dat$uniqueSample)]<-"fecal"	
dat$swabType[grep("fecal",dat$uniqueSample)]<-"fecal"
levels(as.factor(dat$swabType))
table(dat$swabType)

head(strsplit(as.character(dat$uniqueSample)," ")[1])
splitUniqueSample <- function(vv,browse=F){
	if(browse) browser()
	splitStrings <- strsplit(as.character(vv)," ")
	hamster <- unlist(lapply(splitStrings,function(xx){return(xx[1])}))  # lapply returns a list of the same length as vector specified
	transType <- unlist(lapply(splitStrings,function(xx){return(xx[2])}))
	swabType <- unlist(lapply(splitStrings,function(xx){return(xx[3])}))
	return(data.frame(hamster=hamster,transType=transType,swabType=swabType))	
}


splitUniqueSample(head(dat$uniqueSample))

splitDF <- splitUniqueSample(dat$uniqueSample)

dat <- cbind(dat,splitDF) #combine dat and splitDF
head(dat)
#dat

dat$Ct <- as.numeric(as.character(dat$Ct))
dat$log10TCID50.ml <- as.numeric(as.character(dat$log10TCID50.ml))
dat$copies.5ul <- as.numeric(as.character(dat$copies.5ul))
## REPEAT THIS STEP FOR OTHER NUMERIC VALUES CURRENTLY STORED AS FACTORS, including hamster id--that will have to happen after I convert hamster to hamsterID
colnames(dat)




dat$hamster
dat$hamsterID<-strsplit(as.character(unlist(dat$hamster)),split="D",fixed=T)
dat$hamsterID#this successfully split the text, but it got rid of "D" and not the number


for (ii in 1:length(dat$hamsterID)){  #for loop to make dat$dpi equal to the second column in hamsterID
	dat$dpi[ii]<-dat$hamsterID[[ii]][2]
}
dat$dpi  ## WOOOOOO!  Now just have to get rid of "Aer"
dat$dpi<-gsub("Aer","",dat$dpi) #DONE!!!  :D
dat$dpi

dat$hamsterID<- data.frame(matrix(unlist(str_split(dat$hamster,pattern="D",n=2)),ncol=2,byrow=T))
dat$hamsterID<- dat$hamsterID[,1]
dat$hamsterID   #ERMAGHERD IT WERKDDDDDD!


dat$hamsterID<- data.frame(matrix(unlist(str_split(dat$hamsterID,pattern="H",n=2)),ncol=2,byrow=T))
dat$hamsterID<-dat$hamsterID[,2]
dat$hamsterID <- as.numeric(as.character(dat$hamsterID))
is.numeric(dat$hamsterID)

sort(as.numeric(dat$hamsterID),decreasing=F)



H17<-subset(dat,dat$hamsterID==17)
H17.oral<-subset(H17,swabType=="oral")
H17.oral$hamsterID 
H17.oral$dpi 
print(H17.oral$Ct)

plot(H17.oral$dpi,H17.oral$Ct,type="l",col="blue")  

# MAKE DPI NUMERIC
dat$dpi <- as.numeric(dat$dpi)

# Assignment:   1. CREATE FUNCTION to do this for generic hamsterID
# 							2. USE sapply over values of hamsterID to plot for all

# Create a unique color vector the length 
# of as many hamsters as we've got
my_cols <- rainbow(length(unique(dat$hamsterID)))

# Assign a color to each hamster in a unique column
dat$color <- my_cols[dat$hamsterID]

# Make that color semi-transparent
dat$color <- adjustcolor(dat$color, alpha.f = 0.6)

# create blank plot area of an appropriate size
plot(x = c(0, 11), 
     y = c(20, 42), type = "n",  
     xlab="Days Post Inoculation", ylab="", # label axes
     main="All hamsters in 10^5 qPCR ") #axis limit

# Create plotting function
ci_plot <- function(data = dat,
                    hamsterID = NULL,
                    swabType = "oral",
                    var = "Ct"){
  
  # Subset the data
  new_data <- dat[which(dat$hamsterID == hamsterID &
                          dat$swabType == "oral"),]
  
  # Extract the plotting variable of interest
  new_var <- new_data[,var]
  
  # Assign x, y, and color
  x <- new_data$dpi
  y <- new_var
  color <- new_data$color
  
  # If there is not existing plot, first draw base plot
  if(is.null(dev.list()["RStudioGD"])){
    plot(x = c(0, 11), 
         y = c(20, 42), type = "n",  
         xlab="Days Post Inoculation", ylab="", # label axes
         main="All hamsters in 10^5 qPCR ") #axis limit
  }
  
  # Add lines
  lines(x = x,
        y = y,
        col = color)
}

# Now that you're plotting function is created, you can plot individual hammies
# like so
ci_plot(hamsterID = 1)
ci_plot(hamsterID = 2)
ci_plot(hamsterID = 3)
ci_plot(hamsterID = 4)
ci_plot(hamsterID = 5)


# You could write 48 lines, but that would take a while.
# so if it were me I'd just write a for loop 
# to draw all the lines on the plot
dev.off()

for (i in unique(dat$hamsterID)){
  ci_plot(hamsterID = i)
}

# At the end, go ahead and add a legend
legend_dat <- dat[!duplicated(dat$hamsterID),]
legend_dat <- legend_dat[order(legend_dat$hamsterID),]
legend(x = "topright",
       col = legend_dat$color,
       lty = 1,
       legend = legend_dat$hamsterID,
       cex = 0.5,
       title = "Hamster ID",
       ncol = 2)
rm(legend_dat)

# You can also plot everything by using
# sapply (but I generally find these functions not very readable)
dev.off() # clear graphics window
sapply(X = unique(sort(dat$hamsterID)),
       FUN = function(x){
         ci_plot(data = dat,
                 hamsterID = x,
                 swabType = "oral",
                 var = "Ct")
       })












################################
################################
################################
################################
jcols<-rainbow(16,s=1,alpha=0.4)

ct.plot<-function(data,ham,swab,ct,dpi){
			hamster<-subset(data,ham)
			hamster.oral<-subset(hamster,swab)
			lines(dpi,ct,col=jcols[ham])
		
}



plot(1:11, seq(1,6, length = 11), type = "n",  # create blank plot area of an appropriate size
		 xlab="Days Post Inoculation", ylab="", # label axes
		 ylim = c(0, 6),main="All hamsters in 10^5 qPCR ") #axis limit

ct.plot<-function(data,ham,swab){
	hamster <- data[which(data$hamsterID == ham &
													data$swabType == swab),]
	#hamster<-subset(data,data$hamsterID==ham);hamster
	#hamster.swab<-subset(hamster,dat$swabType==swab)
	#print(hamster.swab)
	lines(1:9,data$Ct,col=jcols[ham])
	### OR:  
	#  lines(data$dpi, data$Ct, col=jcols[ham]
}

ct.plot(dat,17,"oral")  #no lines

sapply(ham==17:35,ct.plot(dat,ham,"oral") )

table(dat$Ct*dat$hamsterID)
dat$hamsterID
example(sapply)




					







