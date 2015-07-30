rankall <- function(outcome, num = "best") {
	## Read outcome data
	rawData<-read.csv("outcome-of-care-measures.csv",colClasses = "character", stringsAsFactors = FALSE);
	
	## Check that state and outcome are valid
	##Create outcome possible values
	oList<-c("heart attack", "heart failure","pneumonia")
	if(missing(outcome) || !outcome %in% oList){
		stop("invalid outcome");
	}else if(num!="best" && num!="worst" && suppressWarnings(is.na(as.numeric(num)))){
		stop("invalid num");
	}
	
	## Subsetting data & Casting
	hosp<-subset(rawData, select=c(2,7,11,17,23))	
	suppressWarnings(hosp[,3]<-as.numeric(hosp[,3]))
	suppressWarnings(hosp[,4]<-as.numeric(hosp[,4]))
	suppressWarnings(hosp[,5]<-as.numeric(hosp[,5]))
	names(hosp)<-c(c("hospital","state"),oList)
	
	## Exclude hospitals without data
	hosp<-hosp[complete.cases(hosp),]
	
	##Subsetting specific data
	hosp<-subset(hosp, select=c("hospital","state",outcome))
	names(hosp)<-c("hospital","state","Values")
	
	## For each state, find the hospital of the given rank
	#result <- tapply(hosp,hosp$state,rankState,num,simplyfy=TRUE)
	result <- split(hosp,hosp$state)
	result<- lapply(result,rankState,num)
	result
	
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	### Sort result by state

}

rankState<-function(hList, num = "best"){
	#1-step: Sort data by value descendant
	#ranking<-unique(sort(hosp$Values, decreasing=FALSE));
	ranking<-sort(hList$Values, decreasing=FALSE);

	#2-step: Get de n-essim value of ranking (NA if is too much hight)
	if(num=="best") rValue<-ranking[1]
	else if(num=="worst") rValue<-tail(ranking,1)
	else {
		num<-as.numeric(num)
		rValue<-ranking[num]
	}	
	
	#3-step: Get name list of elements with value = ranking
	rankList<-sort(hList$hospital[hList$Values==rValue], decreasing=FALSE);		
	
	#4-step: Sort list by name and return the first element 
	## Return hospital name in that state with the given rank
	## 30-day death rate
	
	elem<-data.frame(c(rankList[1],head(hList$state,1)), nrow=1, ncol=2)
	#colnames(elem)<-c("hospital","state")
	#elem<-c(elem$hospital)
	#elem<-data.frame()
	#elem$hospital

	
}
