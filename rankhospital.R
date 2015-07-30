rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	rawData<-read.csv("outcome-of-care-measures.csv",colClasses = "character", stringsAsFactors = FALSE);

	##Create outcome possible values
	oList<-c("heart attack", "heart failure","pneumonia")
	
	## Check that state and outcome are valid
	if(missing(state) || nrow(subset(rawData, State==state))==0) {
		stop("invalid state");
	}else if(missing(outcome) || !outcome %in% oList){
		stop("invalid outcome");
	}else if(num!="best" && num!="worst" && suppressWarnings(is.na(as.numeric(num)))){
		stop("invalid state");
	}
	
	## Subsetting data & Casting
	hosp<-subset(rawData, State==state, select=c(2,11,17,23))	
	#hosp[,1]<-as.character(hosp[,1])
	suppressWarnings(hosp[,2]<-as.numeric(hosp[,2]))
	suppressWarnings(hosp[,3]<-as.numeric(hosp[,3]))
	suppressWarnings(hosp[,4]<-as.numeric(hosp[,4]))
	names(hosp)<-c(c("Name"),oList)
	
	##Subsetting specific data
	hosp<-subset(hosp, select=c("Name",outcome))
	names(hosp)<-c("Name","Values")
	
	## Exclude hospitals without data
	hosp<-hosp[complete.cases(hosp),]
	
	#1-step: Sort data by value descendant
	#ranking<-unique(sort(hosp$Values, decreasing=FALSE));
	ranking<-sort(hosp$Values, decreasing=FALSE);

	#2-step: Get de n-essim value of ranking (NA if is too much hight)
	if(num=="best") rValue<-ranking[1]
	else if(num=="worst") rValue<-tail(ranking,1)
	else {
		num<-as.numeric(num)
		rValue<-ranking[num]
	}
	
	#3-step: Get name list of elements with value = ranking
	hospList<-sort(hosp$Name[hosp$Values==rValue], decreasing=FALSE);
		
	#4-step: Sort list by name and return the first element 
	## Return hospital name in that state with the given rank
	## 30-day death rate
	hospList[1]

}