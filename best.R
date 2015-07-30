best <- function(state, outcome) {
	## Read outcome data
	rawData<-read.csv("outcome-of-care-measures.csv",colClasses = "character", stringsAsFactors = FALSE);

	##Create outcome possible values
	oList<-c("heart attack", "heart failure","pneumonia")
	
	## Check that state and outcome are valid
	if(missing(state) || nrow(subset(rawData, State==state))==0) {
		stop("invalid state");
	}else if(missing(outcome) || !outcome %in% oList){
		stop("invalid outcome");
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
	
	minValue<-min(hosp$Values)
	
	##Sort result alphabetical
	ranking<-sort(hosp$Name[hosp$Values==minValue], decreasing=FALSE);	
	#ranking<-as.character(ranking)
	
	## Return hospital name in that state with lowest 30-day death
	## rate
	ranking[1]

}