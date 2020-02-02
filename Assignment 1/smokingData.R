
#' [National Youth Tobacco Survey](http://www.cdc.gov/tobacco/data_statistics/surveys/nyts/index.htm)


#+ getData, eval=FALSE, include=FALSE
dataDir = '../data'
zurl = 
		'http://www.cdc.gov/tobacco/data_statistics/surveys/nyts/zip_files/2014dataset-codebook-SAS.zip'
zfile = file.path(dataDir, basename(zurl))
if(!file.exists(zfile))
	download.file(zurl, zfile)

unzip(zfile, exdir=dataDir)
unzip(zfile, list=TRUE)

x = xOrig = haven::read_sas(
		file.path(dataDir, 
				grep("dataset", unzip(zfile, list=TRUE)$Name, value=TRUE)
		)
)

xNames = unlist(lapply(x, attributes))

formats = data.frame(ID=names(x), label = xNames)
rownames(formats) = 1:nrow(formats)
formats$shortLabel = 
		trimws(gsub(
						paste("RECODE:?|^P[[:digit:]]+(D|M):?|[[:punct:]]|identifier|survey conducted$|unique|origin|",
								"when|used|Latin, or Spanish|seriously|think|out of a", sep=""), 
						"", formats$label, ignore.case=TRUE))

formats$shortLabel = gsub("Anyone who live with you", "AWLWY", formats$shortLabel)		
formats$shortLabel = gsub("ever smoked|ever used", "ever", formats$shortLabel, ignore.case=TRUE)		
formats$shortLabel = gsub("current 1 or more days in", "C1MD", formats$shortLabel, ignore.case=TRUE)		
formats$shortLabel = gsub("how get tob prods|where buy tob prods", "how get", formats$shortLabel, ignore.case=TRUE)		
formats$shortLabel = gsub("if 1 of best friends offered", "friend offer", formats$shortLabel, ignore.case=TRUE)		
formats$shortLabel = gsub("how much person harms themselves when they", "harm when", formats$shortLabel, ignore.case=TRUE)		
formats$shortLabel = gsub(" a |[[:space:]]+", " ", formats$shortLabel)

formats$shortLabel = trimws(formats$shortLabel)

formats$colName = trimws(substr(formats$shortLabel, 1,25))

formats$colName = gsub("[[:space:]]+", "_", formats$colName)


names(x) = formats[
		match(names(x), formats$ID)
		,'colName']
		
x$Sex = factor(x$Sex, levels=1:2, labels=c("M","F"))
x$Age = x$Age + 8
x$Race = factor(x$RaceEth_no_mult_grp, levels = 1:6, 
		labels = c('white','black','hispanic','asian','native','pacific'))

Sage = c('Age_smkd_cigar_cigarillo','Age_first_tried_cigt_smkg',
		'Age_an_electronic_cigaret', 'Age_chew_tobacco_snuff')
for(Dage in Sage) {
	x[which(x[[Dage]]==1),Dage] = Inf
	x[[Dage]]= x[[Dage]] + 6
}

toBinary = c('ecigar_r','eslt_r','ecigt_r','epipe_r', 'ebidis_r', 'ehookah_r', 'esnus_r',
		'edissolv_r','eelcigt_r')
toBinary = unique(c(toBinary, gsub("^e", "c", toBinary)))
toBinary = c(toBinary, 'qn44','qn45')

toBinary = formats[match(toBinary, formats$ID), 'colName']

for(D in toBinary) x[[D]] = x[[D]] == 1

smoke = x
smokeFormats = formats


smoke$RuralUrban = factor(
		substr(smoke$Sampling_stratum, 2, 2),
		levels=c('U','R'), labels=c('Urban','Rural'))

smoke$fipsState = substr(smoke$Primary_Sampling_Unit, 1, 2)

library(XML)
theurl <- "https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code"
tables <- readHTMLTable(RCurl::getURL(theurl))
fipsTable = tables[[1]]

smoke$state = factor(as.character(fipsTable[
		match(smoke$fipsState, as.character(fipsTable[['Numeric code']])),
		'Alpha code']))



save(smoke, smokeFormats, file='../data/smoke.RData')

system("scp ../data/smoke.RData darjeeling.pbrown.ca:/var/www/html/teaching/astwo/data")
system("scp smokingData.R darjeeling.pbrown.ca:/var/www/html/teaching/astwo/data")
system(paste("scp '", 
				file.path(dataDir, 
						grep("pdf$", unzip(zfile, list=TRUE)$Name, value=TRUE)
),
				"' darjeeling.pbrown.ca:/var/www/html/teaching/astwo/data", sep=""))

system("scp ../data/smoke.RData englishbreakfast.pbrown.ca:/var/www/html/teaching/astwo/data")
system("scp smokingData.R englishbreakfast.pbrown.ca:/var/www/html/teaching/astwo/data")
system(paste("scp '", 
				file.path(dataDir, 
						grep("pdf$", unzip(zfile, list=TRUE)$Name, value=TRUE)
				),
				"' englishbreakfast.pbrown.ca:/var/www/html/teaching/astwo/data", sep=""))

#'
