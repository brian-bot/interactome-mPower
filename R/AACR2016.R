library(XLConnect)
library(rjson)
library(stringr)
library(wordcloud)
library(org.Hs.eg.db)
source("code/common.R")
source("code/geoFuncs.R")

options( java.parameters = "-Xmx4g" )
aacr2016 <- readWorksheetFromFile("data/aacr_2016_v6.xlsx",sheet="sheet1")
author2016.LB_CT<- readWorksheetFromFile("data/AM 2016 Interactome Data_LB-CT Abstract Authors.xlsx",sheet="C3FACA87F1984C19B6660D55B28E8DC")

# Not able to load two big excel files at the same time. Converted file to .csv using xlsx2csv 
author2016.csv <- read.csv("data/authorData2016.csv", header=T)
gc()

# remove duplicates
dups <- duplicated(aacr2016$CONTROLNUMBER)
aacr2016 <- aacr2016[!dups,]

# fix city, state, country are all NA
locationNA <- aacr2016[is.na(aacr2016$PRESENTER.CITY) & is.na(aacr2016$PRESENTER.STATE) & is.na(aacr2016$PRESENTER.COUNTRY),]$CONTROLNUMBER
locationFromAuthor <- author2016.csv[author2016.csv$CONTROLNUMBER == locationNA,][1,]
aacr2016[aacr2016$CONTROLNUMBER == locationNA,]$PRESENTER.CITY = as.character(locationFromAuthor$INSTITUTIONCITY)
aacr2016[aacr2016$CONTROLNUMBER == locationNA,]$PRESENTER.STATE = as.character(locationFromAuthor$INSTITUTIONSTATE)
aacr2016[aacr2016$CONTROLNUMBER == locationNA,]$PRESENTER.COUNTRY = as.character(locationFromAuthor$INSTITUTIONCOUNTRYNAME)

# fix the case of category description
aacr2016[aacr2016$Category == "CT Clinical Trials (including combination trials)",]$Category = "CT CLINICAL TRIALS (including combination trials)"

# Get lat, lng of the location
location <- findLatLong(aacr2016$PRESENTER.CITY, aacr2016$PRESENTER.STATE, aacr2016$PRESENTER.COUNTRY)
aacr2016 <- cbind(aacr2016, location)

# Get description of Categrory
aacr2016$CategoryDes <- sapply(aacr2016$Category,function(x){
  sub("(\\s\\(.+)|(\\:.+)", "", sub('.{2,3}\\s{1,3}', '', x))
})

# Trim the category file
aacr2016$category_interactome1_general = str_trim(aacr2016$category_interactome1_general)
aacr2016$category_interactome2_description_small = str_trim(aacr2016$category_interactome2_description_small)
# Make dtm
txt <- prepareText(paste(aacr2016$ABSTRACT.TITLE,
                         aacr2016$AbstractBody,
                         aacr2016$Category,
                         aacr2016$Subcategory,
                         aacr2016$Subclassification,
                         aacr2016$KEYWORD1, 
                         aacr2016$KEYWORD2, 
                         aacr2016$KEYWORD3,
                         aacr2016$KEYWORD4,
                         aacr2016$category_interactome1_general,
                         aacr2016$category_interactome2_description_small,
                         aacr2016$category_interactome3_moa_small))

aacr2016$txt <- prepareText(txt)

dtm <- makeDTM(aacr2016$txt)
d <- proxy::dist(as.matrix(dtm), method = "cosine")
D <- as.matrix(d)

# export abstract text to json 
apply(aacr2016, 1, function(x){
  id <- sub(" ", "",x[["CONTROLNUMBER"]])
  l <- list()
  l[["ID"]] <- id
  l[["institution"]] <- x[["PRESENTER.INSTITUTION"]]
  authors <- author2016.csv[author2016.csv$CONTROLNUMBER==id,]
  l[["authors"]] <- paste(authors$"AUTHORFIRSTNAME", authors$"AUTHORLASTNAME",sep=" ",collapse=", ")
  l[["text"]] <- x[["AbstractBody"]]
  l[["keywords"]] <- paste(x[["KEYWORD1"]],x[["KEYWORD3"]],x[["KEYWORD3"]],x[["KEYWORD4"]],sep=";")
  l[["category"]] <- x[["CategoryDes"]]
  l[["sage"]] <- x[["category_interactome1_general"]]
  l[["moa"]] <- x[['category_interactome3_moa_small']]
  l[["des"]] <- x[['category_interactome2_description_small']]
  fileConn<-file(paste("./json/",id,".json",sep=""))
  writeLines(toJSON(l), fileConn)
  close(fileConn)
  return(NA)
})

df <- aacr2016
rownames(df) <- df$CONTROLNUMBER
S <- 1 - D
ids <- df$CONTROLNUMBER
rownames(S) <- ids
colnames(S) <- ids

# Abstract clustering
aacr2016HC <- cluster_mat(S,"euclidean",'ward.D2')
aacr2016HC.JSON <- HCtoJSON(aacr2016HC)
fileConn<-file("aacr2016_txt.json")
writeLines(toJSON(aacr2016HC.JSON), fileConn)
close(fileConn)

# Location data
aacr2016Geo <- aacr2016[,c("CONTROLNUMBER","PRESENTER.FIRST","PRESENTER.LAST","PRESENTER.INSTITUTION","PRESENTER.CITY","PRESENTER.COUNTRY","ABSTRACT.TITLE","lat","lng","CategoryDes", "category_interactome1_general","category_interactome3_moa_small","category_interactome2_description_small")]
colnames(aacr2016Geo) <- c("name","presenterFirst","presenterLast","institution","city","country","title","lat","lng","category","sage","moa",'des')
write.csv(aacr2016Geo, file = "aacr2016_geo_data.csv")

# AACR catergory clustering
root <- list(name="AACR category",title="")
tmp <- split(df, factor(df$CategoryDes))
categoryCluster(root, tmp, "aacr2016_cat.json")

# # Sage category clustering
# root <- list(name="Sage category",title="")
# tmp <- split(df, factor(df$category_interactome1_general))
# categoryCluster(root, tmp, "aacr2016_sage.json")

# Mechanism of Action clustering
df[is.na(df$category_interactome3_moa_small),]$category_interactome3_moa_small = "ADC"
df[df$category_interactome3_moa_small=="other",]$category_interactome3_moa_small = "Other"
root <- list(name="Sage category",title="")
tmp <- split(df, factor(df$category_interactome3_moa_small))
categoryCluster2(root, tmp, "aacr2016_sage.json")


word.freq <- as.numeric(as.array(slam::rollup(dtm, 1, FUN=function(x) { sum(x > 0)})) )

# word cloud
pdf("wordCloud.pdf"); 
names(word.freq) <- colnames(dtm)
word.freq.s <- sort(word.freq,decreasing=T)
wordcloud(names(word.freq.s)[1:70], word.freq.s[1:70],random.color=FALSE, colors=brewer.pal(9,"BuGn"),random.order=FALSE); 
dev.off()

pdf("locationCloud.pdf")
loc <- aacr2016$PRESENTER.COUNTRY
loc <- loc[!is.na(loc)]
isUs <- loc == "United States"
loc[isUs] <- aacr2016$PRESENTER.STATE[isUs]
tmp <- table(loc)
wordcloud(names(tmp), tmp, min.freq=1,random.color=FALSE,colors=rev(brewer.pal(9,"RdGy")))
dev.off()
