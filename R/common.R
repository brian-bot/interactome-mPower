library(tm)
library(slam)

abstract.stop.words <- c("cancer","express","studi","activ","result",
                         "increas","gene","protein","level","signific",
                         "compar","associ","develop","analysi","use","observ",
                         "demonstr","found","includ","determin","suggest","conclusion",
                         "background","effect","identifi","data","potenti","method","may",
                         "evalu","perform","assess","show","shown","can","howev","one","two","report",
                         "also")

concatenate_and_split_hyphens <- function(x){
  gsub("\\s(\\w+)-(\\w+)\\s"," \\1\\2 \\1 \\2 ",x)
}

removeAloneNumbers <- function(x){
  gsub("\\s\\d+\\s","", x)
}

removepvalue <- function(x){
  gsub("p\\s?[=<>]\\s?[\\.\\d]+"," ",x,perl=TRUE)
}

greekSubs <- 
  list(a="&#945;",
       b="&#946;",
       g="&#947;" ,
       d="&#948;",
       e="&#949;",
       z="&#950;",
       h="&#951;",
       th="&#952;",
       i="&#953;",
       k="&#954;",
       l="&#955;",
       m="&#956;",
       n="&#957;",
       ks="&#958;",
       p="&#960;",
       r="&#961;",
       s="&#962;",
       t="&#964;" ,
       y="&#965;",
       f="&#966;",
       x="&#967;" ,
       ps="&#968;",
       w="&#969;")

replace.greek <- function(x){
  for(i in 1:length(greekSubs)){
    x <- gsub(greekSubs[[i]], names(greekSubs)[i], x)
  }
  return (x)
}

strip.markup <- function(x){
  gsub("</?[A-Za-z]+>|<[A-Za-z]+\\s?/>"," ", x)
}

strip.specialchar <- function(x){
  gsub("&#\\d+;"," ", iconv(x, "latin1", "UTF-8"))
}

prepareText <- function(x){
  concatenate_and_split_hyphens(removepvalue(tolower(strip.specialchar(replace.greek(strip.markup(x))))))
}

makeDTM <- function(textVec){
  corpus <- Corpus(VectorSource(textVec))
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, concatenate_and_split_hyphens)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeAloneNumbers)
  corpus <- tm_map(corpus, stemDocument)
  corpus <- tm_map(corpus, removeWords, abstract.stop.words)
  
  # Added by XG
  corpus <- tm_map(corpus, PlainTextDocument)
  
  dtm <- DocumentTermMatrix(corpus,
                            control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
  word.freq <- as.numeric(as.array(slam::rollup(dtm, 1, FUN=function(x) { sum(x > 0)})) )
  dtm.sub <- dtm[, word.freq > 2]
  return(dtm.sub)
}

# add more common functions -XG
cluster_mat <- function(mat, distance, method, cor_method){
  if(!(method %in% c("ward.D","ward.D2","single", "complete", "average", "mcquitty", "median", "centroid"))){
    stop("clustering method has to one form the list: 'ward.D', 'ward.D2', 'single', 'complete', 'average', 'mcquitty', 'median' or 'centroid'.")
  }
  
  if(!(distance %in% c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) & class(distance) != "dist"){
    print(!(distance %in% c("correlation", "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) | class(distance) != "dist")
    stop("distance has to be a dissimilarity structure as produced by dist or one measure form the list: 'correlation', 'euclidean', 'maximum', 'manhattan', 'canberra', 'binary', 'minkowski'")
  }
  if (distance == "correlation"){
    d <- as.dist(1 - cor(t(mat),method=cor_method))
  }
  else {
    if(class(distance) == "dist") {
      d <- distance
    }
    else{
      d <- dist(mat,method = distance)
    }
  }
  #ward.D2 doesn't work in flashClust
  #return(flashClust(d, method = method)) #hclust replaced by flashClust from WCGNA (much faster than hclust)
  return (hclust(d,method=method))
}

# @param hc is the hcluster of dist(S)
HCtoJSON<-function(hc){
  
  labels<-hc$labels
  merge<-data.frame(hc$merge)
  height<- hc$height
  for (i in (1:nrow(merge))) {
    if (merge[i,1]<0 & merge[i,2]<0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", height=\"",height[i],"\", children=list(list(",listNodeInfo(labels[-merge[i,1]]),"),list(",listNodeInfo(labels[-merge[i,2]]),")))")))
    }else if (merge[i,1]>0 & merge[i,2]<0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", height=\"",height[i],"\", children=list(node", merge[i,1], ", list(",listNodeInfo(labels[-merge[i,2]]),")))")))
    }else if (merge[i,1]<0 & merge[i,2]>0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", height=\"",height[i],"\", children=list(list(",listNodeInfo(labels[-merge[i,1]]),"), node", merge[i,2],"))")))  
    }else if (merge[i,1]>0 & merge[i,2]>0) {
      eval(parse(text=paste0("node", i, "<-list(name=\"node", i, "\", height=\"",height[i],"\", children=list(node",merge[i,1] , ", node" , merge[i,2]," ))")))
    }
  }
  
  eval(parse(text=paste0("JSON<-(node",nrow(merge), ")")))
  
  return(JSON)
}

listNodeInfo <- function(id){
  title <- sub("\"","<q>",df[id,"ABSTRACT.TITLE"])
  title <- sub("\"","</q>",title)
  L <- paste0('name =\"',id,'\",',
              'title=\"',title,'\",',
              'presenterFirst=\"',df[id,"PRESENTER.FIRST"],'\",',
              'presenterLast=\"',df[id,"PRESENTER.LAST"],'\",',
              'presenterInstitution=\"',df[id,"PRESENTER.INSTITUTION"],'\",',
              'presenterCity=\"',df[id,"PRESENTER.CITY"],'\",',
              'presenterCountry=\"',df[id,"PRESENTER.COUNTRY"],'\",',
              'keywords=\"',paste(df[id,"KEYWORD1"],df[id,"KEYWORD2"],df[id,"KEYWORD3"],df[id,"KEYWORD4"],sep=";"),'\",',
              'sage=\"',df[id,"category_interactome1_general"],'\",',
              'moa=\"',df[id,"category_interactome3_moa_small"],'\",',
              'des=\"',df[id,"category_interactome2_description_small"],'\",',
              'category=\"',df[id,"CategoryDes"],'\"')
  return(L)
}

categoryCluster <- function(root, tmp, filename){
  root[["children"]] <- lapply(names(tmp), function(categoryName){
    L <- list(id=categoryName,name=categoryName,presenterLast=categoryName)
    children <- list()
    tbl <- tmp[[categoryName]]
    for(i in 1:nrow(tbl)){
      row <- tbl[i,]
      children[[i]] <- list(id = row[["CONTROLNUMBER"]],
                            name=row[["CONTROLNUMBER"]],
                            title=row[["ABSTRACT.TITLE"]],
                            presenterFirst=row[["PRESENTER.FIRST"]],
                            presenterLast=row[["PRESENTER.LAST"]],
                            presenterInstitution=row[["PRESENTER.INSTITUTION"]],
                            presenterCity=row[["PRESENTER.CITY"]],
                            presenterCountry=row[["PRESENTER.COUNTRY"]],
                            keywords=paste(row[["KEYWORD1"]],row[["KEYWORD3"]],row[["KEYWORD3"]],row[["KEYWORD4"]],sep=";"),
                            category=row[["CategoryDes"]],
                            sage=row[["category_interactome1_general"]],
                            moa=row[['category_interactome3_moa_small']],
                            des=row[['category_interactome2_description_small']])
    }
    L[["children"]] <- children
    return (L)
  })
  
  fileConn<-file(filename)
  writeLines(toJSON(root), fileConn)
  close(fileConn)
}

categoryCluster2 <- function(root, tmp, filename){
  root[["children"]] <- lapply(names(tmp), function(categoryName){
    L <- list(id=categoryName,name=categoryName)
    tbl1 <- tmp[[categoryName]]
    tmp2 <- split(tbl1, factor(tbl1$category_interactome2_description_small))
    L[["children"]] <- lapply(names(tmp2), function(categoryName2){
      L2 <- list(id=paste(categoryName2," ",sep = " "),name=categoryName2)
      children <- list()
      tbl2 <- tmp2[[categoryName2]]
      for(i in 1:nrow(tbl2)){
        row <- tbl2[i,]
        children[[i]] <- list(id = row[["CONTROLNUMBER"]],
                              name=row[["CONTROLNUMBER"]],
                              title=row[["ABSTRACT.TITLE"]],
                              presenterFirst=row[["PRESENTER.FIRST"]],
                              presenterLast=row[["PRESENTER.LAST"]],
                              presenterInstitution=row[["PRESENTER.INSTITUTION"]],
                              presenterCity=row[["PRESENTER.CITY"]],
                              presenterCountry=row[["PRESENTER.COUNTRY"]],
                              keywords=paste(row[["KEYWORD1"]],row[["KEYWORD3"]],row[["KEYWORD3"]],row[["KEYWORD4"]],sep=";"),
                              category=row[["CategoryDes"]],
                              sage=row[["category_interactome1_general"]],
                              moa=row[['category_interactome3_moa_small']],
                              des=row[['category_interactome2_description_small']])
      }
      L2[["children"]] <- children
      return (L2)
    })
    return(L)
  })
  
  fileConn<-file(filename)
  writeLines(toJSON(root), fileConn)
  close(fileConn)
}
