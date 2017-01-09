require(rjson)
require(stringr)
require(proxy)
require(wordcloud)
require(synapseClient)
synapseLogin()

source("R/common.R")

## GET THE IDU INFORMATION
tIdu <- synTableQuery("SELECT * FROM syn5586694 WHERE accepted=true")
iduMap <- synDownloadTableColumns(tIdu, "idu")

iduText <- sapply(iduMap, function(x){
  paste(readLines(x, warn=FALSE), collapse=" ")
})

## BUILD UP IDU DATA FRAME
iduInfo <- tIdu@values
rownames(iduInfo) <- as.character(iduInfo$principalId)
iduInfo$iduText <- iduText[ as.character(iduInfo$idu) ]

txt <- prepareText(iduInfo$iduText)
dtm <- makeDTM(txt)
d <- proxy::dist(as.matrix(dtm), method = "cosine")
D <- as.matrix(d)
S <- 1 - D
rownames(S) <- as.character(iduInfo$userName)
colnames(S) <- as.character(iduInfo$userName)

edges <- NULL
for(i in 1:(nrow(S)-1)){
  for(j in (i+1):ncol(S)){
    if(as.numeric(S[i,j])>0.2){
      edges <- rbind(edges, c(rownames(S)[i], colnames(S)[j], S[i,j]))
    }
  }
}
for(k in setdiff(rownames(S), unique(c(edges[, 1], edges[, 2])))){
  edges <- rbind(edges, c(k, NA, NA))
}
edges <- as.data.frame(edges, stringsAsFactors = FALSE)
colnames(edges) <- c('node1', 'node2', 'value')
write.table(edges, '~/mpower-edges.tsv', row.names=FALSE, quote=FALSE, sep="\t", na="")



# # IDU CLUSTERING
# iduHC <- cluster_mat(S,"euclidean",'ward.D2')
# iduHCjson <- HCtoJSON(iduHC)
# 
# writeLines(toJSON(iduHCjson), "~/idu_txt.json")
# 
# 
# ## WORD CLOUD
# word.freq <- as.numeric(as.array(slam::rollup(dtm, 1, FUN=function(x) { sum(x > 0)})) )
# names(word.freq) <- colnames(dtm)
# word.freq.s <- sort(word.freq,decreasing=T)
# 
# pdf("~/idu_wordCloud.pdf")
# wordcloud(names(word.freq.s)[1:70], word.freq.s[1:70],random.color=FALSE, colors=brewer.pal(9,"BuGn"),random.order=FALSE); 
# dev.off()
# 
# 
# require(qgraph)
# qgraph(S, layout='spring', vsize=3)
