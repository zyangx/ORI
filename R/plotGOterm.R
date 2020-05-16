plotGOterm <- function(DimNum=1, TopTerm=15){
    library(wordcloud)

    GoCoor.m <- coorTrans(GOStat.m)
    wordFreq.v <- sort(GoCoor.m[DimNum,], decreasing=T)

    wordcloud(names(wordFreq.v)[1:TopTerm], wordFreq.v[1:TopTerm],  min.freq = 0, color = brewer.pal(10,"Paired") )

}
