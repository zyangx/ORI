plot2Dscatter <- function(coord.m, group.v, color.v=c()){
    library(RColorBrewer)
    library(ggpubr)

    data.df <- as.data.frame(t(coord.m[1:2,]))
    data.df$Sample <- group.v
    colnames(data.df) <- c("PC1", "PC2", "Sample")
    if( length(color.v) > 0){
       myPalette = color.v
    }else{
      groupNum <- length(unique(group.v))
      myPalette = brewer.pal(groupNum,"Set1")
    }

    p <- ggscatter(data.df,  x = "PC1", y = "PC2", color = "Sample", shape = "Sample",  palette = myPalette, size = 1.2,  ellipse = F, ellipse.level = 0.99, ellipse.type = "norm", main="ORI", xlab="Dim-1", ylab="Dim-2" , xlim=c(-0.025, 0.025), ylim=c(0, 0.08)  ) + theme_light() 
    p <- p + theme(axis.title.x=element_text(size=rel(0.75)), axis.title.y=element_text(size=rel(0.75)), axis.text.x=element_text(size=rel(0.75)), axis.text.y=element_text(size=rel(0.75)), plot.title = element_text(size=rel(0.85), hjust = 0.5), legend.title=element_text(size=rel(0.6)), legend.text=element_text(size=rel(0.5)))
    p <- p + geom_vline(xintercept = 0, linetype="dashed") + geom_hline(yintercept = 0.04,linetype="dashed")
    p
}


