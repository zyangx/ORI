getDiffDim <- function(coord.m, group.v){
    groupNum <- length(unique(group.v))
    if(groupNum == 2){
        stat.m <- matrix(NA, nrow = nrow(coord.m), ncol=3)
        colnames(stat.m) <- c("t-value", "p-value", "adj.p.value")
        rownames(stat.m) <- paste0("Dim", 1:450)
        for (i in 1:nrow(coord.m)){
            t.o <- t.test(coord.m[i,] ~ group.v)
            stat.m[i,1] <- t.o$stat
            stat.m[i,2] <- t.o$p.v
        }
        stat.m[,3] <- p.adjust(stat.m[,2], method = "BH")
        sigDifDim <- rownames(stat.m)[which(stat.m[,3] < 0.05)]
        return(list(sigDifDim,stat.m))
    }else{
        
        stat.m <- matrix(NA, nrow = nrow(coord.m), ncol=3)
        colnames(stat.m) <- c("Stat", "p-value", "adj.p.value")
        rownames(stat.m) <- paste0("Dim", 1:450)
        for (i in 1:nrow(coord.m)){
            t.o <-  kruskal.test(coord.m[i,] ~ as.factor(group.v))
            stat.m[i,1] <- t.o$stat
            stat.m[i,2] <- t.o$p.v
        }
        stat.m[,3] <- p.adjust(stat.m[,2], method = "BH")
        sigDifDim <- rownames(stat.m)[which(stat.m[,3] < 0.05)]
        return(list(sigDifDim,stat.m))

    }
}
