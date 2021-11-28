### ORI
Package: ORI
Title: A Universal Coordinate System Presenting Transcriptional State of Saccharomyces cerevisiae
Version: 0.0.0.9000
Authors: Zhen Yang (Email: zhenyang@fudan.edu.cn);  Yungang He (Email: heyungang@fudan.edu.cn)
Description: With understand of freedom of gene expression, we constructed a coordinate system to point expression state of S. cerevisiae in a high dimensional space. When the coordinate system was applied as bases of transcriptional state to represent experimental expression data, detailed expression difference between both replications and different experiments can be easily identified and visualized. 
Depends: R (>= 3.5.2)
License: GNU General Public License(GPL)
Encoding: UTF-8
LazyData: true

### Example

##load the example data and code
library("impute")
library("RColorBrewer")
library("ggpubr")
library("wordcloud")

load("../data/U4529d3.rda")
load("../data/S450.rda")
load("../data/GOstat.rda")
load("../data/testdata2.Rdata")

source("./coorTrans.R")
source("./getDiffDim.R")
source("./plotParaC.R")
source("./plot2Dscatter.R")
source("./plotGOterm.R")

# get New Dim(coordinates) for the Input data
newCoor.m <- coorTrans(expSel.m)

# get Differential Dims for the input data
DiffDim.o <- getDiffDim(newCoor.m, groups.v)

# plot  parallel coordinate for different samples
plotParaC(newCoor.m, groups.v)

# plot 2D-scatter for different samples
plot2Dscatter(newCoor.m, groups.v)

# plot wordcloud of GO terms for the specific Dim
plotGOterm(1, 20)


## scripts/code
Scripts and test code are available in the R subfolder

## data
The real example data is available in the data subfolder
