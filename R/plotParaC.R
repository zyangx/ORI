plotParaC <- function(coord.m, group.v){

#Input matrix, rows are coordinate，coloms are the samples
#The input data be normalized 

source("polar2cart.R")

input.m <-as.matrix(coord.m);
temp_dim<-dim(input.m);
tmp.m <- scale(input.m)
inputNorm.m <-matrix(0,nrow=nrow(tmp.m),ncol=ncol(tmp.m));
for (i in 1:ncol(tmp.m)){
    temp <- tmp.m[,i]
    temp <- (temp+3)*(temp>-3)-(temp-3)*(temp>3);
    temp <- temp/6;
    inputNorm.m[,i] <- temp;
};

radius=1;
depth=9; #The number of layers of spiral
decrease_ratio=0.15; # ratio of the decreasing length of each side
theta<-seq(from=0, to=2*pi*depth, length.out=5655)
theta<-rev(sort(theta));

New_X_Y <- polar2cart(0,0,radius*exp(decrease_ratio*theta),theta+3/4*pi,as.deg=FALSE);

step_length<-floor(2*pi);
dime=(length(theta)-628)/step_length;
plot(New_X_Y, type="l", lwd=2, yaxt="n",xaxt="n", xlab="", ylab="");

for (i in 1:dime){
     lines(c(New_X_Y$x[step_length*(i-1)+1], New_X_Y$x[step_length*(i-1)+628]), 
	       c(New_X_Y$y[step_length*(i-1)+1], New_X_Y$y[step_length*(i-1)+628]), col="#E41A1C",type = "l", lwd=1);
		   
	 if(i%%10 == 0 && i <= 100 ){
	     lines(c(New_X_Y$x[step_length*(i-1)+1], New_X_Y$x[step_length*(i-1)+628]), 
	       c(New_X_Y$y[step_length*(i-1)+1], New_X_Y$y[step_length*(i-1)+628]), col="#377EB8",type = "l", lwd=2);
		   
	     text(New_X_Y$x[step_length*(i-1)+1], New_X_Y$y[step_length*(i-1)+1],as.character(i)
		 ,cex = 2, col = NULL, font =0.8 )
	 };
};
lines(New_X_Y,type="l",lwd=2); #Redrawing borders

dime<-min(dime, temp_dim[1]); #The return value cannot exceed the bounds
data_point_X <- matrix(nrow=dime, ncol=temp_dim[2]);
data_point_Y <- matrix(nrow=dime, ncol=temp_dim[2]);

for (i in 1:dime){	   
	data_point_X[i,1:temp_dim[2]]<-c(New_X_Y$x[step_length*(i-1)+1]-
	             (New_X_Y$x[step_length*(i-1)+1]-New_X_Y$x[step_length*(i-1)+628])*inputNorm.m[i,1:temp_dim[2]]);
	data_point_Y[i,1:temp_dim[2]]<-c(New_X_Y$y[step_length*(i-1)+1]-
	             (New_X_Y$y[step_length*(i-1)+1]-New_X_Y$y[step_length*(i-1)+628])*inputNorm.m[i,1:temp_dim[2]]);
};


mycol <-c("#CD0000","#3A89CC","#769C30","#D99536","#7B0078","#BFBC3B","#6E8B3D","#00688B","#C10077","#CAAA76","#EEEE00","#458B00","#8B4513","#008B8B","#6E8B3D","#8B7D6B","#7FFF00","#CDBA96","#ADFF2F")

groupNum <- length(unique(group.v))
if(groupNum > 0){
    group <- unique(group.v)
    for(i in 1:ncol(data_point_X)){
       index <- which(group == group.v[i])
       lines(data_point_X[1:450,i], data_point_Y[1:450,i], col=mycol[index] , type = "l", lwd=1.2);
    }
    legend("bottomleft", legend = group, lty=1,col=mycol[1:groupNum])
}else{
    for(i in 1:ncol(data_point_X)){ 
       lines(data_point_X[1:450,i], data_point_Y[1:450,i], col=mycol[i] , type = "l", lwd=1.2);
    }
}

}

