polar2cart<-function(x,y,dist,bearing,as.deg=FALSE){
  ## Translate Polar coordinates into Cartesian coordinates
  ## based on starting location, distance, and bearing
  ## as.deg indicates if the bearing is in degrees (T) or radians (F)
  
  if(as.deg){
    ##if bearing is in degrees, convert to radians
    bearing=bearing*pi/180
  }
  
  newy<-x+dist*sin(bearing)  ##X
  newx<-y+dist*cos(bearing)  ##Y
  return(list("x"=newx,"y"=newy))
}

newcoor<-function(A,b)
{# least squares solutions for Ax=b
  b[is.na(b)]=0; #缺失值补0
  result<-solve(crossprod(A), crossprod(A, b));
  return(result);
}

plotParaC<-function(Input_data){
#Input_data是一个矩阵, 每行是一个坐标，每列是一个样本
#目标是把结果以变形的parallele coordinates 呈现
#本函数仅绘制变形的parallele coordinates
#输出绘制Input_data所需的转化后坐标，以便外部命令绘制并调整线条风格

#数据转化为矩阵，标准化,再转化为百分比(0-1)
#缺失值补0
Input_dataM<-as.matrix(Input_data);
temp_dim<-dim(Input_dataM);
Input_dataM[is.na(Input_dataM)]=0; #缺失值补0
Input_normalization<-matrix(0,nrow=temp_dim[1],ncol=temp_dim[2]);
for (i in 1:temp_dim[2]){
     temp_mean<-mean(Input_dataM[1:temp_dim[1],i]);
     temp_sd<-sd(Input_dataM[1:temp_dim[1],i]);
	 temp<-c((Input_dataM[1:temp_dim[1],i]-temp_mean)/temp_sd);
	 temp=(temp+3)*(temp>-3)-(temp+3)*(temp>3);
	 temp=temp/6;
     Input_normalization[1:temp_dim[1],i]=temp;
};

radius=1;
depth=9; #给出螺旋层数
decrease_ratio=0.15; #每边长递减的长度比例
theta<-seq(from=0, to=2*pi*depth, length.out=5655)
theta<-rev(sort(theta));

New_X_Y<-polar2cart(0,0,radius*exp(decrease_ratio*theta),theta+3/4*pi,as.deg=FALSE);

step_length<-floor(2*pi);
dime=(length(theta)-628)/step_length;
plot(New_X_Y,type="l",lwd=2,yaxt="n",xaxt="n",xlab="",ylab="");
for (i in 1:dime){
     lines(c(New_X_Y$x[step_length*(i-1)+1], New_X_Y$x[step_length*(i-1)+628]), 
	       c(New_X_Y$y[step_length*(i-1)+1], New_X_Y$y[step_length*(i-1)+628]), col="red",type = "l", lwd=1);
		   
	 if(i%%10 == 0 && i <= 100 ){
	     lines(c(New_X_Y$x[step_length*(i-1)+1], New_X_Y$x[step_length*(i-1)+628]), 
	       c(New_X_Y$y[step_length*(i-1)+1], New_X_Y$y[step_length*(i-1)+628]), col="red",type = "l", lwd=2);
		   
	     text(New_X_Y$x[step_length*(i-1)+1], New_X_Y$y[step_length*(i-1)+1],as.character(i)
		 ,cex = 2, col = NULL, font =2 )
	 };
};
lines(New_X_Y,type="l",lwd=2); #重绘边界

dime<-min(dime, temp_dim[1]); #返回值不能超界限
data_point_X<-matrix(nrow=dime, ncol=temp_dim[2]);
data_point_Y<-matrix(nrow=dime, ncol=temp_dim[2]);

for (i in 1:dime){	   
	data_point_X[i,1:temp_dim[2]]<-c(New_X_Y$x[step_length*(i-1)+1]-
	             (New_X_Y$x[step_length*(i-1)+1]-New_X_Y$x[step_length*(i-1)+628])*Input_normalization[i,1:temp_dim[2]]);
	data_point_Y[i,1:temp_dim[2]]<-c(New_X_Y$y[step_length*(i-1)+1]-
	             (New_X_Y$y[step_length*(i-1)+1]-New_X_Y$y[step_length*(i-1)+628])*Input_normalization[i,1:temp_dim[2]]);
};

return(list("x"=data_point_X,"y"=data_point_Y)); 
}