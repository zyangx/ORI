U4529d3保留了3位有效数字，精度较U4529低

U4529d3文件较大，拆分为三个文件：U4529d3_1, U4529d3_2, 及U4529d3_3，在分别导入R环境后，需合并为完整的矩阵:

U4529d3.m  <- cbind(cbind(U4529d3_1.m, U4529d3_2.m), U4529d3_3.m)

