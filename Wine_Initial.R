# Import dataset (wine)
data <- read.csv("C:/Users/Owner/Documents/R/Scripts/wine.data", header=FALSE);
#wine; 3 (48), 2 (71), 1 (59)

# Convert dataset to matrix
mrow <- nrow(data);
mcol <- ncol(data);

# Remove classification column for wine
data_mtx <- matrix(as.matrix(data[,2:mcol]), nrow = mrow);

#No of clusters, C and No of datapoints, N
C <- 3;
N <- mrow;
class_mtx1 <- t(matrix(1,59));
class_mtx2 <- t(matrix(2,71));
class_mtx3 <- t(matrix(3,48));
class_mtx <- cbind(class_mtx1, class_mtx2, class_mtx3);

#Normalize data matrix
data_mtx <- scale(data_mtx);

