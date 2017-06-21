# Import dataset (iris)
data <- iris;

# Convert dataset to matrix
mrow <- nrow(data);
mcol <- ncol(data);

# Remove classification column for iris
data_mtx <- matrix(as.matrix(data[,1:(mcol-1)]), nrow = mrow);

#No of clusters, C and No of datapoints, N
C <- 3;
N <- mrow;
class_mtx <- t(matrix(rep(1:C,rep(N/3,C))));

#Normalize data matrix
data_mtx <- scale(data_mtx);