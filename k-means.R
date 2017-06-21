#Get cluster using K-Means
km_list <- kmeans(data_mtx,center=3);
clust_new <- t(km_list$cluster);
v <- km_list$centers;
