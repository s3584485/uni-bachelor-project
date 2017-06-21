#Supervision
#Initialise boolean for labels
#Initial labels, n per class
b <- matrix(,N);
Build_b <- function (mtx1, mtx2, n)
{
   for (i in 1:C)
   {
      count <- 0;
     
         for (j in 1:N)
         {
         
            if (mtx1[j,1] == i)
            {
               mtx2[j,1] <- sample(c(1,0),1);
               if ((mtx2[j,1] == 1) && (count < n+1))
               {
                  count <- count + 1;
               }
               if (count == n+1)
               {
                  mtx2[j,1] <- 0;
               }
            }       
         }
      
   }
   return (mtx2);
}
b <- Build_b(t(class_mtx),b, 1);

#Number of labelled data and unlabelled data
label_total <- 0;
unlabel_total <- 0;

Label_count <- function (total)
{   
   for(j in 1:N)
   {
      if(b[j,] == 1)
      {
         total <- total +1;
      }
   }
   return (total);
}

Unlabel_count <- function (total)
{
   for(j in 1:N)
   {
      if(b[j,] == 0)
      {
         total <- total +1;
      }   
   }
   return (total);
}

label_total <- Label_count(label_total);
unlabel_total <- Unlabel_count(unlabel_total);

#Initialise membership matrix, f
prob.h <- 0.9; #high probability
prob.l <- (1-prob.h)/(C-1); #low probability
f_mtx <- matrix(, C, N);

#Use classification matrix for determining F
Determine_f <- function(mtx)
{
   for (i in 1:C)
   {
      for (j in 1:N)
      {
         if (class_mtx[1,j] == i)
         {
            mtx[i,j] <- prob.h;
         }
         else
         {
            mtx[i,j] <- prob.l;
         }
      }
   }
   return (mtx);
}
f_mtx <- Determine_f(f_mtx);

#Initialise partition matrix, U_init
U0 <- matrix(,C,N);
U_init <- function(U,mtx,b)
{
   for (i in 1:C)
   {
      for (k in 1:N)
      {
         if (b[k,] == 0)
         {
            U[i,k] <- 1/C;
         }
         else
         {
            U[i,k] <- mtx[i,k] * b[k,];
         }
      }
   }
   return (U);
}
U0 <- U_init(U0,f_mtx,b);
U <- U0;

#Initial cluster centres, v[1],v[2],...
v <- matrix(1:C,C,mcol-1);
V_calc <- function(U,v,mtx)
{
   for (i in 1:C) 
   {
      eqn1 <- 0;
      eqn2 <- 0;
      for (k in 1:N)
      {
         eqn1 <- eqn1 + (U[i,k])^2 * mtx[k,];
         eqn2 <- eqn2 + (U[i,k])^2;
         
      }
      v[i,] <- eqn1/ eqn2;
   }
   return (v);
}
v0 <- V_calc(U0,v,data_mtx);
v <- v0;

#Euclidean Distance Squared function
#Distance of centre to datapoint
Dist_Euclid <- function(i, k, mtx, v)
{
   dist <- 0;
   dist <- sum((abs(mtx[k,] - v[i,]))^2);
   
   return (dist);
}

#Update partition matrix, U
#Learning rate, alpha with modifier
alpha <- unlabel_total/label_total; 

#Tolerance limit, delta for ending iterations
delta <- 0.001;

Update_U <- function(a, mtx1, mtx2, b, U, v) 
{
   for (i in 1:C)
   {
      for (j in 1:N)
      {
         dist1 <- Dist_Euclid(i, j, mtx1, v);
         dist2 <- 0;
         dist_sum <- 0;
         f_sum <- 0;
         
         for (l in 1:C)
         {
            dist_ratio <- 0;
            dist2 <- Dist_Euclid(l, j, mtx1, v);
            dist_ratio <- (dist1/dist2)^2;
            dist_sum <- dist_sum + dist_ratio;
            f_sum <- f_sum + mtx2[l,j];
         }
         
         U[i,j] <- (1/(1+a))*
            ((1 + a*(1 - (b[j,]* f_sum)))/dist_sum + 
                (a*(mtx2[i,j]*b[j,])));
         
      }
   }
   return (U);
}

U_new <- Update_U(alpha, data_mtx, f_mtx, b, U, v);

#Compare U' and U to build tolerance comparison
U_dist <- function(U1, U2)
{
   u.i1 <- 0;
   for (i in 1:C)
   {
      u.k1 <- 0;
      
      
      for (k in 1:N)
      {
         
         u.k1 <- u.k1 + (U1[i,k] - U2[i,k])^2;   
      }
      u.i1 <- u.i1 + u.k1;
   }
   
   return(u.i1);
}

iter <- 1;
tolerance <- U_dist(U, U_new);
while (tolerance > delta)
{
   U <- U_new;
   v <- V_calc(U,v,data_mtx);
   U_new <- Update_U(alpha, data_mtx, f_mtx, b, U, v);
   iter <- iter + 1;
   tolerance <- U_dist(U, U_new);
}

clust_new <- matrix( , 1, N);
Clust_choice <- function (U, clust)
{
   for(k in 1:N)
   {
      i <- 1;
      j <- 2;
      while (j < C+1)
      {
         if (U[i,k] > U[j,k])
         {
            clust[1,k] <- i;
            j <- j + 1;
         }
         else if (U[i,k] < U[j,k])
         {
            clust[1,k] <- j;
            i <- j;
            j <- j + 1;
         }
         else
         {
            clust[1,k] <- sample(c(i,j),1);
            i <- clust[1,k];
            j <- j + 1;  
            
         }
         
      }
      
   }
   
   return (clust);
}

clust_new <- Clust_choice(U_new, clust_new);