
normal_clust = kmeans(alldata,3)
############# Problem 4a ################
plot(alldata,col=normal_clust$cluster,pch = normal_cluster$cluster)


first_k_eigen = sort(unique(eigen(knn_lap_norm)$values))[1:3]
k_vectors = c()
for(i in 1:3)
{
	for(j in 1:60)
	{
		if(first_k_eigen[i]== eigen(knn_lap_norm)$values[j])
			{
				k_vectors = c(k_vectors,j)
				break
			}
	}

}
k_dimensional_vectors = cbind(eigen(knn_lap_norm)$vectors[,k_vectors])

library("ppls")
normalized_vector  = matrix(rep(0),0,3)
for(i in 1:60)
{
	normalized_vector=rbind(normalized_vector,normalize.vector(Re(k_dimensional_vectors[i,])))
	
}
clust = kmeans(normalized_vector,3)


############# Problem 4b:For KNN Graph################
plot(alldata,col=clust$cluster,pch = clust$cluster)






first_k_eigen_gauss = sort(unique(eigen(gauss_lap_norm)$values))[1:3]
k_vectors_gauss = c()
for(i in 1:3)
{
	for(j in 1:60)
	{
		if(first_k_eigen_gauss[i]== eigen(gauss_lap_norm)$values[j])
			{
				k_vectors_gauss = c(k_vectors_gauss,j)
				break
			}
	}

}
k_dimensional_vectors_gauss = cbind(eigen(gauss_lap_norm)$vectors[,k_vectors_gauss])

library("ppls")
library('gaggleUtil')
normalized_vector_gauss  = normalize(k_dimensional_vectors_gauss)

clust = kmeans(normalized_vector_gauss,3)


############# Problem 4b:For Gaussian Graph################
plot(alldata,col=clust$cluster,pch = clust$cluster)