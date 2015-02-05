library("igraph")
knn_adjacency_matrix= get.adjacency(G1)
gauss_lap = graph.laplacian(graph.adjacency(gauss_adjacency_matrix))
gauss_lap_norm = graph.laplacian(graph.adjacency(gauss_adjacency_matrix),  normalized=TRUE)

knn_lap = graph.laplacian(graph.adjacency(knn_adjacency_matrix))
knn_lap_norm = graph.laplacian(graph.adjacency(knn_adjacency_matrix),normalized =  TRUE)

######### Problem 3a ##############
plot(c(1:60),sort(eigen(knn_lap)$values),type="l",col="blue")
points(c(1:60),sort(eigen(knn_lap)$values),pch=19,bg='blue',col="blue")

plot(c(1:60),sort(eigen(knn_lap_norm)$values),type="l",col="blue")
points(c(1:60),sort(eigen(knn_lap_norm)$values),pch=19,bg='blue',col="blue")

plot(c(1:60),sort(eigen(gauss_lap_norm)$values),type="l",col="blue")
points(c(1:60),sort(eigen(gauss_lap_norm)$values),pch=19,bg='blue',col="blue")

plot(c(1:60),sort(eigen(gauss_lap)$values),type="l",col="blue")
points(c(1:60),sort(eigen(gauss_lap)$values),pch=19,bg='blue',col="blue")

second_smallest_knn =  sort(unique(eigen(knn_lap)$values))[2]
knn_j=0
for(i in 1:60)
{
	if(eigen(knn_lap)$values[i]==second_smallest_knn)
	{
		knn_j=i		
	}
}


second_smallest_knn_norm =  sort(unique(eigen(knn_lap_norm)$values))[2]
knn_norm_j=0
for(i in 1:60)
{
	if(eigen(knn_lap_norm)$values[i]==second_smallest_knn_norm)
	{
		knn_norm_j=i		
	}
}

second_smallest_gauss =  sort(unique(eigen(gauss_lap)$values))[2]
gauss_j=0
for(i in 1:60)
{
	if(eigen(gauss_lap)$values[i]==second_smallest_gauss)
	{
		gauss_j=i		
	}
}

second_smallest_gauss_norm =  sort(unique(eigen(gauss_lap_norm)$values))[2]
gauss_norm_j=0
for(i in 1:60)
{
	if(eigen(gauss_lap_norm)$values[i]==second_smallest_gauss_norm)
	{
		gauss_norm_j=i		
	}
}

######### Problem 3c ##############

plot(c(1:60),sort(eigen(knn_lap)$vectors[,knn_j]),col="blue",type='l')
points(c(1:60),sort(eigen(knn_lap)$vectors[,knn_j]),pch=19,bg='blue',col="blue")

plot(c(1:60),sort(eigen(knn_lap_norm)$vectors[,knn_norm_j]),col="blue",type='l')
points(c(1:60),sort(eigen(knn_lap_norm)$vectors[,knn_norm_j]),pch=19,bg='blue',col="blue")

plot(c(1:60),sort(eigen(gauss_lap)$vectors[,gauss_j]),col="blue",type='l')
points(c(1:60),sort(eigen(gauss_lap)$vectors[,gauss_j]),pch=19,bg='blue',col="blue")

plot(c(1:60),sort(eigen(gauss_lap_norm)$vectors[,gauss_norm_j]),col="blue",type='l')
points(c(1:60),sort(eigen(gauss_lap_norm)$vectors[,gauss_norm_j]),pch=19,bg='blue',col="blue")



######### Problem 3d ##############



mat1 = matrix(rep(0),0,3)
mat2 = matrix(rep(0),0,3)
points_in_community1_knn=c()
points_in_community2_knn=c()

for(i in 1:60)
{
	if(Re(eigen(knn_lap)$vectors[,knn_j][i]) < 0.0)
	{
		mat1=rbind(mat1,c(alldata[i,],i))
		points_in_community1_knn = c(points_in_community1_knn,i)	
	}
	else
	{
		mat2=rbind(mat2,c(alldata[i,],i))
		points_in_community2_knn = c(points_in_community2_knn,i)	
	}
}

mat1 = matrix(rep(0),0,3)
mat2 = matrix(rep(0),0,3)
points_in_community1_knn_norm=c()
points_in_community2_knn_norm=c()

for(i in 1:60)
{
	if(Re(eigen(knn_lap_norm)$vectors[,knn_norm_j][i]) < 0.00)
	{
		mat1=rbind(mat1,c(alldata[i,],i))
		points_in_community1_knn_norm = c(points_in_community1_knn_norm,i)	
	}
	else
	{
		mat2=rbind(mat2,c(alldata[i,],i))
		points_in_community2_knn_norm = c(points_in_community2_knn_norm,i)	
	}
}




mat1 = matrix(rep(0),0,3)
mat2 = matrix(rep(0),0,3)
points_in_community1_gauss=c()
points_in_community2_gauss=c()

for(i in 1:60)
{
	if(Re(eigen(gauss_lap)$vectors[,gauss_j][i]) < 0.05)
	{
		mat1=rbind(mat1,c(alldata[i,],i))
		points_in_community1_gauss = c(points_in_community1_gauss,i)	
	}
	else
	{
		mat2=rbind(mat2,c(alldata[i,],i))
		points_in_community2_gauss = c(points_in_community2_gauss,i)	
	}
}

mat1 = matrix(rep(0),0,3)
mat2 = matrix(rep(0),0,3)
points_in_community1_gauss_norm =c()
points_in_community2_gauss_norm =c()

for(i in 1:60)
{
	if(Re(eigen(gauss_lap_norm)$vectors[,gauss_norm_j][i]) < 0.05)
	{
		mat1=rbind(mat1,c(alldata[i,],i))
		points_in_community1_gauss_norm = c(points_in_community1_gauss_norm,i)	
	}
	else
	{
		mat2=rbind(mat2,c(alldata[i,],i))
		points_in_community2_gauss_norm = c(points_in_community2_gauss_norm,i)	
	}
}






total_edges_in_community1 = 0
is_point_in_comm1 = c(1:60)
is_point_in_comm1 = !(is_point_in_comm1 %in% points_in_community2_knn)
for(i in 1:length(points_in_community1_knn))
{
	for(j in 1:60)
	{
		if(knn_adjacency_matrix[points_in_community1_knn[i],j] == 1 & is_point_in_comm1[j])
			total_edges_in_community1 = total_edges_in_community1 + 1
			
	}
}

total_edges_in_community2 = 0
is_point_in_comm2 = c(1:60)
is_point_in_comm2 = !(is_point_in_comm2 %in% points_in_community1_knn)
for(i in 1:length(points_in_community2_knn))
{
	for(j in 1:60)
	{
		if(knn_adjacency_matrix[points_in_community2_knn[i],j] == 1 & is_point_in_comm2[j])
			total_edges_in_community2 = total_edges_in_community2 + 1
			
	}

}


total_edges_leaving_community=0
for(i in 1:length(points_in_community1_knn))
{
	for(j in 1:60)	
	{
		if(knn_adjacency_matrix[points_in_community1_knn[i],j] == 1 & !is_point_in_comm1[j])		
			total_edges_leaving_community =  total_edges_leaving_community+1		
	}
}

total_edges_leaving_community2=0
for(i in 1:length(points_in_community2_knn))
{
	for(j in 1:60)	
	{
		if(knn_adjacency_matrix[points_in_community2_knn[i],j] == 1 & !is_point_in_comm2[j])
			total_edges_leaving_community2 =  total_edges_leaving_community2+1		
	}
}

######### Problem 3e ##############

conductance1 = total_edges_leaving_community/ (2*total_edges_in_community1 +  total_edges_leaving_community)
conductance2 = total_edges_leaving_community2/ (2*total_edges_in_community2 +  total_edges_leaving_community2)


