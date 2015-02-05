library("KRLS")

gauss_similarity_matrix = gausskernel(X = alldata, sigma = 1)
plot(var1,col='red',pch=22,bg='red',xlim=range(0,max(alldata[,1])),ylim =range (0,max(alldata[,2])))
points(var2,col='green',pch=24,bg='green')	
points(var3,col='blue',pch=19,bg='blue')

gauss_similarity_matrix = gausskernel(X = alldata, sigma = 1)


gauss_adjacency_matrix = matrix(rep(0),60,60)
for(i in 1:60)
{
	for(j in i:60)
	{
		if(gauss_similarity_matrix[i,j] >= 0.5)
		{
			lines(c(alldata[i,1],alldata[j,1]),c(alldata [i,2],alldata[j,2]))
			gauss_adjacency_matrix[i,j] = 1
		}
		else
		{
			gauss_adjacency_matrix[i,j] = 0

		}
	}
}
