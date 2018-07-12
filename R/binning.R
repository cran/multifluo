binning <-
function(dataset,n.bin,fun="mean")
{
	newDim=round(dim(dataset)/n.bin)
	size.x=newDim[1]
	size.y=newDim[2]
	newDataset=matrix(NA,size.x,size.y)
	for(i in 1:size.x)
	{

		for(j in 1:size.y)
		{	
		jIni=(n.bin*j-(n.bin-1))
		jFin=n.bin*j
		iIni=(n.bin*i-(n.bin-1))
		iFin=n.bin*i
			subDataset=dataset[iIni:iFin,jIni:jFin]	
			if(sum(!is.na(subDataset))>0)
			{	if(fun=="mean")
				{
					newDataset[i,j]=mean(as.vector(subDataset[!is.na(subDataset)]),na.rm=T)
				}
				if(fun=="sum")
				{
					newDataset[i,j]=sum(as.vector(subDataset[!is.na(subDataset)]),na.rm=T)
				}
				
			}
		}
	}
	return(newDataset)
}
