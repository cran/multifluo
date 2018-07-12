pcapic <-
function(dfbool,d=c(203,204),lim=NULL)
{
	# calcul des eigenvectors
	resPCA=pca(data=dfbool$data, zone="zone",pixel="pixel")
	#df37Ascaled=reshape.images(zone.img=zone.img,list.img=list.img,reduction=reduction)
	rawdata=dfbool$data[,-which(colnames(dfbool$data)%in% c("zone","pixel"))]
	newData=scale(as.matrix(rawdata),center=TRUE)%*%as.matrix(resPCA$EigenVectors[,1])
	newMatrice=matrix(NA,203,204)
	newMatrice[dfbool$boolean]=newData
	plotimage(newMatrice,lim)
	return(newMatrice)
}
