translation <-
function(dtt,dtc=NULL,x,y,interact="none",dtt.lim=NULL,dtc.lim=NULL,n.around=c(5,5))
{
		if(!interact %in% c("none","around","exact")){stop("Please choose interact between 'none','around' or 'exact'")}
		findingTranslationVector=function(matToTranslate,matToCompare,xlim=c(-10,10),ylim=c(-10,10))
		{#findingTranslationVector(matToTranslate=gp,matToCompare=lifetime,xlim=c(-10,10),ylim=c(-10,10))
			if(interact=="around"&(mode(dtt)!="logical"||mode(dtc)!="logical")){stop("around option can only be done with dtt and dtc containing NA (to find the forms to detect)")}
			if(is.logical(dtt)){dtt.lim=c(0,1)}
			if(is.logical(dtc)){dtc.lim=c(0,1)}
			commonNa=function(param,matToTranslate,matToCompare)
			{
				x=param[1]
				y=param[2]
				matMod=translation(matToTranslate,x=x,y=y,interact="none")$data
				return(sum(matToCompare&matMod,na.rm=TRUE))
			}
		
			xToKeep=0
			yToKeep=0
			maxSimilitude=0
			for(x in seq(-10,10,1 ))
			{
				for(y in seq(-10,10,1 ))
				{
					nbNaCommuns=commonNa(c(x,y),matToTranslate,matToCompare)
					if(nbNaCommuns>maxSimilitude)
					{
						xToKeep=x
						yToKeep=y
						maxSimilitude=nbNaCommuns
					}
				}
			}
			return(c(xToKeep,yToKeep))
		}

	if(interact=="fix"||interact=="around")
	{
		img1=plotimage(dtt,lim=dtt.lim)
		point1=grabPoint(img1)
		img2=plotimage(dtc,lim=dtc.lim)
		point2=grabPoint(img2)
		segment=point2-point1
		x=segment[1]
		y=segment[2]
	}
	if(interact=="around"){vectTrans=findingTranslationVector(dtt,dtc,xlim=c(-n.around[1],n.around[1]),ylim=c(-n.around[2],n.around[2]));x=vectTrans[1];y=vectTrans[2]}
	res=dtt
	res[,]=NA
	xmax=dim(dtt)[1]
	ymax=dim(dtt)[2]
	x0=abs(x)
	y0=abs(y)
	if(x>=0&y>=0)
	{
		res[(1+x0):xmax,(1+y0):ymax]=as.matrix(dtt[1:(xmax-x0),1:(ymax-y0)])
	}
	if(x<=0&y>=0)
	{
		res[1:(xmax-x0),(1+y0):ymax]=as.matrix(dtt[(1+x0):xmax,1:(ymax-y0)])
	}
	if(x<=0&y<=0)
	{
		res[1:(xmax-x0),1:(ymax-y0)]=as.matrix(dtt[(1+x0):xmax,(1+y0):ymax])
	}
	if(x>=0&y<=0)
	{
		res[(1+x0):xmax,1:(ymax-y0)]=as.matrix(dtt[1:(xmax-x0),(1+y0):ymax])
	}
	#print(paste("Translation of x=", round(x,2),"; y=",round(y,2),sep=""))
	#if(return.vect){
	list.res=list();list.res[["data"]]=res; list.res[["translation"]]=c(x,y); res=list.res
	#}
	return(res)
}
