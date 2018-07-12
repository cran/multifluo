summaryrshp <-
function(rshp,test="hsd",p.adj="none",alpha=0.05)
{#summary.rshp(rshp=df.scaled)
	resDataframe=list()
	zones=levels(as.factor(rshp[,"zone"]))
	vars=colnames(rshp)[!colnames(rshp)%in%c("zone","pixel")]
	n.var=length(vars)
	n.zones=length(zones)
	
	for(j in 1:n.var)
	{
		if(test=="hsd"){resHsd=HSD.test(lm(paste(vars[j],"~zone",sep=""),data=rshp),trt="zone",alpha=alpha)}
		if(test=="lsd"){resHsd=LSD.test(lm(paste(vars[j],"~zone",sep=""),data=rshp),trt="zone",p.adj=p.adj,alpha=alpha)}
		resDataframe[[j]]=list()
		resDataframe[[j]][["anova"]]=anova(lm(paste(vars[j],"~zone",sep=""),data=rshp))
		resDataframe[[j]][["summary"]]=summary(rshp[,vars[j]])
		resDataframe[[j]][["stats"]]=data.frame()
		for(i in 1:n.zones)
		{
			resDataframe[[j]][["stats"]][i,"zone"]=zones[i]	
			resDataframe[[j]][["stats"]][i,"N"]=length(rshp[rshp[,"zone"]==zones[i],vars[j]])
			resDataframe[[j]][["stats"]][i,"Avg"]=round(mean(rshp[rshp[,"zone"]==zones[i],vars[j]],na.rm=TRUE),digits=3)
			resDataframe[[j]][["stats"]][i,"Sd"]=round(sd(rshp[rshp[,"zone"]==zones[i],vars[j]],na.rm=TRUE),digits=3)
			resDataframe[[j]][["stats"]][i,"Gp"]=resHsd$groups[rownames(resHsd$groups)==zones[i],"groups"]
		}
	}
	names(resDataframe)=vars
return(resDataframe)
}
