require(plotrix)
pictureSessions = read.csv("supportFiles/pictureSessions.csv")
alldata = read.csv("finishedData/allgroup.csv", stringsAsFactors=FALSE)


nicegraph360 = function (a, b, c, main_, norm=FALSE){
	agg = aggregate(a, by=list(b,c), FUN=mean)
	names(agg) = c("direction", "group", "value")
	agg$direction[agg$direction == 8] = 360
	agg$direction[agg$direction == 9] = 315
	agg$direction[agg$direction == 6] = 270
	agg$direction[agg$direction == 3] = 225
	agg$direction[agg$direction == 2] = 180
	agg$direction[agg$direction == 1] = 135
	agg$direction[agg$direction == 4] = 90
	agg$direction[agg$direction == 7] = 45
	agg = agg[agg$direction > 40,] 
	if(norm){
		valmin = aggregate(value ~ group, agg, FUN=min)
		names(valmin) = c("group","valmin")
		agg = merge(agg, valmin, by = "group")
		valmax = aggregate(value ~ group, agg, FUN=max)
		names(valmax) = c("group","valmax")
		agg = merge(agg, valmax, by = "group")
		agg$value = (agg$value - agg$valmin)/(agg$valmax - agg$valmin)
	}
	agg = agg[order(agg$direction),]
	maxy = max(agg$value)
	png(paste("circlegraphs/",main_, ".png", sep=""))
	polar.plot(agg$value[agg$group=="black"], label.pos=c(0,45,90,135,180,225,270,315), labels = agg$direction[agg$group=="black"], main=main_,lwd=2,line.col="black", rp.type="p", start=45, clockwise=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="yellow"],lwd=2,line.col="yellow", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="darkblue"],lwd=2,line.col="darkblue", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="darkred"],lwd=2,line.col="darkred", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="darkgreen"],lwd=2,line.col="darkgreen", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="lightblue"],lwd=2,line.col="lightblue", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="lightgreen"],lwd=2,line.col="lightgreen", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="pink"],lwd=2,line.col="pink", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="purple"],lwd=2,line.col="purple", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	colours=c("black", "yellow", "darkred", "darkblue", "darkgreen", "lightblue", "lightgreen", "pink", "purple")
	legend("bottomright", legend=colours, fill=colours, box.lwd = 0,box.col = "white")
	dev.off()
}

nicesdgraph360 = function (a, b, c, main_, norm=FALSE){
	agg = aggregate(a, by=list(b,c), FUN=sum)
	names(agg) = c("direction", "group", "value")
	agg$direction[agg$direction == 8] = 360
	agg$direction[agg$direction == 9] = 315
	agg$direction[agg$direction == 6] = 270
	agg$direction[agg$direction == 3] = 225
	agg$direction[agg$direction == 2] = 180
	agg$direction[agg$direction == 1] = 135
	agg$direction[agg$direction == 4] = 90
	agg$direction[agg$direction == 7] = 45
	agg = agg[agg$direction > 40,] 
	if(norm){
		valmin = aggregate(value ~ group, agg, FUN=min)
		names(valmin) = c("group","valmin")
		agg = merge(agg, valmin, by = "group")
		valmax = aggregate(value ~ group, agg, FUN=max)
		names(valmax) = c("group","valmax")
		agg = merge(agg, valmax, by = "group")
		agg$value = (agg$value - agg$valmin)/(agg$valmax - agg$valmin)
	}
	agg = agg[order(agg$direction),]
	maxy = max(agg$value)
	agg$value = agg$value ^ 0.5
	png(paste("circlegraphs/",main_, ".png", sep=""))
	polar.plot(agg$value[agg$group=="black"], label.pos=c(0,45,90,135,180,225,270,315), labels = agg$direction[agg$group=="black"], main=main_,lwd=2,line.col="black", rp.type="p", start=45, clockwise=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="yellow"],lwd=2,line.col="yellow", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="darkblue"],lwd=2,line.col="darkblue", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="darkred"],lwd=2,line.col="darkred", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="darkgreen"],lwd=2,line.col="darkgreen", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="lightblue"],lwd=2,line.col="lightblue", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="lightgreen"],lwd=2,line.col="lightgreen", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="pink"],lwd=2,line.col="pink", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	polar.plot(agg$value[agg$group=="purple"],lwd=2,line.col="purple", rp.type="p", start=45, clockwise=TRUE, add=TRUE, radial.lim=c(0,maxy))
	colours=c("black", "yellow", "darkred", "darkblue", "darkgreen", "lightblue", "lightgreen", "pink", "purple")
	legend("bottomright", legend=colours, fill=colours, box.lwd = 0,box.col = "white")
	dev.off()
}



#raw
nicegraph360(alldata$redmean , alldata$direction, alldata$group, main_="Mean raw red light", norm=FALSE)
nicegraph360(alldata$greenmean , alldata$direction, alldata$group, main_="Mean raw green light", norm=FALSE)
nicegraph360(alldata$bluemean , alldata$direction, alldata$group, main_="Mean raw blue light", norm=FALSE)

#normalised
nicegraph360(alldata$redmean , alldata$direction, alldata$group, main_="Scaled mean red light", norm=TRUE)
nicegraph360(alldata$greenmean , alldata$direction, alldata$group, main_="Scaled mean raw green light", norm=TRUE)
nicegraph360(alldata$bluemean , alldata$direction, alldata$group, main_="Scaled mean raw blue light", norm=TRUE)

#sunny raw
sunny = c(4216:4295,4665:4769,4807:5195)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Mean raw red sunny light", norm=FALSE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Mean raw green sunny light", norm=FALSE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Mean raw blue sunny light", norm=FALSE)

#sunny scaled
sunny = c(4216:4295,4665:4769,4807:5195)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Scaled Mean red sunny light", norm=TRUE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Scaled Mean green sunny light", norm=TRUE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Scaled Mean blue sunny light", norm=TRUE)

#diffuse raw
sunny = c(4199:4206,4208:4215,4296:4373, 4392:4445, 4446:4556, 4740:4806)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Mean raw red diffuse light", norm=FALSE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Mean raw green diffuse light", norm=FALSE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Mean raw blue diffuse light", norm=FALSE)

#diffuse scaled
sunny = c(4199:4206,4208:4215,4296:4373, 4392:4445, 4446:4556, 4740:4806)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Scaled Mean red diffuse light", norm=TRUE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Scaled Mean green diffuse light", norm=TRUE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], main_="Scaled Mean blue diffuse light", norm=TRUE)


#combined standard deviation for each colour group
# from http://www.emathzone.com/tutorials/basic-statistics/combined-variance.html
colourgroupmean = aggregate(alldata$redmean, by=list(alldata$direction,alldata$group), FUN=mean)
names(colourgroupmean) = c("direction", "group", "groupmean")
sdcalc = merge(alldata, colourgroupmean, by = c("direction","group"))
colourgroupdata = aggregate(alldata$samplesize, by=list(alldata$direction,alldata$group), FUN=sum)
names(colourgroupdata) = c("direction", "group", "groupsum")
sdcalc2 = merge(sdcalc, colourgroupdata, by = c("direction","group"))
combinedsd = ((sdcalc2$redmean - sdcalc2$groupmean) ^ 2) * sdcalc2$samplesize / sdcalc2$groupsum
nicesdgraph360(combinedsd , alldata$direction, alldata$group, main_="Combined Standard Deviation by colour red light", norm=FALSE)


#this adjusts each direction/ colour combination to a common "white" brightness and sees if the results still matter, and they do
whitebar = aggregate(alldata$redmean[alldata$group=="white"], by=list(alldata$direction[alldata$group=="white"],alldata$group[alldata$group=="white"]), FUN=mean)
names(whitebar) = c("direction", "group", "whitebal")
whiten = merge(alldata, whitebar, by = c("direction","group"))
brighten = 1- whiten$whitebal
#normalised
nicegraph360(alldata$redmean + brighten , alldata$direction, alldata$group, main_="Scaled for white and range mean red light", norm=TRUE)
nicegraph360(alldata$greenmean + brighten , alldata$direction, alldata$group, main_="Scaled for white and range mean raw green light", norm=TRUE)
nicegraph360(alldata$bluemean + brighten , alldata$direction, alldata$group, main_="Scaled for white and range mean raw blue light", norm=TRUE)






