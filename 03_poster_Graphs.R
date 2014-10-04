pictureSessions = read.csv("supportfiles/pictureSessions.csv")
alldata = read.csv("finishedData/allgroup.csv", stringsAsFactors=FALSE)

nicegraph360 = function (a, b, c, xlab_, ylab_, main_, norm=FALSE){
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
    repeatedNorth = agg[agg$direction == 360,]
    repeatedNorth$direction = 0
    agg = rbind(agg,repeatedNorth)
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
    plot(agg$direction[agg$group=="black"], agg$value[agg$group=="black"], col="black", ylim=c(0,maxy), xlim=c(0,450), xlab=xlab_, type="l", main=main_, ylab=ylab_, xaxt='n')
    axis(side=1, at=seq(from=0, to=360, by=45), cex.axis=1)
    lines(agg$direction[agg$group=="yellow"], agg$value[agg$group=="yellow"], col="yellow")
    lines(agg$direction[agg$group=="darkblue"], agg$value[agg$group=="darkblue"], col="darkblue")
    lines(agg$direction[agg$group=="darkgreen"], agg$value[agg$group=="darkgreen"], col="darkgreen")
    lines(agg$direction[agg$group=="lightblue"], agg$value[agg$group=="lightblue"], col="lightblue")
    lines(agg$direction[agg$group=="lightgreen"], agg$value[agg$group=="lightgreen"], col="lightgreen")
    lines(agg$direction[agg$group=="pink"], agg$value[agg$group=="pink"], col="pink")
    lines(agg$direction[agg$group=="purple"], agg$value[agg$group=="purple"], col="purple") 
    colours=c("black", "yellow", "darkblue", "darkgreen", "lightblue", "lightgreen", "pink", "purple")
    legend("right", legend=colours, fill=colours, box.lwd = 0,box.col = "white")
}

nicesdgraph360 = function (a, b, c, xlab_, ylab_, main_){
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
    repeatedNorth = agg[agg$direction == 360,]
    repeatedNorth$direction = 0
    agg = rbind(agg,repeatedNorth)
    agg = agg[order(agg$direction),]
    agg$value = agg$value ^ 0.5
  maxy = max(agg$value)
    plot(agg$direction[agg$group=="black"], agg$value[agg$group=="black"], col="black", ylim=c(0,maxy), xlab=xlab_, type="l", main=main_, ylab=ylab_, xaxt='n')
    axis(side=1, at=seq(from=0, to=360, by=45), cex.axis=1)
    lines(agg$direction[agg$group=="yellow"], agg$value[agg$group=="yellow"], col="yellow")
    lines(agg$direction[agg$group=="darkblue"], agg$value[agg$group=="darkblue"], col="darkblue")
    lines(agg$direction[agg$group=="darkgreen"], agg$value[agg$group=="darkgreen"], col="darkgreen")
    lines(agg$direction[agg$group=="lightblue"], agg$value[agg$group=="lightblue"], col="lightblue")
    lines(agg$direction[agg$group=="lightgreen"], agg$value[agg$group=="lightgreen"], col="lightgreen")
    lines(agg$direction[agg$group=="pink"], agg$value[agg$group=="pink"], col="pink")
    lines(agg$direction[agg$group=="purple"], agg$value[agg$group=="purple"], col="purple") 

}


#raw
nicegraph360(alldata$redmean , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw red light", norm=FALSE)
nicegraph360(alldata$greenmean , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw green light", norm=FALSE)
nicegraph360(alldata$bluemean , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw blue light", norm=FALSE)

#normalised
nicegraph360(alldata$redmean , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled mean red light", norm=TRUE)
nicegraph360(alldata$greenmean , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled mean raw green light", norm=TRUE)
nicegraph360(alldata$bluemean , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled mean raw blue light", norm=TRUE)

#sunny raw
sunny = c(4216:4295,4665:4769,4807:5195)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw red sunny light", norm=FALSE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw green sunny light", norm=FALSE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw blue sunny light", norm=FALSE)

#sunny scaled
sunny = c(4216:4295,4665:4769,4807:5195)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled Mean red sunny light", norm=TRUE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled Mean green sunny light", norm=TRUE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled Mean blue sunny light", norm=TRUE)

#diffuse raw
sunny = c(4199:4206,4208:4215,4296:4373, 4392:4445, 4446:4556, 4740:4806)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw red diffuse light", norm=FALSE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw green diffuse light", norm=FALSE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Mean raw blue diffuse light", norm=FALSE)

#diffuse scaled
sunny = c(4199:4206,4208:4215,4296:4373, 4392:4445, 4446:4556, 4740:4806)
sunnyones = alldata$file %in% paste("IMG_", sunny,".JPG", sep="")
nicegraph360(alldata$redmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled Mean red diffuse light", norm=TRUE)
nicegraph360(alldata$greenmean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled Mean green diffuse light", norm=TRUE)
nicegraph360(alldata$bluemean[sunnyones] , alldata$direction[sunnyones], alldata$group[sunnyones], xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled Mean blue diffuse light", norm=TRUE)


#combined standard deviation for each colour group
# from http://www.emathzone.com/tutorials/basic-statistics/combined-variance.html
colourgroupmean = aggregate(alldata$redmean, by=list(alldata$direction,alldata$group), FUN=mean)
names(colourgroupmean) = c("direction", "group", "groupmean")
sdcalc = merge(alldata, colourgroupmean, by = c("direction","group"))
colourgroupdata = aggregate(alldata$samplesize, by=list(alldata$direction,alldata$group), FUN=sum)
names(colourgroupdata) = c("direction", "group", "groupsum")
sdcalc2 = merge(sdcalc, colourgroupdata, by = c("direction","group"))
combinedsd = ((sdcalc2$redmean - sdcalc2$groupmean) ^ 2) * sdcalc2$samplesize / sdcalc2$groupsum
nicesdgraph360(combinedsd , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Combined Standard Deviation by colour red light")


#this adjusts each direction/ colour combination to a common "white" brightness and sees if the results still matter, and they do
whitebar = aggregate(alldata$redmean[alldata$group=="white"], by=list(alldata$direction[alldata$group=="white"],alldata$group[alldata$group=="white"]), FUN=mean)
names(whitebar) = c("direction", "group", "whitebal")
whiten = merge(alldata, whitebar, by = c("direction","group"))
brighten = 1- whiten$whitebal
#normalised
nicegraph360(alldata$redmean + brighten , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled for white and range mean red light", norm=TRUE)
nicegraph360(alldata$greenmean + brighten , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled for white and range mean raw green light", norm=TRUE)
nicegraph360(alldata$bluemean + brighten , alldata$direction, alldata$group, xlab_="Bearing (degrees)", ylab_="light intensity", main_="Scaled for white and range mean raw blue light", norm=TRUE)
