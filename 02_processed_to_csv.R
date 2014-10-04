
alldata = data.frame(direction=integer(), group=character(), greenmean=numeric(), greensd=numeric(), redmean=numeric(), redsd=numeric(), bluemean=numeric(), bluesd=numeric(), samplesize=integer(), file=character())
alldata$file= as.character(alldata$file)
alldata$group= as.character(alldata$group)

inputfolder <- "processed_images"
files_list <- list.files(inputfolder, pattern="*csv")

for (n in 1:length(files_list)){
  datafile = read.csv(paste(inputfolder, "/", files_list[n], sep=""), stringsAsFactors=FALSE)
  onefile = aggregate(green ~ direction + group, datafile, FUN=mean)
  names(onefile) = c("direction", "group", "greenmean")
  greensdfile = aggregate(green ~ direction + group, datafile, FUN=sd)
  onefile$greensd = greensdfile$green
  redmeanfile = aggregate(red ~ direction + group, datafile, FUN=mean)
  onefile$redmean = redmeanfile$red
  redsdfile = aggregate(red ~ direction + group, datafile, FUN=sd)
  onefile$redsd = redsdfile$red
  bluemeanfile = aggregate(blue ~ direction + group, datafile, FUN=mean)
  onefile$bluemean = bluemeanfile$blue
  bluesdfile = aggregate(blue ~ direction + group, datafile, FUN=sd)
  onefile$bluesd = bluesdfile$blue
  agg = aggregate(blue ~ direction + group, datafile, FUN=length)
  onefile$samplesize = agg$blue
  onefile$file = substr(files_list[n],1,12)
  alldata = rbind(alldata,onefile)
}
  
write.csv(alldata, file="combined_data/allgroup.csv", row.names=FALSE)
