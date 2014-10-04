library(jpeg)
library(reshape2)

if(!(file.exists("processed_images"))){
dir.create("processed_images")
}


compass = read.csv("supportfiles/photoDir.csv", stringsAsFactors=FALSE)
compass$direction = as.integer(compass$direction)
compass = compass[!is.na(compass$direction),]

photoinfo = read.csv("supportfiles/photos.csv", stringsAsFactors=FALSE)

keyinfo = photoinfo [,c("FileName", "ISO", "Aperture", "LightValue", "ShutterSpeed", "CreateDate")]

weather = read.table("supportfiles/Jul-14.prn.txt", stringsAsFactors=FALSE, skip=4, header=FALSE)
names(weather) = c("DATE", "TIME", "TEMP", "RH", "WINDSPD", "WINDIR", "GLOBAL", "UVA", "UVB", "VISIBLE", "RAIN", "PRESS", "MAXGUST", "GUSTIME")
weather$datetime = paste(gsub("/",":",weather$DATE), weather$TIME)
weather$datetime = paste(substr(weather$datetime,7,10), ":", substr(weather$datetime,4,5), ":", substr(weather$datetime,1,2), substr(weather$datetime,11,19), sep="")
weather = weather[, c("datetime", "GLOBAL", "UVA", "UVB", "VISIBLE", "RAIN")]

inputfolder <- "images"

files_list <- list.files(inputfolder, pattern="*JPG")
for (n in 1:length(files_list)){
  if (files_list[n] %in% compass$SourceFile){
    orig <- readJPEG(paste(inputfolder, "/",files_list[n], sep=""))
    midzone = orig[(1056-175):(1056+200), (1408-285):(1408+295),] #stand sized photos so taking a Row,Col, Depth block.
    rvals <- midzone[,,1] #layers in depth of a matrix
    gvals <- midzone[,,2]
    bvals <- midzone[,,3]
    im = melt(rvals) #rearrage data into three columns
    names(im)= c("rows", "columns","red")
    gmelt = melt(gvals)
    names(gmelt)= c("rows", "columns","green")
    bmelt = melt(bvals)
    names(bmelt)= c("rows", "columns","blue")
    im$green = gmelt$green
    im$blue = bmelt$blue #add G/B to the im variable
    dataHSV = rgb2hsv(im$red, im$green, im$blue, maxColorValue = 1)
    im$hue = dataHSV[1,]
    im$sat = dataHSV[2,]
    im$val = dataHSV[3,] #adding HSV data to image
    #reverse the rows to make it the same way "up" as looking at photo (0 at top in photo)
    maxrows = max(im$rows)
    im$rows = -1* im$rows + 1 + maxrows
    #now the dark near the bottom
    lowest40threshold = ((max(im$rows) - min(im$rows)) * 0.4) + min(im$rows)
    dark25ofLow40threshold = ((max(im$val[im$rows < lowest40threshold]) - min(im$val[im$rows < lowest40threshold])) * 0.25) + min(im$val[im$rows < lowest40threshold])
    rowOfDark = min (im$rows[im$val < dark25ofLow40threshold & im$rows < lowest40threshold]) #min = lowest row
    im = im[im$rows > rowOfDark -20 & im$rows < rowOfDark + 270 ,] # bit below the minimum
    
    top70threshold = max(im$rows) - 70
    im$whiteness = apply(im[,c("red","green","blue")],1,min)
    rowWhiteness = aggregate(whiteness ~ rows, data=im, FUN=sum)
    maxVal = max(rowWhiteness$whiteness[rowWhiteness$rows > top70threshold])
    splitrow = max(rowWhiteness$rows[rowWhiteness$rows > top70threshold & rowWhiteness$whiteness == maxVal]) # max is higher of two
    im = im[im$rows <= splitrow,]
    
    
    right50threshold = ((max(im$columns) - min(im$columns)) * 0.3) + min(im$columns)
    dark30ofRight50threshold = ((max(im$val[im$columns > right50threshold]) - min(im$val[im$columns > right50threshold])) * 0.3) + min(im$val[im$columns > right50threshold])
    colOfDark = max(im$columns[im$val < dark30ofRight50threshold & im$columns > right50threshold]) #max = most right row
    im = im[im$columns > colOfDark - 476 & im$columns < colOfDark + 4,] # bit to left of threshold
    
    
    #dark in last 50 cols
    colThres = max(im$columns) - 50
    dark30 = ((max(im$val[im$columns > colThres]) - min(im$val[im$columns > colThres])) * 0.3) + min(im$val[im$columns > colThres])
    colOfDark = max(im$columns[im$val < dark30 & im$columns > colThres]) #max = most right row
    im$columns = im$columns - colOfDark
    
    rowThres = min(im$rows) + 50
    dark30 = ((max(im$val[im$rows < rowThres & im$columns==0]) - min(im$val[im$rows < rowThres & im$columns==0])) * 0.3) + min(im$val[im$rows < rowThres & im$columns==0])
    rowOfDark = min(im$rows[im$val < dark30 & im$rows < rowThres & im$columns==0]) #max = most right row
    im$rows = im$rows - rowOfDark
    
    
    
    #RGB <- rgb(im$red, im$green, im$blue)
    #plot(im$columns, im$rows, col=RGB, main=paste("image", as.character(i)), pch=".")
    #abline(h=0)
    #abline(v=0)
    
    im$group = "discard"
    #from right anticlockwise
    
    im$group[im$columns < -20 & im$rows < 65 & im$columns > -130 & im$rows > 5] = "black"
    im$group[im$columns < -160 & im$rows < 60 & im$columns > -280 & im$rows > -5] = "purple"
    im$group[im$columns < -320 & im$rows < 55 & im$columns > -430 & im$rows > -5] = "yellow"
    im$group[im$columns < -40 & im$rows < 140 & im$columns > -140 & im$rows > 90] = "darkgreen"
    im$group[im$columns < -175 & im$rows < 140 & im$columns > -280 & im$rows > 90] = "pink" 
    im$group[im$columns < -320 & im$rows < 140 & im$columns > -430 & im$rows > 80] = "darkred" 
    im$group[im$columns < -50 & im$rows < 230 & im$columns > -140 & im$rows > 180] = "lightgreen"
    im$group[im$columns < -190 & im$rows < 220 & im$columns > -290 & im$rows > 165] = "darkblue"
    im$group[im$columns < -320 & im$rows < 225 & im$columns > -430 & im$rows > 177] = "lightblue"
    im$group[im$columns < -5 & im$rows < 240 & im$columns > -20 & im$rows > 177] = "white" #reference colour
    im = im[im$group != "discard" ,c("rows","columns","red","green","blue","group")]
    im$file = files_list[n]
    im = merge(im, compass, by.x = "file", by.y="SourceFile")
    im = merge(im, keyinfo, by.x = "file", by.y="FileName")
    minutes = as.numeric(substr(im$CreateDate,15,16))
    nearest5minutes = 5* round(minutes/5)
    im$CreateDate5 = paste(substr(im$CreateDate,1,14),formatC(nearest5minutes, width=2, flag="0"),":00", sep="")
    im2 = merge(im, weather, by.x = "CreateDate5", by.y="datetime")
    outfile = paste("processed_images/", files_list[n], ".csv", sep="")
    write.csv(im2, file=outfile, row.names = FALSE)
    
    
    
  }
}

