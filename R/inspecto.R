
# main function -----

inspect <- function(ID) {
  
  cat(sprintf('%s\n',ID))
  # try all three experiments:
  checkArea(ID)
  checkCurvature(ID)
  checkDistance(ID)
  
}

# top level functions for tasks -----

checkArea <- function(ID) {
  
  if (plotCalibration(ID, 'area')) {
    
    plotArea(ID)
    
  } else {
    cat('calibration missing/incomplete for area task\n')
  }
  
}

checkCurvature <- function(ID) {

  if (plotCalibration(ID, 'curvature')) {
    
    plotCurvature(ID)
    
  } else {
    cat('calibration missing/incomplete for curvature task\n')
  }
  
  
}


checkDistance <- function(ID) {
  
  if (plotCalibration(ID, 'distance')) {
    
    plotDistance(ID)
    
  } else {
    cat('calibration missing/incomplete for distance task\n')
  }
  
  
}

# generic calibration checker -----

plotCalibration <- function(ID, task) {
  
  # get all color calibrations for the task:
  colcal <- list.files( path = sprintf('../data/%s/color/', task))
  # keep only those for the participant:
  colcal <- colcal[which(substr(colcal, 1, nchar(ID)) == ID)]
  
  # if there are no numbers... we return to the caller: nothing to do!
  if (length(colcal) == 0) { return(FALSE) }
  
  
  # select the last one:
  numbers <- c()
  for (calfile in colcal) {
    # find the first period (just before the file extension)
    pidx <- unlist(gregexpr('[.]', calfile))[1]
    # find the last underscore (just before the calibration number)
    uidx <- tail(unlist(gregexpr('_', calfile)), n=1)
    # extract number/index of color calibration file:
    numbers <- c(numbers, as.numeric(substr(calfile, uidx+1, pidx-1)))
  }
  
  # if there are no numbers... we return to the caller: nothing to do!
  if (length(numbers) == 0) { return(FALSE) }
  
  calidx <- max(numbers)

  colorcalibration <- readLines( con  = sprintf('../data/%s/color/%s_col_cal_%d.txt', task, ID, calidx),
                                 warn = FALSE)
  colorlist <- list()
  for (cc in colorcalibration) {
    ccentry <- unlist(strsplit(cc, ":\t"))
    # values <- as.numeric(unlist(strsplit( substr(ccentry[2],2,nchar(ccentry[2])-1), ',')))
    # print(values)
    values <- round( (as.numeric(unlist(strsplit( substr(ccentry[2],2,nchar(ccentry[2])-1), ','))) + 1) * (255/2) )
    # print(rgb(red=values[1], green=values[2], blue=values[3], maxColorValue = 255))
    
    # print(strsplit(ccentry[[2]], '][,'))
    
    colorlist[ccentry[1]] <- rgb(red=values[1], green=values[2], blue=values[3], maxColorValue = 255)
  }
  # print(colorlist)
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
  # now read in the blind spot mapping info
  
  # bsmaps <- list.files( path = sprintf('../data/%s/mapping/', task),
  #                       pattern = sprintf('%s_*.txt', ID))
  

  # get all color calibrations for the task:
  bsmaps <- list.files( path = sprintf('../data/%s/mapping/', task))
  # print(bsmaps)
  # keep only those for the participant:
  bsLH <- bsmaps[which(substr(bsmaps, 1, nchar(ID)+3) == sprintf('%s_LH',ID))]
  bsRH <- bsmaps[which(substr(bsmaps, 1, nchar(ID)+3) == sprintf('%s_RH',ID))]
  
  # print(bsLH)
  # print(bsRH)
  
  blindspots <- list()
  
  for (hf in c('LH','RH')) {
    
    bs <- list('LH'=bsLH, 'RH'=bsRH)[[hf]]
    
    # print(bs)
    
    # select the last one:
    numbers <- c()
    for (bsm in bs) {
      # find the first period (just before the file extension)
      pidx <- unlist(gregexpr('[.]', bsm))[1]
      # find the last underscore (just before the calibration number)
      uidx <- tail(unlist(gregexpr('_', bsm)), n=1)
      # extract number/index of color calibration file:
      numbers <- c(numbers, as.numeric(substr(bsm, uidx+1, pidx-1)))
    }
    # for which ever blind spot map is not there, we can skip and not look for task data
    if (length(numbers) == 0) { return(FALSE) }
    
    # print(numbers)
    
    # if there is one, we read in the one with the highest number:
    bsidx <- max(numbers)
    # print(bsidx)
    
    
    hfbsmap <- readLines( con  = sprintf('../data/%s/mapping/%s_%s_blindspot_%d.txt', task, ID, hf, bsidx),
                          warn = FALSE)
    # print(hfbsmap)
    bsprops <- list()
    for (bspl in hfbsmap) {
      # print(bspl)
      bsentry <- unlist(strsplit(bspl, ":\t"))
      # print(bsentry)
      values <- as.numeric(unlist(strsplit( substr(bsentry[2],2,nchar(bsentry[2])-1), ','))) 
      # print(values)
      bsprops[[bsentry[1]]] <- values
    }
    # print(bsprops)

    blindspots[[hf]] <- bsprops

  }

  
  # screen properties:
  # size       = [59.8, 33.6] # in cm
  # distance   = 49.53 # in cm
  
  # red lens is on the right / green on the left
  layout(mat=matrix(c(1),nrow=1))
  par(mar=c(0.5,0.5,2,0.5))
  
  plot(-1000,-1000,
       main=sprintf('%s calibration for %s', task, ID), xlab='',ylab='',
       xlim=c(-25,25),ylim=c(-17,17),
       ax=F,bty='n',asp=1)
  
  polygon(x=c(-30,30,30,-30),
          y=c(-20,-20,20,20),
          col=colorlist$background,
          border=NA)
  lines(x=c(-1,1),y=c(0,0),lw=3,col='black')
  lines(x=c(0,0),y=c(-1,1),lw=3,col='black')
  
  
  for (bsloc in c('RH','LH')) {
    
    polygon(x=(cos(seq(0,2*pi,pi/90))*blindspots[[bsloc]]$size[1]*0.5)+blindspots[[bsloc]]$position[1],
            y=(sin(seq(0,2*pi,pi/90))*blindspots[[bsloc]]$size[2]*0.5)+blindspots[[bsloc]]$position[2],
            col=colorlist[[list('RH'='red', 'LH'='green')[[bsloc]]]],
            border=NA
            )
    
  }
  # print(blindspots)
  
  return(TRUE)

}


# task data functions ------


findTaskFiles <- function(ID, task) {
  
  alltaskfiles <- list.files( path = sprintf('../data/%s/', task) )
  Pfiles <- alltaskfiles[which(substr(alltaskfiles, 1, nchar(ID)) == ID)]
  
  lastfiles <- list()
  
  for (hf in c('LH','RH')) {
    
    # files for this hemifield:
    # print(Pfiles)
    # print(grepl(sprintf('_%s_', hf), Pfiles))
    # 
    Hfiles <- Pfiles[which(grepl(sprintf('_%s_', hf), Pfiles))]
    
    nums <- c()
    
    for (hfile in Hfiles) {
      # find the first period (just before the file extension)
      pidx <- unlist(gregexpr('[.]', hfile))[1]
      # find the last underscore (just before the calibration number)
      uidx <- tail(unlist(gregexpr('_', hfile)), n=1)
      # extract number/index of color calibration file:
      nums <- c(nums, as.numeric(substr(hfile, uidx+1, pidx-1)))
    }
    # for which ever blind spot map is not there, we can skip and not look for task data
    if (length(nums) == 0) {
      cat(sprintf('no data file found for %s, %s hemifield\n', task, list('RH'='right','LH'='left')[[hf]]))
      lastfiles[[hf]] <- NULL
    } else {
      if (task == 'distance') {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'dist', hf, max(nums))
      } else {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, task, hf, max(nums))
      }
    }
    
  }
  
  return(lastfiles)
  
}

plotArea <- function(ID) {
  
  files <- findTaskFiles(ID, 'area')
  
  layout(mat=matrix(c(1,2),nrow=1))
  par(mar=c(4,4,2,0.5))
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      plot(-1000,-1000,
           main='',xlab='',ylab='',
           xlim=c(0,1),ylim=c(0,1),
           ax=F,bty='n')
      next
    } 
    # print(files[[hf]])
    df <- read.delim(
      file = files[[hf]],
      skip=1,
      header=TRUE,
      sep='\t'
    )
    
    df <- df[which(df$FinalDiff != "Trial aborted"),]
    df$FinalDiff <- as.numeric(df$FinalDiff)
    
    # print(unique(df$OriginalDiff))
    
    plot(-1000,-1000,
         main=sprintf('%s %s area', hf, ID),xlab='trial',ylab='curvature',
         xlim=c(0,(dim(df)[1]+1)),ylim=c(-3,6),
         ax=F,bty='n'
    )

    colors <- c( '#FF0000',
                 '#00FF00',
                 '#0000FF',
                 '#FFFF00',
                 '#FF00FF',
                 '#00FFFF',
                 '#FF7700',
                 '#7700FF',
                 '#0077FF',
                 '#FF0077' )
  
    staircases <- sort(unique(df$OriginalDiff))
    for (scid in c(1:length(staircases))) {
      idx <- which(df$OriginalDiff == staircases[scid])
      lines( x = idx,
             y = df$FinalDiff[idx],
             col=colors[scid])
    }

    axis(side=1, at=c(1,40,80,120,160,200))
    axis(side=2, at=c(-3,0,3,6))
    
  }

}

plotCurvature <- function(ID) {
  
  files <- findTaskFiles(ID, 'curvature')
  
  layout(mat=matrix(c(1,2),nrow=1))
  par(mar=c(4,4,2,0.5))
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      plot(-1000,-1000,
           main='',xlab='',ylab='',
           xlim=c(0,1),ylim=c(0,1),
           ax=F,bty='n')
      next
    } 
    # print(files[[hf]])
    df <- read.delim(
                      file = files[[hf]],
                      skip=1,
                      header=TRUE,
                      sep='\t'
                    )

    df <- df[which(df$Response != "Trial aborted"),]
    
    df$StaircaseIndex <- df$Staircase + (2 * df$GreenStim) + (4 * df$Stimulus_position) + 1
    
    plot(-1000,-1000,
         main=sprintf('%s %s area', hf, ID),xlab='trial',ylab='curvature',
         xlim=c(0,(dim(df)[1]+1)),ylim=c(-0.4,0.4),
         ax=F,bty='n'
         )
    
    colors <- c( '#FF0000',
                 '#00FF00',
                 '#0000FF',
                 '#FFFF00',
                 '#FF00FF',
                 '#00FFFF',
                 '#FF7700',
                 '#7700FF')
    
    for (scid in unique(df$StaircaseIndex)) {
      idx <- which(df$StaircaseIndex == scid)
      lines( x = idx,
             y = df$CorrectedCurvature[idx],
             col=colors[scid])
    }
    
    axis(side=1, at=c(1,40,80,120,160,200,240))
    axis(side=2, at=c(-0.4,-0.2,0,0.2,0.4))
    
  }

}

plotDistance <- function(ID) {
  
  files <- findTaskFiles(ID, 'distance')
  
  layout(mat=matrix(c(1,2),nrow=1))
  par(mar=c(4,4,2,0.5))
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      plot(-1000,-1000,
           main='',xlab='',ylab='',
           xlim=c(0,1),ylim=c(0,1),
           ax=F,bty='n')
      next
    } 
    # print(files[[hf]])
    df <- read.delim(
      file = files[[hf]],
      skip=1,
      header=TRUE,
      sep='\t'
    )
    
    df <- df[which(df$Resp %in% c("1","2")),]

    plot(-1000,-1000,
         main=sprintf('%s %s area', hf, ID),xlab='trial',ylab='curvature',
         xlim=c(0,(dim(df)[1]+1)),ylim=c(-3.5,3.5),
         ax=F,bty='n'
    )

colors <- c( '#FF0000',
             '#00FF00',
             '#0000FF',
             '#FFFF00',
             '#FF00FF',
             '#00FFFF',
             '#FF7700',
             '#7700FF')

    for (scid in unique(df$Stair)) {
      idx <- which(df$Stair == scid)
      lines( x = idx,
             y = df$Difference[idx],
             col=colors[scid])
    }

    axis(side=1, at=c(1,40,80,120,160,200,240))
    axis(side=2, at=c(-3.5,-2,-1,0,1,2,3.5))
    
  }
  

}