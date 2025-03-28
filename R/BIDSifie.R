
# main functions -----

findParticipants <- function() {
  
  IDs <- c()
  
  for (task in c('area', 'curvature', 'distance')) {
    taskCCs <- list.files( path = sprintf('../data/%s/color/',task),
                           pattern = '*.txt')
    
    for (CC in taskCCs) {
      
      # print(CC)
      
      us1_idx <- unlist(gregexpr('_', CC))[1]
      
      # print(us1_idx)
      
      ppID <- substr( x     = CC, 
                      start = 1, 
                      stop  = us1_idx-1)
      
      IDs <- c(IDs, ppID)
      
    }
    
  }
  
  IDs <- unique(IDs)
  
  IDs <- IDs[which(nchar(IDs) > 8)]
  
  demographics <- getDemographics()
  
  return(IDs)
  
}


cleanDemographics <- function() {
  
  # read in collected csv:
  df <- read.csv('../data/demographics.csv', stringsAsFactors = F)
  
  # shorten column/variable names:
  nameReplacements <- list('What.is.your.age...in.years.' = 'age',
                           'What.is.your.sex.' = 'sex',
                           'How.is.your.eyesight.' = 'eyesight',
                           'What.is.your.participant.ID.' = 'ID',
                           'Are.you.right..or.left.handed.' = 'handedness')
  
  for (name in names(nameReplacements)) {
    names(df)[which(names(df) == name)] <- nameReplacements[[name]]
  }
  
  # shorten responses:
  df$eyesight[which(df$eyesight == '"corrected to normal" (I need contacts or glasses and I\'m wearing them now)')] <- 'corrected'
  df$eyesight[which(df$eyesight == '"normal" (I don\'t need contacts or glasses)')] <- 'normal'
  
  df$handedness[which(df$handedness == 'left handed')]  <- 'left'
  df$handedness[which(df$handedness == 'right handed')] <- 'right'
  
  # drop timestamp, reorder columns:
  df <- df[,c('ID', 'age', 'sex', 'eyesight', 'handedness')]
  
  # remove duplicate entries, keep first entry:
  df <- df[!duplicated(df$ID),]
  
  
  write.csv(df, '../data/cleaned_demographics.csv', row.names = F)
  return(df)
  
}

getDemographics <- function() {
  
  df <- read.csv('../data/cleaned_demographics.csv', stringsAsFactors = F)
  
  return(df)
  
}

BIDSifyAll <- function() {
  
  cleanDemographics()
  
  prepTaskFolders(tasks=c('area','curvature','distance'))
  
  participants <- findParticipants()
  
  # for each participants, decide if they are already BIDSified
  # if not, BIDSify them!
  
  for (pp_no in c(1:length(participants))) {
    
    ppID <- participants[pp_no]
    
    cat(sprintf('%s (%d/%d)\n',ppID,pp_no,length(participants)))
    
    if (!(isBIDSified(ppID))) {
      BIDSifiyParticipant(ppID)
    }
    
  }
  
  
  reverseChecks()
  
  
  combis <- expand.grid(task = c('area', 'curvature', 'distance'), data = c('task', 'eyetracking'))
  
  for (combidx in c(1:dim(combis)[1])) {
    
    file.copy( from = '../Intrepid2a_BIDS/participants.tsv',
               to = sprintf('../Intrepid2a_BIDS/%s-%s/participants.tsv', combis$data[combidx], combis$task[combidx]))
    
  }
  
  
  projectdir <- getwd()
  
  for (combidx in c(1:dim(combis)[1])) {
    
    setwd(sprintf('../Intrepid2a_BIDS/%s-%s/', combis$data[combidx], combis$task[combidx]))
    
    zip(sprintf('../%s-%s.zip',combis$data[combidx], combis$task[combidx]), '.', flags='-r')
    
    setwd(projectdir)
    
  }
  
  
}

reverseChecks <- function() {
  
  # we remove participants that do not have complete data,
  # and in the process we delete data from inomplete participant data sets
  
  participants <- read.delim(file = '../Intrepid2a_BIDS/participants.tsv',
                             sep='\t')
  
  for (ID in participants$ID) {
    
    idx <- which(participants$ID == ID)
    
    for (task in c('area', 'curvature', 'distance')) {
      
      if (participants[idx,task] == FALSE) {
        # no record:
        next()
      }
      
      calibration_file <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.json', task, ID, task, ID, task)
      # print(calibration_file)
      if (!file.exists(calibration_file)) {
        cat(sprintf('calibration not present for %s - %s: removing record\n', ID, task))
        participants[idx,task] <- FALSE
      }
      
      
      if (!file.exists(sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.tsv', task, ID, task, ID, task))) {
        cat(sprintf('psychophysics not present for %s - %s: rmoeving recording\n', ID, task))
        participants[idx,task] <- FALSE
      } else {
        df <- read.delim(sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.tsv', task, ID, task, ID, task),
                         sep = '\t',
                         stringsAsFactors = F)
        if (!all(c('left','right') %in% unique(df$HemiField))) {
          cat(sprintf('psychophysics not complete for %s - %s: removing record\n', ID, task))
          participants[idx,task]
          # we could do something more strict than test if both hemifields are there
          # print(c(length(which(df$HemiField == 'left')), length(which(df$HemiField == 'right'))))
          # and actually count how many trials are there... then use some minimum cut off like 150 or so?
        }
      }
      
      if (!file.exists(sprintf('../Intrepid2a_BIDS/eyetracking-%s/sub-%s/ses-%s/beh/sub-%s_eyetracking.tar.gz', task, ID, task, ID))) {
        cat(sprintf('eyetracking not present for %s - %s: removing record\n', ID, task))
        participants[idx,task] <- FALSE
      }
      
      # if the participant is not in the record as having done the task, all files should be removed:
      if (participants[idx,task] == FALSE) {
        calibration_file <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.json', task, ID, task, ID, task)
        psychophycs_file <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.tsv', task, ID, task, ID, task)
        eyetracking_file <- sprintf('../Intrepid2a_BIDS/eyetracking-%s/sub-%s/ses-%s/beh/sub-%s_eyetracking.tar.gz', task, ID, task, ID)
        for (file in c(calibration_file,psychophycs_file,eyetracking_file)) {
          if (file.exists(file)) {
            file.remove(file)
          }
        }
      }
      
    }
    
  }
  write.table( participants,
               file      = '../Intrepid2a_BIDS/participants.tsv',
               sep       = '\t',
               row.names = FALSE,
               col.names = TRUE,
               dec       = ".")

}

isBIDSified <- function(ID) {
  
  # how do we even check this?
  # ...
  
  # BIDSifying doesn't take that long anyway...
  # if we want everyone to be BIDSified, we can just say that
  # none of them are yet: 
  return(FALSE)
  
}


BIDSifiyParticipant <- function(ID) {
  
  # first check if we know the participant:
  demographics <- read.csv('../data/cleaned_demographics.csv', stringsAsFactors = F)
  
  if (!(ID %in% demographics$ID)) {
    cat(sprintf('WARNING: participant NOT in cleaned demographics: %s!\n', ID))
    return()
  }
  
  # try all three experiments:
  BIDScheckArea(ID)
  BIDScheckCurvature(ID)
  BIDScheckDistance(ID)
  
}

# top level functions for tasks -----

BIDScheckArea <- function(ID) {
  
  if (BIDSifyCalibration(ID, 'area')) {
    
    BIDSifyArea(ID)
    
    if (taskBehavior(ID=ID, task='area')) {
      BIDSifyEyeTrackingData(ID=ID, task='area')
    }
    
  } else {
    cat('calibration missing/incomplete for area task\n')
  }
  
}

taskBehavior <- function(ID, task) {
  
  output <- TRUE
  calibration_file <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.json', task, ID, task, ID, task)
  psychophycs_file <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.tsv', task, ID, task, ID, task)
  for (file in c(calibration_file,psychophycs_file)) {
    if (!file.exists(file)) {
      output = FALSE
    }
  }
  
  return(output)
  
}

BIDScheckCurvature <- function(ID) {

  if (BIDSifyCalibration(ID, 'curvature')) {
    
    BIDSifyCurvature(ID)
    
    if (taskBehavior(ID=ID, task='area')) {
      BIDSifyEyeTrackingData(ID=ID, task='curvature')
    }
    
  } else {
    cat('calibration missing/incomplete for curvature task\n')
  }
  
  
}


BIDScheckDistance <- function(ID) {
  
  # cat('checking distance calibration...\n')
  
  if (BIDSifyCalibration(ID, 'distance')) {
    
    # cat('calibration OK, BIDSify distance psychophysics...\n')
    
    BIDSifyDistance(ID)
    
    if (taskBehavior(ID=ID, task='distance')) {
      BIDSifyEyeTrackingData(ID=ID, task='distance')
    }
    
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
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, 'dist', hf, min(nums))
      } else {
        lastfiles[[hf]] <- sprintf('../data/%s/%s_%s_%s_%d.txt', task, ID, task, hf, max(nums))
      }
    }
    
  }
  
  return(lastfiles)
  
}

getExtension <- function(file) {
  strsplit(file, ".", fixed=T)[[1]][-1]
}


findEyeTrackingFiles <- function(ID, task) {
  
  allfiles <- list.files( path = sprintf('../data/%s/eyetracking/%s/', task, ID) )
  ETfiles <- allfiles[ which( unlist(lapply(allfiles, getExtension)) == 'csv')  ]
  
  lastfiles <- list()
  
  for (hf in c('LH','RH')) {
    
    Hfiles <- ETfiles[which(grepl(hf, ETfiles))]
    
    nums <- c()
    
    for (hfile in Hfiles) {
      # find the first period (just before the file extension)
      pidx <- unlist(gregexpr('[.]', hfile))[1]
      
      # find the last hemifield indicator (just before the calibration number)
      # hidx <- tail(grep(hf, hfile,value=FALSE, fixed=TRUE), n=1) + 1
      
      hidx <- 7
      
      # extract number/index of color calibration file:
      nums <- c(nums, as.numeric(substr(hfile, hidx, pidx-1)))
    }
    # for which ever blind spot map is not there, we can skip and not look for task data
    if (length(nums) == 0) {
      cat(sprintf('no data file found for %s, %s hemifield\n', task, list('RH'='right','LH'='left')[[hf]]))
      lastfiles[[hf]] <- NULL
    } else { 
      lastfiles[[hf]] <- sprintf('../data/%s/eyetracking/%s/dist%s%s.csv', task, ID, hf, max(nums))
    }
    
  }
  
  return(lastfiles)
  
}

# plotArea <- function(ID) {
#   
#   files <- findTaskFiles(ID, 'area')
#   
#   layout(mat=matrix(c(1,2),nrow=1))
#   par(mar=c(4,4,2,0.5))
#   
#   for (hf in c('LH','RH')) {
#     
#     if (is.null(files[[hf]])) {
#       plot(-1000,-1000,
#            main='',xlab='',ylab='',
#            xlim=c(0,1),ylim=c(0,1),
#            ax=F,bty='n')
#       next
#     } 
#     # print(files[[hf]])
#     df <- read.delim(
#       file = files[[hf]],
#       skip=1,
#       header=TRUE,
#       sep='\t'
#     )
#     
#     df <- df[which(df$FinalDiff != "Trial aborted"),]
#     df$FinalDiff <- as.numeric(df$FinalDiff)
#     
#     # print(unique(df$OriginalDiff))
#     
#     plot(-1000,-1000,
#          main=sprintf('%s %s area', hf, ID),xlab='trial',ylab='area',
#          xlim=c(0,(dim(df)[1]+1)),ylim=c(-3,6),
#          ax=F,bty='n'
#     )
# 
#     colors <- c( '#FF0000',
#                  '#00FF00',
#                  '#0000FF',
#                  '#FFFF00',
#                  '#FF00FF',
#                  '#00FFFF',
#                  '#FF7700',
#                  '#7700FF',
#                  '#0077FF',
#                  '#FF0077' )
#   
#     staircases <- sort(unique(df$OriginalDiff))
#     for (scid in c(1:length(staircases))) {
#       idx <- which(df$OriginalDiff == staircases[scid])
#       lines( x = idx,
#              y = df$FinalDiff[idx],
#              col=colors[scid])
#     }
# 
#     axis(side=1, at=c(1,40,80,120,160,200))
#     axis(side=2, at=c(-3,0,3,6))
#     
#   }
# 
# }
# 
# plotCurvature <- function(ID) {
#   
#   files <- findTaskFiles(ID, 'curvature')
#   
#   layout(mat=matrix(c(1,2),nrow=1))
#   par(mar=c(4,4,2,0.5))
#   
#   for (hf in c('LH','RH')) {
#     
#     if (is.null(files[[hf]])) {
#       plot(-1000,-1000,
#            main='',xlab='',ylab='',
#            xlim=c(0,1),ylim=c(0,1),
#            ax=F,bty='n')
#       next
#     } 
#     # print(files[[hf]])
#     df <- read.delim(
#                       file = files[[hf]],
#                       skip=1,
#                       header=TRUE,
#                       sep='\t'
#                     )
# 
#     df <- df[which(df$Response != "Trial aborted"),]
#     
#     df$StaircaseIndex <- df$Staircase + (2 * df$GreenStim) + (4 * df$Stimulus_position) + 1
#     
#     plot(-1000,-1000,
#          main=sprintf('%s %s curvature', hf, ID),xlab='trial',ylab='curvature',
#          xlim=c(0,(dim(df)[1]+1)),ylim=c(-0.4,0.4),
#          ax=F,bty='n'
#          )
#     
#     colors <- c( '#FF0000',
#                  '#00FF00',
#                  '#0000FF',
#                  '#FFFF00',
#                  '#FF00FF',
#                  '#00FFFF',
#                  '#FF7700',
#                  '#7700FF')
#     
#     for (scid in unique(df$StaircaseIndex)) {
#       idx <- which(df$StaircaseIndex == scid)
#       lines( x = idx,
#              y = df$CorrectedCurvature[idx],
#              col=colors[scid])
#     }
#     
#     axis(side=1, at=c(1,40,80,120,160,200,240))
#     axis(side=2, at=c(-0.4,-0.2,0,0.2,0.4))
#     
#   }
# 
# }
# 
# plotDistance <- function(ID) {
#   
#   files <- findTaskFiles(ID, 'distance')
#   
#   layout(mat=matrix(c(1,2),nrow=1))
#   par(mar=c(4,4,2,0.5))
#   
#   for (hf in c('LH','RH')) {
#     
#     if (is.null(files[[hf]])) {
#       plot(-1000,-1000,
#            main='',xlab='',ylab='',
#            xlim=c(0,1),ylim=c(0,1),
#            ax=F,bty='n')
#       next
#     } 
#     # print(files[[hf]])
#     df <- read.delim(
#       file = files[[hf]],
#       skip=1,
#       header=TRUE,
#       sep='\t'
#     )
#     
#     df <- df[which(df$Resp %in% c("1","2")),]
# 
#     plot(-1000,-1000,
#          main=sprintf('%s %s distance', hf, ID),xlab='trial',ylab='distance',
#          xlim=c(0,(dim(df)[1]+1)),ylim=c(-3.5,3.5),
#          ax=F,bty='n'
#     )
# 
# colors <- c( '#FF0000',
#              '#00FF00',
#              '#0000FF',
#              '#FFFF00',
#              '#FF00FF',
#              '#00FFFF',
#              '#FF7700',
#              '#7700FF')
# 
#     for (scid in unique(df$Stair)) {
#       idx <- which(df$Stair == scid)
#       lines( x = idx,
#              y = df$Difference[idx],
#              col=colors[scid])
#     }
# 
#     axis(side=1, at=c(1,40,80,120,160,200,240))
#     axis(side=2, at=c(-3.5,-2,-1,0,1,2,3.5))
#     
#   }
#   
# 
# }


# BIDSification replacement functions -----


prepTaskFolders <- function(tasks) {
  
  dataset_description <- list()
  
  # REQUIRED:
  # dataset_description[['Name']] <- sprintf('Intrepid2a %s replication', task)
  dataset_description[['BIDSVersion']] <- '1.10.0'
  
  # RECOMMENDED:
  # dataset_description[['HEDVersion']] <- # string or array of strings
  # dataset_description[['DatasetLinks']] <- # if BIDS URI's are used : object of strings
  dataset_description[['DatasetType']] <- "raw"
  # dataset_description[['License']] <- # string, see https://bids-specification.readthedocs.io/en/stable/appendices/licenses.html
  dataset_description[['Authors']] <- list("Cavanagh, Patrick", "'t Hart, Bernard Marius") # array of strings, esp if data is from elsewhere
  
  # dataset_description[['GeneratedBy']] <- # array of objects # see https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html
  # dataset_description[['SourceDatasets']] <- # array of objects # see https://bids-specification.readthedocs.io/en/stable/modality-agnostic-files.html
  
  # OPTIONAL  
  dataset_description[['Acknowledgements']] <- "We thank Mohammed Khan for help in collecting the data." # string
  # dataset_description[['HowToAcknowledge']] <- # string
  dataset_description[['Funding']] <- list("Templeton Foundation XXXX")# array of strings
  dataset_description[['EthicsApproval']] <- list("York Univeristy's 'Human Participants Review Committee', protocol e2019-194; https://www.yorku.ca/research/human-participants/; Office of Research Ethics, ore@yorku.ca") # array of strings
  # dataset_description[['ReferencesAndLinks']] <- # array of strings
  # dataset_description[['DatasetDOI']] <- # string
  
  # json_dataset_description <- RJSONIO::toJSON(dataset_description)
  
  for (task in tasks) {
    
    dataset_description[['Name']] <- sprintf('Intrepid2a %s replication', task)
    
    json_dataset_description <- RJSONIO::toJSON(dataset_description)

    for (part in c('task','eyetracking')) {
      
      folder <- sprintf('../Intrepid2a_BIDS/%s-%s/', part, task)
      
      dir.create( path = folder,
                  recursive = TRUE,
                  showWarnings = FALSE)
      
      file <- sprintf('%sdataset_description.json', folder)
      
      write(json_dataset_description, file=file)
      
    }
    
  }
  
}


BIDSifyCalibration <- function(ID, task) {
  
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
  
  # print(calidx)
  
  colorcalibration <- readLines( con  = sprintf('../data/%s/color/%s_col_cal_%d.txt', task, ID, calidx),
                                 warn = FALSE)
  colorlist <- list()
  for (cc in colorcalibration) {
    ccentry <- unlist(strsplit(cc, ":\t"))
    
    colorlist[[ccentry[1]]] <- list()
    psychopy <- as.numeric(unlist(strsplit( substr(ccentry[2],2,nchar(ccentry[2])-1), ',')))
    colorlist[[ccentry[1]]][['psychopy']] <- psychopy
    # conversion to RGB string:
    values <- round( (psychopy + 1) * (255/2) )
    colorlist[[ccentry[1]]][['hex']] <- rgb(red=values[1], green=values[2], blue=values[3], maxColorValue = 255)
    
  }
  
  # json_colorlist <- jsonlite::toJSON(colorlist) # jsonlite::toJSON(colorlist, force = TRUE) ?
  # print(json_colorlist)
  
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  
  # now read in the blind spot mapping info
  
  # get all color calibrations for the task:
  bsmaps <- list.files( path = sprintf('../data/%s/mapping/', task))
  # keep only those for the participant:
  bsLH <- bsmaps[which(substr(bsmaps, 1, nchar(ID)+3) == sprintf('%s_LH',ID))]
  bsRH <- bsmaps[which(substr(bsmaps, 1, nchar(ID)+3) == sprintf('%s_RH',ID))]
  
  blindspots <- list()
  
  for (hf in c('LH','RH')) {
    
    bs <- list('LH'=bsLH, 'RH'=bsRH)[[hf]]
    
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
    
    # if there is one, we read in the one with the highest number:
    bsidx <- max(numbers)
    hfbsmap <- readLines( con  = sprintf('../data/%s/mapping/%s_%s_blindspot_%d.txt', task, ID, hf, bsidx),
                          warn = FALSE)
    bsprops <- list()
    for (bspl in hfbsmap) {
      bsentry <- unlist(strsplit(bspl, ":\t"))
      values <- as.numeric(unlist(strsplit( substr(bsentry[2],2,nchar(bsentry[2])-1), ','))) 
      bsprops[[bsentry[1]]] <- values
    }

    blindspots[[list('LH'='left', 'RH'='right')[[hf]]]] <- bsprops
    
  }
  
  calibration <- list()
  calibration[['blindspot_mapping']] <- blindspots
  calibration[['colors']] <- colorlist
  json_calibration <- RJSONIO::toJSON(calibration)
  
  # print(json_calibration)
  
  
  folder <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/', task, ID, task)
  
  dir.create( path = folder,
              recursive = TRUE,
              showWarnings = FALSE)
  
  file <- sprintf('%ssub-%s_%s.json', folder, ID, task)
  # file <- sprintf('%ssub-%d_%s.json', folder, 1, task)
  
  write(json_calibration, file=file)
  
  return(TRUE)
  
}

BIDSifyEyeTrackingData <- function(ID, task) {
  
  
  # first find the eye-tracking files:
  files <- findEyeTrackingFiles(ID=ID, task=task)
  
  # there will be 4 files in a .gz file,
  gzfiles <- c()
  
  for (file_idx in names(files)) {
    
    file <- files[[file_idx]]
    
    # source file:
    df <- read.csv(file, stringsAsFactors = FALSE)
    
    # remove inconsistencies due to calibration interrupting the file
    df <- fixLiveTrack(df)
    
    # split of the comments for an event file:
    idx <- which(nchar(df$Comment) > 1)
    events <- df[idx,c('Timestamp','Comment')]
    eyetracking <- df[,which(names(df) != c('Comment'))]
    
    # write them out:
    # filename <- sprintf('%ssub-%s_run-%s_eyetracking.tsv', folder, ID, list('LH'='left', 'RH'='right')[[file_idx]])
    filename <- sprintf('sub-%s_run-%s_eyetracking.tsv', ID, list('LH'='left', 'RH'='right')[[file_idx]])
    write.table( eyetracking,
                 file      = filename,
                 sep       = '\t',
                 row.names = FALSE,
                 col.names = TRUE,
                 dec       = ".")
    
    gzfiles <- c(gzfiles, filename)
    
    # filename <- sprintf('%ssub-%s_run-%s_events.tsv', folder, ID, list('LH'='left', 'RH'='right')[[file_idx]])
    filename <- sprintf('sub-%s_run-%s_events.tsv', ID, list('LH'='left', 'RH'='right')[[file_idx]])
    write.table( events,
                 file      = filename,
                 sep       = '\t',
                 row.names = FALSE,
                 col.names = TRUE,
                 dec       = ".")
    
    gzfiles <- c(gzfiles, filename)
    
    
  }
  
  if (length(gzfiles) < 4) {
    cat('fewer files than expected for eyetracking\n')
    return()
  }
  

  if (length(gzfiles) > 0) {
    filename <- sprintf('sub-%s_eyetracking.tar.gz', ID)
    tar(filename, files = gzfiles, compression='gzip', tar='/usr/bin/tar')
  
    # now move to this folder:
    folder <- sprintf('../Intrepid2a_BIDS/eyetracking-%s/sub-%s/ses-%s/beh/', task, ID, task)
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
    print(filename)
    print(folder)
    file.rename(filename, sprintf('%s%s', folder,filename))
    while (file.exists(filename)) {
      # that's ok?
    }
    file.remove(gzfiles)
  } else {
    cat('no eyetracking data\n')
  }
  
  recordParticipant(ID, task)
  
}


fixLiveTrack <- function(df) {
  
  df <- df[which(df$Timestamp != 'Timestamp'),]
  
  df <- df[,which(names(df) != 'Trigger')]
  
  for (colname in names(df)) {
    
    if (colname != 'Comment') {
      df[,colname] <- as.numeric(df[,colname])
    }
    
  }
  
  # find spots where time goes back down (calibration):
  idxs <- which(diff(df$Timestamp) < 0)
  
  # how many samples are there:
  nsamples <- length(df$Timestamp)
  
  # loop through reset points:
  for (idx_no in c(1:length(idxs))) {
    idx <- idxs[idx_no]
    lval <- df$Timestamp[idx]
    if (idx_no == length(idxs)) {
      tidx <- c((idx+1):nsamples)
    } else {
      tidx <- c((idx+1):idxs[idx_no+1])
    }
    df$Timestamp[tidx] <- df$Timestamp[tidx] + lval
  }
  
  # fix instances where timestamps go up too much:
  # find spots where time goes back down (calibration):
  idxs <- which(diff(df$Timestamp) > 2500)
  
  # loop through reset points:
  for (idx_no in c(1:length(idxs))) {
    idx <- idxs[idx_no]
    lval <- df$Timestamp[idx] - df$Timestamp[idx+1] + 2000
    if (idx_no == length(idxs)) {
      tidx <- c((idx+1):nsamples)
    } else {
      tidx <- c((idx+1):idxs[idx_no+1])
    }
    df$Timestamp[tidx] <- df$Timestamp[tidx] + lval
  }
  

  return(df)
  
}


BIDSifyArea <- function(ID) {
  
  files <- findTaskFiles(ID, 'area')
  
  allData <- NA
  
  writeoutput <- TRUE
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      writeoutput <- FALSE
      next
    } 
    df <- read.delim(
      file = files[[hf]],
      skip=1,
      header=TRUE,
      sep='\t'
    )
    
    # df <- df[which(df$FinalDiff != "Trial aborted"),]
    df$abort <- FALSE
    df$abort[which(df$FinalDiff == "Trial aborted")] <- TRUE
    
    df$FinalDiff[which(df$FinalDiff == "Trial aborted")] <- NA
    df$FinalDiff <- as.numeric(df$FinalDiff)
    
    df$HemiField <- list('LH'='left', 'RH'='right')[[hf]]
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, df)
    } else {
      allData <- df
    }
    
  }
  
  if (writeoutput) {
    # write tsv file:
    folder <- sprintf('../Intrepid2a_BIDS/task-area/sub-%s/ses-area/beh/', ID)
    dir.create(path         = folder,
               recursive    = TRUE,
               showWarnings = FALSE)
    
    filename <- sprintf('%ssub-%s_area.tsv', folder, ID)
    
    write.table( allData,
                 file      = filename,
                 sep       = '\t',
                 row.names = FALSE,
                 col.names = TRUE,
                 dec       = ".")
  }
  # recordParticipant(ID, 'area')
  
}

BIDSifyCurvature <- function(ID) {
  
  files <- findTaskFiles(ID, 'curvature')
  
  allData <- NA
  
  writeoutput <- TRUE

  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      writeoutput <- FALSE
      next
    } 
    df <- read.delim(
      file = files[[hf]],
      skip=1,
      header=TRUE,
      sep='\t'
    )
    
    # df <- df[which(df$Response != "Trial aborted"),]
    
    df$abort <- FALSE
    df$abort[which(df$Response == "Trial aborted")] <- TRUE
    
    df$StaircaseIndex <- df$Staircase + (2 * df$GreenStim) + (4 * df$Stimulus_position) + 1
    
    df$HemiField <- list('LH'='left', 'RH'='right')[[hf]]
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, df)
    } else {
      allData <- df
    }

  }
  
  if (writeoutput) {
    # write tsv file:
    folder <- sprintf('../Intrepid2a_BIDS/task-curvature/sub-%s/ses-curvature/beh/', ID)
    dir.create(path         = folder,
               recursive    = TRUE,
               showWarnings = FALSE)
    
    filename <- sprintf('%ssub-%s_curvature.tsv', folder, ID)
    
    write.table( allData,
                 file      = filename,
                 sep       = '\t',
                 row.names = FALSE,
                 col.names = TRUE,
                 dec       = ".")
  }
  # recordParticipant(ID, 'curvature')
  
}

BIDSifyDistance <- function(ID) {
  
  files <- findTaskFiles(ID, 'distance')
  
  allData <- NA
  
  writeoutput <- TRUE
  
  for (hf in c('LH','RH')) {
    
    if (is.null(files[[hf]])) {
      writeoutput <- FALSE
      next
    } 
    df <- read.delim(
      file = files[[hf]],
      skip=1,
      header=TRUE,
      sep='\t'
    )
    # print(files[[hf]])
    # print(dim(df))
    
    # df <- df[which(df$Resp %in% c("1","2")),]
    df$abort <- 1
    df$abort[which(df$Resp %in% c("1","2"))] <- 0
    df$abort[which(df$Resp == 'auto abort')] <- 1
    df$abort[which(df$Resp == 'abort')]      <- 2
    
    df$Resp[!which(df$Resp %in% c("1","2"))] <- NA
    
    suppressWarnings( df$Resp <- as.numeric(df$Resp) ) # is this smart or the opposite?
    
    df$Targ_chosen[!which(df$Targ_chosen %in% c("False", "True"))] <- NA
    df$Targ_chosen <- as.logical(df$Targ_chosen)
    
    df$Reversal[!which(df$Reversal %in% c("False", "True"))] <- NA
    df$Reversal <- as.logical(df$Reversal)
    
    df$HemiField <- list('LH'='left', 'RH'='right')[[hf]]
    
    if (is.data.frame(allData)) {
      allData <- rbind(allData, df)
    } else {
      allData <- df
    }

  }
  
  if (writeoutput) {
    # write tsv file:
    folder <- sprintf('../Intrepid2a_BIDS/task-distance/sub-%s/ses-distance/beh/', ID)
    filename <- sprintf('%ssub-%s_distance.tsv', folder, ID)
    
    write.table( allData,
                 file      = filename,
                 sep       = '\t',
                 row.names = FALSE,
                 col.names = TRUE,
                 dec       = ".")
  }
  # recordParticipant(ID, 'distance')
  
}


recordParticipant <- function(ID, task) {
  
  # check if the participant actually has all the required data!
  # if not: do NOT record the participant...
  
  # task-area/sub-tor03a5eb/ses-area/beh
  calibration_file <- sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.json', task, ID, task, ID, task)
  # print(calibration_file)
  if (!file.exists(calibration_file)) {
    cat(sprintf('calibration not present for %s - %s: NOT recording\n', ID, task))
    return()
  }
  if (!file.exists(sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.tsv', task, ID, task, ID, task))) {
    cat(sprintf('psychophysics not present for %s - %s: NOT recording\n', ID, task))
    return()
  } else {
    df <- read.delim(sprintf('../Intrepid2a_BIDS/task-%s/sub-%s/ses-%s/beh/sub-%s_%s.tsv', task, ID, task, ID, task),
                     sep = '\t',
                     stringsAsFactors = F)
    # print(dim(df))
    # print(unique(df$HemiField))
    # print(c('left','right') %in% unique(df$HemiField))
    if (!all(c('left','right') %in% unique(df$HemiField))) {
      cat(sprintf('psychophysics not complete for %s - %s: NOT recording\n', ID, task))
      return()
      # we could do something more strict than test if both hemifields are there
      print(c(length(which(df$HemiField == 'left')), length(which(df$HemiField == 'right'))))
      # and actually count how many trials are there... then use some minimum cut off like 150 or so?
    }
  }
  
  # Sys.sleep(0.5)
  
  # print(list.dirs(sprintf('../Intrepid2a_BIDS/eyetracking-%s/sub-%s/ses-%s/beh/', task, ID, task)))
  # print(list.files(sprintf('../Intrepid2a_BIDS/eyetracking-%s/sub-%s/ses-%s/beh/', task, ID, task)))
  
  if (!file.exists(sprintf('../Intrepid2a_BIDS/eyetracking-%s/sub-%s/ses-%s/beh/sub-%s_eyetracking.tar.gz', task, ID, task, ID))) {
    cat(sprintf('eyetracking not present for %s - %s: NOT recording\n', ID, task))
    return()
  }
  
  
  demographics <- read.csv('../data/cleaned_demographics.csv', stringsAsFactors = F)
  
  # this should have been done earlier in `BIDSifyParticipant()`:
  if (!(ID %in% demographics$ID)) {
    cat(sprintf('participant %s NOT in cleaned demographics: SKIPPING!\n', ID))
    return()
  }
  
  PPidx <- which(demographics$ID == ID)
  
  if (file.exists('../Intrepid2a_BIDS/participants.tsv')) {
    
    participants <- read.csv('../Intrepid2a_BIDS/participants.tsv', sep='\t', stringsAsFactors = F)
    
    if (ID %in% participants$ID) {
      participants[,task] <- TRUE
    } else {
      ID         <- c(participants$ID,         ID                              )
      age        <- c(participants$age,        demographics$age[PPidx]         )
      sex        <- c(participants$sex,        demographics$sex[PPidx]         )
      eyesight   <- c(participants$eyesight,   demographics$eyesight[PPidx]    )
      handedness <- c(participants$handedness, demographics$handedness[PPidx]  )
      area       <- c(participants$area,       (task == 'area')                )
      curvature  <- c(participants$curvature,  (task == 'curvature')           )
      distance   <- c(participants$distance,   (task == 'distance')            )
      # print(c(length(ID),length(age),length(sex),length(eyesight),length(handedness),length(area),length(curvature),length(distance)))
      participants <- data.frame(ID, age, sex, eyesight, handedness, area, curvature, distance)
    }
    
  } else {
    ID         <- ID
    age        <- demographics$age[PPidx]
    sex        <- demographics$sex[PPidx]
    eyesight   <- demographics$eyesight[PPidx]
    handedness <- demographics$handedness[PPidx]
    area       <- (task == 'area')
    curvature  <- (task == 'curvature')
    distance   <- (task == 'distance')
    
    participants <- data.frame(ID, age, sex, eyesight, handedness, area, curvature, distance)
  }
  
  write.table( participants,
               file      = '../Intrepid2a_BIDS/participants.tsv',
               sep       = '\t',
               row.names = FALSE,
               col.names = TRUE,
               dec       = ".")
  
}