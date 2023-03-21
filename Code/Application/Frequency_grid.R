# set working directory
setwd('C:\\Users\\Meier\\Institut für Statistik Dropbox\\Johanna Meier\\Structural Breaks + DL\\Application')

# load LaTeX font
sysfonts::font_add(family = "CM Roman", regular="C:\\Users\\Meier\\AppData\\Local\\Microsoft\\Windows\\Fonts\\cmunrm.ttf")

# read data
#data <- read.csv('Data\\Frequency of grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_24.09.2022.csv', sep=',', header=TRUE)
#data <- read.csv('Data\\Frequency of grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_30.09.2022.csv', sep=',', header=TRUE)
data <- read.csv('Data\\Frequency of grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_01.10.2022.csv', sep=',', header=TRUE)
#data <- read.csv('Data\\Frequency of grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_02.10.2022.csv', sep=',', header=TRUE)
#data <- read.csv('Data\\Frequency of grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_08.10.2022.csv', sep=',', header=TRUE)
#data <- read.csv('Data\\Frequency of grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_15.10.2022.csv', sep=',', header=TRUE)

# process date info
colnames(data) <- c('time', 'loc1', 'loc2')
data$time <- gsub('.000Z','',data$time)
data$time <- gsub('T',' ',data$time)
data$time <- as.POSIXct(data$time, format='%Y-%m-%d %H:%M:%S')

# first plot
plot(data$time, data$loc1, type='l')
plot(data$time, data$loc2, type='l')

# condense to data by the minute (mean over 60 seconds)
split_min <- cut(as.POSIXct(data$time), breaks = "60 secs")
data$min <- split_min # make minutely vaiable
data_min <- aggregate(. ~ min, data, FUN=mean)
data_min$time <- unlist(lapply(data_min$min, as.character))
data_min <- data_min[-1]
data_min$time <- as.POSIXct(data_min$time, format='%Y-%m-%d %H:%M:%S')

# save data
write.csv(data_min, paste0('Data\\Frequency of grid\\gridfreqs_',as.Date(data_min$time,tz = "Europe/Berlin")[1],'.csv'), row.names=FALSE)

# ts plot
plot(data_min$time, data_min$loc1,type='l')

#--------------------------- break points for loc 1 ---------------------------

# break point estimation + confidence intervals
bp_loc1 <- strucchange::breakpoints(ts(data_min$loc1) ~ 1)
summary(bp_loc1)
ci_loc1 <- confint(bp_loc1)
ci_loc1
data_min$time[ci_loc1$confint]

# plot break points
plot(bp_loc1)
pdf(paste('Figures/', paste(format(data_min$time,"%d-%m-%Y")[1],'Loc1',sep='_'), '.pdf', sep=''), width=7, height=5)
showtext::showtext_auto()
plot(data_min$loc1, type='l', xaxt = "n", xlab='', ylab='Frequency', main=paste(format(data_min$time,"%d/%m/%Y")[1],'Location 1',sep=' - '), family="CM Roman", cex.lab=1.5, cex.main=1.5)
lines(bp_loc1) # break points
lines(ci_loc1, lwd=2) # confidence intervals
axis(1, at = bp_loc1$breakpoints, labels=FALSE)
text(x = bp_loc1$breakpoints,y = par("usr")[3]- 0.8,labels = format(data_min$time,"%H:%M")[bp_loc1$breakpoints],xpd = NA,srt = 35, family="CM Roman", cex=1.5)
showtext::showtext_auto(FALSE)
dev.off()

#--------------------------- break points for loc 2 ---------------------------

# break point estimation + confidence intervals
bp_loc2 <- strucchange::breakpoints(ts(data_min$loc2) ~ 1)
summary(bp_loc2)
ci_loc2 <- confint(bp_loc2)
ci_loc2
format(data_min$time,"%H:%M")[ci_loc2$confint]

# plot break points
plot(bp_loc2)
pdf(paste('Figures/', paste(format(data_min$time,"%d-%m-%Y")[1],'Loc2',sep='_'), '.pdf', sep=''), width=7, height=5)
showtext::showtext_auto()
plot(data_min$loc2, type='l', xaxt = "n", xlab='', ylab='Frequency', main=paste(format(data_min$time,"%d/%m/%Y")[1],'Location 2',sep=' - '), family="CM Roman", cex.lab=1.5, cex.main=1.5)
lines(bp_loc2) # break points
lines(ci_loc2, lwd=2) # confidence intervals
axis(1, at = bp_loc2$breakpoints, labels=FALSE)
text(x = bp_loc2$breakpoints,y = par("usr")[3]- 0.25,labels = format(data_min$time,"%H:%M")[bp_loc2$breakpoints],xpd = NA,srt = 35, family="CM Roman", cex=1.5)
showtext::showtext_auto(FALSE)
dev.off()

#--------------------------- to LaTeX --------------------------------
# print latex table
bp_to_latex <- function(bp){
  ts_names <- names(bp)
  cat('\\begin{tabular}{lll}\n')
  cat('\\toprule\n')
  cat('Data & Estimate & 95\\%-CI \\\\\n')
  cat('\\midrule\n')
  # loop over all time series
  for(i in 1:length(bp)){
    num <- length(bp[[i]])/3
    k <- 1
    # loop over number of break points per time series
    for(j in 1:num){
      if(j==1){
        cat(sprintf('%s & %s & %s , %s\\\\\n', ts_names[i],bp[[i]][k], bp[[i]][k+num], bp[[i]][k+num*2]))
      }else{
        cat(sprintf(' & %s & %s , %s\\\\\n', bp[[i]][k], bp[[i]][k+num], bp[[i]][k+num*2]))
        
      }
      k = k + 1
    }
  }  
  cat('\\bottomrule\n')
  cat('\\end{tabular}')
}

#bp <- list(Date1Loc1 = format(data_min$time,"%H:%M")[ci_loc1$confint], Date1Loc2 = format(data_min$time,"%H:%M")[ci_loc2$confint])
bp <- list(Date2Loc1 = format(data_min$time,"%H:%M")[ci_loc1$confint], Date2Loc2 = format(data_min$time,"%H:%M")[ci_loc2$confint])
sink("Data\\tab_breakpoints_freq.txt")
bp_to_latex(bp)
sink()

