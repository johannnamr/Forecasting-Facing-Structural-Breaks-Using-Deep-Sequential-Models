# set working directory
setwd('C:\\Users\\Meier\\Institut für Statistik Dropbox\\Johanna Meier\\Structural Breaks + DL\\Application')

# load LaTeX font
sysfonts::font_add(family = "CM Roman", regular="C:\\Users\\Meier\\AppData\\Local\\Microsoft\\Windows\\Fonts\\cmunrm.ttf")

# read data
#data <- read.csv('Data\\Grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_18.09.2022.csv', sep=';', header=TRUE)
#data <- read.csv('Data\\Grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_01.10.2022.csv', sep=';', header=TRUE)
#data <- read.csv('Data\\Grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_08.10.2022.csv', sep=';', header=TRUE)
data <- read.csv('Data\\Grid\\energy-charts_Messung_der_Netzspannung_und_-frequenz_am_08.01.2023.csv', sep=';', header=TRUE)

# process date info
colnames(data) <- c('time', 'freq', 'voltage')
data$time <- gsub('.000Z','',data$time)
data$time <- gsub('T',' ',data$time)
data$time <- as.POSIXct(data$time, format='%Y-%m-%d %H:%M:%S')

# first plot
plot(data$time, data$freq, type='l')
plot(data$time, data$voltage, type='l')

# condense to data by the minute (mean over 60 seconds)
split_min <- cut(as.POSIXct(data$time), breaks = "60 secs")
data$min <- split_min # make minutely vaiable
data_min <- aggregate(. ~ min, data, FUN=mean)
data_min$time <- unlist(lapply(data_min$min, as.character))
data_min <- data_min[-1]
data_min$time <- as.POSIXct(data_min$time, format='%Y-%m-%d %H:%M:%S')
data_min <- subset(data_min, select = -c(freq) )

# save data
write.csv(data_min, paste0('Data\\Grid\\grid_',as.Date(data_min$time,tz = "Europe/Berlin")[1],'.csv'), row.names=FALSE)

# ts plot
plot(data_min$time, data_min$voltage,type='l')

#--------------------------- break points  ---------------------------

# break point estimation + confidence intervals
bp <- strucchange::breakpoints(ts(data_min$voltage) ~ 1)
summary(bp)
ci <- confint(bp)
ci
data_min$time[ci$confint]

#------------------------ break point plots --------------------------

# plot break points
plot(bp)
pdf(paste('Figures/breakpoints_voltage_', format(data_min$time,"%d-%m-%Y")[1], '.pdf', sep=''), width=7, height=5)
showtext::showtext_auto()
plot(data_min$voltage, type='l', xaxt = "n", xlab='Time', ylab='Voltage (V)', main=format(data_min$time,"%d/%m/%Y")[1], family="CM Roman", cex.lab=1.5, cex.main=1.5, ylim = c(234.5,241.5))
lines(bp) # break points
lines(ci, lwd=2) # confidence intervals
axis(1, at = bp$breakpoints, labels=FALSE)
text(x = bp$breakpoints,y = par("usr")[3]- 0.8,labels = format(data_min$time,"%H:%M")[bp$breakpoints],xpd = NA,srt = 35, family="CM Roman", cex=1.2)
showtext::showtext_auto(FALSE)
dev.off()

#----------------------------- ADF test  -----------------------------

sink(paste('Data/tab_adf_voltage_', format(data_min$time,"%d-%m-%Y")[1], '.txt', sep=''))
for(i in 1:(length(bp$breakpoints)+1)){
  if(i==1){
    print(tseries::adf.test(data_min$voltage[i:bp$breakpoints[i]]))
  }else{
    if(i>1 && i<=length(bp$breakpoints)){
      print(tseries::adf.test(data_min$voltage[bp$breakpoints[i-1]:bp$breakpoints[i]]))
      }else{
        print(tseries::adf.test(data_min$voltage[bp$breakpoints[i-1]:length(data_min$voltage)]))
    }
  }
}
sink()

#---------------------break points to LaTeX --------------------------------
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
        cat(sprintf('%s & %s & %s , %s\\\\\n', ts_names[i], bp[[i]][k+num],bp[[i]][k], bp[[i]][k+num*2]))
      }else{
        cat(sprintf(' & %s & %s , %s\\\\\n', bp[[i]][k+num], bp[[i]][k], bp[[i]][k+num*2]))
        
      }
      k = k + 1
    }
  }  
  cat('\\bottomrule\n')
  cat('\\end{tabular}')
}

bp <- list(Date = format(data_min$time,"%H:%M")[ci$confint])
sink(paste('Data/tab_breakpoints_voltage_', format(data_min$time,"%d-%m-%Y")[1], '.txt', sep=''))
bp_to_latex(bp)
sink()

