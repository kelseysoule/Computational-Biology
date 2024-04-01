###################
# Function: gendata
# generates a random data set with different treatment group numbers sizes, means and standard deviations, and puts it all into a single data frame
# input: nGroup = the # of treatment groups
# output: dataframe
# ----------------
gendata <- function(nGroup=3) {
  nSize <- c(rep(NA, times=nGroup))
  nMean <- c(rep(NA, times=nGroup))
  nSD <- c(rep(NA, times=nGroup))
  for (i in 1:nGroup) {
    nSize[i] = sample(20:100,size=1)  # number of observations in each group
    nMean[i] = sample(-10:10,size=1)  # mean temp of each group, centered on modal temperature
    nSD[i] = sample(1:5,size=1)  # standard deviation of each group
  }
  ID <- 1:(sum(nSize)) # id vector for each row
  resVar <- c()
  for (i in 1:nGroup) {
    resVar = c(resVar, rnorm(n=nSize[i],mean=nMean[i],sd=nSD[i]))
  }
  TGroup <- rep(nName,nSize)
  ANOdata <- data.frame(ID,TGroup,resVar)
  return(ANOdata)
}

###################
# Function: stats_analysis
# does an analysis of variance on the dataframe created by gendata
# input: dataframe
# output: model statistics
# ----------------
stats_analysis <- function(data = ANOdata) {
  ANOmodel <- aov(resVar~TGroup,data=data) 
  return(ANOmodel)
  print(summary(ANOmodel))
}


###################
# Function: plot_data
# plots the results of the stats analysis, pooling all treatment groups
# input: stats list
# output: histogram plot
# ---------------- 
plot_data <- function(data=ANOmodel) {
  ANOPlot <- ggplot(data=data, aes(x=resVar, y=after_stat(density))) +
    geom_histogram(color="grey60",fill="thistle1",size=0.2) +  geom_density(linetype="dotted", linewidth=0.75)
  return(ANOPlot)
  print(ANOPlot)
}



###################
# Function: boxplot_data
# plots the results of the stats analysis in a boxplot, showing the counts of group sizes
# input: ANOmodel
# output: boxplot
#-----------------
boxplot_data <- function(data=ANOmodel) {
  group_sizes <- table(ANOmodel$model$TGroup)
  boxPlot <- ggplot(data = data$model) +
    aes(x = TGroup, y = resVar, fill = TGroup) +
    geom_boxplot(show.legend = FALSE) + 
    annotate("text",
           x = as.numeric(factor(names(group_sizes))), # Convert group names to numeric positions
           y = aggregate(resVar ~ TGroup, data = ANOmodel$model, median)[, 2],
           label = paste("n =", group_sizes),
           col = "black",
           vjust = -0.75)  # Add counts above the bars
  return(boxPlot)
  print(boxPlot)
}
