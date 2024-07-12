#' clustertable
#'
#' @param df  table of dataset, obtaib from the original, that need to do the cluster
#' @param original  table of dataset, only with Quantitative data
#' @param name name of the column, going to do the cluster
#' @param k number of cluster
#'
#' @return a table and the plot of cluster
#' @export
#'
#' @examples df <- data.frame(Length=ddt$LENGTH, Weight = ddt$WEIGHT); clustertable(df, original=ddt, name = "SPECIES")
clustertable <- function(df, original=ddt,name = "SPECIES", k=NULL){
  library (ggplot2)
  if(is.null(df)){
    na.omit(original)
    df <- data.frame(original[,1], original[,2])
  }

  # original refer to original dataset
  # name is the the qualitative variable from original we would like to calculate % in the cluster
  # k is the number of cluster, can either assign or using default
  if (is.null(k)) {
    # default of k = the levels of qualitative variable
    k <- length(unique(original[[name]]))
  }

  # finding k-means clustering
  q <- kmeans(df,centers = k,trace = FALSE)

  # print the detail of center in each cluster
  print(q$centers)
  # the cluster of each observation
  q$cluster -> cluster
  df$Cluster <- as.factor(q$cluster)

  # the first columns in df will be x
  # the second columns in df will be y
  # using color to distinct the cluster
  plot <- ggplot(df, aes(x = df[,1], y = df[,2], color = Cluster)) +
    geom_point(alpha = 0.6, size = 3) +
    geom_point(data = data.frame(x = q$centers[,1], y = q$centers[,2]), aes(x = x, y = y), color = "black", size = 10, shape = 8) +
    labs(title = "K-Means Clustering", x = colnames(df)[1], y = colnames(df)[2]) +
    # using color to distinct the cluster
    scale_color_discrete(name = "Cluster")
  # plot observation with cluster
  print(plot)

  ## print the table show the proportionate of the variables distributed in each clusters
  # get the number of unique variables
  # be the row name of the table
  uniqueVariable <- sort(unique(original[[name]]))
  unmOf_univariable = length(uniqueVariable)
  # create a empty table to store the data
  pre <- matrix(nrow= unmOf_univariable, ncol=1)
  # count the proportionate and create the table
  for(i in 1:k){
    indx <- which(cluster == i)
    tab <- table(factor(original[[name]][indx], levels = uniqueVariable))
    now <- t(t(round(tab/sum(tab) * 100)))
    pre <- cbind(pre, now)
  }
  # the first column is empty, delete it
  proportiontable <- pre[,-1]
  # print out the proportion table
  print(proportiontable)
}
