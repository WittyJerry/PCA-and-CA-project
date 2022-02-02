data <- read.table ("C:/Users/DELL/Dropbox/My PC (JERRYOLATOYAN)/Desktop/PCA analysis/PCA.csv", header=TRUE, sep=",", row.names=1, check.names=FALSE)
summary (data)
library (Factoshiny)
result <- Factoshiny(data)

