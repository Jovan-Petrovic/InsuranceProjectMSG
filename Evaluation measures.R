# function for computing evaluation measures
compute.eval.metrics <- function(cmatrix) {
  TP <- cmatrix[2,2] # true positive
  TN <- cmatrix[1,1] # true negative
  FP <- cmatrix[2,1] # false positive
  FN <- cmatrix[1,2] # false negative
  acc <- sum(diag(cmatrix)) / sum(cmatrix)
  precision <- TP / (TP + FP)
  NPV <- TN / (TN + FN)
  recall <- TP / (TP + FN)
  
  F1 <- 2*precision*recall / (precision + recall)
  c(accuracy = acc, precision = precision, recall = recall, NPV = NPV , F1 = F1)
}