#' @title find_DEGs
#' @description Creates a data frame of 0s and 1s to represent differential expression based on a specified FDR threshold.
#' @param x A data frame of false discovery rates (FDRs) or p values.
#' @return A data frame with the same dimensions as the input dataframe, containing 0s and 1s.
#' @details FDRs < 0.2 are assigned a 1 to indicate differential gene expression. FDRs > 0.2 are assigned a 0 to indicate no differential gene expression.
#' @examples
#' \dontrun{
#' DEs_labeled<-find_DEGs(pvals)
#' @rdname find_DEGs
#' @export
find_DEGs<-function(x){
  # create empty matrix that is the same size as input DF
  DEs<-matrix(data="NA", nrow=nrow(x), ncol=ncol(x))
  # if FDR for cell is <0.1, then give 1 (1 == DE, 0 == NDE)
  for (i in 1:nrow(x)){
    for (j in 1:ncol(x)){
      if (x[i,j]<0.2){
        DEs[i,j]<-1
      } else {
        DEs[i,j]<-0
      }
    }
  }
  DEs<-as.data.frame(DEs)
  DEs_num = as.data.frame(sapply(DEs, as.numeric)) # turn all 1s and 0s into numeric
  names(DEs_num)<-names(x)
  rownames(DEs_num)<-rownames(x)
  DEs_num2<-data.frame(de_count=rowSums(DEs_num)) # compress counts for all combinations to determine all true DEGs
  ## ^ rowSum > 0--> true DE; 1 --> NDE
  return(DEs_num)
}
