#' @title find_DEGs
#' @description looks at dataframes of FDRs and creates matrix of same size as input df, labeling cells with 1s if FDR < 0.2 and 0 if FDR > 0.2
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
