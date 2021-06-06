#' @title Label_DEs
#' @description interprets rowSums of find_DEGs function to create list of 0 (NDE) and 1s (DEs) and GeneIDs
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname label_DEs
#' @export
label_DEs<-function(x){
  df_sums<-data.frame(DE=rowSums(x))
  DEs<-matrix(data="NA", nrow=nrow(df_sums), ncol=ncol(df_sums))
  for (i in 1:nrow(df_sums)){
    for (j in 1:ncol(df_sums)){
      if (df_sums[i,j]>=1){
        DEs[i,j]<-1
      } else {
        DEs[i,j]<-0
      }
    }
  }
  DEs<-as.data.frame(DEs)
  names(DEs)[names(DEs) == "V1"] <- "labels" # rename new column
  rownames(DEs)<-rownames(df_sums)
  DEs$GeneID<-rownames(DEs)
  return(DEs)

}
