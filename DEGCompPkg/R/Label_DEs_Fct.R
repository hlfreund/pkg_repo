#' @title Label_DEs
#' @description Uses the output from find_DEGs() to create an array with individual 0 or 1 labels per gene ID.
#' @param x A data frame of 0s and 1s.
#' @return An array with two columns, labels for 0 or 1, and GeneIDs that correspond to each label.
#' @details This function accepts a data frame containg 0s and 1s. For the purpose of this function, 1s indicate differentail gene expression and 0s indicate no differeential gene expression. This function takes the rowSums() of 0s and 1s per gene IDs, and assigns any gene with a sum > 1 with a 1 to indicate differential gene expression. Genee IDs with a sum of 0 are labeled with a 0 to represent no differential gene expression.
#' @examples
#' \dontrun{
#' common_DEs<-label_DEs(DEs_labeled)
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
