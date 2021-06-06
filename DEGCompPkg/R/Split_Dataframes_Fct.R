#' @title split_df
#' @description Creates data frame out of each column in a given data frame.
#' @param x A data frame
#' @param z A label to be added as a suffix to the name of each created data frame.
#' @return A data frame for each column from the input data frame.
#' @details This function creates a data frame in the Global Env. for each column in the input data frame. In case multiple columns have similar names, or you would like to keep track of the dataframe where these columns originated from, you can add a suffix to the end of the data frame name.
#' @examples
#' \dontrun{
#' split_df(pval_from_edgeR, "er")
#' @rdname split_df
#' @export
split_df<-function(x, z=NULL){
  y<- split.default(x, names(x))
  x<-x[ , order(names(x))]
  for(i in seq_along(x)){
    df<-paste(names(x[i]), z, sep="_")
    #print(df)
    assign(df, y[[i]], envir = .GlobalEnv)

  }

}
