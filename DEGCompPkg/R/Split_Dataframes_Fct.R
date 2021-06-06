#' @title split_df
#' @description split data frame columns into separate dataframes, and adds suffix to dfs' names in case multiple Dfs that were created have the same names
#' @param x PARAM_DESCRIPTION
#' @param z PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
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
