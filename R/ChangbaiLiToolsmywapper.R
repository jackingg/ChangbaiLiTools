#' Wrapper function for ggplot2 for data data1
#'
#' Computes the mean, variance andata1 sdata1 of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(data1)
#' plotPoint(data1)
plotPoint<-function(x){
  library(magrittr)
 x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_point()
}

#' Wrapper function for ggplot2 for data data1
#'
#' Computes the mean, variance andata1 sdata1 of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(data1)
#' plotArea(data1)
plotArea<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_area()
}

#' Wrapper function for ggplot2 for data data1
#'
#' Computes the mean, variance andata1 sdata1 of a vector
#'
#' @param x data.frame
#'
#' @return ggplot2
#' @export
#' @examples
#' data(data1)
#' plotLine(data1)
plotLine<-function(x){
  library(magrittr)
  x%>% ggplot2::ggplot()+ggplot2::aes(x=x, y=p)+ggplot2::geom_line()
}


#' dplyr Wrapper
#'
#' This is a wrapper for dplyr
#'
#' @param x data.frame
#' 
#' @return data.frame
#' @export
#' @examples 
#' data(data1)
#' mydplyr(data1)
mydplyr <- function(x) {
  library(magrittr)
  xa<- data1 %>% dplyr::mutate(mean = sum(x*p))
  xa
}

