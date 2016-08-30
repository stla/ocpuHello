#' Hello World
#'
#' Basic hello world function to be called from the demo app
#'
#' @export
#' @param myname your name. Required.
hello <- function(name = ""){
  if(name == ""){
    stop("Tell me your name!")
  }
  list(
    message = paste("hello", name, "!")
  )
}

#' Average
#'
#' @param x vector
#' @export
average <- function(x) if(mode(x)=="numeric") mean(x) else ""

#' Identity
#'
#' @param x argument
id <- function(x) identity(x)


#' Reverse a character string
#'
#' @export
#' @param string the string to be reversed
reverse <- function(string){
  paste0(rev(rawToChar(charToRaw(string), multiple = TRUE)), collapse="")
}

#' Get a dataframe
#'
#' @export
getdf <- function(){
  df <- iris[1:10,]
  write.csv(df, file="df.csv")
  return(df)
}

#' Get list for datatables
#' @export
getDTlist <- function(){
  df <- data.frame(Name=LETTERS[1:5], Value=25:21)
  list(
    data = df,
    columns = data.frame(data=colnames(df))
  )
}

#' Get list for datatables
#' @export
getIris <- function(){
  df <- iris
  colnames(df) <- sub(".", "_", colnames(df), fixed=TRUE)
  list(
    data = df,
    columns = data.frame(data=colnames(df))
  )
}

#' Get list for jsonTable
#' @export
getMtcars <- function(){
  df <- mtcars
  list(
    data = df,
    columns = colnames(df),
    list = as.list(df)
  )
}

#' Get iris
#' @export
getIris2 <- function(){
  df <- iris
  colnames(df) <- sub(".", "_", colnames(df), fixed=TRUE)
  return(df)
}

#' Knit regression
#' @export
knitRegression <- function(dat, conflevel="95", filetype="word_document"){
  dat <- as.data.frame(dat)
  conflevel <- as.numeric(conflevel)/100
  out <- rmarkdown::render(system.file(package = "opencpuHello", "templates", "regression.Rmd"),
                           output_dir = getwd(),
                           output_format = filetype,
                           params = list(set_title="Regression analysis"))
  return(basename(out))
}
