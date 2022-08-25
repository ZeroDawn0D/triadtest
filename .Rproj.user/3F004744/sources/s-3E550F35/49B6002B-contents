library(triad)
load(file="light.rda")

check.time <- function(data){
  time1 <- as.numeric(Sys.time()) * 1000
  data.triad <- del_tri(data)
  time2 <- as.numeric(Sys.time()) * 1000
  plot(data.triad)
  time3 <- as.numeric(Sys.time()) * 1000

  cat("del_tri():", (time2-time1), "milliseconds\n")
  cat("plotting:", (time3-time2), "milliseconds\n")
  return(time2-time1)
}

check.time.interp <- function(data){
  time1 <- as.numeric(Sys.time()) * 1000
  data.trish<- interp::tri.mesh(data)
  time2 <- as.numeric(Sys.time()) * 1000
  plot(data.trish)
  time3 <- as.numeric(Sys.time()) * 1000

  cat("tri.mesh():", (time2-time1), "milliseconds\n")
  cat("plotting:", (time3-time2), "milliseconds\n")
  return(time2-time1)
}

gen.data.interp <- function(multiple=100){
  x_n <- c()
  y_time <- c()
  for(i in 1:10){
    n <- multiple*i
    x <- runif(n,0,n)
    y <- runif(n,0,n)
    data <- data.frame(x,y)
    x_n <- c(x_n,n)
    y_time <- c(y_time, check.time.interp(data))
  }
  linep <- data.frame(x_n, y_time)
  return(linep)
}

gen.data <- function(multiple=100){
  x_n <- c()
  y_time <- c()
  for(i in 1:10){
    n <- multiple*i
    x <- runif(n,0,n)
    y <- runif(n,0,n)
    data <- data.frame(x,y)
    x_n <- c(x_n,n)
    y_time <- c(y_time, check.time(data))
  }
  linep <- data.frame(x_n, y_time)
  return(linep)
}

gen.plots <- function(multiple=100){
  gendat.triad <- gen.data(multiple)
  gendat.interp <- gen.data.interp(multiple)

  double.gendat <- data.frame(x = gendat.interp$x_n,
                              y_triad = gendat.triad$y_time,
                              y_interp = gendat.interp$y_time)
  ggplot(double.gendat, aes(x=x, group = 1))+
    geom_line(aes(y=y_triad, color="triad::del_tri()"))+
    geom_line(aes(y=y_interp, color = "interp::tri.mesh()"))+
    xlab("Number of points")+
    ylab("Time(in ms)")
}
