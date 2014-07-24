
library(shiny)


# the datasets
circle <- read.csv("./circles.csv",
                   stringsAsFactors=FALSE,
                   colClasses="numeric")

moon <- read.csv("./moons.csv", 
                 stringsAsFactors=FALSE,
                 colClasses="numeric")



add.poly.features <- function(x.mat, degree=2){
  new.mat <- matrix(1, nrow=nrow(x.mat))
  for (i in 1:degree){
    for (j in 0:i){
      new.mat <- cbind(new.mat, (x.mat[,1]^(i-j) * (x.mat[,2]^j)))
    }
  }
  return(new.mat)
}

hypothesis.function <- function(param.vec, x.mat){
  zed <- x.mat %*% matrix(param.vec)
  return(1 / (1 + exp(-zed)))
}

get.gradient <- function(param.vec, x.mat, y.vec, lambda=0){
  m <- nrow(x.mat)
  modtheta <- param.vec
  modtheta[1] <- 0
  the.hyp <- hypothesis.function(param.vec, x.mat)
  gradient <- (t(x.mat) %*% (the.hyp - y.vec) + lambda*modtheta) / m
  return(gradient)
}

cost.function <- function(param.vec, x.mat, y.vec, lambda=0){
  m <- nrow(x.mat)
  the.hyp <- hypothesis.function(param.vec, x.mat)
  cost <- (((t(-y.vec) %*% log(the.hyp)) - (t(1-y.vec) %*% log(1-the.hyp))) / m) +
    ((lambda / (2*m)) * sum(param.vec[2:length(param.vec)] ^ 2))
  
  return(cost)
}


shinyServer(function(input, output) {
  
  output$da.plot <- renderPlot({
    
    if(input$pattern=="moon")
      da.dataset <- moon
    else
      da.dataset <- circle
    
    da.lambda <- input$lambda
    da.degree <- input$degree
    
    design.mat <- add.poly.features(da.dataset[,c(1,2)], degree=da.degree)
    
    result <- optim(par=rep(0, ncol(design.mat)),
                    cost.function, 
                    get.gradient,
                    x.mat=design.mat,
                    y.vec=as.matrix(da.dataset[,3]),
                    lambda=da.lambda,
                    method=input$opt)
    
    predictions <- hypothesis.function(result$par, design.mat)
    accuracy <- paste0(round(sum(round(predictions) ==
                                       da.dataset[,3]) / 3, 2), "%")
    
    thex1 <- da.dataset[,1]
    thex2 <- da.dataset[,2]
    somex <- seq(min(thex1), max(thex1), by=.05)
    somex2 <- seq(min(thex2), max(thex2), length.out=length(somex))
    
    z <- matrix(0, nrow=length(somex), ncol=length(somex))
    
    for (i in 1:length(somex)){
      for (j in 1:length(somex)){
        keep <- add.poly.features(t(matrix(c(somex[i], somex2[j]))), da.degree)
        z[i, j] <- as.matrix(keep) %*% result$par
      }
    }
    
    plot(da.dataset$X2 ~ da.dataset$X1,  pch=20, 
         col=c("red","green3")[da.dataset$Y+1],
         xlab="X1", ylab="X2")
    title(paste("Degree:", da.degree,
                " -  Lambda:", da.lambda,
                "     -      Accuracy:", accuracy))
    
    contour(somex, t(somex2), z, nlevels=1, add=TRUE, drawlabels=FALSE)
  })
})