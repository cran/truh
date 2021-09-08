## ----class.source="bg-warning",fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----

n = 2000
d = 2

#Sampling the baseline (uninfected)
set.seed(1)
p<-runif(n,0,1)
set.seed(10)
U<- (p<=0.3)*matrix(rnorm(d*n),n,d)+
  (p>0.3 & p<=0.6)*cbind(matrix(rnorm(n),n,1),
                matrix(rnorm(n,-4),n,1))+
  (p>0.6)*cbind(matrix(rnorm(n,4),n,1),
          matrix(rnorm(n,-2),n,1))

## ----class.source="bg-warning",fig.height = 4, fig.width = 6, fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----
# Sampling the treated (infected)
m = 500
set.seed(50)
V1<-cbind(matrix(rnorm(m,4),m,1),
          matrix(rnorm(m,-2),m,1))

#Scatter plot of the data
grp = c(rep('Baseline',n),
                    rep('Treated',m))
plot(c(U[,1],V1[,1]), c(U[,2],V1[,2]),
     pch = 19,
     col = factor(grp),
     xlab = 'X_1',
     ylab = 'X_2')

# Legend
legend("topright",
       legend = levels(factor(grp)),
       pch = 19,
       col = factor(levels(factor(grp))))

## ----class.source="bg-warning",fig.height = 4, fig.width = 6,fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----
# Sampling the treated (infected)
m = 500
set.seed(20)
q<-runif(m,0,1)
set.seed(50)
V2<-(q<=0.5)*cbind(matrix(rnorm(m,2),m,1),
          matrix(rnorm(m,-2),m,1))+
  (q>0.5)*cbind(matrix(rnorm(m,3),m,1),
          matrix(rnorm(m,3),m,1))

#Scatter plot of the data
plot(c(U[,1],V2[,1]), c(U[,2],V2[,2]),
     pch = 19,
     col = factor(grp),
     xlab = 'X_1',
     ylab = 'X_2')

# Legend
legend("topright",
       legend = levels(factor(grp)),
       pch = 19,
       col = factor(levels(factor(grp))))

## ----class.source="bg-warning",fig.height = 4, fig.width = 6,fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----
# Sampling the treated (infected)
m = 500
set.seed(20)
q<-runif(m,0,1)
set.seed(50)
V3<-(q<=0.8)*matrix(rnorm(d*m),m,d)+
  (q>0.8 & q<=0.9)*cbind(matrix(rnorm(m),m,1),
                matrix(rnorm(m,-4),m,1))+
  (q>0.9)*cbind(matrix(rnorm(m,4),m,1),
          matrix(rnorm(m,-2),m,1))

#Scatter plot of the data
plot(c(U[,1],V3[,1]), c(U[,2],V3[,2]),
     pch = 19,
     col = factor(grp),
     xlab = 'X_1',
     ylab = 'X_2')

# Legend
legend("topright",
       legend = levels(factor(grp)),
       pch = 19,
       col = factor(levels(factor(grp))))
    

## ----class.source="bg-warning",fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----
library(truh)
truh.1 = truh(V1,U,B=200)
truh.1$pval

## ----class.source="bg-warning",fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----
library(truh)
truh.2 = truh(V2,U,B=200)
truh.2$pval

## ----class.source="bg-warning",fig.align="center",eval=TRUE,echo=TRUE,message=FALSE,warning=FALSE----
library(truh)
truh.3 = truh(V3,U,B=200)
truh.3$pval

