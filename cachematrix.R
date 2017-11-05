### MakeCacheMAtrix function is use to store a matrix and caches matrix inverse. 


MakeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y) {
    x <<- y
    j <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) j <<- inverse
  getinverse <- function() j
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### CacheSolve function computes the inverse return by MakeCacheMatrix. If inverse already calculated then this function retrieve the inverse form cache 

CacheSolve <- function(x, ...) {
  j <- x$getinverse()
  if (!is.null(j)) {
    message("Getting Cached Data")
    return(j)
  }
  data <- x$get()
  j <- solve(data, ...)
  x$setinverse(j)
  j
}
#########################################
################ Test Run ###############
#########################################
a<-matrix(1:4,2,2) # Creat Matrix 2x2

b<- MakeCacheMatrix(a)
CacheSolve(b)

[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5

CacheSolve(b)

Getting Cached Data

[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5