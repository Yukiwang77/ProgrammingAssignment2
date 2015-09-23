## Matrix inversion can be time consuming when dealing with large amount of data or computing repeatedly. 
## The following function caches the inverse of a matrix in the memory instead of recomputing. 
## Since this assignment assumes the matrix supplied is always invertible, there is no check for the input matrix.

## makecacheMatrix is a function to create a list to:
## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse matrix
## 4 get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
m<- NULL
set<-function(y){
    x<<-y
    m<<-NULL
}
get<-function() x
setinv<-function(inverse) m <<-inverse
getinv<-function() m 
list( set = set, get = get,  setinv=setinv, getinv=getinv)

}

## The following function calculate the inverse of the matrix created by makeCacheMatrix.
## It checks if the inverse of the matrix has been calculated. 
## If so, it retrieves the inverse from cache and skips recomputation.
## If not, it calculates the inverse and set the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
m<-x$getinv()
if(!is.null(m)){
  message("getting cached data")
  return(m)
}
data<-x$get()
m<-solve(data, ...)
x$setinv(m)
m
## Return a matrix that is the inverse of 'x'
}
##TestRun
## To randomly generate a square matrix:
## x=matrix(1:4,2,2)
## > x
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## To create the sepcial list of matrix
## > m=makeCacheMatrix(x)
## m$get()
##       [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## To solve the matrix for the first time when the cache doesn't store the inverse result
## > cacheSolve(m)
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## To call the inverse matrix for the second time when the inverse result already exist in cache
## > cacheSolve(m)
## getting cached data
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5