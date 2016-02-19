##In programming some operations are processor expensive "take a long time",
##so therefore it is a good idea calculate just one time and store the result, 
##this is the case of matrix inversion.The functions  makeCacheMatrix and 
##cacheSolve are used to cache the inverse of a matrix.

#' @param x A matrix
#' @return return a list containing (set, get , setinverse, getinverse)
#' @examples
#' x = matrix(c(2,3,5,0,0,1,1,0,1),3,3)
#' m = makeCacheMatrix(x)
#' m$get()
#'      [,1] [,2] [,3]
#'[1,]    2    0    1
#'[2,]    3    0    0
#'[3,]    5    1    1
makeCacheMatrix <- function(x = matrix()) {
  matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inverse <<- inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


# cacheSolve first checks if the inverse has already been computed. 
# If it is true, cacheSolve gets the result and skips the
# computation. If it is false, it computes the inverse, 
# sets the value in the cache
#' @param x A list containing (set, get , setinverse, getinverse)
#' @return return a matrix that is the inverse of x
#' @examples
#' > cacheSolve(m)
#'[,1]       [,2] [,3]
#'[1,]    0  0.3333333    0
#'[2,]   -1 -1.0000000    1
#'[3,]    1 -0.6666667    0
#'> cacheSolve(m)
#'getting cached data.
#'[,1]       [,2] [,3]
#'[1,]    0  0.3333333    0
#'[2,]   -1 -1.0000000    1
#'[3,]    1 -0.6666667    0
cacheSolve <- function(x, ...) {
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data, ...)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
