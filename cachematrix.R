## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#creates a special "matrix", which is really a list 
#containing a function to
#set the matrix
#get the matrix
#set the inverse matrix
#get the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setinv <- function(m_in) inv_mat <<- m_in
  getinv <- function() inv_mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
#The following function calculates the inverse of the matrix 
#created with the above function. However, it first checks to 
#see if the inverse has already been calculated. If so, it 
#gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the matrix and stores  
#it in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
}
