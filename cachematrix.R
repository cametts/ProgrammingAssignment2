## Calculate inverse of a given matrix and store inverse in cache
## Return cached inverse if applicable 

## establish functions to store and retrieve matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #establish variable for matrix inverse
  set <- function(y) { #establish the matrix 
    x <<- y
    m <<- NULL #reset inverse variable
  }
  get <- function() x #return the matrix 
  setinverse <- function(solve) m <<- solve #set the inverse
  getinverse <- function() m #return the inverse 
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## calculate or retrieve pre-calculated inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse() #return the inverse 
  if(!is.null(m)) { #true if the inverse has already been calculated 
    message("getting cached data")
    return(m) #return previously calculated inverse from cache
  }
  data <- x$get() #set data equal to the matrix
  m <- solve(data, ...) #calculate the inverse
  x$setinverse(m) #set inverse (to retrieve from cache later)
  m #return inverse 
}
