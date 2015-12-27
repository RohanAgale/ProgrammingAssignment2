## This function creates a list of functions for setting and getting a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y     #super-assignment operator for global var
    inv<<-NULL;  #To make inv null for new/changed x matrix
  }
  get<-function(){ x }
  setInv<-function(MInv){
    inv<<-MInv
  }
  getInv<-function() { inv }
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## This function returns a matrix inverse of a matrix. If an inverse is already calculated, then it returns cached copy.
## Otherwise, it returns a newwly calculated inverse matrix and saves it for future reference.

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <-solve(data)
  x$setInv(m)
  m
}
