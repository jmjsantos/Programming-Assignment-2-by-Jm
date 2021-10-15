makeCacheMatrix <- function(a = matrix()) 
{
  Jm<-NULL
  set<-function(g) {
    a<<-g
    inver<<-NULL
}
  get<-function() a
  setInver<-function(inverse) Inver<<-inverse
  getInver<-function() Inver
  list(set=set,
       get=get,
       setInver=setInver,
       getInver=getInver)
}
cacheSolve <- function(a, ...) {
  inver<-x$getInver()
  if(!is.null(inver)) {
    message("Matrix being retrieved")
    return(inver)
}
  answer<-a$get()
  inver<-solve(answer,...)
  a$setInver(inver)
  inver
}
