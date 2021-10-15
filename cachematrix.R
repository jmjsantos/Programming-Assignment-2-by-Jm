makeCacheMatrix <- function(u = matrix()) 
{
  Inver<-NULL
  set<-function(g) {
    u<<-g
    inver<<-NULL
}
  get<-function() u
  setInver<-function(inverse) {Inver<<-inverse}
  getInver<-function() Inver
  list(set=set,
       get=get,
       setInver=setInver,
       getInver=getInver)
}
cacheSolve <- function(u, ...) {
  inver<-u$getInver()
  if(!is.null(inver)) {
    message("Matrix being retrieved")
    return(inver)
}
  answer<-u$get()
  inver<-solve(answer,...)
  u$setInver(inver)
  inver
}
