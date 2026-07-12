"CIr" <-
  function (r, n, level=.95)
  {
    
    z <- r2z(r)
    uciz <- CIz(z, n, level)[2]
    lciz <- CIz(z, n, level)[1]
    ur <- z2r(uciz)
    lr <- z2r(lciz)
    mat <- list(lr,ur)
    return(as.numeric(mat))
  }

"CIz" <-
  function (z, n, level=.95) 
  { 
    noma <- 1-level
    sez <- SEz(n)
    zs <- - qnorm(noma/2)
    mez <- zs*sez
    lcl <- z - mez
    ucl <- z + mez
    mat <- list(lcl, ucl)
    return(as.numeric(mat))
  }

"SEz" <-
  function(n) { 1/sqrt(n-3) }

"r2z" <-
  function (x) { .5 * log((1+x)/(1-x)) }

"z2r" <-
  function (x) { (exp(2*x)-1)/(exp(2*x)+1) }

