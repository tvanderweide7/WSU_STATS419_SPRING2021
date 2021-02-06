is.wholenumber = function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}


handShake = function(n=1, plotMe = FALSE) {
  # n must be an integer greater than 0:
  if (n < 1) { 
    stop("n must be greater than 0")
  }
  if (!is.wholenumber(n)) {
    stop("n must be an integer")
  }

  if (plotMe) {
    color = rand_color(n, luminosity = "bright", transparency = 0)
    plot(-n:n, -n:n, type = "n", asp = 1)
    draw.circle(0,0,n/2, border = "purple", lty = 1)
    for (i in 1:n) {
      x0 = ((n/2) * cos((2 * pi * i) / n)) 
      y0 = ((n/2) * sin((2 * pi * i) / n))
      points(x0,y0)
    }
    for (i in 1:n) {
      for (j in 1:(n)) {
        if (i == j) next # We can't shake hands with ourselves
        if (j < i) next # We already shook their hand
          x0 = ((n/2) * cos((2 * pi * i) / n)) 
          y0 = ((n/2) * sin((2 * pi * i) / n))
          x1 = ((n/2) * cos((2 * pi * j) / n)) 
          y1 = ((n/2) * sin((2 * pi * j) / n))
          segments(x0 = x0, y0 = y0, x1 , y1, col = color[i]) # a new color each time
          Sys.sleep(.25) # adding in a delay so we can see what's happening
      } # end of j loop
    } # end of i loop
  } # end of plotting loop
  return(n*(n - 1)/2)
}


#alphaCount = function(string, )