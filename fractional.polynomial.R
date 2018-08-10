# fractional.polynomial.R
# make fractional polynomial
# november 2017

fp = function(x, power){
  # add 1 because of zeros
  if(power==0){y = log(x+1)}
  if(power!=0){y = (x+1)^power}
  # standardise 
  y = y / max(y)
  return(y)
}

inv.fp = function(x, power, max.y){
  # unstandardise 
  x = x * max.y
  # back-transform
  if(power==0){y = exp(x) - 1}
  if(power!=0){y = (x^(1/power)) - 1}
  return(y)
}