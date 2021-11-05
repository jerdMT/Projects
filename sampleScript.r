my.mean = function(x){
  n = length(x)
  s = 0 
  for(i in 1:n){
    s=s+x[i]
  }
  avg = s/n
  return(avg)
}

a=c(1:100)

trial = my.mean(a)
ans = mean(a)

plot(a)