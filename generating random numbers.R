#gnerate random numbers between 0 and 1
x=1:100
y=rnorm(x,0,1)
y1= rnorm(x,1,1)
#plotting pdf of the points
plot(density(y))
lines(density(y1), add=T, col='red', lwd=2)
