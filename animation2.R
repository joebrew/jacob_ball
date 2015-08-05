deltaT <- 1/24
x <- seq(0,14,by=deltaT)
num.sims <- 100      # number simulations to run
N <- 20              # population size
init.infect <- I <- 1      # intial number of hammies infected
my_directory <- '~/Desktop/temporary' # place to save pngs
S <- N - I
R <- N - S - I

plot(0,0, type ="n", xlim = c(min(x), max(x)),
     ylim = c(0, N), bty = "n", xlab = "Days Since Introduction", ylab = "Number of Hamsters", cex.main=1,
     main = "d)  N=20, R0=1")

cum.inc <- 0
## CDF of transmission and recovery fits----to be used in rbinom()
haz.trans <- 0.2457601
haz.recov <- 0.22
## set up matrix to get final size distribution


setwd(my_directory)
for(ss in 1:num.sims)              # do num.sims outbreaks
{
  
  image_name <- ss
  while(nchar(image_name) <= 5){
    image_name <- paste0(0, image_name)
  }
  
  sim4 <- data.frame(S, I, R, cum.inc)  # creates a data frame that tells you numbers of S I and R at each time step
  for(ii in 2:length(x))        # run through time series start at 2 since 1 is initial conditions
  {
    # use CDF * I/N as the probability in rbinom()
    new.inf <- rbinom(1, sim4$S[ii-1] , pexp(x[ii]-x[ii-1], haz.trans*sim4$I[ii-1]) )   # lambda * I where lambda is estimated FOI
    new.recov <- rbinom(1, sim4$I[ii-1], pexp(x[ii]-x[ii-1], haz.recov))   # Number new recovered = gamma*I (aka p.recov*I)
    temp.S <- sim4$S[ii-1] - new.inf
    temp.I <- sim4$I[ii-1] + new.inf - new.recov
    temp.R <- sim4$R[ii-1] + new.recov
    sim4 <- rbind(sim4, c(temp.S, temp.I, temp.R, N-temp.S))
    #cum.inc.matrix.4[ss,ii] <-cum.inc.matrix.4[ss,ii] + new.inf
  } 
  
  lines(x, sim4[,"S"],col = "blue") # add current simulation's susceptible time series to plot
  lines(x, sim4[,"I"], col="red")
  lines(x, sim4[,"R"], col="green")
  lines(x, sim4[,"cum.inc"])
  
  dev.copy(png, filename = paste0(my_directory, '/', image_name, '.png'))
  dev.off()
}


# You now have an ordered sequence of .pngs in your my_directory
# and you can use some gif software to put them all together
# In linux you can cd into my_directory, and then run the following via command line:
# > convert -delay 10 -loop 0 *.png result.gif
# This will generate a gif called result.gif using all of the pngs in that folder
# In Mac / Windows, you're on your own.

# If you want a video (mp4) instead of a gif, you can use a progra mlike ffmpeg
# Cd into my_directory, and then run the following:
# > ffmpeg -start_number 000001 -r 5 -i %6d.png my_video.mp4
