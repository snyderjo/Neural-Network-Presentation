library(dplyr)
library(ggplot2)
library(gganimate)
library(gridExtra)


##set the number of observations
nObs = 1000

##set the "true" parameters
#choose numbers between -4 and 4, please
intercept = 2
slope = 3

##initialize parameters
#choosing a randomly non-zero value
init_intercept = rnorm(1)/16
init_slope = rnorm(1)/16





x = runif(nObs,-1,1)

y = slope * x + intercept + 0.2 * rnorm(length(x))

grad_desc_data = data.frame(inter = 1, x = x, y = y)

X_mat = as.matrix(grad_desc_data[,c("inter","x")])


p = ggplot(data = grad_desc_data) + 
  aes(x = x, y = y) +
  geom_point() + 
  geom_abline(slope = slope, intercept = intercept
                , colour = 3, size = 1)

p + geom_abline(slope = init_slope, intercept = init_intercept, col = 2)


cost = function(X,y,theta) {
  mean((y - X %*% theta)^2)
}


num_epoch = 100
alpha = .1



#initialize
theta = matrix(c(init_intercept,init_slope),nrow = 2)

cost_history = 
  data.frame(
    epoch = vector("integer",num_epoch + 1)
    ,cost = vector("numeric",num_epoch + 1)
    )
param_history = 
  data.frame(
    epoch = vector("integer",num_epoch + 1)
    ,inter = vector("numeric",num_epoch + 1)
    ,slope  = vector("numeric",num_epoch + 1)
    )

cost_history[1,] = c(0,cost(X_mat,y, theta))
param_history[1,] = c(0,t(theta))

for (i in 1:num_epoch){
  error = (y - X_mat %*% theta)
  delta = -t(X_mat) %*% error / length(y)
  theta = theta - alpha * delta
  
  cost_history[i+1,] = c(i,cost(X_mat,y, theta))
  param_history[i+1,] = c(i,t(theta))
}



##plots
dataPlot = ggplot(data = grad_desc_data) + 
  aes(x = x, y = y) +
  geom_point() + 
  geom_abline(slope = init_slope, intercept = init_intercept
              , colour = 2
              , size = .75
              , lty = 2
              , alpha = .5
              ) +
  geom_abline(intercept = intercept,slope = slope,col = 3,size = 1) 


dataPlot_anim = dataPlot +
  geom_abline(data = param_history,aes(slope = slope, intercept = inter)
              , colour = 2, size = 1
  ) + 
  labs(title="Epoch: {frame_time}") +
  transition_time(epoch)



lossPlot = 
  ggplot(data = cost_history, aes(x=epoch,y=cost)) +
  geom_blank()+
  geom_point(x=cost_history$epoch[1],y=cost_history$cost[1],col="red")


lossPlot_anim = lossPlot + 
  geom_line(col="red") + 
  transition_reveal(epoch)


##contour plot creation
axis_range = seq(-4,4,.25)

cost_df = expand.grid(inter = axis_range, slope = axis_range)

cost_df$cost = sapply(
  split(cost_df,1:dim(cost_df)[1])
  ,function(theta) do.call(cost,list(X = X_mat, y = y,theta = as.matrix(t(theta))))
  )

param_seg = param_history %>% mutate(next_inter=lead(inter),next_slope=lead(slope))


lossCountour = ggplot(data = cost_df, aes(x=inter,y=slope)) + 
  geom_contour(aes(z = cost, colour = stat(level))) + 
  geom_point(data = data.frame(inter = intercept,slope = slope)
             , colour = 3
             , size = 2
             ) +
  geom_point(data = data.frame(inter = init_intercept,slope = init_slope)
             , colour = 2
             ) 



lossCountour_anim = lossCountour + 
  labs(title="Epoch: {frame_along}") +
  geom_line(data = param_history, colour = 2) +
  transition_reveal(epoch)


#initial state
grid.arrange(
  dataPlot
  , lossPlot
  , lossCountour 
  , ncol = 2
)






#graph gradient descent
lossPlot_anim
lossCountour_anim
dataPlot_anim


print(theta)
#how does this estimate compare to the OLS estimate? 
lm(y~x)
