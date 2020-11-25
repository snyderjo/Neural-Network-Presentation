library(ggplot2)
library(gridExtra)
library(dplyr)

x = runif(1000,-1,1)

##set the "true" parameters
#choose numbers between -4 and 4, please
intercept = 2
slope = 3

##initialize parameters
#choose numbers between -4 and 4, please
init_intercept = 0
init_slope = 0

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


num_epoch = 200
alpha = .05



#initialize
theta = matrix(c(init_intercept,init_slope),nrow = 2)

cost_history = vector("numeric",num_epoch)
param_history = 
  data.frame(
    inter = vector("numeric",num_epoch)
    ,slope  = vector("numeric",num_epoch)
    )

cost_history[1] = cost(X_mat,y, theta)
param_history[1,] = t(theta)

for (i in 2:num_epoch){
  error = (y - X_mat %*% theta)
  delta = -t(X_mat) %*% error / length(y)
  theta = theta - alpha * delta
  
  cost_history[i] = cost(X_mat,y, theta)
  param_history[i,] = t(theta)
}


data_plot = ggplot(data = grad_desc_data) + 
  aes(x = x, y = y) +
  geom_point() + 
  geom_abline(slope = slope, intercept = intercept
              , colour = 3, size = 1
              ) + 
  geom_abline(slope = init_slope, intercept = init_intercept
              , colour = 2
              , size = .75
              , lty = 2
              , alpha = .5
              )




lossPlot = qplot(1:length(cost_history),cost_history
      ,geom = "line"
      ,xlim = c(0,num_epoch)
      ,ylim = range(cost_history)
      ,ylab = "Loss"
      ,xlab = "epoch"
      )






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





grid.arrange(
  grid::textGrob(paste("Epoch:", 0))
  , data_plot
  , qplot(1:length(cost_history[1:1]),cost_history[1:1]
          ,xlim = c(0,num_epoch)
          ,ylim = range(cost_history)
          ,ylab = "Loss"
          ,xlab = "epoch"
  )
  , lossCountour 
  , ncol = 2
)




plot_sequence = c(seq(5,num_epoch,10))

for (i in plot_sequence){
  grid.arrange(
    grid::textGrob(paste("Epoch:", i - 1))
    
    , data_plot + 
      geom_abline(data = param_history[i,]
                  ,aes(slope = slope, intercept = inter)
                  , colour = 2, size = 1
                  )

    , qplot(1:length(cost_history[1:i]),cost_history[1:i]
            ,geom = "line"
            ,xlim = c(0,num_epoch)
            ,ylim = range(cost_history)
            ,ylab = "Loss"
            ,xlab = "epoch"
            )
    
    , lossCountour +
      geom_segment(data = param_seg[1:i,]
                   ,aes(xend = next_inter,yend = next_slope)
                   ,colour = 2
                   ,arrow = arrow(length = unit(0.01, "npc")
                                  )
                   )
    , ncol = 2
    )
  Sys.sleep(5)
}

print(theta)
#how does this estimate compare to the OLS estimate? 
lm(y~x)
