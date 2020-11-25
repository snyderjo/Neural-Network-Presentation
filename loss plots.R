setwd("S:\\CO\\Dept\\Cancer\\Team\\Administrative\\Analyst\\John\\gradient descent")

x_seq = seq(2,12,by=.01)

loss = function(x){(x-7)**2}

y_seq = loss(x_seq)

plot(x_seq,y_seq
     , type = 'l'
     ,ylab = expression(paste('Loss = (',hat(y),' - y)^2'))
     ,xlab = expression(hat(y))
     ,xlim = c(0,14))
dev.copy(png,"loss1.png")
dev.off()

abline(v = 8, col = 2,lty = 2)
text(9,20,
     expression(paste(hat(y)," = 8"))
     ,col = 2
)
dev.copy(png,"loss2.png")
dev.off()

abline(h = 1,col = 2, lty = 2)
text(1, 2, "loss = 1",col = 2,new = F)
dev.copy(png,"loss3.png")
dev.off()


points(8,loss(8),col = 3, new=F)
segments(x0=7.5,y0=loss(8) - .5 * 2 
         ,x1=8.5,y1=loss(8) + .5 * 2
         ,new=F,col=3,lwd=2)
text(10, 2,
  expression(paste(delta,"{Loss}/",delta,hat(y), " = -2",sep = ""))
     ,col=3)
dev.copy(png,"loss4.png")
dev.off()