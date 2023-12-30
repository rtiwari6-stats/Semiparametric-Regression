#Read csv file input
skeena = read.csv("Skeena_River.csv")

# Perform a linear regression of Y on X. 
#Y=spawners, X=recruits
linreg = lm(Recruits ~ Spawners, data = skeena)
summary(linreg)
plot(skeena$Spawners, skeena$Recruits)
abline(lm(Recruits ~ Spawners, data = skeena))

#Check for heteroscedasticity
plot(fitted(linreg),abs(residuals(linreg)),xlab="fitted values",
     ylab="absolute residuals",pch='*',cex=2,main='Heteroscedasticity Check')
plot(skeena$Spawners,abs(residuals(linreg)),xlab="Spawners",
     ylab="absolute residuals",pch='*',cex=2,main='Heteroscedasticity Check')

# regress log(Y) on log(X)
linreg_log = lm(log(Recruits) ~ log(Spawners), data = skeena)
summary(linreg_log)
#which is 1951 and which is 1955?
plot(log(skeena$Spawners), log(skeena$Recruits))
abline(lm(log(Recruits) ~ log(Spawners), data = skeena))

#Check for heteroscedasticity
plot(fitted(linreg_log),abs(residuals(linreg_log)),xlab="fitted values",
     ylab="absolute residuals",pch='*',cex=2,main='Heteroscedasticity Check')
plot(log(skeena$Spawners),abs(residuals(linreg_log)),xlab="log(Spawners)",
     ylab="absolute residuals",pch='*',cex=2,main='Heteroscedasticity Check')

#	Plot the fitted lines of the absolute residuals against log(X). 
hetero_mod = lm(abs(residuals(linreg_log))~log(Spawners), data=skeena)
summary(hetero_mod)
par(mfrow=c(1,1))
plot(log(skeena$Spawners),abs(residuals(linreg_log)),xlab="log(Spawners)",
     ylab="absolute residuals",pch='*',cex=2,main='Heteroscedasticity check')
lines(log(skeena$Spawners),fitted(hetero_mod),lwd=3,col="blue")

#On page 36 of the book, there is a command polygon, which allows you to form 
#a confidence interval which is shaded, unlike what I did in class. 
#Give it a try, and report your plot
t = qt(0.975,df=length(skeena$Spawners)-2)
par(mfrow=c(1,1))
ord = order(skeena$Spawners)
x = log(skeena$Spawners[ord])
y = log(skeena$Recruits[ord])
pred = predict(lm(y~x),newdata=as.data.frame(skeena$Spawners),se.fit=TRUE)
plot(x,pred$fit,type="n",xlab="log(Spawners)",ylab="log(Recruits)",
     main="Skeena River Predictions")
upperCI = pred$fit+t*pred$se.fit
lowerCI = pred$fit-t*pred$se.fit
polygon(x=c(x, rev(x)), y=c(upperCI, rev(lowerCI)), col="gray", border=NA)
lines(x,pred$fit,col="blue",lwd=5)

#lines plot for CI
plot(x,pred$fit,type="n",xlab="log(Spawners)",ylab="log(Recruits)",
     main="Skeena River Predictions")
lines(x, upperCI, col ="red", lwd=2)
lines(x, lowerCI, col ="red", lwd=2)
lines(x,pred$fit,col="blue",lwd=5)






