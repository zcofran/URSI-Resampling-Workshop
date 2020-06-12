d <- read.csv("https://raw.githubusercontent.com/zcofran/ursi-resampling-workshop/master/URSI%20ECV%20dataset.csv")

attach(d)

# Compare humans and Neandertals
n <- ecv[group==3]
h <- c(ecv[group==4],ecv[group==5])

boxplot(h, n, border=c('seagreen','lightgoldenrod'), ylab="Brain size", names=c("Humans","Neandertals"))
		points(rnorm(length(h), 1, 0.01), h, cex=0.5, col='seagreen', pch=16)
		points(rnorm(length(n), 2, 0.01), n, cex=1, col='lightgoldenrod', pch=16)

m <- sample(c(h,n))
	abs(mean(m[1:length(h)]) - mean(m[length(h+1):length(m)]))
	boxplot(m[1:length(h)], m[length(h+1):length(m)], border=c('seagreen','lightgoldenrod'), ylab="Brain size", names=c("Random 1-93","Random 94-101"))
	points(rnorm(length(h), 1, 0.01), m[1:93], cex=0.5, col='seagreen', pch=16)
#	points(2, mean(n),pch=16, cex=2)
	points(rnorm(length(n), 2, 0.01), m[94:101], cex=1, col='lightgoldenrod', pch=16)

# Ex. 1: Permutation test for mean difference
xm <- NA	# test statistic to be resampled, can be called anything
for(i in 1:1000)	{
	m <- sample(c(h,n), replace=F)		# permute the combined data
	xm[i] <- abs(mean(m[1:93]) - mean(m[94:101]))	# match empirical sample sizes
	}
hist(xm, main="Permation test", xlab="Difference between means")	# Plot resampled stats
	abline(v=(mean(n)-mean(h)), col= 'seagreen', lwd=2)	# observed difference
	abline(v=quantile(xm, 0.95), col=2, lty=2, lwd=2)	# resampled 95%ile
	length(xm[xm>=77.01269])/length(xm)		# Proportion of resampled ≥ observed
	
# Ex. 2: Bootstrapped CIs
ph <- NA
for (i in 1:1000)	{
	ph[i] <- mean(sample(h,length(n), replace=T))
	}
summary(ph)
hist(ph, main="Bootstrap", xlab="Bootstrapped mean")	# Plot resampled stats
	abline(v=mean(n), col= 'lightgoldenrod', lwd=2)	# observed difference
	abline(v=quantile(ph, 0.95), col=2, lty=2, lwd=2)	# resampled 95%ile
	length(ph[ph>=mean(n)])/length(ph)		# Proportion of resampled ≥ observed

boxplot(h,h,h,h, border='white', names=c("1","2","3...","...N"), main="Resampled humans", xlab="Resampling iteration", ylab="Brain size")
	rug(h,col='seagreen', side=2)
	for(i in 1:4)	{
	points(rnorm(8,i,0.01), sample(h,8, replace=T),pch=16, col='seagreen')
	}

