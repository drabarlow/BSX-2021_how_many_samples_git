# Script for carrying out power analysis for the lizard experiment

# declare variables
nsamp = 30
meanA = 10
meanB = 9
stdev = 1
x = 0
nreps = 100

# here I am just making an empty plot with sensible axes and labels
plot(1, type="n", xlim=c(0, 3), ylim=c(8, 11),
     xlab="population", ylab="snout-vent length (cm)",
     axes=FALSE
)

axis(1, at=c(1,2), labels=c("A", "B"), lwd=-1, lwd.ticks=1)
axis(2)

# here is the loop to repeat the analysis for nreps
for (i in 1:nreps) {

	# take nsamps from a normal distribution centred on true mean
	lizA <- rnorm(nsamp, meanA, stdev)
	lizB <- rnorm(nsamp, meanB, stdev)

	# add sample means to plot
	points(jitter(1), mean(lizA))
	points(jitter(2), mean(lizB))

	# run t test, increment x if significant
	my_t <- t.test(lizA, lizB)
	if(my_t$p.value < 0.05) {x = x + 1}
}

# add title reporting outcome of t tests
title(main=paste("p < 0.05 in", x, "out of", nreps, "tests"))
