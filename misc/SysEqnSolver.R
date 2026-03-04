#!/usr/bin/env Rscript
#Above line allows code to be run using ./SysEqnSolver.R in terminal

library(insight)

coeffmat <- matrix(
		c(1,-2,1,1),
		nrow=2,
		ncol=2,
		byrow=TRUE
	)
resvec <- c(0,3)

print_color('Coefficient Matrix\n', 'bcyan')
print(coeffmat)

print_color('Inverted Coefficient Matrix\n', 'bcyan')
print(solve(coeffmat))

print_color('Resultant Vector\n', 'bgreen')
print(resvec)

print_color('Variable Vector\n', 'bviolet')
varvec <- solve(coeffmat) %*% resvec
print(varvec)

print_color('Normalized Variable Vector\n', 'bviolet')
print(varvec/sum(varvec))
