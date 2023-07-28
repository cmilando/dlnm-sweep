# DLNM sweep
A set of functions to enable DLNM parameter sweep

# so you are going to make a function and then use it
# need to be clever about writing this so its not annoying
# will likely need some for-loops on the other side
varlist = c('Tmax_C', 'Tmin_C')
argvar_list = c()
lag_list = c()
arglag_list = c()

# it should look as similar to crossbasis create as possible
# that is really what you are creating is a list of crossbases
# basically you don't want to do any of this yourself, 
# just make a new combination based on anything that has a sublist item that
# has length > 1
# so what you want is a function that is crossbasis_expand
library(dlnm)
var_list = c('Tmax_C', 'Tmin_C')
argvar_list = list(ns = list(model = 'ns', df = 1:3),
                   bs = list(model = 'bs', df = 1:3))
lag_list = c(5, 30)
arglag_list = list(poly = list(model = 'poly', df = 1:3))

# then use expand_grid to make all the options
# actually probably makes sense to use the `updateList` thing from crossbasis 
# function

# then run dlmn in parallel

# then do the thing that greg just mentioned
# except you'll have to pick centering values and things to compare against
# for each city ...
# right you have to make one of these for each city?
pred = crosspred(basis, model, cen = 0.01, at = c(0.95))
# this then gives you the beta and conf intervals
c(pred$allRRfit[1], pred$allRRlow[1], pred$allRRhigh[1])

# then what I want to do is make a graph with the y-axis as the scenario
# ranked from lowest (bottom) to highest center estimate
# with RR on the x-axis
# so you can read out which scenario had the highest RR
# you'll probably need some short-hand to describe the model types
# Var: Tmax, NS[3]-knots[5] x Lag[30]-poly[2]