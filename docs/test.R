library(dlnm) ; library(splines) ; library(foreign) ; library(tsModel)

# CHECK VERSION OF THE PACKAGE
if(packageVersion("dlnm")<"2.2.0")
  stop("update dlnm package to version >= 2.2.0")

# LOAD THE DATA
lndn <- read.csv(paste0("https://raw.githubusercontent.com/gasparrini/",
                        "2014_gasparrini_BMCmrm_Rcodedata/e629be7809853f390d305d8e73ac4b152a54f092/london.csv"))


################################################################################
# DERIVE THE CROSS-BASIS

# KNOTS FOR EXPOSURE-RESPONSE FUNCTION
vk <- equalknots(lndn$tmean, fun="bs", degree = 2, df = 4)

# KNOTS FOR THE LAG-RESPONSE FUNCTION
maxlag <- 25
lk <- logknots(x = maxlag, nk = 3)

# CENTERING VALUE (AND PERCENTILE)
cen <- 20
sum(lndn$tmean<cen,na.rm=T)/sum(!is.na(lndn$tmean))

# COMPUTE THE CROSS-BASIS
cb <- crossbasis(x = lndn$tmean, 
                 lag=maxlag, 
                 argvar=list(fun="bs", degree = 2, knots = vk), 
                 arglag=list(knots=lk))

# SUMMARY
summary(cb)

# COMPUTE 99TH PERCENTILES
perc <- quantile(lndn$tmean,c(0.99))

################################################################################
# RUN THE MODEL AND OBTAIN PREDICTIONS

# RUN THE MODEL
model <- glm(death~cb+ns(time,10*14)+dow,family=quasipoisson(),lndn)

# PREDICTION AT 99TH PERCENTILES
pred <- crosspred(cb,model,at=perc,bylag=0.2,cen=cen)

# assume this is already exponentiated
list(est = pred$allRRfit, lb = pred$allRRlow, ub = pred$allRRhigh)



################################################################################
# Now confirm you get the same thing with cb_expand
source('expand_arglist.R')
source('make_cb_list.R')

## >> this probably needs to be an internal thing, you just want to create the CB

## problem is that knots can be absolute or percentile, how to differentiate?
## you could make it a function

## could leverage "equal knots" or one of the other knot making functions
## or.. you could just set the percentiles and assume that either way it will work out
## so, assume that the knots are the percentiles, and then it converts them in data
## yeah this is it.

## you also could call this a bunch and pass in X, so maybe it doesn't make sense
## to do this this way. 

## just create them all in a loop, as you want them, and use append()
## to append to the list as you need to

cb2 <- make_cb_list(x = lndn,
                    var_list = c('tmean'), 
                    argvar_list = list(set1 = list(fun="bs", degree = 2, knots = list(vk))) ,
                    lag_list = maxlag,
                    arglag_list = list(set1 = list(knots = list(lk))))

## HUGGGEE
attr(cb2[[1]], 'summary_str')
attr(cb2[[1]], 'var_name')
# attr(cb2[[1]], 'summary_str') <- NULL
# attr(cb2[[1]], 'var_name') <- NULL
# identical(cb, cb2[[1]])

## slightly longer example
var_list <- c('tmean', 'tmin')

cb3 <- list()

for(var in var_list) {
  
  # make var knots that vary by var
  vk2 = equalknots(lndn[, var], fun="bs", degree = 2, df = 4)
  vk3 = equalknots(lndn[, var], fun="ns", df = 4)
  k50 = quantile(lndn[, var], probs = .5)
  k10 = quantile(lndn[, var], probs = c(1:9)/10)
  
  cb3_var = make_cb_list(x = lndn,
                         var_list = var, 
                         argvar_list = list(set1 = list(fun="bs", degree = c(2, 3), 
                                                        knots = list(vk2, k50, k10)),
                                            set1 = list(fun="ns",  
                                                        knots = list(vk3, k50, k10))) ,
                         lag_list = maxlag,
                         arglag_list = list(set1 = list(knots = list(lk))))
  
  cb3 <- append(cb3, cb3_var)
}

set.seed(1)
model.1 <- glm(death~cb+ns(time,10*14)+dow,family=quasipoisson(),lndn)
set.seed(1)
model.1b <- glm(death~cb+ns(time,10*14)+dow,family=quasipoisson(),lndn)
identical(model.1, model.1b)
# ok so these are never the same exactly

# setup for parallel processing
library(future)          # parallel processing
library(future.apply)    # parallel processing
library(progressr)       # progress updates during parallel
library(sys)
library(rstudioapi)
plan(multisession)
handlers(global = TRUE)
handlers("progress")

get_model_obj <- function(cb_list) {
  cb_seq <- seq(length(cb_list))
  p      <- progressr::progressor(along = cb_seq)
  future_lapply(cb_seq, function(i) {
    cb <- cb_list[[i]]
    p(message = sprintf("%s", attr(cb, "summary_str")))
    m  <- glm(death ~ cb + ns(time,10*14) + dow, family = quasipoisson(), lndn)
    attr(m, "var_name") <- attr(cb, "var_name")
    attr(m, "summary_str") <- attr(cb, "summary_str")
    m
  })
}

# clearly this could be expanded to a two-cross basis example
# by created an example grid of the length of each cb_list
with_progress(models <- get_model_obj(cb3))


## now, get outputs
# COMPUTE 99TH PERCENTILES
# CENTERING VALUE (AND PERCENTILE)
get_pred <- function(cb_list, model_list) {
  stopifnot(length(cb_list) == length(model_list))
  cb_seq <- seq(length(cb_list))
  p      <- progressr::progressor(along = cb_seq)
  out <- future_lapply(cb_seq, function(i) {
    cb <- cb_list[[i]]
    m  <- model_list[[i]]
    p(message = sprintf("%s", attr(cb, "summary_str")))
    # get prediction and centering values
    perc <- quantile(lndn[, attr(cb, "var_name")],c(0.99))
    cen  <- quantile(lndn[, attr(cb, "var_name")],c(0.94)) # ~ 20c for Tmin
    # PREDICTION AT 99TH PERCENTILES
    pred <- crosspred(cb, m, at=perc, bylag=0.2, cen=cen)
    # assume this is already exponentiated
    data.frame(var_name = attr(cb, "var_name"), 
               summary_str = attr(cb, "summary_str"),
               est = pred$allRRfit[1], 
               lb = pred$allRRlow[1], 
               ub = pred$allRRhigh[1])
  })
  o <- data.frame(do.call(rbind, out))
  row.names(o) <- 1:nrow(o)
  return(o)
}

with_progress(RRs <- get_pred(cb3, models))
RRs

# ok now plot
RRs %>%
  ggplot(., aes(y = reorder(summary_str, est))) +
  geom_point(aes(x=est), shape=15, size=3) +
  geom_linerange(aes(xmin=lb, xmax=ub)) + ylab(NULL)

