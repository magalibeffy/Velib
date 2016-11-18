step1measures.rewrited <- function (Data, Time, ID = FALSE, verbose = TRUE) {
  require(pastecs)
  data = Data
  time = Time
  input.data = data
  input.time = time
  if (dim(data)[1] != dim(time)[1] || dim(data)[2] != dim(time)[2]) 
    stop("data and time must be the same size.")
  sample.size = dim(data)[1]
  if (ID) {
    IDvector = data[, 1]
    data = data[, -1]
    time = time[, -1]
  }
  max.num.obs = dim(data)[2]
  clean.data = matrix(ncol = max.num.obs, nrow = sample.size)
  clean.time = matrix(ncol = max.num.obs, nrow = sample.size)
  num.obs = rep(999, sample.size)
  less.than.4.obs = NULL
  for (i_sample in 1:sample.size) {
    real.obs.pos = which(!is.na(data[i_sample, ]))
    num.obs[i_sample] = length(real.obs.pos)
    clean.data[i_sample, ] = as.vector(c(unlist(data[i_sample, 
                                                     real.obs.pos]), rep(NA, max.num.obs - num.obs[i_sample])))
    clean.time[i_sample, ] = as.vector(c(unlist(time[i_sample, 
                                                     real.obs.pos]), rep(NA, max.num.obs - num.obs[i_sample])))
    if (length(real.obs.pos) < 4) 
      less.than.4.obs = c(less.than.4.obs, i_sample)
    clean.data.pos = which(!is.na(clean.data[i_sample, ]))
    if (any(is.na(clean.time[i_sample, clean.data.pos]))) 
      stop(paste("There must be a time associated to every observation. Line: ", 
                 i_sample, sep = ""))
  }
  if (!is.null(less.than.4.obs)) {
    clean.data = clean.data[-less.than.4.obs, ]
    clean.time = clean.time[-less.than.4.obs, ]
  }
  sample.size = nrow(clean.data)
  if (ID) {
    if (!is.null(less.than.4.obs)) IDvector = IDvector[-less.than.4.obs]
  }else IDvector = seq(1:sample.size)
  data = clean.data
  time = clean.time
  output = data.frame(matrix(ncol = 25, nrow = sample.size))
  output = data.frame(matrix(ncol = 25, nrow = sample.size))
  names = c("ID", "m1", "m2", "m3", "m4", "m5", "m6", "m7", 
            "m8", "m9", "m10", "m11", "m12", "m13", "m14", "m15", 
            "m16", "m17", "m18", "m19", "m20", "m21", "m22", "m23", 
            "m24")
  colnames(output) = names
  output$ID = IDvector
  # m1 : Range
  max <- apply(data, 1, function(x) max(x, na.rm=T))
  min <- apply(data, 1, function(x) min(x, na.rm=T))
  output$m1 <- max - min
  # m2 : Mean-over-time
  output$m2 <- apply(data, 1, function(x) mean(x, na.rm=T))
  # m3 : Standard deviation
  output$m3 <- apply(data, 1, function(x) sd(x, na.rm=T))
  # m4 : Coefficient of variation
  output$m4 <- 100 * output$m3/output$m2
  # m5 : Change
  # output$m5 <- data[,max.num.obs] - data[,1]
  last.data <- apply(data, 1, function(x) last(x, na.rm=T))
  first.data <- apply(data, 1, function(x) first(x, na.rm=T))
  output$m5 <- last.data - first.data
  # m6 :  Mean change per unit time
  # output$m6 <- (data[,max.num.obs] - data[,1]) / (time[,max.num.obs] - time[,1])
  last.time <- apply(time, 1, function(x) last(x, na.rm=T))
  first.time <- apply(time, 1, function(x) first(x, na.rm=T))
  output$m6 <- output$m5 / (last.time - first.time)
  # m7 : Change relative to the first score
  output$m7 <- output$m5 / first.data
  # m8 : Change relative to the mean over time
  output$m8 <- output$m5 / output$m2
  # m9 : Slope of the linear model
  for (i in 1:sample.size) {
    b = coefficients(lm(data[i, ] ~ time[i, ]))
    output$m9[i] = b[2]
  }
  # m10 : R^2: Proportion of variance explained by the linear model
  for (i in 1:sample.size) {
    model = lm(data[i, ] ~ time[i, ])
    r = resid(model)
    RSS = r %*% r
    Y = subset(data[i, ], is.na(data[i, ]) == FALSE)
    m = length(Y)
    SYY = Y %*% Y - (sum(Y)^2)/m
    SSREG = SYY[1] - RSS[1]
    output$m10[i] = SSREG/SYY
  }
  # m11 : Maximum of the first differences
  FD = matrix(nrow = sample.size, ncol = max.num.obs - 1)
  for (i in 1:sample.size) {
    for (j in 1:(max.num.obs - 1)) {
      FD[i, j] = data[i, (j + 1)] - data[i, j]
    }
  }
  output$m11 <- apply(FD, 1, function(x) max(x, na.rm=T))
  # m12 : SD of the first differences
  output$m12 <- apply(FD, 1, function(x) sd(x, na.rm=T))
  # m13 : SD of the first differences per time unit
  FDunit = matrix(nrow = sample.size, ncol = max.num.obs - 1)
  for (i in 1:sample.size) {
    for (j in 1:(max.num.obs - 1)) {
      FDunit[i, j] = FD[i, j]/(time[i, j + 1] - time[i, j])
    }
  }
  output$m13 <- apply(FDunit, 1, function(x) sd(x, na.rm=T))
  # m14 : Mean of the absolute first differences
  output$m14 <- apply(FD, 1, function(x) mean(abs(x), na.rm=T))
  # m15 : Maximum of the absolute first differences
  output$m15 <- apply(FD, 1, function(x) max(abs(x), na.rm=T))
  # m16 : Ratio of the maximum absolute difference to the mean-over-time
  output$m16 <- output$m15 / output$m2
  # m17 : Ratio of the maximum absolute first difference to the slope
  output$m17 <- output$m15 / output$m9
  # m18 : Ratio of the SD of the first differences to the slope
  output$m18 <- output$m12 / output$m9
  # m19 : Mean of the second differences
  SD = matrix(nrow = sample.size, ncol = max.num.obs - 2)
  for (i in 1:sample.size) {
    for (j in 1:(max.num.obs - 2)) {
      SD[i, j] = FD[i, (j + 1)] - FD[i, j]
    }
  }
  output$m19 <- apply(SD, 1, function(x) mean(x, na.rm=T))
  # m20 : Mean of the absolute second differences
  output$m20 <- apply(SD, 1, function(x) mean(abs(x), na.rm=T))
  # m21 : Maximum of the absolute second differences
  output$m21 <- apply(SD, 1, function(x) max(abs(x), na.rm=T))
  # m22 : Ration of the maximum absolute second difference to the mean-over-time
  output$m22 <- output$m21 /output$m2
  # m23 : Ratio of the maximum absolute second difference to mean absolute first difference
  output$m23 <- output$m21 /output$m14
  # m24 : Ratio of the mean absolute second difference to the mean absolute first difference
  output$m24 <- output$m20 /output$m14
  
  temp.data = output$m2[(output$m2 != 0)]
  abs.temp.data = abs(temp.data)
  if (length(temp.data) == 0) mean.0 = 1e-04 else mean.0 = temp.data[which.min(abs.temp.data)]/100
  m4.na.pos = which(is.na(output$m4) | is.infinite(output$m4))
  if (length(m4.na.pos) != 0) output$m4[m4.na.pos] = 100 * output$m3[m4.na.pos]/mean.0
  m8.na.pos = which(is.na(output$m8) | is.infinite(output$m8))
  if (length(m8.na.pos) != 0) {
    if (length(m8.na.pos) > 1){
      output$m8[m8.na.pos] = (apply(data[m8.na.pos, ], 1, last, na.rm = TRUE) - apply(data[m8.na.pos,], 1, first, na.rm = TRUE))/mean.0
    }else output$m8[m8.na.pos] = (last(data[m8.na.pos, ], na.rm = TRUE) - first(data[m8.na.pos, ], na.rm = TRUE))/mean.0
  }
  m16.na.pos = which(is.na(output$m16) | is.infinite(output$m16))
  if (length(m16.na.pos) != 0) output$m16[m16.na.pos] = output$m15[m16.na.pos]/mean.0
  m22.na.pos = which(is.na(output$m22) | is.infinite(output$m22))
  if (length(m22.na.pos) != 0) output$m22[m22.na.pos] = output$m21[m22.na.pos]/mean.0
  temp.data = data[(data[, 1] != 0), 1]
  abs.temp.data = abs(temp.data)
  if (length(temp.data) == 0){
    y1.0 = 1e-04 
  }else y1.0 = temp.data[which.min(abs.temp.data)]/100
  m7.na.pos = which(is.na(output$m7) | is.infinite(output$m7))
  if (length(m7.na.pos) != 0) {
    if (length(m7.na.pos) > 1) {
      output$m7[m7.na.pos] = (apply(data[m7.na.pos, ], 1, last, na.rm = TRUE) - apply(data[m7.na.pos, ], 1, first, na.rm = TRUE))/y1.0
    }else output$m7[m7.na.pos] = (last(data[m7.na.pos, ], na.rm = TRUE) - first(data[m7.na.pos, ], na.rm = TRUE))/y1.0
  }
  form10 = vector(length = sample.size)
  for (i_test in 1:sample.size) {
    model = lm(data[i_test, ] ~ time[i_test, ])
    r = resid(model)
    RSS = r %*% r
    Y = subset(data[i_test, ], is.na(data[i_test, ]) == FALSE)
    m = length(Y)
    form10[i_test] = Y %*% Y - (sum(Y)^2)/m
    if (form10[i_test] == 0) form10[i_test] = NA
  }
  if (!is.na(min(form10))) syy.0 = min(form10) else syy.0 = 1e-04
  m10.na.pos = which(is.na(output$m10) | is.infinite(output$m10))
  if (length(m10.na.pos) != 0) {
    for (i_na in m10.na.pos) {
      model = lm(data[i_na, ] ~ time[i_na, ])
      r = resid(model)
      RSS = r %*% r
      Y = subset(data[i_na, ], is.na(data[i_na, ]) == FALSE)
      m = length(Y)
      SSREG = SYY[1] - RSS[1]
      output$m10[i_na] = SSREG/syy.0
    }
  }
  temp.data = output$m9[(output$m9 != 0)]
  abs.temp.data = abs(temp.data)
  if (length(temp.data) == 0) slope.0 = 1e-04 else slope.0 = temp.data[which.min(abs.temp.data)]/100
  m17.na.pos = which(is.na(output$m17) | is.infinite(output$m17))
  if (length(m17.na.pos) != 0) output$m17[m17.na.pos] = output$m15[m17.na.pos]/slope.0
  m18.na.pos = which(is.na(output$m18) | is.infinite(output$m18))
  if (length(m18.na.pos) != 0)  output$m18[m18.na.pos] = output$m12[m18.na.pos]/slope.0
  temp.data = output$m14[(output$m14 != 0)]
  abs.temp.data = abs(temp.data)
  if (length(temp.data) == 0) mean.abs.0 = 1e-04 else mean.abs.0 = temp.data[which.min(abs.temp.data)]/100
  m23.na.pos = which(is.na(output$m23) | is.infinite(output$m23))
  if (length(m23.na.pos) != 0) output$m23[m23.na.pos] = output$m21[m23.na.pos]/mean.abs.0
  m24.na.pos = which(is.na(output$m24) | is.infinite(output$m24))
  if (length(m24.na.pos) != 0) {
    abs.na.data = abs(SD[m24.na.pos, ])
    if (length(m24.na.pos) > 1) {
      output$m24[m24.na.pos] = apply(abs.na.data, 1, mean, 
                                     na.rm = TRUE)/mean.abs.0
    }else output$m24[m24.na.pos] = mean(abs.na.data, na.rm = TRUE)/mean.abs.0
  }
  # trajMeasures = structure(list(measurments = output, data = cbind(IDvector, clean.data), time = cbind(IDvector, clean.time)), class = "trajMeasures")
  # return(trajMeasures)
  return(output)
}

# step2factors.rewrited <- function (trajMeasures, num.factors = NULL, discard = NULL, verbose = TRUE, 
#           ...) 
# {
#   if (!is.null(discard)) {
#     if (class(discard) == "character") 
#       vars.to.discard = which(names(trajMeasures$measurments) %in% 
#                                 discard)
#     else vars.to.discard = discard
#     if (19 %in% vars.to.discard) 
#       stop("m18 will automatically be removed. Do not include it in the 'discard' variable.")
#     if (length(vars.to.discard) != length(discard)) 
#       stop("Not all variables in 'discard' are to be removed. There is an error in the format of 'discard'.")
#     data = trajMeasures$measurments[, -vars.to.discard]
#   }
#   else data = trajMeasures$measurments
#   dim.of.data = dim(data)
#   sample.size = dim.of.data[1]
#   IDvector = data[1]
#   data = data[-1]
#   if (cor(data$m17, data$m18) >= 0.95) {
#     data = data[, -which(names(data) == "m18")]
#   }
#   corr.vars = check.correlation(data, verbose = FALSE, is.return = TRUE)
#   if (!is.null(corr.vars)) {
#     corr.vars.pos = which(names(data) %in% corr.vars[, 1])
#     data = data[, -corr.vars.pos]
#     print(paste(corr.vars[, 1], "is removed because it is perfectly correlated with", 
#                 corr.vars[, 2]))
#   }
#   if (num.factors > ncol(data) && !is.null(num.factors)) 
#     stop("Requesting more factors in 'num.factors' than available variables.")
#   max.num.obs = dim(data)[2]
#   eigen.values = NULL
#   pricipal.factors = NULL
#   if (is.null(num.factors)) {
#     if (verbose) 
#       print("Computing reduced correlation e-values...")
#     eigen.values = reduced.eigen(data)
#     num.factors = length(which(eigen.values$values >= 1))
#   }
#   principal.factors = principal(data, rotate = "varimax", nfactors = num.factors, 
#                                 ...)
#   principal.variables = c(rep(NA, num.factors))
#   for (i_factors in 1:num.factors) {
#     principal.variables[i_factors] = which.max(abs(principal.factors$loadings[, 
#                                                                               i_factors]))
#   }
#   principal.variables = sort(principal.variables)
#   output = IDvector
#   for (i_col in 1:num.factors) {
#     output = cbind(output, data[principal.variables[i_col]])
#   }
#   trajFactors = structure(list(factors = output, e.values = eigen.values, 
#                                princ.fact = principal.factors, measurments = trajMeasures$measurments, 
#                                data = trajMeasures$data, time = trajMeasures$time), 
#                           class = "trajFactors")
#   return(trajFactors)
# }
