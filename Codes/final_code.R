# clearing the memory
rm(list = ls())

# libraries
library(tictoc)
library(data.table)

library(caret)
library(randomForest)
library(xgboost)
library(Matrix)
library(SparseM)
library(foreach)
library(doParallel) 
library(compiler)


# loading required helper functions
source("code/prediction_helper.R")
source("code/ga_helper.R")
source("code/genetic_algorithms.R")

# loading data version 2.8
load("data/data_2_8_full.Rda")
load("data/final_predictions.Rda")

# loading feature sets
load("data/var_ranks_2_4.Rda")
load("data/features_2_4_rfopt.Rda")
load("data/errors_2_4_xgb.R")


# splitting training and testing
d.class <- df[ is.na(df$returnQuantity),]
d.label <- df[!is.na(df$returnQuantity),]
rm(df)

# listing variables which are not used as predictors
unused.vars <- c("unique_ID", "returnQuantity", "orderID", "orderDate", "articleID", "voucherID", 
                 "customerID", "colorCode", "special_group")

#==========================================
# Model Parameters

# Fixed ones

# for 
pbil.prob <- 0.02
pbil.shift <- 0.05
pbil.size  <- c(30, 100, 300)
pbil.lr <- c(0.05, 0.1)

psol.size <- c(30, 100, 300)
psol.phi <- seq(1.8, 2.1, 0.1)
 
psog.size <- c(30, 100, 300)
psog.phi <- seq(1.8, 2.1, 0.1)

ga.cross <- 0.8 # c
ga.elit <- c(0.03, 0.1, 0.3)
ga.size <- c(30, 100, 300)
ga.mut <- 0.05


bsa.mixrate <- c(0.5, 1)
bsa.size <- c(30, 100, 300)

pbil.grid <- expand.grid(size = pbil.size, lr = pbil.lr)
psol.grid <- expand.grid(size = psol.size, phi = psol.phi)
psog.grid <- expand.grid(size=psog.size, phi = psog.phi)
ga.grid   <- expand.grid(size=ga.size, elit = ga.elit)
bsa.grid  <- expand.grid(size=bsa.size, mixrate= bsa.mixrate)


#############################################
#                                           #
#             3. DATA PREPARATION           #
#                                           #
#############################################

# diplaying information
print("Preparing the data...")

# marking unused variables
drops <- names(df) %in% unused.vars

# creating features and imputing values [level 1]
data    <- add_returns(d.label, d.class)
d.label <- data$train
d.class <- data$test
d.label <- d.label[, !colnames(d.label) %in% "new"]
rm(data)

# sorting coloumns in datasets
d.label   <- d.label[,   order(names(d.label))]
d.class  <- d.class[, order(names(d.class))]

# remarking unused variables
drops <- names(d.label) %in% unused.vars

# saving feature lists
x.vars <- c(names(var.ranks[1:which.min(x.errors)]), "returnBin", "C1", "C2", "C3", "C4", "C5", "saswoXt")
r.vars <- attr(terms(r.vars), "term.labels")
r.vars <- paste(r.vars, collapse = " + ")
r.vars <- paste("returnBin ~ ", r.vars, " + C1 + C2 + C3 + C4 + C5 + saswoXt", sep="")
r.vars <- as.formula(r.vars)

# additional data transformations [level 1]
d.class$returnBin <- "NA"
x.y <- as.numeric(d.label$returnBin)-1
x.x <- sparse.model.matrix(returnBin ~ . -1, data = d.label[x.vars])
x.v <- sparse.model.matrix(returnBin ~ . -1, data = d.class[x.vars])

# creating error matrix
# errors <- vector(mode = "numeric", length = models)
# names(errors) <- rep("NA", length(errors))
# names(errors)[(1):(nrow(f.grid))] <- "RF"
# names(errors)[(1+nrow(f.grid)):(length(errors))] <- "XGB"
# 
# # creating predictions matrix
# predictions <- matrix(nrow = nrow(d.class),   ncol = models)
# colnames(predictions) <- names(errors) 

# information
time.set[[1, 2]] <- proc.time()
time.trial <- proc.time() - time.set[[1, 1]]
print(paste0("Data preparation took ", round(time.trial[3]/60, digits = 0), " minutes."))
#beep(5)

load("data/final_predictions.Rda")
d.valid <- dd
forecasts <- 

