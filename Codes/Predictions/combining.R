setwd("C:/Users/Lara/Desktop/Predictions")
load("tmp2_predictions_raw_1.Rda")
predictions1 <- predictions
load("tmp2_predictions_raw_2.Rda")
predictions2 <- predictions
load("tmp2_predictions_raw_3.Rda")
predictions3 <- predictions
load("tmp2_predictions_raw_4.Rda")
predictions4 <- predictions
load("tmp2_predictions_raw_5.Rda")
predictions5 <- predictions
load("tmp2_predictions_raw_6.Rda")
predictions6 <- predictions
load("tmp2_predictions_raw_7.Rda")
predictions7 <- predictions
load("tmp2_predictions_raw_8.Rda")
predictions8 <- predictions
load("tmp2_predictions_raw_9.Rda")
predictions9 <- predictions


predictions <- cbind(predictions1, predictions2, predictions3, predictions4, predictions5, predictions6, predictions7, predictions8, predictions9)
rm(predictions1, predictions2, predictions3, predictions4, predictions5, predictions6, predictions7, predictions8, predictions9)

save(predictions, file="final_predictions.Rda")


#setwd("//clapton.wiwi.hu-berlin.de/APA_2016/Final FInal/Final")
#load("data_2_8_full.Rda")
