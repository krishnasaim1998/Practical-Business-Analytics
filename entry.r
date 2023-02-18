rm(list=ls())


CSV_NAME                        <-"Telecom_customer_churn.csv"      #name of dataset file
TARGET_VARIABLE                 <-"churn"                           #name of the variable we are trying to predict
TYPE_DISCRETE                   <- "DISCRETE"  
TYPE_ORDINAL                    <- "ORDINAL"   
TYPE_SYMBOLIC                   <- "SYMBOLIC"  
TYPE_NUMERIC                    <- "NUMERIC"
CORR_CUTOFF                     <- 0.8                              #cut off for determining multicollinearity
CUT_OFF                         <- 7                                #threshold empty bins for identifying discrete
OUTLIER_CONF                    <- 0.99                             #threshold p for outlier detection
CUMULATIVE_VARIANCE_THRESHOLD   <- 0.98                             #variance to retain during PCA transformation
KFOLDS                          <- 7                                #number of folds in model evaluation
PARAMETER_TUNE_ENABLED          <- FALSE                             #Enable the parameter tuning portion of the code in main()


debugSource("functions.r")
MYLIBRARIES<-c("outliers","mltools",'data.table','ggplot2','e1071','xgboost','caret','pROC','randomForest','ggcorrplot','corrplot','magrittr','knitr','kableExtra')

library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

main<-function(){
  

  
  #read the dataset from the csv file but limit it to the first 25000 rows
  #we will pre-process this subset of our data frame which includes using a 
  #correlation matrix to identify redundant fields
  #columns defined in the constant 'UNWANTED' will be dropped
  df_small<-readCsv(CSV_NAME,limitRows = 25000)
  df_small<-forceDropUnwantedCols(df_small,UNWANTED)
  df_small<-preprocess(df_small, remove_redundant = TRUE)
  #by limiting the dataset to only 1/4 the total number of rows, we are able to
  #identify columns that need to be dropped due to high multicollinearity
  #running this on the entire data set of 100,00 records is not possible due to 
  #the high memory requirement of generating the correlation matrix
  
  
  
  #we now read a larger subset of the dataset. However we will still limit
  #it to 70% of the available records to prevent memory exhaustion
  #This larger data set is now pre processed but without checking for multicollinearity
  #and without dropping the redundant fields
  #(if RStudio crashes due to inability to allocate memory for pre-processing,
  #please restart Rstudio and try again. If the problems persists, please try reducing
  #the limitRow argument in the following line)
  df_big<-readCsv(CSV_NAME,limitRows = 70000)
  df_big_revenue<- replaceAllNA(data.frame(df_big$revMean)) #save the mean monthly revenue per customer to prevent loss during redundancy check
  df_big<-forceDropUnwantedCols(df_big,UNWANTED)
  df_big<-preprocess(df_big, remove_redundant = FALSE)

  #we now preserve only those columns that passed multicollinearity check on the smaller data set
  #df_big<-df_big[,which(names(df_big) %in% names(df_small))]
  
  
  #Plot Variables most correlated to our target
  #plotTargetCorrelation(df_small)
  
  
  #we now stratify the dataset and allocate fold ids
  stratifiedDf_big<-stratify(df_big,KFOLDS)
  stratifiedDf_small<-stratify(df_small,KFOLDS)
  
  #remove unstratified data frames to allow gc to collect if necessary
  #rm(df_small)
  
  
  
  #we will now perform our experiments of trying to predict churn
  #we will perform these experiments on reduced dimensions.
  #we use PCA to reduce number of dimensions that are shown to
  #our models.This dimensionality reduction was necessary due to overfitting 
  #and poor generalisation on the test data experienced due to the large number 
  #of columns in our dataset and particularly after one hot encoding
  
  #The call to execExperimentOnReducedDimensions will evaluate the delegated model calls
  #using k-FOLD cross validation
  
  
  #First we try using a Random Forest classifier to predict churn
  results<-data.frame()
  exp1<-execExperimentOnReducedDimensions(
        name_experiment                = "random forest",
        stratifiedDf                   = stratifiedDf_big,
        cumulativeVariance             = CUMULATIVE_VARIANCE_THRESHOLD,
        delegatedEXP                   = random_forest,
        forest_size                    = 100,
        mtry                           = 3
  )
  results<-rbind(results,data.frame(exp1))
  
  
  #Here we try using xgb with binary logistic regression for predicting churn
  exp2<-execExperimentOnReducedDimensions(
         name_experiment               = "xgb + logistic regression",
         stratifiedDf                  = stratifiedDf_big,
         cumulativeVariance            = CUMULATIVE_VARIANCE_THRESHOLD,
         delegatedEXP                  = xgbLogistic,
         num_rounds                    = 80
       )
  results<-rbind(results,data.frame(exp2))
  
  
  #The mean evaluation metrics for both models will now be presented
  print(formattable::formattable(results))
  
  
  # We can see from the results printed above to the viewer window that
  # the xgb model meets our minimum acceptance criteria for model,
  # however the random forest model does not. We will now try to tune
  # parameters of the random forest model to try and increase its performance
  #
  #
  # We use the smaller stratified dataset to try and identify any trends in the
  # parameter tuning.
  #
  # We tune mtry and forestSize parameters of the random forest as well as the number
  # of principal components to use
  
  
  # BELOW TAKES A LONG TIME  TO RUN. Please enable using the constant at the top of
  # the file
  
  if(PARAMETER_TUNE_ENABLED){
    tuneResult<-data.frame()
    crange<-c(0.98,0.95,0.9,0.8,0.7)
    mrange<- seq(20,5,-5)
    frange<-append(seq(3000,500,-500),c(400,200,50))
    count<-0L
    progressBar <- txtProgressBar(min = 0, max = length(crange)*length(mrange)*length(frange), style = 3) #define progress bar
    for(f in frange){
      for(m in mrange){
        for(c in crange){
          
          meta<-data.frame(ForestSize=f,MTRY=m, cumulative_variance=c)
          
          iterResult<-execExperimentOnReducedDimensions(
                 name_experiment                = "random forest tuning param tuning",
                 stratifiedDf                   = stratifiedDf_small,
                 cumulativeVariance             = c,
                 delegatedEXP                   = random_forest,
                 forest_size                    = f,
                 mtry                           = m
          )
          newRow<-cbind(meta,data.frame(iterResult))
          tuneResult<-rbind(tuneResult,newRow)
          count<-count+1
          setTxtProgressBar(progressBar, count) #increase the progress bar
        }
      }
    }
    close(progressBar)
    print(formattable::formattable(tuneResult))
  }
  
  # From the tuning results in the viewer window, it is clear that modifying the
  # parameters will not improve the random forest model to the minimum required spec

  
  
  # The Xgboot model meets our minimum acceptance criteria. We will 
  # now find the mean monthly revenue for those customers that our  
  # xgb model predicts as churn
  
  
  #We now use the xgb model to predict churn. We use 70% of the data to build a model
  #and apply the prediction on the remaining data
  
  split <- train_test_split_reduceDimensions(df_big,splitFactor = 0.7, cumulativeVariance= CUMULATIVE_VARIANCE_THRESHOLD)
  train<- split[[1]]
  test = split[[2]]

  xgb <- Train_XGB(train,80)
  model <- xgb[[1]]
  threshold <- xgb[[2]]
  
  predictors<-which(names(test)!=TARGET_VARIABLE)
  test_predicted <-predict(object = model, newdata = data.matrix(test[predictors]))
  test_predicted <- test_predicted>=threshold
  
  #add the predicted churn column back into the train subset of the
  #original (non pca reduced) dataset
  df_big_test = df_big[-(1:nrow(train)),]
  df_big_test$PredictedChurn <- test_predicted

  #add back the unscaled monthly revenue column to the test subset
  df_big_test$revMean<-df_big_revenue[-(1:nrow(train)),]


  avg_revenue_predicted_customers<-mean(df_big_test[df_big_test$PredictedChurn==1,"revMean"])
  writeLines(paste('\n\nMean monthly revenue per customer predicted at risk of attrition:',avg_revenue_predicted_customers,'\n\n'))
  writeLines(paste('\n\nMean monthly revenue per customer that does churn:',mean(df_big_test[df_big_test$churn==1,"revMean"]),'\n\n'))
  writeLines(paste('\n\nMean monthly revenue per customer:',mean(df_big_test[,"revMean"]),'\n\n'))
  
  print('EOM1')
  print('EOM2')
}

xgb.set.config(verbosity = 0)
main()
