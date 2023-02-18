#***********************************************************************************
# PRACTICAL BUSINESS ANALYTICS
# Customer Attrition Prediction
#
# Mohammed Anees
# Team Janus
# Dept. of Computer Science
# University of Surrey
# GUILDFORD
# Surrey GU2 7JN
#
# The code in this file borrows from lab files only in the general steps to follow.
# All the code is written by us, with no function copied from any other sources
# or taken from lab files
#
# ***********************************************************************************


#columns that have been determined to be continuous ordinals using data dictionary to prevent misclassification as discrete
KNOWN_ORDINALS      <-c('revMean','totcalls','changerev','adjqty','mouMean','totmrcMean','daMean','ovrmouMean','ovrrevMean','vceovrMean','datovrMean','roamMean','dropvceMean','dropdatMean','blckvceMean','blckdatMean','unanvceMean','unandatMean','plcdvceMean','plcddatMean','recvvceMean','recvsmsMean','compvceMean','compdatMean','custcareMean','ccrndmouMean','ccmouMean','inoneminMean','threewayMean','moucvceMean','moucdatMean','mourvceMean','owylisvceMean','mouowylisvMean','iwylisvceMean','mouiwylisvMean','peakvceMean','peakdatMean','moupeavMean','moupeadMean','opkvceMean','opkdatMean','mouopkvMean','mouopkdMean','dropblkMean','attemptMean','completeMean','callfwdvMean','callwaitMean','totmou','totrev','adjrev','avgrev','avg3rev','avg6rev','adjmou','avgqty','avg6qty')

#columns to drop
UNWANTED            <-c('CustomerID')


# ************************************************
# readCsv() :
#
# Reads a CSV file and returns a dataframe
# INPUT: string   - fileName    - name of file to read
#        int      - limitRows   - max rows to return. will return entire
#                                 file if set to default NULL
#
# OUTPUT : Frame - dataset
#
# ************************************************
readCsv<-function(fileName,limitRows=NULL){
  
  csvDf<-read.csv(fileName,encoding="UTF-8",stringsAsFactors = FALSE)
  origNames<-names(csvDf)
  #remove any punctuations or spaces for column names
  newNames<-removePunctuations(origNames)
  names(csvDf)<-newNames
  if(is.null(limitRows)){
    return(csvDf)
  }
  else{
    return(csvDf[1:limitRows,])
  }
}

# ************************************************
# removePunctuations() :
#
# Removes any spaces or punctuations in a given string
# INPUT:  string   - text    - text that needs punctuations removed
#
# OUTPUT :string
#
# ************************************************
removePunctuations<-function(text){
  return(gsub("[[:punct:][:blank:]]+", "", text))
}

# ************************************************
# forceOrdinal() :
#
# Takes an input data frame and a vector representing its inferred types ie.. ordinal 
# discrete or symbolic and uses prior knowledge from the constant KNOWN_ORDINALS
# to force columns as continuous ordinals to fix type misclassification
# INPUT: data frame         - df            - dataframe
#        vector string      - field_types   - vector representing the inferred types of
#                                             given data frame
#
# OUTPUT : vector string - updated field_type vector with prior knowledge coerced
#
# ************************************************
forceOrdinal<-function(df,field_types){
  colNames<-names(df)
  for(i in 1:(ncol(df))){
    for(j in KNOWN_ORDINALS){
      if(colNames[i]==j){
        field_types[i]=TYPE_ORDINAL
      }
    }
  }
  return(field_types)
}

# ************************************************
# forceDropUnwantedCols() :
#
# Drops unwanted columns from dataset
# INPUT: data frame         - df            - dataframe
#        vector string      - cols          - names of columns to drop
#
# OUTPUT : data frame - data frame with unwanted columns removed
#
# ************************************************
forceDropUnwantedCols<-function(df,cols){
  if(length(cols)==0){
    return(df)
  }
  return(df[ , -which(names(df) %in% cols)])
}

# ************************************************
# replaceAllNA() :
#
# Takes an input data frame containing only continous ordinals and 
# replaces all NAs accross all columns using provided substitution
#
# INPUT: data frame         - df            - dataframe
#        string             - replace       - substition for replacement
#                                               *MEAN: mean of remaining values
#                                                 from column will be used for
#                                                 substitution
#`                                              *ZERO: will replace with 0`
#
# OUTPUT : data frame - all NA values replaced  
#
# ************************************************
replaceAllNA<-function(df, replace="MEAN"){
  for(f in names(df)){
    indexes<-which(is.na(df[,f]))
    if(replace=="MEAN"){
      df[,f] = ifelse(is.na(df[,f]),ave(df[,f], FUN = function(x) mean(x, na.rm = 'TRUE')),df[,f])
      if(length(indexes)>0){
        print(paste('Replaced',length(indexes),'NAs for column',f,'with mean'))
      }
    }
    else if (replace=="ZERO"){
      df[,f] = ifelse(is.na(df[,f]),0,df[,f])
      if(length(indexes)>0){
        print(paste('Replaced',length(indexes),'NAs for column',f,'with 0'))
      }
    }
    else{
      stop('Uknown replace')
    }
  }
  return(df)
}

# ************************************************
# GetInitialColumnTypes() :
#
# Classifies each column of a data frame as either numeric or symbolic
# INPUT: data frame         - df            - data frame
#
# OUTPUT : vector string - field(column) types in the same order as the column
#
# ************************************************
GetInitialColumnTypes<-function(df){
  result<-vector()
  for(colName in names(df)){
    if (is.numeric(df[,colName])) {
      result<-append(result,TYPE_NUMERIC)
    }
    else {
      result<-append(result,TYPE_SYMBOLIC)
    }
  }
  return(result)
}



# ************************************************
# NumericAsOrdinalsOrDiscrete() :
#
# Classifies each numeric column in a data frame as either a continous or 
# discrete ordinal. Uses histogram analysis to bucket each value in a numeric column
# into one of 10 buckets. A column is deemed as discrete if the number of buckets containing
# fewer than 1% of the data excedes the provided cutoff

# INPUT: data frame         - dataset             - dataframe
#        vector string      - field_types         - vector representing the inferred types of
#                                                   given data frame. possible values: numeric, symbolic
#        integer            - cutoff              - maximum number of buckets that can be empty in a 
#                                                   continuous ordinal
#        vector string      - known_continous_ordinals - names of column that have already deemed to be 
#                                                        continuous using prior analyis
#
# OUTPUT : vector string -  field_type vector describing each column as ordinal,discrete or symbolic
#
# ************************************************
NumericAsOrdinalsOrDiscrete<-function(dataset,field_types,cutoff, known_continous_ordinals){
  rows = nrow(dataset)
  
  #iterate over all columns in the data frame
  for(idx in (1:ncol(dataset))){
    
    if (field_types[idx]==TYPE_NUMERIC) {
      
      #if we have prior knowledge of a field being a continuous ordinal,
      #set the type accordingly and proceed to next variable
      if(names(dataset)[idx] %in% known_continous_ordinals){
        field_types[idx]<-TYPE_ORDINAL
        next
      }
      
      #create histogram analysis for binning/bucketing the values in the column
      histDistr<-hist(dataset[,idx], breaks = 10,plot=FALSE)
      
      #count bins where they contain fewer than 1% of records
      emptyCount<-0L
      for(i in histDistr$counts){
        if((i/rows)<0.01){
          emptyCount<-emptyCount+1L
        }
      }
      
      #if the number of empty/sparce bins is fewer than cut off, determine
      #the variable to be discrete
      if (emptyCount>cutoff){
        field_types[idx]<-TYPE_DISCRETE

        #plot the histogram distribution for manual verification when assigning as discrete
        hist(dataset[,idx], breaks = 10, plot=TRUE,
             main=paste("Distribution for detected discrete numeric-",names(dataset)[idx]),xlab="Values",ylab="Count")
      }
      else
        field_types[idx]<-TYPE_ORDINAL
    } 
  }
  return(field_types)
}


# ************************************************
# RemoveOutliers() :
#
# Uses Chi-squared test to identify outlier in a data frame containing
# only continous ordinals and replaces them with NA
#
# INPUT: data frame         - dfWithOrdinals          - dataframe
#        double             - threshold               - confidence p-value 
#
# OUTPUT : data frame - data frame with outliers replaced with NA
#
# ************************************************
RemoveOutliers<-function(dfWithOrdinals,threshold){
  for(idx in 1:(ncol(dfWithOrdinals))){
    outlierIndexes<-outliers::scores(dfWithOrdinals[,idx],type="chisq",prob=abs(threshold))
    dfWithOrdinals[which(outlierIndexes),idx]<-NA
    if(length(which(outlierIndexes))>0){
      print(paste('Replaced',length(which(outlierIndexes)),' outliers for column',names(dfWithOrdinals)[idx],'with NA'))
    }
  }
  return(dfWithOrdinals)
}


# ************************************************
# StandardiseRange() :
#
# Rescales all values in a data frame to lie in [0,1]
#
# INPUT: data frame         - df          - data frame containing only continous ordinals
#
# OUTPUT : data frame - data frame with all values in [0,1]
#
# ************************************************
StandardiseRange<-function(df){
  for(f in names(df)){
    df[,f] = ave(df[,f], FUN = function(x){(x-min(x))/(max(x)-min(x))})
  }
  return(df)
}

# ************************************************
# EncodeCategorical() :
#
# Applies one-hot encoding to all columns in a data frame
#
# INPUT: data frame         - datasetWithCategorical          - data frame containing only
#                                                               symbolic/categorical data
#
# OUTPUT : data frame - data frame with increased dimensions and one-hot encoded fields
#
# ************************************************
EncodeCategorical<-function(datasetWithCategorical){
  #input datasetWithCategorical contains only symbolic or discrete data
  few_names<-vector() #names of columns with few levels
  many_names<-vector() #names of columns with many levels
  
  
  #iterate over all columns
  for (field in names(datasetWithCategorical)){
    #convert column to factor
    asFactor<-factor(datasetWithCategorical[,field], exclude = NULL)
    
    #if the column contains fewer than three values, ie.. only one value 
    #or only two values, do not apply one hot encoding. 
    #Record names of such variables in the vector few_names,
    # else record the name in vector many_names
    if(length(levels(asFactor))<3){
      few_names<-append(few_names,field)
    }
    else{
      many_names<-append(many_names,field)
    }
  }
  
  #create a dataframe 'few' of those variables that only contain
  #one or two values and set each column as a factor
  few<-datasetWithCategorical[,few_names]
  for (field in names(few)){
    few[,field]<-factor(few[,field], exclude = NULL)
  }
  few<-one_hot(as.data.table(few))
  
  #now drop all the additional columns generated for constant or binary columns
  #so that they are represented in only one column
  q<-seq(1, ncol(few), by = 2) 
  few<-few[,..q]
  names(few)<-few_names #rename columns with few levels to their original names
  
  
  
  #apply one hot encoding on columns with many levels/values
  
  many<-datasetWithCategorical[,many_names] #dataframe of columns with many levels
  for (field in names(many)){
    many[,field]<-factor(many[,field], exclude = NULL)
  }
  many<-one_hot(as.data.table(many))
  names(many)<-removePunctuations(names(many))
  
  #join both data frames into result data frame
  result<-cbind(few,many)
  return(result)
  
} 

# ************************************************
# Remove_RedundantFields() :
#
# Identifies collinearity between columns and drops columns that 
# are highly correlated by building a correlation matrix
#
# INPUT: data frame         - dataset         - data frame
#        double             - cutoff          - threshold above which field
#                                               is determined to be redundant
#
# OUTPUT : data frame - data frame with redundant columns removed
#
# ************************************************
Remove_RedundantFields<-function(dataset,cutoff,plot_most_correlated=TRUE){
  
  #create correlation matrix
  cr<-cor(dataset, use = "complete.obs")

  #identify cell indexes where the value indicates multicollinearity
  #higher than the argument cutoff
  correlated<-which(abs(cr)>=cutoff,arr.ind = TRUE)
  
  plotMostCorrelated(dataset,20)
  
  processed<-vector() #vector to keep track of pairs of variables that have been previously seen
  discard<-vector() #vector containing names of variables that have been determined as redundant
  
  #iterate over indexes of correlated variables
  for (i in 1:nrow(correlated)) {
    firstIdx <- correlated[i, 1]
    secondIdx  <- correlated[i, 2]
    if(firstIdx==secondIdx){
      next #skip diagonal cells
    }
    
    #if the complement of this pair was previously seen, skip to next pair
    if(paste(secondIdx,"~",firstIdx) %in% processed){
      next
    }
    
    #assign second variable in the pair to be discarded
    discard<-append(discard,secondIdx)
    
    #record this pair as seen
    processed<-append(processed,paste(firstIdx,"~",secondIdx))
  }
  #get unique set of column names to discard
  discard<-unique(discard)
  print("Removing the following fields")
  for(i in discard){
    print(names(dataset)[i])
  }

  #drop redundant fields
  return(dataset[,-discard])
}

# ************************************************
# preprocess() :
#
# Preprocess a data frame to remove outliers in its continuous fields,
# replace missing continuous fields with column mean, normalise ordinal values to
# the range [0,1], one-hot encode categories and drop redundant fields
#
# INPUT: data frame   - df                - dataframe
#        logical     - remove_redundant   - switch to enable dropping redundant fields
#
# OUTPUT : data frame - preprocessed data frame
#
# ************************************************
preprocess<-function(df, remove_redundant=FALSE){
  
  #make initial identification of columns as numeric or symbolic
  field_types_initial<-GetInitialColumnTypes(df)
  
  #seperate the numerics into continous ordinals or as discrete
  field_types<-NumericAsOrdinalsOrDiscrete(df,field_types_initial,CUT_OFF,KNOWN_ORDINALS)
  results<-data.frame(fields=names(df),types=field_types)
  print(formattable::formattable(results))
  
  #remove outliers from the coinous values
  ordinals<-df[,which(field_types==TYPE_ORDINAL)]
  ordinals<-RemoveOutliers(ordinals,OUTLIER_CONF)
  
  #replace missing values with MEAN and normailse valuse to the range [0,1]
  ordinals<-replaceAllNA(ordinals, replace="MEAN")
  ordinals<-as.data.frame(scale(ordinals,center = TRUE,scale=TRUE))
  ordinals<-StandardiseRange(ordinals)
  
  #apply one-hot encoding to all non ordinal variables
  categoricalReadyForML<-EncodeCategorical(df[which(field_types==TYPE_SYMBOLIC | field_types==TYPE_DISCRETE)])
  rm(df)
  
  #combine data frames into single data frame
  combinedML<-cbind(ordinals,categoricalReadyForML)
  rm(ordinals,categoricalReadyForML)
  gc()
  
  
  #remove redundant fields based on multicollinearity
  if(remove_redundant){
    combinedML<-Remove_RedundantFields(combinedML,CORR_CUTOFF)
  }
  
  return(combinedML)
}

# ************************************************
# getPCA() :
#
# Performs dimensionality reduction using PCA. Maximum number of principal
# components to keep, or total required cumulative variance, must be supplied.
# Allows dimensionality reduction on a data frame using eignevalues exctracted
# from an earlier call
#
# INPUT: data frame         - df                  - dataframe
#        double             - cumulativeVariance  - if supplied, the number of principal
#                                                  components preserved in the output data set
#                                                  will contain this fraction of the total cumulative
#                                                  variance in the original data
#        int                - num_pc              -required number of principal components to preserve
#        object             - pcaAnalysis         - analysis object generated from previous call. 
#
# OUTPUT : data frame - reduced dimension data frame
#          pcaAnalysis - object containing PCA analysis performed on the given data frame
#
# ************************************************
getPCA<-function(df, cumulativeVariance=1, num_pc=0L,pcaAnalysis=NULL){
  
  #select all columns except target variable
  input<-df[which(names(df)!=TARGET_VARIABLE)]
  
  #if PCA analysis from previous call was not passed,
  #perform PCA
  if(is.null(pcaAnalysis)){
    pcaAnalysis<-prcomp(input,scale=FALSE)
  }
  cumm<-cumsum(pcaAnalysis$sdev^2 / sum(pcaAnalysis$sdev^2)) #get cumulative variance from PC1->PCn
  
  #if number of required principal components was not provided, identify
  #based on cumulative variance identified and supplied threshold
  if(num_pc==0){
    num_pc<-length(which(cumm<=cumulativeVariance))
  }
  
  #get principal components
  pcs<-predict(pcaAnalysis,input)
  pcs<-data.frame(pcs)
  
  
  outputCol<-df[which(names(df)==TARGET_VARIABLE)] #target column from original dataframe
  
  
  #select only required number of principal components and add back the target
  # column if it was present in the supplied dataframe
  result<-data.frame()
  if(ncol(outputCol)>0){
    result<-cbind(pcs[1:num_pc],df[which(names(df)==TARGET_VARIABLE)])
  }
  else{
    result<-cs[1:num_pc]
  }
  return(list(result,pcaAnalysis))
}

# ************************************************
# train_test_split_reduceDimensions() :
#
# Splits the data frame into train and test subsets for simple hold out evaluation of models
# returned data frames are linear transformed into lower dimensions using PCA
#
# INPUT: data frame   - df             - dataframe
#        double       - splitFactor    - fraction of original data frame to use as 'Train'
#        ellipsis     - ...            - arguments passed to getPCA() call necessary for 
#                                        dimension reduction
#
# OUTPUT : data frame - train data frame
#          data frame - test data frame
#
# ************************************************
train_test_split_reduceDimensions<-function(df,splitFactor,...){
  trRange<- 1:ceiling(nrow(df)*splitFactor) #get number of records in train
  
  train<-df[trRange,]
  test<-df[-trRange,]

  #get pca analysis on train data
  r<-getPCA(train,...)
  pcTrain=r[[1]] #get reduced dimension train data
  pcaAnalysis=r[[2]] #get analysis object
  
  num_pc<- ncol(pcTrain)-1 #identify number of principal components returned in previous call
  #get reduced dimension test data with matching number of PC components. Reuse the analysis object
  pcTest<-getPCA(test,num_pc=num_pc, pcaAnalysis = pcaAnalysis)[[1]] 
  
  return(list(train=pcTrain,test=pcTest))
}


# ************************************************
# addFoldIds() :
#
# Adds a fold id column to a data frame in the sequence 1,2,3,... num_folds,1,2,3,...numfolds,1,2,3,...
#
# INPUT: data frame   - df          - dataframe
#        int          - num_folds   - number of folds needed to group rows
#
# OUTPUT : data frame - data frame containing new *fold* column
#
# ************************************************
addFoldIds<-function(df,num_folds){
  sequence<-head(rep(1:num_folds,ceiling(nrow(df)/num_folds)) ,nrow(df))
  df$fold<-sequence
  return(df)
}

# ************************************************
# stratify() :
#
# Stratifies a data frame into 'folds' where each fold contains the same distribution of the
# TARGET_VARIABLE as the global balance. A fold id column is added to the data frame to identify groups or
# folds. The output data frame has its rows shuffled
#
# INPUT: data frame   - df          - dataframe
#        int          - num_folds   - total number of folds needed to group rows
#
# OUTPUT : data frame - data frame containing new *fold* column with class balanced folds
#
# ************************************************
stratify<-function(df,num_folds){
  #find index of output column
  pos<-which(names(df)==TARGET_VARIABLE)
  positiveRecords<-df[which(df[,TARGET_VARIABLE]==1),] #records belonging to the positive class
  negativeRecords<-df[which(df[,TARGET_VARIABLE]==0),] #records belonging to the negative class
  
  positiveRecords<-addFoldIds(positiveRecords,num_folds) #add fold ids to positive class records
  negativeRecords<-addFoldIds(negativeRecords,num_folds) #add fold ids to positive class records
  
  
  #combine both data frames and shuffle
  stratifiedDf<-rbind(positiveRecords,negativeRecords)
  stratifiedDfShuffled<-stratifiedDf[sample(nrow(stratifiedDf)),]
  return(stratifiedDfShuffled)
}

# ************************************************
# splitStratifiedDf() :
#
# Splits a stratified data frame into test and train subsets
# All records belonging to the given foldId will be alloted to the test subset
# and all other records to the train subset
#
# INPUT: data frame   - df       - dataframe
#        int          - foldId   - fold number to allocate as test data
#
# OUTPUT : data frame - train data frame
#          data frame - test data frame
#
# ************************************************
splitStratifiedDf<-function(df,foldId){
  train<-df[which(df[,"fold"]!=foldId),]
  test<-df[which(df[,"fold"]==foldId),]
  
  return(list(train=train,test=test))
}

# ************************************************
# reduceDimensions() :
#
# Use linear transformation to map train and test data frames into lower 
# dimensions by calling getPCA. The PCA analysis done on the train dataset is reused
# to maintain the same transformation on the test dataset
#
# INPUT: data frame   - train     - dataframe
#        data frame   - test      - data frame
#        ellipsis     - ...       - arguments passed to getPCA() call necessary for 
#                                   dimension reduction
#        logical      - verbose   - allows print statements to be suppressed
#
# OUTPUT : data frame - train data frame
#          data frame - test data frame
#
# ************************************************
reduceDimensions<-function(train,test,verbose=FALSE,...){
  if(verbose){
    print(paste('Performing PCA analysis for dimensionality reduction on given train data'))
  }
  r<-getPCA(train,...)  #get pca analysis on train data
  pcTrain=r[[1]] #get reduced dimension train data
  pcaAnalysis=r[[2]]  #get analysis object
  
  
  #get reduced dimension test data with matching number of PC components. Reuse the analysis object
  num_pc<- ncol(pcTrain)-1
  pcTest<-getPCA(test,num_pc=num_pc, pcaAnalysis = pcaAnalysis)[[1]]
  
  if(verbose){
    print(paste('Selected first',num_pc,'principal components of',ncol(train)))
  }
  
  
  return(list(train=pcTrain,test=pcTest))
}

# ************************************************
# splitStratifiedDf_reduceDimensions() :
#
# Splits a stratified data frame into test and train subsets
# All records belonging to the given foldId will be alloted to the test subset
# and all other records to the train subset. Maps train and test data frames into lower dimension
# using call to reduceDimensions()
#
# INPUT: data frame   - stratifiedDf       - data frame containing fold ids
#        int          - foldId             - fold number to allocate as test data
#        logical      - verbose            - allows print statements to be suppressed
#        ellipsis     - ...                - arguments passed to reduceDimensions() call necessary for 
#                                            dimension reduction
#
# OUTPUT : data frame - train data frame in reduced dimensions
#          data frame - test data frame in reduced dimensions
#
# ************************************************
splitStratifiedDf_reduceDimensions<-function(stratifiedDf,foldId,verbose=FALSE,...){
   r1<-splitStratifiedDf(stratifiedDf,foldId)
   train<-r1[["train"]]
   test<-r1[["test"]]
   
   train<-train[,which(names(train)!="fold")]
   test<-test[,which(names(test)!="fold")]
   
   r2<-reduceDimensions(train,test,verbose,...)
   train_reduced<-r2[["train"]]
   test_reduced<-r2[["test"]]
   
   return(list(train=train_reduced,test=test_reduced))
}


# ************************************************
# execExperimentOnReducedDimensions() :
#
# Invokes a delegate taking train and test data as input and returns a summary of its results
# Invokes the delegate to the experiment to perform K-fold-cross validation of the underlying model
# Returns mean results over all iteration
# Performs 'KFOLDS' iteration. To change number of iterations, modify global variable
# Experiments are performed on reduced dimension space. The information loss in the lower space
# is an adjustable parameter
#
# INPUT: data frame   - stratifiedDf       - data frame containing fold ids
#        double       - cumulativeVariance  -total variance of the orignal data to preserve
#                                           if equal to 1, number of dimensions will be equal to
#                                           the original data
#        function      - delegatedEXP       - experiment function to invoke
#        string       -  name_experiment    - name given to the experiment. will be added to the output result
#        ellipsis     -  ...                - arguments passed to the delegate function
#
# OUTPUT : list - summary of the experiment performance over KFOLD iteration
#
# ************************************************
execExperimentOnReducedDimensions<-function(stratifiedDf,cumulativeVariance,delegatedEXP,name_experiment,...){
  
  iterationResults<-data.frame()
  for (i in 1:KFOLDS){
    print(paste('Running experiment',name_experiment,'on fold',i))
    r<-splitStratifiedDf_reduceDimensions(stratifiedDf, foldId = i, cumulativeVariance = cumulativeVariance, verbose = i==1)
    evalReport<-data.frame(delegatedEXP(r[["train"]],r[["test"]],plot = i==1,...))
    iterationResults<-rbind(iterationResults,evalReport)
  } 
  
  meanResult<-round(colMeans(iterationResults),digits=2)
  meanResult<-c(Experiment=name_experiment, meanResult)
  return(as.list(meanResult))
}

# ************************************************
# random_forest() :
#
# Uses a random forest model to predict column defined in the global constant variable TARGET_VARIABLE
#
# INPUT: data frame   - train       - data frame subset used for fitting or training model
#        data frame   - test        - data frame subset used for measuring model performance
#        logical      - plot        - allows print statements and graphs to be suppressed
#        int          - forest_size - number of trees to use in forest estimator
#
# OUTPUT : list - model evaluation metrics such as precision, True Positives etc
#
# ************************************************
random_forest<-function(train,test,forest_size,mtry, plot=FALSE){
  
  
  predictors<-which(names(train)!=TARGET_VARIABLE)
  label<-which(names(train)==TARGET_VARIABLE)
  
  model<-randomForest(x=train[predictors],
                      factor(train[,label]),
                      ntree=forest_size,mtry=mtry)
  
  
  prediction_train<-predict(object = model, newdata = train)
  prediction_test<-predict(object = model, newdata = test)
  
  
  result<-getMeasures(test,train,prediction_test,prediction_train,plot)
  return(result)
}

# ************************************************
# getMeasures() :
#
# Uses model prediction probabilities to evaluate its performance. Uses call to detThreshold to
# find optimum threshold value
#
# INPUT: data frame       - train             - data frame subset used for fitting or training model
#        data frame       - test              - data frame subset used for measuring model performance
#        factor or vector - prediction_train  - model output probabilities on train data
#        factor or vector - prediction_test  -  model output probabilities on test data
#        logical          - plot              - allows print statements and graphs to be suppressed
#
# OUTPUT : list - model evaluation metrics such as precision, True Positives etc
#
# ************************************************
getMeasures<-function(train,test,prediction_train,prediction_test, plot=FALSE){
  if(is.factor(prediction_train)){
    prediction_train<-as.numeric(levels(prediction_train))[prediction_train]
  }
  
  if(is.factor(prediction_test)){
    prediction_test<-as.numeric(levels(prediction_test))[prediction_test]
  }
  
  
  label<-which(names(train)==TARGET_VARIABLE)
  
  threshold<-detThreshold(as.vector(train[,label]),as.vector(test[,label]),prediction_train, prediction_test,plot)
  
  prediction_train<-prediction_train>=threshold
  prediction_test<-prediction_test>=threshold
  
  a1<-factor(as.numeric(prediction_test))
  a2<-factor(as.numeric(test[label]==1))
  b1<-factor(as.numeric(prediction_train))
  b2<-factor(as.numeric(train[label]==1))
  
  cm1<-confusionMatrix(a1,a2,positive='1')
  cm2<-confusionMatrix(b1,b2,positive='1') 
  
  #z<-cm2$table
  result<-as.list(cm2$byClass)
  cm2$table<-round(matrixAsPercentage(cm2$table),0)
  result<-result[-which(names(result) %in% c('Prevalence','Detection Rate','Detection Prevalence','Balanced Accuracy'))]
  result<-c(
    TPRate= cm2$table[2,2],
    FPRate= cm2$table[2,1],
    TNRate= cm2$table[1,1],
    FNRate= cm2$table[1,2],
    result
  )
  
  if(plot){
    p<-ggplot(as.data.frame(cm2$table), aes(Prediction,sort(Reference,decreasing = T), fill= Freq)) +
      geom_tile() + geom_text(aes(label=Freq)) +
      scale_fill_gradient(low="white", high="#009194") +
      labs(x = "Prediction",y = "Reference") +
      scale_x_discrete(labels=c("0","1")) +
      scale_y_discrete(labels=c("1","0"))
    show(p)
  }
  return(result)
}

# ************************************************
# detThreshold() :
#
# Uses call to roc function from ‘pROC’ package to generate ROC curve and to determine best threshold
#
# INPUT: vector numeric   - a     - model output probabilities on test data
#        vector numeric   - b     - model output probabilities on train data
#        vector numeric   - c     - reference values for test label
#        vector numeric   - d     -  reference values for train label
#        logical          - plot  - allows print statements and graphs to be suppressed
#
# OUTPUT : double -   best determined threshold
#
# ************************************************
detThreshold<-function(a,b,c,d,plot){
  
  rocObj <- roc(a,c,smoothed = TRUE,ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                plot=plot, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
                print.auc=TRUE, show.thres=TRUE,quiet = TRUE)
  if(plot){
    print(rocObj)
  }
  return(as.numeric(coords(rocObj, "best", ret = "threshold")[1,1]))
}

# ************************************************
# xgbLogistic() :
#
# Applies extreme gradient boosting and binary logistic regression to
# predict column defined in the global constant variable TARGET_VARIABLE
#
# INPUT: data frame   - train       - data frame subset used for fitting or training model
#        data frame   - test        - data frame subset used for measuring model performance
#        logical      - plot        - allows print statements and graphs to be suppressed
#        int          - num_rounds  - number of iterations to reduce logloss eval metric
#
# OUTPUT : list - model evaluation metrics such as precision, True Positives etc
#
# ************************************************
xgbLogistic<-function(train,test, num_rounds, plot=FALSE){
  
  predictors<-which(names(train)!=TARGET_VARIABLE)
  label<-which(names(train)==TARGET_VARIABLE)
  
  model <- xgboost(data = data.matrix(train[predictors]), label = train[,label], nrounds = num_rounds, objective ="binary:logistic", verbose = 0)
  
  
  prediction_train<-predict(object = model, newdata = data.matrix(train[predictors]))
  prediction_test<-predict(object = model, newdata = data.matrix(test[predictors]))
  

  result<-getMeasures(train,test,prediction_train,prediction_test,plot)
  return(result)
}

# ************************************************
# matrixAsPercentage() :
#
# Converts values in each cell of a matrix to a percentage of
# the sum of all cells
#
# INPUT: matrix   - m       - input matrix
#
# OUTPUT : matrix - transformed matrix
#
# ************************************************
matrixAsPercentage<-function(m){
  s<-sum(m)
  for(i in 1:ncol(m)){
    for(j in 1:nrow(m)){
      m[i,j]<-m[i,j]*100/s
    }
  }
  return(m)
}

# ************************************************
# plotMostCorrelated() :
#
# Plots the most correlated columns in a data frame
#
# INPUT:  data frame   - dataset  - data.frame of interest
#
# OUTPUT : NONE
#
# ************************************************
plotMostCorrelated<-function(dataset,top=20){
  cr<-cor(dataset, use = "complete.obs")
  crdf <- as.data.frame(as.table(cr))
  crdf<-subset(crdf, abs(Freq) < 1)
  jm<-crdf[order(-crdf$Freq),]
  jm$Name <- paste(jm$Var1,jm$Var2,sep = ' + ')
  jm<-jm[,3:4]
  zjm<-jm[jm$Freq>0.5,]
  zjm<-zjm[1:top,]
  names(zjm)<-c('Correlation','Variables')
  p <- ggplot(zjm, aes(x =reorder(Variables, -Correlation) , y = Correlation))+
    geom_col(aes(fill = Correlation), width = 0.7)
  p<- p + xlab('Variables')
  q<- p + coord_flip() + labs(title = "Top correlated variable pairs")
  print(q)

}

# ************************************************
# plotTargetCorrelation() :
#
# Plots the columns most closely correlated to the column defined in 
# the constant TARGET_VARIABLE
#
# INPUT:  data frame   - dataset  - data.frame of interest
#
# OUTPUT : NONE
#
# ************************************************
plotTargetCorrelation<-function(dataset,top=20){
  cr<-cor(dataset, use = "complete.obs")
  crdf <- as.data.frame(as.table(cr))
  crdf<-subset(crdf, abs(Freq) < 1)
  jm<-crdf[order(-crdf$Freq),]
  jm<-jm[jm$Var1==TARGET_VARIABLE,]
  jm$Name <- paste(jm$Var2,sep = '')
  jm<-jm[,3:4]
  zjm<-jm
  zjm<-zjm[1:top,]
  names(zjm)<-c('Correlation','Variable')
  p <- ggplot(zjm, aes(x =reorder(Variable, -Correlation) , y = Correlation))+
    geom_col(aes(fill = Correlation), width = 0.7)
  p<- p + xlab('Variable')
  q<- p + coord_flip() + labs(title = "Variable most correlated to Churn")
  print(q)
}


Train_XGB<-function(train,num_rounds){
  predictors<-which(names(train)!=TARGET_VARIABLE)
  label<-which(names(train)==TARGET_VARIABLE)
  
  model <- xgboost(data = data.matrix(train[predictors]), label = train[,label], nrounds = num_rounds, objective ="binary:logistic", verbose = 0)
  
  prediction_train<-predict(object = model, newdata = data.matrix(train[predictors]))
  threshold<-detThreshold(as.vector(train[,label]),FALSE,prediction_train, FALSE,FALSE)
  return(list(model=model,threshold=threshold))
}