> trainData <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/train/X_train.txt")
> dim(trainData)
[1] 7352  561
> head(trainData)


         V1          V2         V3         V4         V5         V6         V7         V8         V9        V10
1 0.2885845 -0.02029417 -0.1329051 -0.9952786 -0.9831106 -0.9135264 -0.9951121 -0.9831846 -0.9235270 -0.9347238
2 0.2784188 -0.01641057 -0.1235202 -0.9982453 -0.9753002 -0.9603220 -0.9988072 -0.9749144 -0.9576862 -0.9430675
3 0.2796531 -0.01946716 -0.1134617 -0.9953796 -0.9671870 -0.9789440 -0.9965199 -0.9636684 -0.9774686 -0.9386916
4 0.2791739 -0.02620065 -0.1232826 -0.9960915 -0.9834027 -0.9906751 -0.9970995 -0.9827498 -0.9893025 -0.9386916
5 0.2766288 -0.01656965 -0.1153619 -0.9981386 -0.9808173 -0.9904816 -0.9983211 -0.9796719 -0.9904411 -0.9424691
6 0.2771988 -0.01009785 -0.1051373 -0.9973350 -0.9904868 -0.9954200 -0.9976274 -0.9902177 -0.9955489 -0.9424691
         
> trainLabel <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/train/y_train.txt")
> table(trainLabel)
trainLabel
   1    2    3    4    5    6 
1226 1073  986 1286 1374 1407 
> trainSubject <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/train/subject_train.txt")
> testData <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/test/X_test.txt")
> dim(testData)
[1] 2947  561
> testLabel <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/test/y_test.txt") 
> table(testLabel)
testLabel
  1   2   3   4   5   6 
496 471 420 491 532 537 
> testSubject <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/test/subject_test.txt")
> joinData <- rbind(trainData, testData)
> dim(joinData)
[1] 10299   561
> joinLabel <- rbind(trainLabel, testLabel)
> dim(joinLabel) 
[1] 10299     1
> joinSubject <- rbind(trainSubject, testSubject)
> dim(joinSubject)
[1] 10299     1
> features <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/features.txt")
> dim(features) 
[1] 561   2
> meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
> length(meanStdIndices)
[1] 66
> joinData <- joinData[, meanStdIndices]
> dim(joinData)
[1] 10299    66
> names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
> names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
> names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
> names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 
> names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
> names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
> names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
> names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 
> 
> # Step3. Uses descriptive activity names to name the activities in the data set
> activity <- read.table("/Users/Nawjif/Documents/WebEx/UCI HAR Dataset/activity_labels.txt")
> activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
> substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
> substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
> activityLabel <- activity[joinLabel[, 1], 2]
> joinLabel[, 1] <- activityLabel
> names(joinLabel) <- "activity"
> 
> # Step4. Appropriately labels the data set with descriptive activity names. 
> names(joinSubject) <- "subject"
> cleanedData <- cbind(joinSubject, joinLabel, joinData)
> dim(cleanedData) 
[1] 10299    68
> write.table(cleanedData, "merged_data.txt") # write out the 1st dataset
> 
> # Step5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
> subjectLen <- length(table(joinSubject)) 
> activityLen <- dim(activity)[1] 
> columnLen <- dim(cleanedData)[2]
> result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
> result <- as.data.frame(result)
> colnames(result) <- colnames(cleanedData)
> row <- 1
> for(i in 1:subjectLen) {
+     for(j in 1:activityLen) {
+         result[row, 1] <- sort(unique(joinSubject)[, 1])[i]
+         result[row, 2] <- activity[j, 2]
+         bool1 <- i == cleanedData$subject
+         bool2 <- activity[j, 2] == cleanedData$activity
+         result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
+         row <- row + 1
+     }
+ }
> head(result)
  subject          activity tBodyAccMeanX tBodyAccMeanY tBodyAccMeanZ tBodyAccStdX tBodyAccStdY tBodyAccStdZ
1       1           walking     0.2773308  -0.017383819    -0.1111481  -0.28374026  0.114461337  -0.26002790
2       1   walkingUpstairs     0.2554617  -0.023953149    -0.0973020  -0.35470803 -0.002320265  -0.01947924
3       1 walkingDownstairs     0.2891883  -0.009918505    -0.1075662   0.03003534 -0.031935943  -0.23043421
4       1           sitting     0.2612376  -0.001308288    -0.1045442  -0.97722901 -0.922618642  -0.93958629
5       1          standing     0.2789176  -0.016137590    -0.1106018  -0.99575990 -0.973190056  -0.97977588
6       1            laying     0.2215982  -0.040513953    -0.1132036  -0.92805647 -0.836827406  -0.82606140
  tGravityAccMeanX tGravityAccMeanY tGravityAccMeanZ tGravityAccStdX tGravityAccStdY tGravityAccStdZ tBodyAccJerkMeanX
1        0.9352232       -0.2821650      -0.06810286      -0.9766096      -0.9713060      -0.9477172        0.07404163
2        0.8933511       -0.3621534      -0.07540294      -0.9563670      -0.9528492      -0.9123794        0.10137273
3        0.9318744       -0.2666103      -0.06211996      -0.9505598      -0.9370187      -0.8959397        0.05415532
4        0.8315099        0.2044116       0.33204370      -0.9684571      -0.9355171      -0.9490409        0.07748252
5        0.9429520       -0.2729838       0.01349058      -0.9937630      -0.9812260      -0.9763241        0.07537665
6       -0.2488818        0.7055498       0.44581772      -0.8968300      -0.9077200      -0.8523663        0.08108653
  tBodyAccJerkMeanY tBodyAccJerkMeanZ tBodyAccJerkStdX tBodyAccJerkStdY tBodyAccJerkStdZ tBodyGyroMeanX tBodyGyroMeanY
1      0.0282721096      -0.004168406      -0.11361560        0.0670025       -0.5026998    -0.04183096    -0.06953005
2      0.0194863076      -0.045562545      -0.44684389       -0.3782744       -0.7065935     0.05054938    -0.16617002
3      0.0296504490      -0.010971973      -0.01228386       -0.1016014       -0.3457350    -0.03507819    -0.09093713
4     -0.0006191028      -0.003367792      -0.98643071       -0.9813720       -0.9879108    -0.04535006    -0.09192415
5      0.0079757309      -0.003685250      -0.99460454       -0.9856487       -0.9922512    -0.02398773    -0.05939722
6      0.0038382040       0.010834236      -0.95848211       -0.9241493       -0.9548551    -0.01655309    -0.06448612
  tBodyGyroMeanZ tBodyGyroStdX tBodyGyroStdY tBodyGyroStdZ tBodyGyroJerkMeanX tBodyGyroJerkMeanY tBodyGyroJerkMeanZ
1     0.08494482    -0.4735355  -0.054607769    -0.3442666        -0.08999754        -0.03984287        -0.04613093
2     0.05835955    -0.5448711   0.004105184    -0.5071687        -0.12223277        -0.04214859        -0.04071255
3     0.09008501    -0.4580305  -0.126349195    -0.1247025        -0.07395920        -0.04399028        -0.02704611
4     0.06293138    -0.9772113  -0.966473895    -0.9414259        -0.09367938        -0.04021181        -0.04670263
5     0.07480075    -0.9871919  -0.987734440    -0.9806456        -0.09960921        -0.04406279        -0.04895055
6     0.14868944    -0.8735439  -0.951090440    -0.9082847        -0.10727095        -0.04151729        -0.07405012
  tBodyGyroJerkStdX tBodyGyroJerkStdY tBodyGyroJerkStdZ tBodyAccMagMean tBodyAccMagStd tGravityAccMagMean
1        -0.2074219        -0.3044685        -0.4042555     -0.13697118    -0.21968865        -0.13697118
2        -0.6147865        -0.6016967        -0.6063320     -0.12992763    -0.32497093        -0.12992763
3        -0.4870273        -0.2388248        -0.2687615      0.02718829     0.01988435         0.02718829
4        -0.9917316        -0.9895181        -0.9879358     -0.94853679    -0.92707842        -0.94853679
5        -0.9929451        -0.9951379        -0.9921085     -0.98427821    -0.98194293        -0.98427821
6        -0.9186085        -0.9679072        -0.9577902     -0.84192915    -0.79514486        -0.84192915
  tGravityAccMagStd tBodyAccJerkMagMean tBodyAccJerkMagStd tBodyGyroMagMean tBodyGyroMagStd tBodyGyroJerkMagMean
1       -0.21968865         -0.14142881        -0.07447175      -0.16097955      -0.1869784           -0.2987037
2       -0.32497093         -0.46650345        -0.47899162      -0.12673559      -0.1486193           -0.5948829
3        0.01988435         -0.08944748        -0.02578772      -0.07574125      -0.2257244           -0.2954638
4       -0.92707842         -0.98736420        -0.98412002      -0.93089249      -0.9345318           -0.9919763
5       -0.98194293         -0.99236779        -0.99309621      -0.97649379      -0.9786900           -0.9949668
6       -0.79514486         -0.95439626        -0.92824563      -0.87475955      -0.8190102           -0.9634610
  tBodyGyroJerkMagStd fBodyAccMeanX fBodyAccMeanY fBodyAccMeanZ fBodyAccStdX fBodyAccStdY fBodyAccStdZ fBodyAccJerkMeanX
1          -0.3253249   -0.20279431   0.089712726    -0.3315601  -0.31913472   0.05604001  -0.27968675       -0.17054696
2          -0.6485530   -0.40432178  -0.190976721    -0.4333497  -0.33742819   0.02176951   0.08595655       -0.47987525
3          -0.3065106    0.03822918   0.001549908    -0.2255745   0.02433084  -0.11296374  -0.29792789       -0.02766387
4          -0.9883087   -0.97964124  -0.944084550    -0.9591849  -0.97641231  -0.91727501  -0.93446956       -0.98659702
5          -0.9947332   -0.99524993  -0.977070848    -0.9852971  -0.99602835  -0.97229310  -0.97793726       -0.99463080
6          -0.9358410   -0.93909905  -0.867065205    -0.8826669  -0.92443743  -0.83362556  -0.81289156       -0.95707388
  fBodyAccJerkMeanY fBodyAccJerkMeanZ fBodyAccJerkStdX fBodyAccJerkStdY fBodyAccJerkStdZ fBodyGyroMeanX fBodyGyroMeanY
1       -0.03522552        -0.4689992       -0.1335866        0.1067399       -0.5347134     -0.3390322    -0.10305942
2       -0.41344459        -0.6854744       -0.4619070       -0.3817771       -0.7260402     -0.4926117    -0.31947461
3       -0.12866716        -0.2883347       -0.0863279       -0.1345800       -0.4017215     -0.3524496    -0.05570225
4       -0.98157947        -0.9860531       -0.9874930       -0.9825139       -0.9883392     -0.9761615    -0.97583859
5       -0.98541870        -0.9907522       -0.9950738       -0.9870182       -0.9923498     -0.9863868    -0.98898446
6       -0.92246261        -0.9480609       -0.9641607       -0.9322179       -0.9605870     -0.8502492    -0.95219149
  fBodyGyroMeanZ fBodyGyroStdX fBodyGyroStdY fBodyGyroStdZ fBodyAccMagMean fBodyAccMagStd fBodyBodyAccJerkMagMean
1    -0.25594094    -0.5166919   -0.03350816    -0.4365622     -0.12862345     -0.3980326             -0.05711940
2    -0.45359721    -0.5658925    0.15153891    -0.5717078     -0.35239594     -0.4162601             -0.44265216
3    -0.03186943    -0.4954225   -0.18141473    -0.2384436      0.09658453     -0.1865303              0.02621849
4    -0.95131554    -0.9779042   -0.96234504    -0.9439178     -0.94778292     -0.9284448             -0.98526213
5    -0.98077312    -0.9874971   -0.98710773    -0.9823453     -0.98535636     -0.9823138             -0.99254248
6    -0.90930272    -0.8822965   -0.95123205    -0.9165825     -0.86176765     -0.7983009             -0.93330036
  fBodyBodyAccJerkMagStd fBodyBodyGyroMagMean fBodyBodyGyroMagStd fBodyBodyGyroJerkMagMean fBodyBodyGyroJerkMagStd
1             -0.1034924           -0.1992526          -0.3210180               -0.3193086              -0.3816019
2             -0.5330599           -0.3259615          -0.1829855               -0.6346651              -0.6939305
3             -0.1040523           -0.1857203          -0.3983504               -0.2819634              -0.3919199
4             -0.9816062           -0.9584356          -0.9321984               -0.9897975              -0.9870496
5             -0.9925360           -0.9846176          -0.9784661               -0.9948154              -0.9946711
6             -0.9218040           -0.8621902          -0.8243194               -0.9423669              -0.9326607

> write.table(result, file = "/Users/Nawjif/Documents/WebEx/hasan.txt")
