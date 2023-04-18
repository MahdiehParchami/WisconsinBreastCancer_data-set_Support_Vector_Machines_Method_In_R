# WisconsinBreastCancer_data-set_Support_Vector_Machines_Method_In_R
Implementing Support Vector Machines Method on Wisconsin Breast Cancer data set In R

I use Wisconsin Breast Cancer data set from Kaggle to build a Support Vector Machines (SVM) classifier that can predict whether a cell is Malignant or Benign.

Interpretation and Recommendations: 

As we can see in the SVM plots, the points in the boxes are close to the decision boundary and are instrumental in determining that boundary.In addition, points that are represented by an “X” are the support vectors or the points that directly affect the classification line. The points marked with an “o” are the other points, which don’t affect the calculation of the line. Moreover, each point that falls on the Malignant side of the decision boundary will be classified as Malignant and each one that falls on the Benign side will be considered a Benign class. Based on this classification we can discover that the new data will belong to which classifications. Because SMV observes similarities between new data and the old data that have been classified as Malignant or Benign. Once it finds similarities between them, it identifies classification for new data.
Based on the high accuracy of the models we can recommend that the most important variables that can be used for breast cancer diagnosis are concavity_mean, perimeter_se, area_se concave.point, radius_mean, texture_mean and perimeter_mean.

