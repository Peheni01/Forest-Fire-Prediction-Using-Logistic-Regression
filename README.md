# Forest-Fire-Prediction-Using-Logistic-Regression

This project presents a logistic regression-based predictive model for forecasting the occurrence of forest fires in Algeria. Utilizing a dataset containing environmental variables and historical fire data from 2012, the model aims to evaluate the likelihood of fire outbreaks based on factors such as temperature, humidity, wind speed, and moisture content.

### Tools & Technologies  
- R - Programming Language for Data Analysis 
- R Packages:
  ```bash
  ggplot2: For creating advanced visualizations.
  GGally: For generating pairwise plots and visual explorations.
  corrplot: For visualizing correlation matrices.
  tidyverse: For data manipulation and visualization.
  epitools: For epidemiological data analysis and visualization.
  RColorBrewer: For enhancing the visual appeal of plots with color schemes.
  ```
### Key features
- Logistic Regression Analysis: Utilizes logistic regression to predict the binary outcome of forest fire occurrence.
- Data Preprocessing: Includes adding new variables, converting data types, and ensuring data cleanliness for accurate analysis.
- Visualization: Employs various plotting functions to visualize relationships between variables and the impact of each on the likelihood of fire occurrence.
- Model Evaluation: Involves splitting the dataset into training and testing sets to validate the accuracy of the predictive model. Misclassification error rates are calculated to assess model performance.

### 📊 Dashboard
![image alt](https://github.com/Peheni01/Forest-Fire-Prediction-Using-Logistic-Regression/blob/badc529afaaba6ab2a4355c90b7ec285e39fa51a/1.%20Dashboard.png)
![image alt](https://github.com/Peheni01/Forest-Fire-Prediction-Using-Logistic-Regression/blob/badc529afaaba6ab2a4355c90b7ec285e39fa51a/2.%20Dashboard.png)
![image alt](https://github.com/Peheni01/Forest-Fire-Prediction-Using-Logistic-Regression/blob/badc529afaaba6ab2a4355c90b7ec285e39fa51a/3.%20Dashboard.png)
![image alt](https://github.com/Peheni01/Forest-Fire-Prediction-Using-Logistic-Regression/blob/badc529afaaba6ab2a4355c90b7ec285e39fa51a/4.%20Dashboard.png)

### Results Analysis and Discussion
The logistic regression model provides a predictive framework for assessing the risk of forest fires based on environmental conditions. By training the model on 80% of the data and testing on the remaining 20%, the project achieves a high accuracy level, demonstrating the model's effectiveness in predicting fire occurrences. The analysis highlights the significant impact of factors such as temperature, humidity, and wind speed on fire dynamics.

### Conclusion
The project successfully applies logistic regression to predict forest fires, offering a valuable tool for forest management authorities and policymakers. By understanding the key factors that contribute to fire outbreaks, strategies can be developed to allocate resources effectively and mitigate the impact of fires, enhancing the resilience of forest ecosystems against fire threats.

### References
- [UCI Machine Learning Repository](https://archive.ics.uci.edu/dataset/547/algerian+forest+fires+dataset)
