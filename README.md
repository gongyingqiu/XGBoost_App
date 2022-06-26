## Summary

This App is designed to build and visualise XGBoost non-linear models without coding. It is built under R Shiny and allows user to pre-process the data, build, visualise and compare different XGBoost models easily. This is especially useful if you are using Emblem given the similar setup for visualisation

Example data used are from [https://www.kaggle.com/code/floser/glm-neural-nets-and-xgboost-for-insurance-pricing/notebook](https://www.kaggle.com/code/floser/glm-neural-nets-and-xgboost-for-insurance-pricing/notebook)

## User Manual


### Data Import and pre-processing (DATA Tab)

![image](https://user-images.githubusercontent.com/97180173/175770872-9516dc39-d56b-4d16-8e9f-a3c872b171ea.png)

- Data import
    - Import the data, please note that data need to be in Rds. format. Choices allowing other import formats are currently under development.
    - Data is available for viewing and summary statistics also been populated.
    
    ![image](https://user-images.githubusercontent.com/97180173/175769748-4c6bd61d-2233-4b85-a0f3-7459f67c1ea2.png)



- Data specification
    - In this session, we have the opportunities to filter factors that required in the model, specify model structure and also select the weight and label/response.
    - Please note, if it is a frequency model or any models with skewed response, choose response to be number of claims (not claim frequency) and set offset to be exposure.
    - Modelling partition factor should be created prior using the app which should have 3 levels (1,2,3), 1 for training, 2 for testing (used for early stopping which reduce the chances of overfitting) and 3 for validation. The option of creating partition factor in-app is currently under development
    - The structure can be saved and imported to reduce manual work in future
    
    ![image](https://user-images.githubusercontent.com/97180173/175770090-a868518a-e3bb-4bd2-a330-c0067ca9a713.png)

- Data manipulation
    - This session allows user to format the data before modelling, e.g. re-band/group categorical and numerical variables to minimise data volatility. As such, pre-app data processing is not required. However, we do suggest certain levels of pre-app data processing to reduce the time spent in-app.
    - In order to use it, choose a feature that required formatting from ‘select factor’ list and click ‘change current factor mapping’ button, it will then bring another table underneath, in which you can modify the column ‘new_factor_level’, then click button ‘confirm current mapping’. Tips: you can actually copy the table to excel by click button ‘csv’, re-band in excel (as this could be simpler than manually type the mapping one by one) and copy back using button ‘paste mapping from clipboard’. Removing the mappings also possible by click the button 'click all factor mapping'.
    - Once finished, click button 'create modelling dataset', this will then create the modelling data where categorical variables been one-hot-encodered.
    
    ![image](https://user-images.githubusercontent.com/97180173/175770293-aa3d2a98-0357-45f6-92b5-6da311235e10.png)

    ![image](https://user-images.githubusercontent.com/97180173/175770299-792835ad-26d2-4715-81a9-05184b1a9bd5.png)

### Modelling (MODEL Tab)

![image](https://user-images.githubusercontent.com/97180173/175771827-e0779f40-2485-474e-a201-c4d83bb2eaa2.png)


- Specify parameter:
    - This allows user to specify the parameters for the modelling, please note, if it’s frequency model, ‘max delta step’ should be set around 0.7 given the skewed response, this will have significant impact on the model accuracy.
    - Specify the factors to included in the model from ‘Factor list’ on the left hand pane.
    - The parameters and selected factors can be saved and imported to reduce the manual work in future
    - Once happy, click ‘fit current model’
    - Tips for parameter setting
        - Lower learning rate should be paired with higher number of trees allowing maximum model variance explained
        - Lower value of max depth should be offset by larger number of trees
        - For larger set of data, e.g. data with large number of rows and features, set learning rate a bit higher, e.g. >0.3 so that less trees required, vice versa.

    ![image](https://user-images.githubusercontent.com/97180173/175770624-a4d2f048-78e4-4c1e-8bf5-10b544d250fc.png)

- Model visualisation
    - This is the main visualisation for the modelling. Actual vs Expected, Shap values can be viewed for each factor split by modelling/Test/Validation
    - Shap can be calculated on both normal / approximate way. Tips: if your data is very big, approximate calculation suggested as normal calculation can take really long time.
    ![image](https://user-images.githubusercontent.com/97180173/175770643-a823d280-bedb-40d7-9554-438dba7375d7.png)
  
    - The green line on the chart is the weighted average contribute of Shap for a particular factor, the proper plot is underneath
    
    ![image](https://user-images.githubusercontent.com/97180173/175770650-0c1ecd4b-51fd-42d5-8a32-175e8de428d0.png)


- Model validation
    - This shows the feature importance ranking and also certain validation metrics. e.g. gini, gains curve
    
    ![image](https://user-images.githubusercontent.com/97180173/175771095-0ce72238-90d6-40a2-aa08-77a867b94bee.png)


### Investigation (INVESTIGATION Tab)

![image](https://user-images.githubusercontent.com/97180173/175771841-e8f925a9-b89e-487a-814b-cfe67a45894e.png)


- 2 way actual vs expected plot

![image](https://user-images.githubusercontent.com/97180173/175771252-b609bd79-1005-4735-9884-e91266aa6fd5.png)



Future development 

- Interactions
- Lift curve
- Correlation matrix
- auto numeric banding
- auto model partition
- Import data format
- Save the processed data
