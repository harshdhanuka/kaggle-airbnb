## kaggle-airbnb
This repository contains information about the Kaggle Closed Competition on "Airbnb Rental Price Predictor Model". 

##### The competition name was 'Predicting Rental Price': Predict rental price using data on renter, property and reviews.

##### Following description has been obtained directly from the Kaggle Competition page. 

#### Description:
People interested in renting an apartment or home, share information about themselves and their property on Airbnb. Those who end up renting the property share their experiences through reviews. The dataset contains information on 90 variables related to the property, host, and reviews for over 35,000 Airbnb rentals in New York.

#### Goal:
Construct a model using the dataset supplied and use it to predict the price of a set of Airbnb rentals included in scoringData.csv.

#### Metric:
Submissions will be evaluated based on RMSE (root mean squared error) (Wikipedia). Lower the RMSE, better the model.

#### Submission File:
The submission file should be in text format (.csv) with only two columns, id and price. The price column must contain predicted price. Number of decimal places to use is up to you. The file should contain a header and have the following format:

"id","price"

23136,133.569231078017

An example of the sample submission file (sample_submission.csv) is shared with the set of files.


## Sample Code

Here is an illustration in R of how you can create a model, apply it to scoringData.csv and prepare a submission file (sample_submission.csv).

#### For the following code to work, ensure analysisData.csv and scoringData.csv are in your working directory.

#### Read data and construct a simple model
data = read.csv('analysisData.csv')
model = lm(price~minimum_nights+review_scores_rating,data)

#### Read scoring data and apply model to generate predictions
scoringData = read.csv('scoringData.csv')
pred = predict(model,newdata=scoringData)

#### Construct submission from predictions
submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission.csv',row.names = F)


## File descriptions

analysisData.csv: Data for building a model

scoringData.csv: Use for applying predictings or scoring

sample_submission.csv: Sample submission file in the correct format
