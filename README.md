# Telecom Customer Churn

## Team Members
- [Rishi Khimani](https://github.com/equitharn/)
- [Valencia Dias](https://github.com/Valencia3112/)
- [Vinit Wani](https://gitlab.com/equitharn/)

## Problem statement 
Descriptive Analysis of telecom customer churn

## Data Dictionary

| Variable Name    	| Description                                                                                                        | Datatype |
|------------------	|--------------------------------------------------------------------------------------------------------------------|----------|
| customerID       	| Customer ID                                                                                                        | object   |
| gender           	| Whether the customer is a male or a female                                                                         | object   |
| SeniorCitizen    	| Whether the customer is a senior citizen or not (1, 0)                                                             | int64  	|
| Partner          	| Whether the customer has a partner or not (Yes, No)                                                                | object  	|
| Dependents       	| Whether the customer has dependents or not (Yes, No)                                                               | object   |
| tenure           	| Number of months the customer has stayed with the company                                                          | int64    |
| PhoneService     	| Whether the customer has a phone service or not (Yes, No)                                                          | object   |
| MultipleLines    	| Whether the customer has multiple lines or not (Yes, No, No phone service)                                         | object   |
| InternetService  	| Customer’s internet service provider (DSL, Fiber optic, No)                                                        | object   |
| OnlineSecurity   	| Whether the customer has online security or not (Yes, No, No internet service)                                     | object   |
| OnlineBackup     	| Whether the customer has online backup or not (Yes, No, No internet service)                                       | object   |
| DeviceProtection 	| Whether the customer has device protection or not (Yes, No, No internet service)                                   | object   |
| TechSupport      	| Whether the customer has tech support or not (Yes, No, No internet service)                                        | object   |
| StreamingTV      	| Whether the customer has streaming TV or not (Yes, No, No internet service)                                        | object   |
| StreamingMovies  	| Whether the customer has streaming movies or not (Yes, No, No internet service)                                    | object   |
| Contract         	| The contract term of the customer (Month-to-month, One year, Two year)                                             | object   |
| PaperlessBilling 	| Whether the customer has paperless billing or not (Yes, No)                                                        | object   |
| PaymentMethod    	| The customer’s payment method (Electronic check, Mailed check, Bank transfer (automatic), Credit card (automatic)) | object   |
| MonthlyCharges   	| The amount charged to the customer monthly                                                                         | float64  |
| TotalCharges     	| The total amount charged to the customer                                                                           | object   |
| Churn            	| Whether the customer churned or not (Yes or No)                                                                    | object   |