# Onion-Analytics
Small-scale farmers throughout the world are exposed to variability in the prices they get for their crops. Most farmers typically sell their produce at the spot price in the few weeks after they harvest their crop. This translates into two problems for many farmers: uncertainty of income and selling at a suboptimal price. What added value can small-scale farmers get by selectively picking the time of sale of their crops? Specifically, would the extra-income generated from selling at optimal price outweigh the cost of inventorying these crops?

---

## Requirements:
* We provide the final ML forecasting tool in the folder titled *Final Tool*. The code for this tool is given *PricePredictionsFinalTool.R* file which needs the *DataEngineeredFinal.csv* (see how this dataset is obtained below). This tool outputs *ResultsFinalTool.csv* which compares the actual prices vs the prices predicted. All the datasets needed for different codes are included in their respective folders and so one can directly use the csv files given here without needing to run the code to form these csv data files.
---


## The code is oraganized as follows:
* To web-scrap the onion prices, run the *Onion-Analytics/Data Sourcing/Webscrapping_Onions.ipynb* file<br/><br/>

* To create the final dataset (DataEngineeredFinal.csv) used in this project, run the *Onion-Analytics/Data Engineering/DataEngineeringMethod1.R* file
  * Be sure to adjust the working directory in line 6 <br/><br/>

* The Lasso and SVM code written for this project can be found in *Onion-Analytics/Analysis of Models/ModelLassoSVM.R* <br/><br/>

* The Neural network code written for this project can be found in *Onion-Analytics/Analysis of Models/Final_NN_model.R* <br/><br/>

* To compute the value of this ML forecasting tool run *Onion-Analytics/Business Value/ValueOnionVFinal.R*
---

## Checkout the folder *Presentation And Report* for a presentation and details of this project
