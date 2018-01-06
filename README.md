# MLE-project
Project for class ML for Ecox, UVA 2017.

 For this project we used data from online marketplace to predict which users will pay for promotion of their goods. We utilized the information stored in profiles as well as the information from description and headline of the goods. For this purpose we used {text2vec} package. After engineering the features we used Neural Nets (implemented via {keras} package) for predictions. The obtained results were decent given the fact that the response class was extremely underrepresented (~2%). We also used online SVMs and logistic regression model, nevertheless, these did not yield meaningful results. 
