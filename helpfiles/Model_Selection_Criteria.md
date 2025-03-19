### Model Selection Criteria

While selecting a model based on established knowledge is ideal for data analysis, it can be challenging to identify the best model for complex or *in vivo* assays. To assist in this process, we introduce four different criteria available in the `drc` package to help you identify the best-fitting model from the candidates you selected. Below is a brief introduction to these criteria.  
**- Akaike Information Criterion (AIC):**  
•	Purpose: Used to compare the relative quality of statistical models for a given dataset.  
•	Calculation: AIC is calculated as ${AIC} = 2k - 2\ln(L)$, where ( k ) is the number of parameters in the model and ( L ) is the maximum likelihood of the model.  
•	Considerations: Penalizes the number of parameters to discourage overfitting. A lower AIC value indicates a better model fit.  
**- Bayesian Information Criterion (BIC):**  
•	Purpose: Similar to AIC, it compares model fit and is particularly useful when the sample size is large.  
•	Calculation: BIC is computed as ${BIC} = k \ln(n) - 2\ln(L)$, where ( n ) is the number of observations.  
•	Considerations: Like AIC, but imposes a larger penalty for models with more parameters, which can make it more conservative in model selection.  
**- Lack-of-fit Test:**  
•	Purpose: Tests whether a model adequately fits the data or if there is significant lack of fit.  
•	Calculation: Derived from an analysis of variance (ANOVA) comparing the model to a general ANOVA model.  
•	Considerations: A high p-value suggests that there is no significant lack of fit, implying the model fits the data well; a low p-value indicates significant lack of fit, suggesting the model may not be appropriate.  
**- Residual Standard Errors:**  
•	Purpose: Measures the typical size of errors made by a regression model; indicates how well the model fits the observed data.  
•	Calculation: Derived from the residuals (differences between observed and predicted values) and adjusted for degrees of freedom.  
•	Considerations: Provides an absolute measure of fit; smaller values indicate better model fit. Unlike AIC and BIC, does not inherently account for model complexity.  

In summary, AIC and BIC are model selection criteria balancing fit and complexity, with BIC being more conservative. RSE provides a direct measure of fit quality, independent of model complexity. Lack of fit p-values evaluate the adequacy of fit, helping to determine if a model captures the data's structure sufficiently.  

#### References

*Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One 10: e0146021*