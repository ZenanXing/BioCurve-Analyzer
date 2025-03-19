### Model Assessment Methods

In order to determine whether the best-fitting model is appropriate for your analysis, four statistical tests were introduced. They have been frequently used in the model assessment in the dose-response analysis. The users are recommended to incorporating all of them to attain a more comprehensive understanding of their model list. Below is a brief overview of these tests and their null hypotheses. For detailed differences of these methods, please refer to the original references listed at the end.  
**- Lack-of-fit test/Neill's test**: Both are ANOVA-based tests. The lack-of-fit test is a classic test for model assessment, while Neill's test is an enhanced version of the lack-of-fit test that does not require replicates of the dose values. They share the same null hypothesis:    
&emsp;*H~0~*: A difference exists between the selected model and the general ANOVA model.  
**- No-effect test**: This test assesses whether there is any dose-response relationship, with the following null hypothesis:  
&emsp;*H~0~*: Data fits a simple linear regression model with slope 0, i.e. a horizontal regression line corresponding to no dose effect.  
**- Parameters ≠ 0  test**: This test examines each parameter in the model individually, with the null hypothesis as follows:  
&emsp;*H~0~*: The estimated parameter is not different from zero.  
If you got "Significant" from this test, it means all the parameters are significantly different from zero. Conversely, if you got "Non-significant", it indicates at least one parameter is not significantly different form zero.  
Please set the cutoff of the *p-values* for these tests based on your own experimental design. The app will indicate whether the results are significant, with the actual p-values listed in the table. An asterisk (*) next to the p-values signifies a significant test result.  
  
#### References

*Ritz C, Martinussen T (2011) Lack-of-fit tests for assessing mean structures for continuous dose-response data. Environ Ecol Stat 18: 349–366*\
*Ritz C, Baty F, Streibig JC, Gerhard D (2015) Dose-Response Analysis Using R. PLoS One 10: e0146021*