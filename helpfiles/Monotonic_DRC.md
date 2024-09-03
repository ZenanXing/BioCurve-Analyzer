#### Monotonic Curves

The following models are used to describe the monotonic curves. In the below formulae, y is the response and x is the dose/concentration.

#### - Log-logistic models

Log-logistic model has been frequently used to describe the typical sigmoidal dose-responsive curves.\
The function and demonstration of the curve is presented in the following plot. The parameters in the function are biologically meaningful, where `c` is the lower plateau of the curve (also known as the lower limit), `d` is the upper asymptote, `b` is the slope at inflection point of the curve, `e` is the estimated effective dose which gives the half of maximum response, and `f` is the parameter determining whether the curve is symmetric or not. "LL.2", "LL.3", "LL.3u", "LL.4", and "LL.5" provide the log-logistic functions with different number of fixed parameters.  
LL.2: c = 0, d = 1, f = 1  
LL.3: c = 0 , f = 1  
LL.3u: d = 1, f = 1  
LL.4: c ≠ 0, d ≠ 1, f = 1  

<p align="center">

<img src="log_logistic_model.png" alt="log-logistic model" width="70%"/>

</p>

#### - Weibull models

The two weibull models are used to describe the asymmetric dose-responsive curves. The Weibull-1 model [1] (long-dashed line) described the dose-response curve descends slowly from the upper limit, but on the other side, the curve approaches the lower limit rapidly. "W1.2", "W1.3", "W1.3u", and "W1.4" provide this model with the different number of fixed parameters. While the Weibull-2 model [2] (short-dashed line) described a different form of asymmetry with rapid change or descent from the upper limit, but a slow approach toward the lower limit. "W2.2", "W2.3", "W2.3u", and "W2.4" provide this model with the different number of fixed parameters.  
W1.2/W2.2: c = 0, d = 1  
W1.3/W2.3: c = 0  
W1.3u/W2.3u: d = 1  
W1.4/W2.4: c ≠ 0, d ≠ 1  

<p align="center">

<img src="weibull_models.png" alt="weibull model" width="70%"/>

</p>

#### References

*Gottschalk PG, Dunn JR (2005) The five-parameter logistic: a characterization and comparison with the four-parameter logistic. Anal Biochem 343: 54--65*\
*Steven S. Seefeldt, Jens Erik Jensen, E. Patrick Fuerst (1995) Log-Logistic Analysis of Herbicide Dose-Response Relationships. Weed Technol 9: 218--227*\
*Ritz C (2010) Toward a unified approach to dose-response modeling in ecotoxicology. Environ Toxicol Chem 29: 220--229*
