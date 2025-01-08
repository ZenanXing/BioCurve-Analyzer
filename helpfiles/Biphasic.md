### Biphasic Curves

The following models are used to describe the biphasic curves.

#### - Brain-Cousens models

The Brain-Cousens model is the one of the earliest attempt scientists has been made to describe the biphasic curves. It is the modified version of Log-logistic model. The `f` is introduced into the classic Log-logistic model. `d` and `c` retain their interpretation as the upper and lower limit of the dose-response curve, while the other parameters lost their original biological meaning in the Log-logistic model. The curvature of the biphasic effect is described by `f`.  

<p align="center">

<img src="brain_cousens_model.png" alt="brain-cousens model" width="70%"/>

</p>

#### - Cedergreen-Ritz-Streibig models

The Cedergreen-Ritz-Streibig model is a modified version of the Brain-Cousens model. It is more robust in handling data variation and can better describe both very large and relatively small hormetic effects. The improvement in this model comes from the introduction of a new parameter, `𝛼`, which controls the rate at which the hormetic effect manifests. However, since sufficient data are rarely available for the low end of the dose-response curve to reliably estimate `𝛼`, we fixed it at one of the three levels - 0.25, 0.5, or 1, as suggested. `d` and `c` retain their interpretation as the upper and lower limit of the dose-response curve,  while the remaining parameters lose their direct biological meaning. This model provides two equations, with the second specifically designed to describe u-shaped curves.  

<p align="center">

<img src="cedergreen_ritz_streibig_model.png" alt="cedergreen-ritz-streibig model" width="70%"/>

</p>

#### - Beta models

The beta function is commonly used as a skewed probability density function in statistics, and it has been adapted to describe the bell-shaped dose-response curves. `a` and `b` are used to describe the curvatrue of curves.  
<p align="center">

<img src="beta_model.png" alt="beta model" width="70%"/>

</p>

#### References

*Brain P, Cousens R (1989) An equation to describe dose responses where there is stimulation of growth at low doses. Weed Res 29: 93--96*\
*Cedergreen N, Ritz C, Streibig JC (2005) Improved empirical models describing hormesis. Environ Toxicol Chem 24: 3166--3172*\
*Nweke CO, Nwangwu OR, Ogbonna CJ, Okechi RN (2021) Statistical modeling of effective doses in hormetic dose-response relationships by reparameterization of a bilogistic model for inverted U-shaped curves. AJB 20: 451--464*\
*Yin X, Kropff MJ, McLaren G, Visperas RM (1995) A nonlinear model for crop development as a function of temperature. Agric For Meteorol 77: 1--16*\
*Yin X, Goudriaan J, Lantinga EA, Vos J, Spiertz HJ (2003) A Flexible Sigmoid Function of Determinate Growth. Ann Bot 91: 361--371*\
*Onofri A (2020) The broken bridge between biologists and statisticians: a blog and R package.*
