### ED Estimation Methods

#### - Type of ED~50~
In biological assays, two types of ED~50~s are commonly used: absolute and relative. These are illustrated in the following figure.  
•	**Relative ED~50~**: This is the dose that achieves 50% of the maximum response relative to the minimum. For instance, in the figure, if the maximum response is 99.7% and the minimum is 14.8%, the relative maximum response is 84.9% (99.7% - 14.8%). Therefore, the relative 50% response is 57.25% (84.9% / 2 + 14.7%).  
•	**Absolute ED~50~**: This is the dose that produces exactly a 50% response. It's meaningful only when the data is normalized against a control treatment.  
<p align="center">

<img src="absolutevsrelative.png" alt="absolute vs relative" width="70%"/>

</p>


#### - ED~50~ Estimation Methods
You can estimate both the relative and absolute ED~50~s by using the build-in functions in the `drc` package, referred to here as the **Ritz-Gerhard Method**. However, due to the diversity of curve patterns, these functions may not always yield valid ED~50~ estimates.  
To address more complex scenarios, we introduced another two methods to estimate ED~50~s. One method is referred as **Serra-Greco Method** developed by Serra et al., while the other one was first introduced by Reed et al. in 1938 and further discussed by Ramakrishnan in 2016, which is known as the **Reed-and-Muench Method**. The differences between these three methods are summarized in the following table. When a curve does not fit any of the models, the Reed-and-Muench Method offers an absolute ED~50~ estimation by assuming linear regression between the two points that bracket the ED~50~ (see the accompanying figure). The primary advantage of the Serra-Greco Method is its effectiveness in estimating ED~50~s from biphasic curves. In such cases, doses that provide 50% of the maximum response on both sides of the curve are equally important for researchers. While the built-in function (Ritz-Gerhard Method) only provides the higher dose, the Serra-Greco Method yields ED~50~ values for both sides, which are more reasonable, as shown in the figure below.  

<p align="center">

<img src="EDestimation_methods.png" alt="ED estimation methods" width="100%"/>

</p>

#### References

*Sebaugh JL (2011) Guidelines for accurate EC50/IC50 estimation. Pharm Stat 10: 128–134*\
*Serra A. Et al. (2020) BMDx: a graphical Shiny application to perform Benchmark Dose analysis for transcriptomics data. Bioinformatics 36: 2932–2933*\
*Reed LJ, Muench H (1938) A simple method of estimating fifty percent endpoints. Am J Epidemiol 27: 493–497*\
*Ramakrishnan MA (2016) Determination of 50% endpoint titer using a simple formula. World J Virol 5: 85–86*\
