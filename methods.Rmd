---
title: "Methods"
author: "Nick Michalak"
date: "10/28/2019"
output: 
  html_document: 
    keep_md: yes
    theme: flatly
    toc: yes
    toc_float: yes
---

<style type="text/css">

body{ /* Normal  */
      font-size: 18px;
  }
</style>

# Methods

## Plain Language Summary
I modeled a separate polling trend for each candidate that accounts for variability in pollster and correlations between polling dates. 

## Data
* I used polling data collected by FiveThirtyEight ([github.com/fivethirtyeight/data](https://github.com/fivethirtyeight/data/tree/master/polls)).

## Wrangling and Selecting Data
* I computed date as the median of the start and the end dates for each poll (e.g., median(2019-10-01, 2019-10-03) = 2019-10-02).
* I excluded head-to-head polls.
* For a given model, I use the most inclusive population of voters from a survey: "Adults" and "Voters" populations are equally inclusive, and they're more inclusive than "Registered Voters," which is more inclusive than "Likely Voters."

## Model
* I estimated polling prediction trends for each candidate via Generalized Additive Mixed Model.  

> $\ln(\frac{p}{1 - p}) = b_0 + f_1(\text{Median Date}) + \epsilon$  

* I modeled proportions as an outcome with a binomial function with a logit link. I weighted by sample size, which is equivalent to modeling binary data (i.e., 1 = success, 0 = failure).
* For each model, I specified random intercepts for pollster (polls nested within pollster) as well as an auto-correlation structure of order 1 with a continuous time covariate (i.e., Continuous AR(1) Correlation).
* I have not come up with a way to handle tracker polls (i.e., same respondents surveyed over time). So, models treat tracker polls the same as other polls.
* When states have fewer than 10 polls, I simply fit a logistic regression line per candidate instead of the more complex GAMM fit (averages reflect the estimates from the logistic regression).

## Estimates ("Averages") and Confidence Intervals (Uncertainty)
* I table the most recent fit estimates as "averages." Note that these values are sensitive to the trend and different from simply averaging the most recent polls.
* For averages based on GAMM fits, the confidence intervals are simultaneous (see the [gratia R package documentation](https://cran.r-project.org/web/packages/gratia/gratia.pdf)).
* For averages based on fewer than 10 observations, confidence intervals are based on the profile method (see the [MASS R package documentation](https://cran.r-project.org/web/packages/MASS/MASS.pdf)).
* Period Polling Averages reflect simple averages per candidate since a given polling start date (e.g., average since October 1st, 2019)  

## Historical Probability of Winning the Nomination
* I input the fitted polling averages for each candidate from the National Polling Average model (described above) into a logistic regression equation built from regressing nomination success (1 or 0) onto Historical Democratic Primary candidates' polling averages during either the first or second part of the year (January to June or June to December) (I pulled data from this [FiveThirtyEight series by Geoffrey Skelley](https://fivethirtyeight.com/features/we-analyzed-40-years-of-primary-polls-even-early-on-theyre-fairly-predictive/)).  

> $\ln(\frac{p(nominated)}{1 - p(nominated)}) = b_0 + b_1(\text{Polling Average}) + \epsilon$  

For example, if a candidate is pulling at 5% in March 2019, then the log odds of their winning the nomination is -3.46 + 8.65(0.05) = -3.02 (i.e., 4.6% = exp(-3.02) / (1 + exp(-3.02))). Their log odds don't improve much if they are polling at 5% in October 2019: -3.36 + 8.28(0.05) = -2.95 (i.e., 5.0% = exp(-2.95) / (1 + exp(-2.95))).

## **Cautionary Words**
This is a work in progress. I made a handful of decisions based on convenience. Here's a list of those:
* I used k = 10 as a default for the basis for the smooth term for each candidate's trend (see `choose.k` in the [mgcv R package documentation](https://cran.r-project.org/web/packages/mgcv/mgcv.pdf)).
* I used defaults for the autoregressive correlation structure.
* I did not apply weights based on pollsters' historical accuracy or bias.

## Resources
* Wood, S.N. (2011) Fast stable restricted maximum likelihood and marginal likelihood estimation of semi parametric generalized linear models. *Journal of the Royal Statistical Society (B) 3(1)*:3-36
* Wood S.N., N. Pya and B. Saefken (2016) Smoothing parameter and model selection for general smooth models (with discussion). *Journal of the American Statistical Association 111*:1548-1575.
* Wood, S.N. (2004) Stable and efficient multiple smoothing parameter estimation for generalized additive models. *Journal of the American Statistical Association. 99*:673-686.
* Wood, S.N. (2017) *Generalized Additive Models: An Introduction with R (2nd edition).* Chapman and Hall/CRC.
* Wood, S.N. (2003) Thin-plate regression splines. *Journal of the Royal Statistical Society (B) 65(1)*:95-114.

## Other Poll Aggregator 🐊 Sites
* 270ToWin: [2020 Democratic Presidential Nomination](https://www.270towin.com/2020-democratic-nomination/)
* The Economist: [Who is ahead in the Democratic primary race?](https://projects.economist.com/democratic-primaries-2020/)
* FiveThirtyEight: [The 2020 Democratic Presidential Primary](https://projects.fivethirtyeight.com/2020-primaries/democratic/)
* New York Times: [Which Democrats Are Leading
the 2020 Presidential Race?](https://www.nytimes.com/interactive/2020/us/elections/democratic-polls.html)
* Real Clear Politics: [2020 Democratic Presidential Nomination](https://www.realclearpolitics.com/epolls/2020/president/us/2020_democratic_presidential_nomination-6730.html)
