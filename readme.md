# 2020 Election Model

An implementation of [Drew Linzer's dynamic Bayesian election forecasting model](https://votamatic.org/wp-content/uploads/2013/07/Linzer-JASA13.pdf).

This is an attempt at a strict implementation of Linzer's paper, done by adapting the code from Pierre Kremp's [version](http://www.slate.com/features/pkremp_forecast/report.html) of the model.

### Files

* /ElectionModelBasic
  * Extremely basic election model built using bsts R package. Part of a blog post I wrote [here](https://tomjs.org/post/introtomodelling/).
* /PollData
  * Poll Data taken from the Economist [here](https://github.com/TheEconomist/us-potus-model/tree/master/data).
* LoadModel.R
  * Wrangle Data and send to Stan
* RevisedPolls.Stan
  * Actual model
