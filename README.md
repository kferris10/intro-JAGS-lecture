## Introduction to JAGS

This repository contains the materials for a lecture that I gave in STAT 506 at Montana State.  I covered how to write models for the JAGS software program and how to call JAGS from R.

The materials in this repository are:

- `jags-models-by-hand.pdf`: The notes I used to practice writing out JAGS models by hand.
- `clean-baseball.R`: The R code used to obtain and clean the baseball data from the `Lahman` package for this example.
- `clean-baseball.csv`: A .csv file of the cleaned baseball data.
- `baseball-age-polynomials`: A saved .RData object containing the model for the orthogonal polynomials.  Used to plot the aging curve.
- `baseball-jags-model.R`: The code used to analyze the data using JAGS.
- `jags-baseball-model.txt`: A text file containing the JAGS model.
- `baseball-stan-model.R`: The code used to analyze the data using STAN (optional).
