#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector simulate_rw_prices(double lastval, NumericVector expected, 
                              double theta, double hist_vol, int window) {
  int n = expected.size();
  NumericVector prices(n);
  prices[0] = lastval;
  double dt = 1.0/(365*24);
  double vol_scale = hist_vol * sqrt(dt);
  double weekly_theta = theta/10; 
  
  for(int i = 1; i < n; ++i) {
    // mean reversion 
    prices[i] = prices[i-1] + theta * (expected[i] - prices[i-1]) * dt;
    
    // add volatility randomness
    prices[i] += vol_scale * R::rnorm(0, 1);
    
    // weekly smoothing
    if((i+1) % window == 0 && i >= window) {
      double avg = mean(prices[Range(i - window + 1, i)]);
      prices[i] = (1 - weekly_theta) * prices[i] + weekly_theta * avg;
    }
  }
  return prices;
}


// This file is part of the PV Analyzer project
// Copyright (c) 2025 Pavel HAJKO
// See the license.txt file in the project root