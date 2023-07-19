#include <Rcpp.h>
#include <iostream>
#include <cmath>
using namespace Rcpp;
using namespace std;

double edge_length(double x1, double y1, double x2, double y2) {
  double len = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2));
  return len; 
}

double sample_angle() {
  double u = R::runif(-3.1415, 3.1415);
  return u;
}

double sample_radius(double size) {
  double u = R::runif(0, size);
  return u;
}

int sample_edge(NumericVector seg_len) {
  double tot_len = 0;
  for(int i = 0; i < seg_len.length(); i++) {
    tot_len += seg_len(i);
  }
  double u = R::runif(0, tot_len);
  int ind = 0;
  while(ind < seg_len.length() & u > seg_len(ind)) {
    u = u - seg_len(ind);
    ind++;
  }
  return ind;
}

// [[Rcpp::export]]
DataFrame grow_polygon(DataFrame polygon, int iterations, double noise) {
  
  NumericVector x = polygon["x"];
  NumericVector y = polygon["y"];
  NumericVector seg_len = polygon["seg_len"];
  IntegerVector lookup;
  
  int final_length = x.length() + iterations;  
  IntegerVector position (final_length);
  NumericVector x_out (final_length);
  NumericVector y_out (final_length); 
  NumericVector seg_len_out (final_length);
  
  for(int i = 0; i < x.length() - 1; i++) {
    lookup.push_back(i + 1);
  }
  lookup.push_back(0);
  
  int ind, next_ind;
  double last_x, last_y, next_x, next_y, new_x, new_y;
  double len, last_len, next_len;
  double origin_x, origin_y, radius, angle;

  for(int i = 0; i < iterations; i++) {
    
    ind = sample_edge(seg_len);
    len = seg_len(ind); 
    
    last_x = x(ind);
    last_y = y(ind);
    
    next_ind = lookup(ind);
    next_x = x(next_ind);
    next_y = y(next_ind);
    
    origin_x = (last_x + next_x) / 2;
    origin_y = (last_y + next_y) / 2;    
    
    angle = sample_angle();
    radius = sample_radius(len * noise);
    
    new_x = origin_x + radius * cos(angle);
    new_y = origin_y + radius * sin(angle);

    last_len = edge_length(last_x, last_y, new_x, new_y);
    next_len = edge_length(new_x, new_y, next_x, next_y);
    
    x.push_back(new_x);
    y.push_back(new_y);
    seg_len.push_back(next_len);
    lookup.push_back(next_ind);
    
    seg_len(ind) = last_len;
    lookup(ind) = lookup.length() - 1;
  }
  
  ind = 0;
  for(int i = 0; i < x.length(); i++) {
    position(ind) = i;  
    ind = lookup(ind);
  }

  DataFrame df = DataFrame::create(
    Named("x") = x, 
    Named("y") = y,
    Named("seg_len") = seg_len,
    Named("position") = position
  );
  return df;
}
