#include <Rcpp.h>
using namespace Rcpp;
// [[Rcpp::export]]  
DataFrame calculate_year_energy(DataFrame year_data, List params, double initial_soc) {
  int n = year_data.nrows();
  // use pre-calculated columns as vectors:
  NumericVector excess_solar = year_data["excess_solar"];
  NumericVector deficit = year_data["deficit"];
  NumericVector battery_max_capacity = year_data["battery_max_capacity"]; 
  NumericVector battery_min_capacity = year_data["battery_min_capacity"];
  
  // initialize columns that are filled with loop
  NumericVector battery_available_charge_capacity(n);
  NumericVector battery_charge_raw(n);
  NumericVector battery_charge_loss(n);
  NumericVector battery_charge(n);
  NumericVector battery_discharge_raw(n);
  NumericVector battery_discharge_loss(n);
  NumericVector battery_discharge(n);
  NumericVector grid_import(n);
  NumericVector grid_export(n);
  NumericVector battery_soc(n);
  NumericVector battery_soc_change(n);
  
  battery_available_charge_capacity.fill(0.0);
  battery_charge_raw.fill(0.0);
  battery_charge_loss.fill(0.0);
  battery_charge.fill(0.0);
  battery_discharge_raw.fill(0.0);
  battery_discharge_loss.fill(0.0);
  battery_discharge.fill(0.0);
  grid_import.fill(0.0);
  grid_export.fill(0.0);
  battery_soc.fill(0.0);
  battery_soc_change.fill(0.0);
  
  double battery_current_soc = initial_soc;
  double battery_charge_efficiency = as<double>(params["battery_charge_efficiency"]);
  double battery_discharge_efficiency = as<double>(params["battery_discharge_efficiency"]);
  
  for (int i = 0; i < n; ++i) {
    battery_available_charge_capacity[i] = battery_max_capacity[i] - battery_current_soc; 
    double max_discharge_possible = battery_current_soc - battery_min_capacity[i];
    
    battery_discharge_raw[i] = std::min(deficit[i] / battery_discharge_efficiency, max_discharge_possible);
    battery_discharge_loss[i] = battery_discharge_raw[i] * (1 - battery_discharge_efficiency);
    battery_discharge[i] = battery_discharge_raw[i] * battery_discharge_efficiency;
    grid_import[i] = std::max(0.0, deficit[i] - battery_discharge[i]);
    
    battery_charge_raw[i] = std::min(excess_solar[i] / battery_charge_efficiency, battery_available_charge_capacity[i]); 
    battery_charge_loss[i] = battery_charge_raw[i] * (1 - battery_charge_efficiency);
    battery_charge[i] = battery_charge_raw[i] * battery_charge_efficiency;
    grid_export[i] = std::max(0.0, excess_solar[i] - battery_charge[i]);
    
    battery_current_soc = std::max(battery_min_capacity[i], std::min(battery_max_capacity[i], battery_current_soc + battery_charge[i] - battery_discharge[i]));
    battery_soc[i] = battery_current_soc;
    battery_soc_change[i] = (i > 0) ? battery_soc[i] - battery_soc[i - 1] : 0.0;
  }
  
  // Add ALL new columns to year_data
  year_data["battery_available_charge_capacity"] = battery_available_charge_capacity;
  year_data["battery_charge_raw"] = battery_charge_raw;
  year_data["battery_charge"] = battery_charge;
  year_data["battery_charge_loss"] = battery_charge_loss;
  year_data["battery_discharge_raw"] = battery_discharge_raw;
  year_data["battery_discharge"] = battery_discharge;
  year_data["battery_discharge_loss"] = battery_discharge_loss;
  year_data["grid_import"] = grid_import;
  year_data["grid_export"] = grid_export;
  year_data["battery_soc"] = battery_soc;
  year_data["battery_soc_change"] = battery_soc_change;
  
  return year_data;
}