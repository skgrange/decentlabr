#' Function to provide a look-up table of variable names for switching variable
#' names and potential variable filtering for Decentlab API data tables. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
decentlab_variables_look_up <- function() {
  
  tibble::tribble(
    ~sensor_type, ~variable,                      ~variable_clean, 
    "dl_lp8",     "sensirion_sht21_temperature",  "temp",          
    "dl_lp8",     "sensirion_sht21_humidity",     "rh",            
    "dl_lp8",     "senseair_lp8_vcap2",           "vcap2",         
    "dl_lp8",     "senseair_lp8_vcap1",           "vcap1",         
    "dl_lp8",     "senseair_lp8_co2",             "co2_rep",           
    "dl_lp8",     "senseair_lp8_ir",              "ir",            
    "dl_lp8",     "senseair_lp8_status",          "status",        
    "hpp",        "senseair_hpp_co2_filtered",    "co2_rep",           
    "hpp",        "senseair_hpp_ir_signal",       "ir",            
    "hpp",        "senseair_hpp_lpl_signal",      "lpl",           
    "hpp",        "senseair_hpp_status",          "status",        
    "hpp",        "sensirion_sht21_humidity",     "rh",            
    "hpp",        "sensirion_sht21_temperature",  "temp",          
    "hpp",        "calibration_a",                "calibration_a", 
    "hpp",        "calibration_b",                "calibration_b", 
    "picarro",    "co2",                          "co2_rep",           
    "picarro",    "co2_dry",                      "co2_rep_dry",       
    "picarro",    "h2o",                          "h2o",           
    "picarro",    "pressure",                     "pressure",      
    "picarro",    "rh",                           "rh",            
    "picarro",    "t",                            "temp",          
    "licor",      "bosch_bmp280_pressure",        "pressure",      
    "licor",      "licor_li850_cell_pressure",    "cell_pressure", 
    "licor",      "licor_li850_cell_temperature", "cell_temp",     
    "licor",      "licor_li850_co2",              "co2_rep",           
    "licor",      "licor_li850_co2_absorption",   "co2_absorption",
    "licor",      "licor_li850_flow_rate",        "flow_rate",     
    "licor",      "licor_li850_h2o",              "h2o",           
    "licor",      "licor_li850_h2o_absorption",   "h2o_absorption",
    "licor",      "sensirion_sht21_humidity",     "rh",            
    "licor",      "sensirion_sht21_temperature",  "temp",          
    "licor",      "calibration_a",                "calibration_a", 
    "licor",      "calibration_b",                "calibration_b", 
    "vaisala",    "vaisala_gmp343_co2",           "co2_rep",           
    "vaisala",    "vaisala_gmp343_temperature",   "cell_temp",     
    "vaisala",    "bosch_bmp280_pressure",        "pressure",      
    "vaisala",    "sensirion_sht21_humidity",     "rh",            
    "vaisala",    "sensirion_sht21_temperature",  "temp",          
    "vaisala",    "calibration_a",                "calibration_a", 
    "vaisala",    "calibration_b",                "calibration_b"
  )
  
}
