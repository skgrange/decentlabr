#' Function to read a provided look-up table of variable names for the 
#' Decentlab API.
#' 
#' \code{read_decentlab_variables}'s return can be used for for switching 
#' variable names and variable filtering for Decentlab API data tables. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Tibble. 
#' 
#' @export
read_decentlab_variables <- function() {
  
  tibble::tribble(
    ~sensor_type,     ~variable,                                 ~variable_clean,     
    "dl_lp8",         "sensirion_sht21_temperature",             "temp",              
    "dl_lp8",         "sensirion_sht21_humidity",                "rh",                
    "dl_lp8",         "senseair_lp8_vcap2",                      "vcap2",             
    "dl_lp8",         "senseair_lp8_vcap1",                      "vcap1",             
    "dl_lp8",         "senseair_lp8_co2",                        "co2_rep",           
    "dl_lp8",         "senseair_lp8_ir",                         "ir",                
    "dl_lp8",         "senseair_lp8_status",                     "status",            
    "senseair_hpp",   "senseair_hpp_co2_filtered",               "co2_rep",           
    "senseair_hpp",   "senseair_hpp_ir_signal",                  "ir",                
    "senseair_hpp",   "senseair_hpp_lpl_signal",                 "lpl",               
    "senseair_hpp",   "senseair_hpp_status",                     "status",            
    "senseair_hpp",   "sensirion_sht21_humidity",                "rh",                
    "senseair_hpp",   "sensirion_sht21_temperature",             "temp",              
    "senseair_hpp",   "senseair_hpp_pressure_filtered",          "pressure",          
    "senseair_hpp",   "calibration_a",                           "calibration_a",     
    "senseair_hpp",   "calibration_b",                           "calibration_b",     
    "licor_li850",    "bosch_bmp280_pressure",                   "pressure",          
    "licor_li850",    "licor_li850_cell_pressure",               "cell_pressure",     
    "licor_li850",    "licor_li850_cell_temperature",            "cell_temp",         
    "licor_li850",    "licor_li850_co2",                         "co2_rep",           
    "licor_li850",    "licor_li850_co2_absorption",              "co2_absorption",    
    "licor_li850",    "licor_li850_flow_rate",                   "flow_rate",         
    "licor_li850",    "licor_li850_h2o",                         "h2o",               
    "licor_li850",    "licor_li850_h2o_absorption",              "h2o_absorption",    
    "licor_li850",    "sensirion_sht21_humidity",                "rh",                
    "licor_li850",    "sensirion_sht21_temperature",             "temp",              
    "licor_li850",    "calibration_a",                           "calibration_a",     
    "licor_li850",    "calibration_b",                           "calibration_b",     
    "vaisala_gmp343", "vaisala_gmp343_co2",                      "co2_rep",           
    "vaisala_gmp343", "vaisala_gmp343_temperature",              "cell_temp",         
    "vaisala_gmp343", "bosch_bmp280_pressure",                   "pressure",          
    "vaisala_gmp343", "sensirion_sht21_humidity",                "rh",                
    "vaisala_gmp343", "sensirion_sht21_temperature",             "temp",              
    "vaisala_gmp343", "calibration_a",                           "calibration_a",     
    "vaisala_gmp343", "calibration_b",                           "calibration_b",     
    "dl_atm22",       "metergroup_atmos22_air_temperature",      "air_temp",          
    "dl_atm22",       "metergroup_atmos22_wind_direction",       "wd",                
    "dl_atm22",       "metergroup_atmos22_wind_speed",           "ws",                
    "senseair_k96",   "calibration_a",                           "calibration_a",     
    "senseair_k96",   "calibration_b",                           "calibration_b",     
    "senseair_k96",   "senseair_k96_humidity_bme280",            "rh",                
    "senseair_k96",   "senseair_k96_lpl_concentration_filtered", "co2_rep",           
    "senseair_k96",   "senseair_k96_lpl_ir_signal",              "ir_lpl",            
    "senseair_k96",   "senseair_k96_lpl_signal",                 "lpl",               
    "senseair_k96",   "senseair_k96_mpl_ir_signal",              "ir_mpl",            
    "senseair_k96",   "senseair_k96_mpl_signal",                 "mpl",               
    "senseair_k96",   "senseair_k96_pressure_filtered",          "pressure",          
    "senseair_k96",   "senseair_k96_sensor_id",                  "sensing_element_id",
    "senseair_k96",   "senseair_k96_status",                     "status",            
    "senseair_k96",   "senseair_k96_temperature_ntc0",           "temp"
  ) %>% 
    filter(!is.na(variable_clean), 
           variable_clean != "")
  
}
