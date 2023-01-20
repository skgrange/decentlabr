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
    ~sensor_type,   ~variable,                                 ~variable_clean, 
    "dl_lp8",       "sensirion_sht21_temperature",             "temp",          
    "dl_lp8",       "sensirion_sht21_humidity",                "rh",            
    "dl_lp8",       "senseair_lp8_vcap2",                      "vcap2",         
    "dl_lp8",       "senseair_lp8_vcap1",                      "vcap1",         
    "dl_lp8",       "senseair_lp8_co2",                        "co2_rep",       
    "dl_lp8",       "senseair_lp8_ir",                         "ir",            
    "dl_lp8",       "senseair_lp8_status",                     "status",        
    "hpp",          "senseair_hpp_co2_filtered",               "co2_rep",       
    "hpp",          "senseair_hpp_ir_signal",                  "ir",            
    "hpp",          "senseair_hpp_lpl_signal",                 "lpl",           
    "hpp",          "senseair_hpp_status",                     "status",        
    "hpp",          "sensirion_sht21_humidity",                "rh",            
    "hpp",          "sensirion_sht21_temperature",             "temp",          
    "hpp",          "senseair_hpp_pressure_filtered",          "pressure",      
    "hpp",          "calibration_a",                           "calibration_a", 
    "hpp",          "calibration_b",                           "calibration_b", 
    "picarro",      "co2",                                     "co2_rep",       
    "picarro",      "co2_dry",                                 "co2_dry_rep",   
    "picarro",      "h2o",                                     "h2o",           
    "picarro",      "pressure",                                "pressure",      
    "picarro",      "rh",                                      "rh",            
    "picarro",      "t",                                       "temp",          
    "licor",        "bosch_bmp280_pressure",                   "pressure",      
    "licor",        "licor_li850_cell_pressure",               "cell_pressure", 
    "licor",        "licor_li850_cell_temperature",            "cell_temp",     
    "licor",        "licor_li850_co2",                         "co2_rep",       
    "licor",        "licor_li850_co2_absorption",              "co2_absorption",
    "licor",        "licor_li850_flow_rate",                   "flow_rate",     
    "licor",        "licor_li850_h2o",                         "h2o",           
    "licor",        "licor_li850_h2o_absorption",              "h2o_absorption",
    "licor",        "sensirion_sht21_humidity",                "rh",            
    "licor",        "sensirion_sht21_temperature",             "temp",          
    "licor",        "calibration_a",                           "calibration_a", 
    "licor",        "calibration_b",                           "calibration_b", 
    "vaisala",      "vaisala_gmp343_co2",                      "co2_rep",       
    "vaisala",      "vaisala_gmp343_temperature",              "cell_temp",     
    "vaisala",      "bosch_bmp280_pressure",                   "pressure",      
    "vaisala",      "sensirion_sht21_humidity",                "rh",            
    "vaisala",      "sensirion_sht21_temperature",             "temp",          
    "vaisala",      "calibration_a",                           "calibration_a", 
    "vaisala",      "calibration_b",                           "calibration_b", 
    "dl_atm22",     "metergroup_atmos22_air_temperature",      "air_temp",      
    "dl_atm22",     "metergroup_atmos22_wind_direction",       "wd",            
    "dl_atm22",     "metergroup_atmos22_wind_speed",           "ws",            
    "senseair_k96", "battery",                                 "",              
    "senseair_k96", "calibration_a",                           "calibration_a", 
    "senseair_k96", "calibration_b",                           "calibration_b", 
    "senseair_k96", "senseair_k96_humidity_bme280",            "rh",            
    "senseair_k96", "senseair_k96_lpl_concentration_filtered", "co2_rep",       
    "senseair_k96", "senseair_k96_lpl_ir_signal",              "ir_lpl",        
    "senseair_k96", "senseair_k96_lpl_signal",                 "lpl",           
    "senseair_k96", "senseair_k96_mpl_concentration_filtered", "",              
    "senseair_k96", "senseair_k96_mpl_ir_signal",              "ir_mpl",        
    "senseair_k96", "senseair_k96_mpl_signal",                 "mpl",           
    "senseair_k96", "senseair_k96_pressure_filtered",          "pressure",      
    "senseair_k96", "senseair_k96_sensor_id",                  "",              
    "senseair_k96", "senseair_k96_status",                     "status",        
    "senseair_k96", "senseair_k96_temperature_ntc0",           "temp",          
    "senseair_k96", "senseair_k96_temperature_ntc1",           ""
  ) %>% 
    filter(!is.na(variable_clean), 
           variable_clean != "")
  
}
