source("api.R")

client <- "GoSungrow"
ps_id <- 1212467

inverter_device_type <- "1"
inverter_key <- device_type_to_key(inverter_device_type, ps = ps_id)
meter_device_type <- "7"
meter_key <- device_type_to_key(meter_device_type, ps = ps_id)
