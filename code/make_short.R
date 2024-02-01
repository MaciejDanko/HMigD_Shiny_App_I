z <- 3
raw_int_flows_a <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_34e.enc'))[seq(1, 1000, z),,,]; gc()#no mov no lfs
raw_int_flows_c <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_34f.enc'))[seq(1, 1000, z),,,]; gc() #simple mov no lfs
raw_int_flows_e <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_34d.enc'))[seq(1, 1000, z),,,]; gc() #a8 mov no lfs
raw_int_flows_b <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_35e.enc'))[seq(1, 1000, z),,,]; gc() #no mov  lfs
raw_int_flows_d <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_35f.enc'))[seq(1, 1000, z),,,]; gc() #simple mov lfs
raw_int_flows_f <<- fill_missing_years_a(get_data_raw('./data/raw_int_flows_35d.enc'))[seq(1, 1000, z),,,]; gc() #a8 mov lfs
dim(raw_int_flows_f )
save(file='./data/raw_int_abcdef_short.RDA', list=c('raw_int_flows_a','raw_int_flows_b','raw_int_flows_c','raw_int_flows_d','raw_int_flows_e','raw_int_flows_f'))
