read_ext_tracks<-function(file="ebtrk_atlc_1988_2015.txt",...,ext_track_widths=NULL,
                          ext_track_colnames=NULL,degW=TRUE){
    if (is.null(ext_track_widths)){
                ext_track_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                           4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
               
    }
  
    if (is.null(ext_track_colnames)){
                ext_track_colnames <- c("storm_id", "storm_name", "month", "day",
                              "hour", "year", "latitude", "longitude",
                           "max_wind", "min_pressure", "rad_max_wind",
                           "eye_diameter", "pressure_1", "rad_1",
                paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                             "storm_type", "distance_to_land", "final"
                            )
    }
                          


ext_tracks <- suppressMessages(read_fwf(file,readr::fwf_widths(ext_track_widths, ext_track_colnames),
na = "-99"))
    
#format the longitude to ensure it has negative values in the Western hemisphere
    if (degW) {ext_tracks$longitude<- -ext_tracks$longitude }
    
    
keepcols=c("storm_name","date","longitude","latitude")
 keepcols<-c(keepcols,c("quadrant","wind_speed","wind_radius","storm_id_unique"))

ext_tracks =mutate(ext_tracks,date = lubridate::ymd_h(paste(year,month,day,hour,sep="")),storm_id_unique = paste0(storm_name,"-",year))
gather_cols<-names(ext_tracks)[dplyr::contains(match="radius_",vars=names(ext_tracks))]

tidy_tracks<-tidyr::gather_(ext_tracks,key_col="speed_quadrant",
                              value_col="wind_radius",gather_cols=gather_cols,
                              na.rm=FALSE,factor_key=FALSE)

tidy_tracks<-tidyr::separate_(tidy_tracks,col="speed_quadrant",
                          into=c("rad_1","wind_speed","quadrant"),
                              sep="_",remove=TRUE,convert=TRUE)

 tidy_tracks$rad_1<-NULL

tidy_tracks<-dplyr::select_(tidy_tracks,.dots=stats::setNames(keepcols,keepcols))
  tidy_tracks = spread(tidy_tracks,quadrant,wind_radius)



return(tidy_tracks)



}