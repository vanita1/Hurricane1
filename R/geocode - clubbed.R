Geocode_Combined<-function(tidy_tracks,stormvalue,cname){


tryCatch ( {
		CountryMap = ggmap(get_googlemap(center=as.numeric(geocode(cname)), scale=2, zoom=4), extent="normal") 
	    }, 
		warning = function(w) {
				message("Failure to fetch geo-location. Try again ")
				message(w)
				return(NULL)
		},
		error = function(e) {
				message("error Failure to fetch geo-location. Try again ")
				message(e)
				return(NULL)
		} 
	)

finalgeom = CountryMap

tidy_tracks1 <- subset(tidy_tracks,storm_id_unique == stormvalue & ne > 0 & nw > 0 & se > 0 & sw > 0)

KATRINA2005f = tidy_tracks1
filter1 = unique(KATRINA2005f$latitude)
track_list1<- vector("list", length(filter1))
code1<- vector("list", nrow(KATRINA2005f))

filter1 = unique(KATRINA2005f$latitude)

for (k in 1: length(filter1)) {

tidy_tracks2 <-  subset(KATRINA2005f, latitude == filter1[k])
tidy_tracks2<- tidy_tracks2 %>% mutate(ne = ne*1852*1,se = se*1852*1,
                                sw = sw*1852*1,
                                   nw = nw*1852*1 )
l_points_1 = setNames(data.frame(matrix(ncol = 9, nrow = 0)), c("longitude", "latitude","stormid","wind_speed","ne","se","sw","nw"))

x_tracks = tidy_tracks2

for (i in 1:nrow(x_tracks)) {

ne_tracks <- base::data.frame(geosphere::destPoint(p = c(x_tracks[i,]$longitude,x_tracks[i,]$latitude),b = 1:90,d = x_tracks[i,]$ne))
ne_tracks$latitude <-  x_tracks[i,]$latitude
ne_tracks$longitude <-  x_tracks[i,]$longitude
ne_tracks$stormid <- x_tracks[i,]$storm_id_unique
ne_tracks$wind_speed<- x_tracks[i,]$wind_speed
ne_tracks$ne = x_tracks[i,]$ne
ne_tracks$se = x_tracks[i,]$se
ne_tracks$sw = x_tracks[i,]$sw
ne_tracks$nw = x_tracks[i,]$nw
nw_tracks<- base::data.frame(geosphere::destPoint(p = c(x_tracks[i,]$longitude,x_tracks[i,]$latitude),b = 271:360,d = x_tracks[i,]$nw)) 
nw_tracks$latitude <-  x_tracks[i,]$latitude
nw_tracks$longitude <-  x_tracks[i,]$longitude
nw_tracks$stormid <- x_tracks[i,]$storm_id_unique
nw_tracks$wind_speed<-x_tracks[i,]$wind_speed
nw_tracks$ne = x_tracks[i,]$ne
nw_tracks$se = x_tracks[i,]$se
nw_tracks$sw = x_tracks[i,]$sw
nw_tracks$nw = x_tracks[i,]$nw                
se_tracks <- base::data.frame(geosphere::destPoint(p = c(x_tracks[i,]$longitude,x_tracks[i,]$latitude),b = 91:180,d = x_tracks[i,]$se))
se_tracks$latitude <-  x_tracks[i,]$latitude
se_tracks$longitude <-  x_tracks[i,]$longitude
se_tracks$stormid <- x_tracks[i,]$storm_id_unique
se_tracks$wind_speed<-x_tracks[i,]$wind_speed
se_tracks$ne = x_tracks[i,]$ne
se_tracks$se = x_tracks[i,]$se
se_tracks$sw = x_tracks[i,]$sw
se_tracks$nw = x_tracks[i,]$nw
sw_tracks <- base::data.frame(geosphere::destPoint(p = c(x_tracks[i,]$longitude,x_tracks[i,]$latitude),b = 181:270,d = x_tracks[i,]$sw))
sw_tracks$latitude <-  x_tracks[i,]$latitude
sw_tracks$longitude <-  x_tracks[i,]$longitude
sw_tracks$stormid <- x_tracks[i,]$storm_id_unique
sw_tracks$wind_speed<-x_tracks[i,]$wind_speed
sw_tracks$ne = x_tracks[i,]$ne
sw_tracks$se = x_tracks[i,]$se
sw_tracks$sw = x_tracks[i,]$sw
sw_tracks$nw = x_tracks[i,]$nw

l_points <- dplyr::bind_rows(list(ne_tracks,se_tracks,sw_tracks,nw_tracks)) 

l_points_1 = dplyr::bind_rows(list(l_points,l_points_1))

 
}

 

track_list1[[k]] = l_points_1


}


for (k in 1:length(track_list1)){
    
    track_list1[[k]]$wind_speed = as.factor(track_list1[[k]]$wind_speed)
    
}

for (k in 1:length(track_list1)){

finalgeom= finalgeom +

geom_hurricane(data = track_list1[[k]],
               aes(x = lon, y = lat, 
                   ne = ne, se = se, nw = nw,sw = sw,
                   fill = wind_speed, color = wind_speed)) +
scale_color_manual(name = "Wind speed (kts)", 
                   values = c("red", "orange", "yellow")) + 
    scale_fill_manual(name = "Wind speed (kts)", 
                      values = c("red", "orange", "yellow"))} 
return(finalgeom)
} 





