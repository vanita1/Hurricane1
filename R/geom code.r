GeomHurricane<-ggplot2::ggproto("GeomHurricane",Geom,
                                
                                required_aes = c("x","y","ne","se","nw","sw"
                                ),
                                
                                default_aes = aes(fill="red",
                                                  colour="red",
                                                  alpha=1,
                                                  scale_radii=1
                                ),
                                
                                draw_key = draw_key_polygon,
                                draw_group = function(data,panel_scales,coord){
                                    coords <- coord$transform(data, panel_scales)
                                    
                                     alpha = coords$wind_speed
                                    
                                    grid::polygonGrob(
                                        x=coords$x, y=coords$y,
                                        gp = grid::gpar(
                                            fill = coords$fill,colour = coords$colour,alpha = coords$alpha
                                        )
                                    )
                                }
)


geom_hurricane<-function (mapping = NULL, 
                          data = NULL, 
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,...){
    
    
    list(
        layer(geom = GeomHurricane,mapping = mapping,data = data,stat = stat, 
              position = position, show.legend = show.legend, inherit.aes = inherit.aes,
              params = list(na.rm = na.rm,
                            ...)),
        layer(geom = GeomHurricane,mapping = mapping,data = data,stat = stat, 
              position = position, show.legend = show.legend, inherit.aes = inherit.aes,
              params = list(na.rm = na.rm,
                            ...))
    )
}