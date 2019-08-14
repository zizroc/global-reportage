## water_plotter.R
## Marcus J Thomson, IIASA
## AUG 2019
## Purpose: Make maps of global water usage for Systemiq.
## Peculiarities: It is not difficult to produce maps showing colour differences. It is difficult to produce a map with multiple overlying plots (bar graphs here); so this script first generates the map in order to make an image of the map, then overlays bar several charts using the viewport function. Since ggsave will not work, you must save the output manually as a PDF or image.
## Dependencies: Needs some data files with GLOBIOM outputs.

#libraries
libs <- c("tidyverse", "gdxrrw", "scales", "ggplot2", "ggmap", "maps", "mapdata", "RColorBrewer", "reshape2", "cowplot", "magick")
lapply(libs, library, character.only=TRUE)
rm(libs)

## Download on webpage: http://support.gams.com/doku.php?id=gdxrrw:interfacing_gams_and_r
## Install binaries by added the folder to the Program File folder R/R-x.xx/library/
## point to newest version of GAMS with igdx
igdx("C:/GAMS/win64/25.1")


#read in mapping data
path_base <- "C:/Users/thomson/Documents/IIASA/Global Report/"
path_data <- paste0(path_base, "data/")
path_outp <- paste0(path_base, "outputs/")

regions_globiom <- read.csv(paste0(path_base, "GLOBIOM_to_maptools_mapping.csv"), header=TRUE, stringsAsFactors = TRUE)
Regions_GLOBIOM <- regions_globiom %>% 
  rename("MAPTOOLS_REGION" = "maptools_COUNTRY")
rm(regions_globiom)

worldmap <- map_data("worldHires")
Worldmap <- worldmap %>% 
  rename("LON" = "long", "LAT" = "lat", "MAPTOOLS_REGION" = "region")
rm(worldmap)

mrgd_maps <- merge(Regions_GLOBIOM, Worldmap, by = "MAPTOOLS_REGION")

data_water <- read.csv(paste0(path_data, "water_data.csv"), header = TRUE, stringsAsFactors = TRUE)

wa_data <- data_water %>% 
  filter(Source == "Total_Withdr", Scenario == "Better Futures") %>% 
  select(Region, X2000, X2010, X2020, X2030, X2040, X2050) %>% 
  melt(., id.var = "Region") %>% 
  rename("GLOBIOM_REGION" = "Region", "km3" = "value") %>% 
  mutate(year = c(2000, 2010, 2020, 2030, 2040, 2050)[as.numeric(.$variable)]) %>% 
  select(GLOBIOM_REGION, year, km3)

mrgd_maps_tmp <- mrgd_maps %>% 
  # mutate(a = factor(GLOBIOM_REGION, levels = lvls_data)) %>% 
  select(-(subregion)) %>% 
  arrange(., order) %>% 
  distinct()



conv_reg_names <- data.frame(GLOBIOM_REGION = c("EasternAsia", 
                                                "Europe", 
                                                "FormerSovietUnion", 
                                                "LatinAmericaCarib", 
                                                "MidEastNorthAfr", 
                                                "NorthAmerica", 
                                                "Oceania", 
                                                "SouthAsia", 
                                                "SouthEastAsia", 
                                                "SubSaharanAfr", 
                                                "World"), 
                             Region_fullnames = c("Eastern Asia", 
                                                  "Europe", 
                                                  "Former Soviet Union", 
                                                  "Latin America & \n the Caribbean", 
                                                  "Middle East & \n North Africa", 
                                                  "North America", 
                                                  "Oceania", 
                                                  "South Asia", 
                                                  "Southeast Asia", 
                                                  "Sub-Saharan Africa", 
                                                  "WORLD"))

maps_df_v1 <- merge(mrgd_maps_tmp, conv_reg_names)

maps_df_v2 <- maps_df_v1 %>% 
  arrange(., order)

cols <- grey.colors(11) #use whatever distinguishes your map regions

wrld_map <- ggplot(data = maps_df_v2, mapping=aes(x=LON, y=LAT, group=group)) + 
  coord_fixed(1.3) + 
  geom_polygon(aes(fill = Region_fullnames), alpha = 0.8) + 
  scale_fill_manual(values = cols) + 
  # scale_fill_gradient2(low = muted("blue"), mid = "grey", high = muted("red"), midpoint = 0) + 
  guides(fill = guide_legend(ncol = 5)) +
  theme_minimal() + 
  theme(axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text = element_blank(), 
        axis.line = element_blank(), 
        plot.title = element_text(size = 11), 
        plot.subtitle = element_text(size = 10), 
        plot.caption = element_text(size = 8), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        legend.text.align = 0, 
        legend.text = element_text(vjust = +0.5, 
                                   hjust = +1, 
                                   size = 7), 
        strip.text.x = element_text(size = 10), 
        panel.grid = element_blank()) + 
  labs(fill = NULL, title = "TITLE", caption = "Data: Deppermann et al. (2019)")


wa_data_v1 <- merge(wa_data, conv_reg_names)

reg_name_loc <- data.frame(name = conv_reg_names$Region_fullnames, 
                           x = c(0.75, 
                                 0.35, 
                                 0.60, 
                                 0.25, 
                                 0.45, 
                                 0.16, 
                                 0.82, 
                                 0.60, 
                                 0.78, 
                                 0.45, 
                                 0.02), 
                           y = c(0.67, 
                                 0.77, 
                                 0.80, 
                                 0.40, 
                                 0.59, 
                                 0.70, 
                                 0.30, 
                                 0.50, 
                                 0.52, 
                                 0.35, 
                                 0.45))


p    <- list()
vprt <- list()
for(i in seq_along(reg_name_loc$name)){
  p[[i]] <- ggplot(data = wa_data_v1 %>% 
               filter(Region_fullnames == reg_name_loc$name[i]), aes(x = year, y = km3)) + 
    geom_col(fill = muted("blue")) + 
    # scale_fill_gradient2(low = muted("blue"), mid = "grey", high = muted("red"), aesthetics = "fill", midpoint = data %>% filter(year == 2010) %>% pull(km3)) + 
    theme_minimal() + 
    geom_hline(yintercept = 0) + 
    theme(legend.position = "none", 
          axis.title = element_text(size = 10, face = "bold"), 
          plot.title = element_blank(), 
          plot.caption = element_text(size = 9), 
          # axis.text.x = element_text(size = 7, angle = 90), 
          axis.text.x = element_blank(), 
          axis.text.y = element_text(size = 8, colour = "black"), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank()) + 
    labs(x = reg_name_loc$name[i], y = NULL)


  if(i <  11){
    vprt[[i]] <- viewport(x = reg_name_loc$x[i],
                          y = reg_name_loc$y[i],
                          height = unit(3, "cm"),
                          width = unit(4, "cm"),
                          just = 0)
  }
  if(i == 11){
    vprt[[i]] <- viewport(x = reg_name_loc$x[i],
                          y = reg_name_loc$y[i],
                          height = unit(4, "cm"),
                          width = unit(5, "cm"),
                          just = 0)
  }
  # ggsave(filename = paste(path_outp, i, "water_abs.svg", sep = "_"), plot = p1)
}

wrld_map
for(i in 1:10){
  print(p[[i]], vp = vprt[[i]])
  }

