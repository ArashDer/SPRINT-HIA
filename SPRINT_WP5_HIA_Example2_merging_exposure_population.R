#The R script below shows the steps of merging the exposure data with the NL population data per grid
#to get the grids with newborns of rhe example year of 2020 : 

library(readr) 

library(data.table) 

a<-fread(file=" aantal_mensen_2020.csv")  #downloaded from CBS website 

 https://www.cbs.nl/nl-nl/visualisaties/dashboard-bevolking/bevolkingsteller  

#Statistics for 100x100 meter grids in the Netherlands: 

https://www.cbs.nl/nl-nl/dossier/nederland-regionaal/geografische-data/kaart-van-100-meter-bij-100-meter-met-statistieken  

colnames(a) 

dput(names(a)) 

library(dplyr) 

#select columns by name 

a2<-a %>% 

  select("crs28992res100m", "aantal_inwoners", "aantal_mannen", "aantal_vrouwen",  

           "aantal_inwoners_0_tot_15_jaar", "aantal_inwoners_15_tot_25_jaar",  

           "aantal_inwoners_25_tot_45_jaar", "aantal_inwoners_45_tot_65_jaar",  

           "aantal_inwoners_65_jaar_en_ouder", "aantal_geboorten") 

 

save(a2, file=" aantal_mensen_2020.Rdata" )  

 

crs_code <- 28992 

# Convert the data frame to an sf object 

points_sf <- st_as_sf(homes, coords = c("X", "Y"), crs = crs_code) 

#### 

# Load necessary library for string manipulation (optional but helpful) 

library(stringr) 

# Example data frame 

# Function to convert the coordinate string into longitude and latitude 

convert_coords <- function(coord_str) { 

  # Extract longitude (e.g., "E0152" -> "E", "0152") 

  lon_dir <- substr(coord_str, 1, 1)  # Extract the direction (E or W) 

  lon_deg <- as.numeric(substr(coord_str, 2, 5)) / 100  # Extract the degrees (and convert from "0152" to 15.2) 

    # Convert to negative if direction is West 

  if (lon_dir == "W") { 

    lon_deg <- -lon_deg 

  } 

    # Extract latitude (e.g., "N3705" -> "N", "3705") 

  lat_dir <- substr(coord_str, 6, 6)  # Extract the direction (N or S) 

  lat_deg <- as.numeric(substr(coord_str, 7, 10)) / 100  # Extract the degrees (and convert from "3705" to 37.05) 

   

  # Convert to negative if direction is South 

  if (lat_dir == "S") { 

    lat_deg <- -lat_deg 

  } 

    # Return the longitude and latitude as a list 

  return(c(lon_deg, lat_deg)) 

} 

# Apply the conversion function to the coordinates column 

coords_converted <- t(sapply(a2$crs28992res100m, convert_coords)) 

# Add the new columns for longitude (X) and latitude (Y) to the data frame 

a2$X <- coords_converted[, 1] 

a2$Y <- coords_converted[, 2] 

# View the updated data frame 

head(a2) 

a2_sf <- st_as_sf(a2, coords = c("X", "Y"), crs = 28992) 

 

####merging the population file with the exposure file 

 

colnames(a2) 

colnames(shapefile) 

a2$C28992R100<-a2$crs28992res100m 

merged_df <- merge(a2, shapefile, by = "C28992R100", all = FALSE) 

merged_df2<-merged_df 

merged_df2$geometry <- st_centroid(merged_df2$geometry) 

############### 

merged_df2_sf <- st_as_sf(homes, coords = "geometry", crs = 28992) 

# Perform spatial join based on proximity within 100 meters (unit) 

joined_data <- st_join(homes_sf, merged_df2, join = st_within) 
