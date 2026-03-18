library(sf)

# Load the shapefiles
eng_wales <- st_read("ShapeFiles/EngWales_Final.shp")
scotland <- st_read("ShapeFiles/Scotland_Final.shp")
ireland <- st_read("ShapeFiles/Ireland_Final.shp")

# Simplify geometries (adjust tolerance as needed)
eng_wales_simp <- st_simplify(eng_wales, dTolerance = 0.001)
scotland_simp <- st_simplify(scotland, dTolerance = 0.001)
ireland_simp <- st_simplify(ireland, dTolerance = 0.001)

# Save simplified versions
st_write(eng_wales_simp, "ShapeFiles/Simplified/EngWales_Simp.shp", delete_dsn = TRUE)
st_write(scotland_simp, "path/to/Scotland_Simplified.shp", delete_dsn = TRUE)
st_write(ireland_simp, "path/to/Ireland_Simplified.shp", delete_dsn = TRUE)
