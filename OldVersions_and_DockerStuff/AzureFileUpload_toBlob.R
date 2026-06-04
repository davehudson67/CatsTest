library(AzureStor)

# -------------------------------
# 🔹 1. Define Azure Blob Storage Connection
# -------------------------------
blob_url <- "http://127.0.0.1:10000/devstoreaccount1"  # Change for real Azure
account_key <- Sys.getenv("ACCOUNT_KEY")

# Connect to Azure Blob Storage
blob_endp <- blob_endpoint(blob_url, key = account_key)

# -------------------------------
# 🔹 2. Ensure Persistent Storage Container Exists
# -------------------------------
persistent_container_name <- "persistent-files"

# Correctly extract container names from list_storage_containers()
existing_containers <- list_storage_containers(blob_endp)
existing_container_names <- sapply(existing_containers, function(cont) cont$name)  # Extract only names

# Create container only if it does not exist
if (!(persistent_container_name %in% existing_container_names)) {
  create_storage_container(blob_endp, persistent_container_name)
  print("Created persistent Azure Blob Storage container: persistent-files")
} else {
  print("Persistent container already exists, skipping creation.")
}

# Retrieve the container object
persistent_container <- storage_container(blob_endp, persistent_container_name)

# -------------------------------
# 🔹 3. Upload HTML Files
# -------------------------------
html_files <- list(
  "data/HomeText.html",
  "data/DataInformation_text_top.html",
  "data/DataInformation_text_bottom.html",
  "data/MapText.html",
  "data/neutering_text.html"  # Neutering scenarios text
)

for (file in html_files) {
  if (file.exists(file)) {
    storage_upload(persistent_container, file, basename(file))
    print(paste("✅ Uploaded:", basename(file)))
  } else {
    print(paste("❌ File not found:", basename(file)))
  }
}

# -------------------------------
# 🔹 4. Upload Excel Data File
# -------------------------------
excel_file <- "data/DataInfoTable.xlsx"

if (file.exists(excel_file)) {
  storage_upload(persistent_container, excel_file, basename(excel_file))
  print("✅ Uploaded: DataInfoTable.xlsx")
} else {
  print("❌ DataInfoTable.xlsx not found")
}

# -------------------------------
# 🔹 5. Upload Shapefile Components
# -------------------------------
shapefile_name <- "UKCatDensitiesCounty"
shapefile_extensions <- c(".shp", ".shx", ".dbf", ".prj", ".cpg", ".qmd")

shapefile_components <- paste0("data/", shapefile_name, shapefile_extensions)

for (file in shapefile_components) {
  if (file.exists(file)) {
    storage_upload(persistent_container, file, basename(file))
    print(paste("✅ Uploaded:", basename(file)))
  } else {
    print(paste("❌ File not found:", basename(file)))
  }
}

# -------------------------------
# 🔹 6. Upload JSON Button Config
# -------------------------------
json_file <- "data/button_config.json"

if (file.exists(json_file)) {
  storage_upload(persistent_container, json_file, "button_config.json")
  print("✅ Uploaded: button_config.json")
} else {
  print("❌ button_config.json not found")
}

# -------------------------------
# 🔹 7. Upload References PDF
# -------------------------------
references_file <- "www/References.pdf"

if (file.exists(references_file)) {
  storage_upload(persistent_container, references_file, "References.pdf")
  print("✅ Uploaded: References.pdf")
} else {
  print("❌ References.pdf not found")
}

# -------------------------------
# 🔹 8. List Uploaded Files in Persistent Storage
# -------------------------------
print("📂 Files in Persistent Blob Storage:")
print(list_storage_files(persistent_container))

