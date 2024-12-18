# Main.R: Master script to reproduce the full analysis workflow

# 1. Load libraries and set up environment
source("ObtainData.R")

# 2. Data extraction and cleaning
source("PrepData.R")

# 3. Data analysis and modeling
source("Analysis1.R")
source("Analysis2.R")
source("Analysis3.R")

cat("All scripts have been successfully sourced. Workflow complete.\n")
