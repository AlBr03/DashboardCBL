library(dplyr)
library(sparklyr)
library(shiny)
library(ggplot2)
library(plotly)
library(pysparklyr)
library(shinyjs)
library(tidyr)
library(dbplyr)
library(yaml)
library(config)
library(curl)


.libPaths("..\\Shiny_dashboard\\.virtualenvs\\r-sparklyr-databricks-13.3\\lib")
sc <- spark_connect(method = "databricks_connect", Sys.getenv("DATABRICKS_HOST"))
Sys.setenv(TZ = "Europe/Amsterdam")

