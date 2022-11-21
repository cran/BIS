## ----install, eval=FALSE, message=FALSE, warning=FALSE------------------------
#  library(devtools)
#  install_github("stefanangrick/BIS")  # GitHub

## ----loading, message=FALSE, warning=FALSE------------------------------------
library("BIS")

## ----datasets, message=FALSE, warning=FALSE-----------------------------------
ds <- get_datasets()
head(ds, 20)

## ----rates, message=FALSE, warning=FALSE--------------------------------------
rates <- get_bis(ds$url[ds$id == "full_cbpol_m_csv"])
head(rates)

## ----plot, message=FALSE, warning=FALSE---------------------------------------
library("dplyr")
library("ggplot2")
library("zoo")

rates_plot <- subset(rates, ref_area %in% c("US", "XM", "JP", "GB", "CH", "CA"))
rates_plot <- mutate(rates_plot, date = as.Date(as.yearmon(date, format = "%Y-%m")))

ggplot(rates_plot, aes(date, obs_value, color = reference_area)) +
  geom_line(show.legend = FALSE) +
  facet_wrap(~reference_area) +
  labs(title = "Central bank policy rates",
       subtitle = "% per annum", x = NULL, y = NULL)

## ----nopivot, eval=FALSE, message=FALSE, warning=FALSE------------------------
#  options(timeout = 600)
#  lbs <- get_bis(ds$url[(ds$id == "full_lbs_d_pub_csv")], auto_pivot = FALSE)
#  lbs <- subset(lbs, l_parent_cty %in% c("US", "DE", "JP"))
#  lbs <- pivot_longer_bis(lbs)

