# README

![check-app](https://github.com/COMPASS-DOE/TEMPESTdash/actions/workflows/check-app.yaml/badge.svg)

To create a new Dropbox token, do
```r
library(rdrop2)
token <- drop_auth(new_user = TRUE) # flush any existing token
saveRDS(token, "droptoken.rds")
```
