# GitHub Actions will set this, but it's handy locally as well,
# as the dashboard code will use test data and not go to Dropbox
Sys.setenv(CI = "true")

# Load application support files into testing environment
shinytest2::load_app_env()
