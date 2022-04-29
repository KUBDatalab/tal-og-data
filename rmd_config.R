rmd_filename <- stringr::str_remove(knitr::current_input(), "\\.Rmd")

# Figure path on disk = base.dir + fig.path
# Figure URL online = base.url + fig.path
knitr::opts_knit$set(base.dir = stringr::str_c(here::here(), "/"), base.url = "/") # project root folder
knitr::opts_chunk$set(fig.path = stringr::str_c(file.path("rmd_images", rmd_filename), "/"))