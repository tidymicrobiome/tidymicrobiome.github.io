install_tidymicrobiome <-
  function(packages = c("core", "all"),
           which_package,
           from = c("tidymicrobiome.org", "gitlab", "github"),
           method = c("auto", "internal", "libcurl",
                      "wget", "curl")) {
    if (!require(remotes)) {
      install.packages("remotes")
    }
    
    if (!require(utils)) {
      install.packages("utils")
    }
    
    if (!require(tidyverse)) {
      install.packages("tidyverse")
    }
    
    library(tidyverse, warn.conflicts = FALSE, verbose = FALSE)
    
    if (!require(BiocManager)) {
      install.packages("BiocManager")
    }
    
    if (!require(Rdisop)) {
      BiocManager::install("Rdisop", ask = FALSE, update = FALSE)
    }
    
    installed_packages <-
      utils::installed.packages() %>%
      as.data.frame()
    
    packages <- match.arg(packages)
    from <- match.arg(from)
    method <- match.arg(method)
    
    temp_path <- tempdir()
    dir.create(temp_path, showWarnings = FALSE, recursive = TRUE)
    unlink(x = file.path(temp_path, dir(temp_path)),
           recursive = TRUE,
           force = TRUE)
    
    
    if (from == "gitlab") {
      file <-
        read.table(
          "https://gitlab.com/tidymicrobiome/packages_repo/-/raw/main/packages/file.csv",
          sep = ",",
          header = TRUE
        )
    }
    
    if (from == "github") {
      file <-
        read.table(
          "https://raw.githubusercontent.com/tidymicrobiome/packages_repo/main/packages/file.csv",
          sep = ",",
          header = TRUE
        )
    }
    
    
    if (from == "tidymicrobiome.org") {
      utils::download.file(
        url = "https://www.tidymicrobiome.org/tidymicrobiome-packages/file.csv",
        destfile = file.path(temp_path, "file.csv"),
        method = method
      )
      file <-
        read.table(file.path(temp_path, "file.csv"),
                   sep = ",",
                   header = TRUE)
    }
    
    ####package list
    core_package_list <-
      c(
        "masstools",
        "massdataset",
        "metid",
        "massstat",
        "massqc",
        "massprocesser",
        "masscleaner",
        "metpath",
        "tidymicrobiome"
      )
    
    if (!missing(which_package)) {
      package_list <-
        which_package
    } else{
      if (packages == "core") {
        package_list <-
          core_package_list
      } else{
        package_list <-
          c(core_package_list,
            "massconverter",
            "massdatabase")
      }
    }
    
    ####download the packages
    for (x in package_list) {
      message("Download ", x, "...")
      ##install masstools first
      if (from == "github") {
        url <-
          paste0(
            "https://github.com/tidymicrobiome/packages_repo/raw/main/packages/",
            file$file_name.y[file$package == x]
          )
      }
      
      if (from == "gitlab") {
        url <-
          paste0(
            "https://gitlab.com/tidymicrobiome/packages_repo/-/raw/main/packages/",
            file$file_name.y[file$package == x],
            "?inline=false"
          )
      }

      
      if (from == "tidymicrobiome.org") {
        url <-
          paste0("https://www.tidymicrobiome.org/tidymicrobiome-packages/",
                 file$file_name.y[file$package == x])
        
        dependent_package <-
          readLines(
            paste0(
              "https://www.tidymicrobiome.org/tidymicrobiome-packages/",
              file$package[file$package == x],
              "_Description.txt"
            )
          )
        
        idx1 <-
          grep("Imports", dependent_package)
        idx2 <-
          grep("License", dependent_package)
        dependent_package <-
          dependent_package[(idx1 + 1):(idx2 - 1)] %>%
          stringr::str_trim(side = "both") %>%
          stringr::str_replace(",", "")
        
        dependent_package <-
          dependent_package[!dependent_package %in% installed_packages$Package]
        
        dependent_package <-
          dependent_package[-c(
            grep("Imports", dependent_package),
            grep("R \\(", dependent_package),
            grep("Depends", dependent_package),
            grep("BugReports", dependent_package),
            grep("URL\\: ", dependent_package),
            grep("License\\: ", dependent_package),
            grep("and reproducible", dependent_package),
            grep("LazyData", dependent_package),
            grep("Encoding\\:", dependent_package),
            grep("VignetteBuilder\\:", dependent_package),
            grep("RoxygenNote\\:", dependent_package),
            grep("Roxygen\\:", dependent_package),
            grep("Description\\:", dependent_package)
          )]
        
        if (length(dependent_package) > 0) {
          dependent_package %>%
            purrr::walk(
              .f = function(temp_pkg) {
                tryCatch(
                  install.packages(temp_pkg),
                  error = function(e) {
                    NULL
                  }
                )
                
                tryCatch(
                  BiocManager::install(temp_pkg, ask = FALSE, update = FALSE),
                  error = function(e) {
                    NULL
                  }
                )
              }
            )
        }
      }
      
      utils::download.file(
        url = url,
        destfile = file.path(temp_path, file$file_name.y[file$package == x]),
        method = method
      )
    }
    
    ####install package
    for (x in package_list) {
      message("Install ", x, "...")
      ##install masstools first
      
      tryCatch(
        detach(name = paste0("package:", x)),
        error = function(e) {
          message(x, " is not loaded")
        }
      )
      
      if (x == "tidymicrobiome") {
        # detach("package:purrr")
        # detach("package:stringr")
        # install.packages("purrr")
        # install.packages("stringr")
        
        install.packages(
          file.path(temp_path, file$file_name.y[file$package == x]),
          repos = NULL,
          dependencies = TRUE
        )
      } else{
        install.packages(
          file.path(temp_path, file$file_name.y[file$package == x]),
          repos = NULL,
          dependencies = TRUE
        )
        
        remotes::install_deps(
          pkgdir = file.path(temp_path, file$file_name.y[file$package == x]),
          dependencies = TRUE,
          upgrade = "never"
        )
      }
      unlink(file.path(temp_path, file$file_name.y[file$package == x]))
    }
    
    message("All done.")
  }
