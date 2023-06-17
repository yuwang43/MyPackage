#' @export
#' @import utils shiny
run_app = function(){

    # Name of package
    mypackage = utils::packageName()


    # Name of app folder inside of inst/
    # Do not include `inst/`
    app_dir = system.file("myapp", package = mypackage)

    if (app_dir == "") {
        stop(paste0("Could not find example directory. Try re-installing `",
                    mypackage, "`.", call. = FALSE))
             }

    shiny::runApp(app_dir, display.mode = "normal")
    }
