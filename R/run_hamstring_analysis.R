#' Run Hamstring Architecture Analysis
#'
#' Interactive ultrasound analysis workflow for measuring pennation angle,
#' muscle thickness, and fascicle length from ultrasound images.
#'
#' @return A data frame of hamstring architecture results.
#' @importFrom graphics locator rasterImage
#' @export
run_hamstring_analysis <- function() {

  img_files <- tcltk::tk_choose.files(
    caption = "Select Ultrasound Images",
    multi = TRUE
  )

  if (length(img_files) == 0) {
    stop("No images selected.")
  }

  all_results <- data.frame()
  depth_choices <- c("45", "60", "70", "80", "90")

  for (file_path in img_files) {

    cat("\nAnalyzing:", basename(file_path), "\n")

    img <- tryCatch({
      readbitmap::read.bitmap(file_path)
    }, error = function(e1) {
      tryCatch({
        jpeg::readJPEG(file_path)
      }, error = function(e2) {
        message("Could not open: ", basename(file_path))
        return(NULL)
      })
    })

    if (is.null(img)) next

    repeat {
      plot(
        1,
        type = "n",
        xlim = c(0, ncol(img)),
        ylim = c(nrow(img), 0),
        xlab = "X",
        ylab = "Y",
        main = paste(
          "Click Points:\n1-2: Superficial Apo\n3-4: Deep Apo\n5-6: Fascicle\n",
          basename(file_path)
        )
      )

      rasterImage(img, 0, 0, ncol(img), nrow(img))

      pts <- locator(6, type = "p", pch = 19, col = "red")
      x <- pts$x
      y <- pts$y

      confirm <- tcltk::tk_messageBox(
        message = "Are you happy with the selected points?",
        icon = "question",
        type = "yesno",
        default = "yes"
      )

      if (tolower(as.character(confirm)) == "yes") break
    }

    depth_choice <- tcltk::tk_select.list(
      depth_choices,
      title = paste("Select scan depth (mm) for", basename(file_path)),
      multiple = FALSE
    )

    if (depth_choice == "") {
      cat("No depth selected. Skipping.\n")
      next
    }

    depth_mm <- as.numeric(depth_choice)
    img_height_px <- dim(img)[1]
    pixels_per_mm <- img_height_px / depth_mm

    sup_vec <- c(x[2] - x[1], y[2] - y[1])
    fasc_vec <- c(x[6] - x[5], y[6] - y[5])

    dot_product <- sum(sup_vec * fasc_vec)

    angle_rad <- acos(
      dot_product /
        (sqrt(sum(sup_vec^2)) * sqrt(sum(fasc_vec^2)))
    )

    angle_deg <- angle_rad * 180 / pi

    line_params <- function(x1, y1, x2, y2) {
      m <- (y2 - y1) / (x2 - x1)
      b <- y1 - m * x1
      list(m = m, b = b)
    }

    sup_line <- line_params(x[1], y[1], x[2], y[2])
    deep_line <- line_params(x[3], y[3], x[4], y[4])

    x_probe <- x[5]
    y_sup <- sup_line$m * x_probe + sup_line$b
    y_deep <- deep_line$m * x_probe + deep_line$b

    thickness_px <- abs(y_deep - y_sup)
    thickness_mm <- round(thickness_px / pixels_per_mm, 2)

    fascicle_length_mm <- round(thickness_mm / sin(angle_rad), 2)

    filename <- basename(file_path)

    side <- if (grepl("RHS", file_path, ignore.case = TRUE)) {
      "RHS"
    } else if (grepl("LHS", file_path, ignore.case = TRUE)) {
      "LHS"
    } else {
      "Unknown"
    }

    state <- if (grepl("contracted", file_path, ignore.case = TRUE)) {
      "Contracted"
    } else if (grepl("relaxed", file_path, ignore.case = TRUE)) {
      "Relaxed"
    } else {
      "Unknown"
    }

    player_folder <- basename(dirname(dirname(file_path)))
    player_name <- gsub("\\s+", " ", player_folder)

    result <- data.frame(
      Player = player_name,
      Side = side,
      State = state,
      Image = filename,
      Depth_mm = depth_mm,
      Pennation_Angle_deg = round(angle_deg, 2),
      Muscle_Thickness_mm = thickness_mm,
      Fascicle_Length_mm = fascicle_length_mm
    )

    print(result)

    all_results <- rbind(all_results, result)
  }

  save_path <- tcltk::tclvalue(
    tcltk::tkgetSaveFile(
      title = "Save Results",
      defaultextension = ".xlsx",
      initialfile = paste0(
        "Hamstring_Architecture_Results_",
        format(Sys.time(), "%Y-%m-%d_%H%M"),
        ".xlsx"
      )
    )
  )

  if (save_path != "") {
    writexl::write_xlsx(all_results, save_path)
    cat("\nResults saved to:", save_path, "\n")
  } else {
    cat("\nNo file saved.\n")
  }

  return(all_results)
}
