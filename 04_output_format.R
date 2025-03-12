
folder_path <- "output"

file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

data_departamento <- list()
data_provincia <- list()
data_distrito <- list()

for (file in file_list) {
  data <- read.csv(file, stringsAsFactors = FALSE)

  if (grepl("departament", file)) {
    data_departamento[[file]] <- data
  } else if (grepl("province", file)) {
    data_provincia[[file]] <- data
  } else if (grepl("district", file)) {
    data_distrito[[file]] <- data
  }
}

format_ubigeo <- function(ubigeo, type) {
  ubigeo <- as.character(ubigeo)

  if (type == "departamento") {
    if (nchar(ubigeo) >= 2) {
      return(paste0(substr(ubigeo, 1, 2), strrep("0", 6 - nchar(substr(ubigeo, 1, 2)))))
    } else if (nchar(ubigeo) == 1) {
      return(paste0("0", substr(ubigeo, 1, 1), strrep("0", 4)))
    }
  }

  if (type == "provincia") {
    if (nchar(ubigeo) >= 4) {
      return(paste0(substr(ubigeo, 1, 4), strrep("0", 6 - nchar(substr(ubigeo, 1, 4)))))
    } else if (nchar(ubigeo) == 3) {
      return(paste0("0", substr(ubigeo, 1, 3), strrep("0", 2)))
    } else if (nchar(ubigeo) == 2) {
      return(paste0("00", substr(ubigeo, 1, 2), strrep("0", 2)))
    } else if (nchar(ubigeo) == 1) {
      return(paste0("000", substr(ubigeo, 1, 1), strrep("0", 2)))
    }
  }

  if (type == "distrito") {
    if (nchar(ubigeo) < 6) {
      return(sprintf("%06d", as.numeric(ubigeo)))
    } else if (nchar(ubigeo) == 6) {
      return(ubigeo)
    }
  }

  return("")
}

if (length(data_departamento) > 0) {
  data_departamento <- lapply(data_departamento, function(df) {
    if ("UBIGEO" %in% colnames(df)) {
      df$UBIGEO <- sapply(df$UBIGEO, format_ubigeo, type = "departamento")
      df <- df[order(as.numeric(df$UBIGEO)), ]
    }
    return(df)
  })
}

if (length(data_provincia) > 0) {
  data_provincia <- lapply(data_provincia, function(df) {
    if ("UBIGEO" %in% colnames(df)) {
      df$UBIGEO <- sapply(df$UBIGEO, format_ubigeo, type = "provincia")
      df <- df[order(as.numeric(df$UBIGEO)), ]
    }
    return(df)
  })
}

if (length(data_distrito) > 0) {
  data_distrito <- lapply(data_distrito, function(df) {
    if ("UBIGEO" %in% colnames(df)) {
      df$UBIGEO <- sapply(df$UBIGEO, format_ubigeo, type = "distrito")
      df <- df[order(as.numeric(df$UBIGEO)), ]
    }
    return(df)
  })
}

export_data <- function(data_list, output_folder) {
  if (length(data_list) > 0) {
    for (file in names(data_list)) {
      write.csv(data_list[[file]],
                file = file.path(output_folder, basename(file)),
                row.names = FALSE)
    }
  }
}


export_data(data_departamento, output_folder)
export_data(data_provincia, output_folder)
export_data(data_distrito, output_folder)

