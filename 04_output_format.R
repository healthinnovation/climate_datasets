
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

rename_first_column <- function(df) {
  if (ncol(df) > 0) colnames(df)[1] <- "UBIGEO"
  return(df)
}

# ✅ Aplicar a todas las listas de manera uniforme
data_departamento <- lapply(data_departamento, rename_first_column)
data_provincia <- lapply(data_provincia, rename_first_column)
data_distrito <- lapply(data_distrito, rename_first_column)


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

apply_format <- function(data_list, type) {
  if (length(data_list) > 0) {
    for (i in seq_along(data_list)) {
      if ("UBIGEO" %in% colnames(data_list[[i]])) {
        # Aplicar formato con sapply()
        data_list[[i]]$UBIGEO <- sapply(data_list[[i]]$UBIGEO, format_ubigeo, type = type)

        # Si deseas ordenar, deja esta línea; si no, elimínala
        data_list[[i]] <- data_list[[i]][order(as.numeric(data_list[[i]]$UBIGEO)), ]
      }
    }
  }
  return(data_list)
}

# ✅ Aplicar formato para cada lista
data_departamento <- apply_format(data_departamento, "departamento")
data_provincia <- apply_format(data_provincia, "provincia")
data_distrito <- apply_format(data_distrito, "distrito")

output_folder<-"output_format"

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

