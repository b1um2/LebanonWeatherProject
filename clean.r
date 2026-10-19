library(tidyverse)

df <- read_csv("temperature_data.csv")

df <- df[complete.cases(df), ] #drop rows with NA

glimpse(df)

display <- function(vec) { #to view details on each column
    mx <- max(vec)
    mn <- min(vec)
    avg <- mean(vec)
    deviation <- sd(vec) ^ 2
    cat("max: ", mx, " \t")
    cat("min: ", mn, " \t")
    cat("mean: ", avg, " \t")
    cat("sigma: ", deviation, " \t")
    cat("\n")
}

for (i in ncol(df):1) {
    clss <- class(df[[i]])

    cat("column (before normalization): ", i, "\t")
    cat("type: ", clss, "\n->\t")

    if (clss == "Date") {
        df[[i]] <- as.numeric(df[[i]])
        clss <- "numeric"
    }

    if (clss == "numeric") {
        mx <- max(df[, i])
        mn <- min(df[, i])
        avg = mean(df[[i]])
        deviation = sd(df[[i]]) ^ 2
        display(df[[i]])

        if (mx == mn) {
            df[[i]] <- NULL
        } else {
            df[[i]] <- (df[[i]] - avg) / deviation

            cat("column  (after normalization): ", i, "\t")
            cat("type: ", clss, "\n->\t")

            display(df[[i]])
            cat("\n")
        }
    }
}

glimpse(df)