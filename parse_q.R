codebook <- list(list(var = "Q1", prompt = "Wie geht es Ihnen?", type = "text"),
                 list(var = "Q2", prompt = "Wie ist diem name?", type = 'text'),
                 list(var = "Q3", prompt = "2 + 2 = ?", type = 'numeric'),
                 list(var = "Q4", prompt = "It is good right?", type = "multi", options = c("Yes", "No"))
                 )

generate_prompt <- function(item) {
    if (item['type'] == 'text') {
        response <- readline(paste0(prompt = item['prompt'], " "))
    } else if (item['type'] == 'numeric') {
        response <- readline(paste0(prompt = item['prompt'], " "))
        response <- as.numeric(response)
    } else if (item['type'] == 'multi') {
        response <- multi_handling(item)
    }
    return(response)
}

multi_handling <- function(item) {
    response <- 0
    while (response < 1 | response > length(item['options'][[1]])) {
        cat(paste0(item['prompt'][[1]], " ("))
        response <- readline(paste0(paste(1:length(item['options'][[1]]), ":", item['options'][[1]], collapse = " / "), " ) "))
        response <- as.numeric(response)
    }
    return(response)
}


data_entry <- function(codebook) {
    single_row <- as.data.frame(lapply(codebook, generate_prompt), stringsAsFactors = FALSE)
    colnames(single_row) <- sapply(codebook, function(x) x['var'])
    return(single_row)
}

multi_handling(list(var = "Q4", prompt = "It is good right?", type = "multi", options = c(5, 6, 9, 10)))

z <- data_entry(codebook)
