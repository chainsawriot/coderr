
codebook <- list(list(var = "v2", prompt = "Original: mentioned any HK politicians, HKSAR government or Chinese government?", type = 'multi', options = c("Yes", "No"), nestedquestion = list(var = "v21", prompt = "Which", type = 'multi_multi', options = c("Positive", "Negative", "Can't Tell")), when = 1),
                 list(var = "v3", prompt = "Added comment?", type = 'multi', options = c("Yes", "No"), when = 1, nestedquestion = list(var = "v30", prompt = "Added comment: mentioned any HK politicians, HKSAR government or Chinese government?", type = 'multi', options = c("Yes", "No"), when = 1, nestedquestion = list(var = "v31", prompt = "Which", type = 'multi_multi', options = c("Positive", "Negative", "Can't Tell")))),
                 list(var = "v4", prompt = "Agree with original posters?", type = "multi", options = c("Yes", "No", "CT")))


### should change it to a switch statement

generate_prompt <- function(item) {
    if (item['type'] == 'text') {
        response <- readline(paste0(prompt = item['prompt'], " "))
    } else if (item['type'] == 'numeric') {
        response <- readline(paste0(prompt = item['prompt'], " "))
        response <- as.numeric(response)
    } else if (item['type'] == 'multi') {
        response <- multi_handling(item)
    } else if (item['type'] == 'multi_multi') {
        response <- multimulti_handling(item)
    }
    if ("nestedquestion" %in% names(item)) {
        if (response == item['when']) {
            nestedanswer <- generate_prompt(item['nestedquestion'][[1]])
            return(list(response, nestedanswer))
        } else {
            return(response)
        }
    } else {
        return(response)
    }
}

multi_handling <- function(item, prompt = NULL) {
    if (is.null(prompt)) {
        prompt <- item['prompt'][[1]]
    }
    response <- 0
    while (is.na(response) | response < 1 | response > length(item['options'][[1]])) {
        cat(paste0(prompt, " ("))
        response <- readline(paste0(paste(1:length(item['options'][[1]]), ":", item['options'][[1]], collapse = " / "), " ) "))
        response <- as.numeric(response)
    }
    return(response)
}

multimulti_handling <- function(item) {
    if ("options" %in% names(item)) {
        data <- data.frame()
    } else {
        data <- c()
    }
    while (TRUE) {
        response <- readline(paste0(item['prompt'], " "))
        if (response == "") {
            break
        } else {
            if ("options" %in% names(item)) {
                res <- multi_handling(item, "how ")
                data <- rbind(data, data.frame(which = response, how = res))
            } else {
                data <- append(data, response)
            }
        }
    }
    return(data)
    
}

#multitext_handling(list(var = "v11", prompt = "Which", type = 'many_text', options = c("Pos", "Neg", "CT")))

data_entry <- function(codebook) {
    single_row <- lapply(codebook, generate_prompt)
    names(single_row) <- sapply(codebook, function(x) x$var)
    return(single_row)
}

#multi_handling(list(var = "Q4", prompt = "It is good right?", type = "multi", options = c(5, 6, 9, 10)))


## nestedQ <- list(var = "v2", prompt = "Original: mentioned any HK politicians, HKSAR government or Chinese government?", type = 'multi', options = c("Yes", "No"), nestedquestion = list(var = "v21", prompt = "Which", type = 'multi_multi', options = c("Positive", "Negative", "Can't Tell")), when = 1)
## multi_handling(nestedQ)

z <- data_entry(simple_codebook)
