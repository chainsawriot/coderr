create_item <- function(var_name = NULL, prompt = NULL, type = NULL, options = NULL, nested_questions = NULL, when = NULL) {
    if (is.null(var_name) | is.null(prompt)) {
        stop("var_name or prompt should not be null.")
    }
    if (!is.null(nested_questions) & is.null(when)) {
        ## don't know when to trigger nested_questions
        stop("Don't know when to trigger nested_questions.")
    }
    if (is.null(type) & !is.null(options)) {
        type <- "multi"
    }
    if (is.null(type) & is.null(when) & is.null(nested_questions)) {
        type <- "text"
    }
    item <- list(var_name = var_name, prompt = prompt, type = type, options = options, nested_questions = nested_questions, when = when)
    attr(item, "class") <- "item"
    return(item)
}

create_itemresponse <- function(response, item) {
    itemresponse <- list(response = response, type = item$type, var_name = item$var_name)
    attr(itemresponse, "class") <- "itemresponse"
    return(itemresponse)
}

print.item <- function(obj) {
    cat(obj$prompt, "\n")
    cat(obj$type, "\n")
    cat(ifelse(is.null(obj$nested_questions), "No nested question", "With nested question(s)"), "\n")
}

ask <- function(obj) {
    UseMethod("ask")
}

ask.default <- function(obj) {
    cat("This is the default", "\n")
}

ask.item <- function(item) {
    rawresponse <- switch(item$type,
                          'text' = readline(paste0(prompt = item$prompt, " ")),
                          'numeric' = as.numeric(readline(paste0(prompt = item$prompt, " "))),
                          'multi' = multi_handling(item),'multi_multi' = multimulti_handling(item)
                          )
    response <- create_itemresponse(response = rawresponse, item = item)
    if (!is.null(item$nested_questions)) {
        if (response$response == item$when) {
            nested_responses <- lapply(item$nested_questions, ask)
            return(list(response, nested_responses))
        } else {
            return(response)
        }
    } else {
        return(response)
    }
}


multi_handling <- function(item) {
    response <- menu(choices = item$options, title = item$prompt)
    return(response)
}

multimulti_handling <- function(item) {
    if (!is.null(item$options)) {
        data <- data.frame()
    } else {
        data <- c()
    }
    while (TRUE) {
        response <- readline(paste0(item$prompt, " "))
        if (response == "") {
            break
        } else {
            if (!is.null(item$options)) {
                res <- multi_handling(item)
                data <- rbind(data, data.frame(written = response, choice = res))
            } else {
                data <- append(data, response)
            }
        }
    }
    return(data)
}

create_codebook <- function(...) {
    codebook <- list(...)
    attr(codebook, "class") <- "codebook"
    return(codebook)
}

admin <- function(obj) {
    UseMethod("admin")
}

admin.default <- function(obj) {
    cat("This is the default", "\n")
}

admin.codebook <- function(codebook) {
    return(lapply(codebook, ask))
}


## For testing
#source("myitems.R")
#x <- ask(itemOS)
