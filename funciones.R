generate.abstracts_Marcos <- function (my.filename, dirAbstracts, author.lastname = seq(23, 
                                                                                        by = 6, length.out = 7), author.firstname = seq(24, by = 6, 
                                                                                                                                        length.out = 7), author.institution = seq(26, by = 6, length.out = 7), 
                                       author.city = seq(27, by = 6, length.out = 7), author.country = seq(3, 
                                                                                                           by = 1, length.out = 7) + 1, author.email = seq(25, by = 6, 
                                                                                                                                                           length.out = 7), author.presenting = seq(22, by = 6, 
                                                                                                                                                                                                    length.out = 7), pres.title = 18, pres.abstract = 21, 
                                       accept = 17, topic1 = 19, topic2 = 3, id = 2, notes = 73, 
                                       ref2 = 77, ref3 = 78, ref4 = 79, duplicated = NULL, accept.all = F, 
                                       noNotes = F, style = "AS2012", notesDay = 81, notesPayment = 82, 
                                       verbose = FALSE, palabrasClaves = 100, idBase = 101) 
{
    initial.wd = getwd()
    my.data <- read.delim(my.filename, sep = "\t")
    setwd(dirAbstracts)
    num.authors <- apply(my.data[, author.lastname], 1, function(x) sum(!is.na(x) & 
                                                                            x != ""))
    my.data <- my.data[num.authors != 0, ]
    my.data[, author.email] = apply(my.data[, author.email], 
                                    2, function(x) as.character(x))
    for (i in 1:nrow(my.data)) for (j in 1:length(author.email)) my.data[i, 
                                                                         author.email[j]] = sub(pattern = "_", replacement = "\\\\_", 
                                                                                                my.data[i, author.email[j]])
    if (accept.all != T) 
        my.data <- my.data[my.data[, accept] == "Yes", ]
    num.authors <- apply(my.data[, author.lastname], 1, function(x) sum(!is.na(x) & 
                                                                            x != ""))
    presenting.author <- as.numeric(unlist(apply(my.data[, author.presenting], 
                                                 1, function(x) which(x == "Yes")[1])))
    presenting.author[is.na(presenting.author)] <- 1
    titolo = autori = affiliazioni = abstract = rep("", dim(my.data)[1])
    
    # # Marcos
    # Para que ciudad y pais de cada autor tengan vacio y no NA
    my.data$nada <- ""
    # my.data[is.na(my.data[, author.city]), author.city] <- ""
    # my.data[is.na(my.data[, author.country]), author.city] <- ""
    # browser()
    
    if (style == "AS2012") {
        for (i in 1:dim(my.data)[1]) {
            if (verbose == TRUE) 
                cat("ID=", i)
            titolo[i] <- paste("{", my.data[i, pres.title], "}", 
                               sep = "")
            TMP <- strsplit(as.character(my.data[, author.firstname[1]]), 
                            c(" "))
            number.of.names <- unlist(lapply(TMP, length))
            authors.initials <- toupper(strsplit(TMP[[i]][1], 
                                                 "")[[1]][1])
            if (number.of.names[i] > 1) 
                for (j in 2:number.of.names[i]) authors.initials <- paste(authors.initials, 
                                                                          toupper(strsplit(TMP[[i]][j], "")[[1]][1]), 
                                                                          sep = "")
            inst.city = paste(unlist((my.data[i, author.institution[1:num.authors[i]]])), 
                              unlist((my.data[i, author.city[1:num.authors[i]]])))
            how.many.inst = length(unique(inst.city))
            one.inst.true = how.many.inst == 1
            if (one.inst.true) 
                index.inst = rep(1, num.authors[i])
            else {
                index.inst = as.numeric(factor(inst.city, levels = unique(inst.city)))
            }
            if (num.authors[i] == 1) {
                tmp <- paste(my.data[i, author.firstname[1]], 
                             " ", my.data[i, author.lastname[1]], generbook:::.fun.get.index.slo(my.data[i, 
                                                                                             author.lastname[1]], authors.initials), sep = "")
                # Marcos
                tmp.mails <- ""
                # Solo agregar esto si tiene email
                if (!is.na(my.data[i, author.email[1]]))
                    tmp.mails <- paste0("\\Email{", my.data[i, author.email[1]], "}\n")
                # tmp.mails = paste("\\Email{", ifelse(!is.na(my.data[i, 
                #                                                     author.email[1]]), paste(my.data[i, author.email[1]]), 
                #                                      ""), "}\n", sep = "")
            }
            else {
                if (presenting.author[i] == 1) {
                    if (one.inst.true) {
                        tmp <- paste("\\Presenting{", my.data[i, 
                                                              author.firstname[1]], " ", my.data[i, author.lastname[1]], 
                                     "}", generbook:::.fun.get.index.slo(my.data[i, author.lastname[1]], 
                                                             authors.initials), sep = "")
                    }
                    else tmp <- paste("\\Presenting{", my.data[i, 
                                                               author.firstname[1]], " ", my.data[i, author.lastname[1]], 
                                      "}", "$^1$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                              author.lastname[1]], authors.initials), 
                                      sep = "")
                }
                else {
                    if (one.inst.true) {
                        tmp <- paste(my.data[i, author.firstname[1]], 
                                     " ", my.data[i, author.lastname[1]], generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                     author.lastname[1]], authors.initials), 
                                     sep = "")
                    }
                    else {
                        tmp <- paste(my.data[i, author.firstname[1]], 
                                     " ", my.data[i, author.lastname[1]], "$^1$", 
                                     generbook:::.fun.get.index.slo(my.data[i, author.lastname[1]], 
                                                        authors.initials), sep = "")
                    }
                }
            }
            if (num.authors[i] > 1) 
                for (j in 2:num.authors[i]) {
                    TMP <- strsplit(as.character(my.data[, author.firstname[j]]), 
                                    c(" "))
                    number.of.names <- unlist(lapply(TMP, length))
                    authors.initials <- toupper(strsplit(TMP[[i]][1], 
                                                         "")[[1]][1])
                    if (number.of.names[i] > 1) 
                        for (jj in 2:number.of.names[i]) authors.initials <- paste(authors.initials, 
                                                                                   toupper(strsplit(TMP[[i]][jj], "")[[1]][1]), 
                                                                                   sep = "")
                    if (j < num.authors[i]) {
                        if (one.inst.true) {
                            if (presenting.author[i] == j) 
                                tmp <- paste(tmp, ", \\Presenting{", 
                                             paste(my.data[i, author.firstname[j]], 
                                                   " ", my.data[i, author.lastname[j]], 
                                                   "}", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                   author.lastname[j]], authors.initials), 
                                                   sep = ""))
                            else tmp <- paste(tmp, paste(my.data[i, 
                                                                 author.firstname[j]], " ", my.data[i, 
                                                                                                    author.lastname[j]], generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                                                                    author.lastname[j]], authors.initials), 
                                                         sep = ""), sep = ", ")
                        }
                        else {
                            if (presenting.author[i] == j) 
                                tmp <- paste(tmp, ", \\Presenting{", 
                                             paste(my.data[i, author.firstname[j]], 
                                                   " ", my.data[i, author.lastname[j]], 
                                                   "}$^", index.inst[j], "$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                         author.lastname[j]], authors.initials), 
                                                   sep = ""), sep = "")
                            else tmp <- paste(tmp, paste(my.data[i, 
                                                                 author.firstname[j]], " ", my.data[i, 
                                                                                                    author.lastname[j]], "$^", index.inst[j], 
                                                         "$", generbook:::.fun.get.index.slo(my.data[i, author.lastname[j]], 
                                                                                 authors.initials), sep = ""), sep = ", ")
                        }
                    }
                    else {
                        if (one.inst.true) {
                            if (presenting.author[i] == j) 
                                tmp <- paste(tmp, " y \\Presenting{", 
                                             paste(my.data[i, author.firstname[j]], 
                                                   " ", my.data[i, author.lastname[j]], 
                                                   "}", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                   author.lastname[j]], authors.initials), 
                                                   sep = ""), sep = "")
                            else tmp <- paste(tmp, paste(my.data[i, 
                                                                 author.firstname[j]], " ", my.data[i, 
                                                                                                    author.lastname[j]], generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                                                                    author.lastname[j]], authors.initials), 
                                                         sep = ""), sep = " y ")
                        }
                        else {
                            if (presenting.author[i] == j) 
                                tmp <- paste(tmp, "y \\Presenting{", 
                                             paste(my.data[i, author.firstname[j]], 
                                                   " ", my.data[i, author.lastname[j]], 
                                                   "}$^", index.inst[j], "$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                         author.lastname[j]], authors.initials), 
                                                   sep = ""), sep = "")
                            else tmp <- paste(tmp, paste(my.data[i, 
                                                                 author.firstname[j]], " ", my.data[i, 
                                                                                                    author.lastname[j]], "$^", index.inst[j], 
                                                         "$", generbook:::.fun.get.index.slo(my.data[i, author.lastname[j]], 
                                                                                 authors.initials), sep = ""), sep = " y ")
                        }
                    }
                }
            autori[i] <- paste("{", tmp, "}", sep = "")
            if (num.authors[i] == 1) {
                # Marcos
                # Si ciudad, pais, email no tienen nada, queda una coma y nada despues, 
                # entonces ahora solo voy a agregar si contienen info
                # obtener institucio, ciudad y pais
                # browser()
                datos <- my.data[i, c(author.institution[1], author.city[1], author.country[1])]
                datos <- sapply(datos, as.character)
                # quedarme con los no vacios y unirlos separandolos por coma
                datos <- paste(datos[datos != ""], collapse = ", ")
                tmp <- paste0("\\Afilliation{", datos, "}\n")
                # tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                #                                               author.institution[1]] != "", paste(my.data[i,
                #                                                                                           author.institution[1]], ", ", sep = ""), ""),
                #              ifelse(my.data[i, author.city[1]] != "", paste(my.data[i,
                #                                                                     author.city[1]], ", ", sep = ""), ""), ifelse(my.data[i,
                #                                                                                                                           author.country[1]] != "", paste(my.data[i,
                #                                                                                                                                                                   author.country[1]]), ""), "}\n", sep = "")
            }
            else {
                if (one.inst.true) {
                    # Marcos
                    datos <- my.data[i, c(author.institution[1], author.city[1], author.country[1])]
                    datos <- sapply(datos, as.character)
                    datos <- paste(datos[datos != ""], collapse = ", ")
                    tmp <- paste0("\\Afilliation{", datos, "}\n")
                    tmp.mails <- ""
                    if (!is.na(my.data[i, author.email[1]])) 
                        tmp.mails <- paste0("\\Email{", my.data[i, author.email[1]], "}\n")
                    # tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                    #                                               author.institution[1]] != "", paste(my.data[i,
                    #                                                                                           author.institution[1]], ", ", sep = ""),
                    #                                       ""), ifelse(my.data[i, author.city[1]] !=
                    #                                                       "", paste(my.data[i, author.city[1]], ", ",
                    #                                                                 sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                    #                                                                                            "", paste(my.data[i, author.country[1]]),
                    #                                                                                        ""), "}\n", sep = "")
                    # tmp.mails = paste("\\Email{", ifelse(!is.na(my.data[i,
                    #                                                     author.email[1]]), paste(my.data[i, author.email[1]]),
                    #                                      ""), "}\n", sep = "")
                    for (j in 2:num.authors[i]) {
                        if (!is.na(my.data[i, author.email[j]]))
                            tmp.mails <- paste0(tmp.mails, "$\\Email{", my.data[i, author.email[j]], "}\n")
                        # tmp.mails <- paste(tmp.mails, "$\\Email{", 
                        #                    ifelse(!is.na(my.data[i, author.email[j]]), 
                        #                           paste(my.data[i, author.email[j]]), ""), 
                        #                    "}\n", sep = "")
                    }
                }
                else {
                    # Marcos
                    datos <- my.data[i, c(author.institution[1], author.city[1], author.country[1])]
                    datos <- sapply(datos, as.character)
                    datos <- paste(datos[datos != ""], collapse = ", ")
                    tmp <- paste0("\\Afilliation{$^1$", datos, "}\n")
                    # tmp <- paste("\\Afilliation{$^", 1, "$", ifelse(my.data[i, 
                    #                                                         author.institution[1]] != "", paste(my.data[i, 
                    #                                                                                                     author.institution[1]], ", ", sep = ""), 
                    #                                                 ""), ifelse(my.data[i, author.city[1]] != 
                    #                                                                 "", paste(my.data[i, author.city[1]], ", ", 
                    #                                                                           sep = ""), ""), ifelse(my.data[i, author.country[1]] != 
                    #                                                                                                      "", paste(my.data[i, author.country[1]]), 
                    #                                                                                                  ""), "}\n ", sep = "")
                    if (num.authors[i] > 1) {
                        for (j in unique(index.inst)[-1]) {
                            which.author.use = which(j == index.inst)[1]
                            datos <- my.data[i, c(author.institution[which.author.use], 
                                                  author.city[which.author.use], 
                                                  author.country[which.author.use])]
                            datos <- sapply(datos, as.character)
                            datos <- paste(datos[datos != ""], collapse = ", ")
                            tmp <- paste0(tmp, "\\Afilliation{$^", j, "$", datos, "}\n")
                            # tmp <- paste(tmp, "\\Afilliation{$^", j, 
                            #              "$", ifelse(my.data[i, author.institution[which.author.use]] != 
                            #                              "", paste(my.data[i, author.institution[which.author.use]], 
                            #                                        ", ", sep = ""), ""), ifelse(my.data[i, 
                            #                                                                             author.city[which.author.use]] != "", 
                            #                                                                     paste(my.data[i, author.city[which.author.use]], 
                            #                                                                           ", ", sep = ""), ""), ifelse(my.data[i, 
                            #                                                                                                                author.country[which.author.use]] != 
                            #                                                                                                            "", paste(my.data[i, author.country[which.author.use]]), 
                            #                                                                                                        ""), "}\n", sep = "")
                        }
                    }
                }
                tmp.mails <- ""
                if (!is.na(my.data[i, author.email[1]]))
                    tmp.mails <- paste0(tmp.mails, "\\Email{", my.data[i, author.email[1]], "}")
                # tmp.mails = paste("\\Email{", ifelse(!is.na(my.data[i, 
                #                                                     author.email[1]]), paste(my.data[i, author.email[1]]), 
                #                                      ""), "}", sep = "")
                for (j in 2:num.authors[i]) 
                    if (!is.na(my.data[i, author.email[j]]))
                        tmp.mails <- paste0(tmp.mails, " \\Email{", my.data[i, author.email[j]], "}")
                    # tmp.mails = paste(tmp.mails, 
                    #                                           paste("\\Email{", ifelse(!is.na(my.data[i, 
                    #                                                                                   author.email[j]]), paste(my.data[i, author.email[j]]), 
                    #                                                                    ""), "}", sep = ""), sep = ", ")
            }
            tmp = paste(tmp, tmp.mails, sep = "\\\\") # Marcos: le cambie el sep "n" a "\\\\" porque me convenia para usarlo en latex
            affiliazioni[i] <- paste("{", tmp, "}", sep = "")
            abstract[i] <- paste("{", my.data[i, pres.abstract], 
                                 "}", sep = "")
            # Marcos
            # palabras claves en palabrasClaves
            # campo de aplic en topic1
            # categoria metod en topic2
            # id en id
            # Elimino el resto de las notas
            # temp <- paste0("{", my.data[i, topic1], "}")
            temp <- paste0("{", my.data[i, palabrasClaves], 
                           "} \n {", my.data[i, topic1],
                           "} \n {", my.data[i, topic2],
                           "} \n {", my.data[i, id],
                           "} \n {", my.data[i, idBase], "}")
            # comento lo que sigue
            # if (noNotes == F) 
            #     temp <- paste("{Topic1: ", my.data[i, topic1], 
            #                   ", Topic2: ", my.data[i, topic2], ". Abstract ID: ", 
            #                   my.data[i, id], ". Accepted: ", my.data[i, 
            #                                                           accept], ". Notes: ", my.data[i, notes], 
            #                   ". Ref1: ", my.data[i, ref2], ". Ref2: ", my.data[i, 
            #                                                                     ref3], ". Ref3: ", my.data[i, ref4], "}", 
            #                   sep = "")
            # else temp <- paste("{Abstract ID: ", my.data[i, id], 
            #                    ". Topic1: ", my.data[i, topic1], ", Topic2: ", 
            #                    my.data[i, topic2], ".}", sep = "")
            
            zz <- file(paste(my.data[i, id], ".tex", sep = ""), 
                       "w")
            cat("\\A", titolo[i], autori[i], affiliazioni[i], 
                temp, abstract[i], sep = "\n", file = zz)
            close(zz)
        }
    }
    else {
        for (i in 1:dim(my.data)[1]) {
            if (verbose == TRUE) 
                cat("ID=", i)
            titolo[i] <- paste("{", my.data[i, pres.title], "}", 
                               sep = "")
            TMP <- strsplit(as.character(my.data[, author.firstname[1]]), 
                            c(" "))
            number.of.names <- unlist(lapply(TMP, length))
            authors.initials <- toupper(strsplit(TMP[[i]][1], 
                                                 "")[[1]][1])
            if (number.of.names[i] > 1) 
                for (j in 2:number.of.names[i]) authors.initials <- paste(authors.initials, 
                                                                          toupper(strsplit(TMP[[i]][j], "")[[1]][1]), 
                                                                          sep = "")
            one.inst.true <- length(unique(unlist((my.data[i, 
                                                           author.institution[1:num.authors[i]]])))) == 
                1 & length(unique(unlist((my.data[i, author.city[1:num.authors[i]]])))) == 
                1
            if (num.authors[i] == 1) {
                tmp <- paste(my.data[i, author.firstname[1]], 
                             " ", my.data[i, author.lastname[1]], generbook:::.fun.get.index.slo(my.data[i, 
                                                                                             author.lastname[1]], authors.initials), sep = "")
            }
            else {
                if (presenting.author[i] == 1) 
                    tmp <- paste("\\Presenting{", my.data[i, author.firstname[1]], 
                                 " ", my.data[i, author.lastname[1]], "}", 
                                 "$^1$", generbook:::.fun.get.index.slo(my.data[i, author.lastname[1]], 
                                                            authors.initials), sep = "")
                else tmp <- paste(my.data[i, author.firstname[1]], 
                                  " ", my.data[i, author.lastname[1]], "$^1$", 
                                  generbook:::.fun.get.index.slo(my.data[i, author.lastname[1]], 
                                                     authors.initials), sep = "")
                if (num.authors[i] > 1) 
                    for (j in 2:num.authors[i]) {
                        TMP <- strsplit(as.character(my.data[, author.firstname[j]]), 
                                        c(" "))
                        number.of.names <- unlist(lapply(TMP, length))
                        authors.initials <- toupper(strsplit(TMP[[i]][1], 
                                                             "")[[1]][1])
                        if (number.of.names[i] > 1) 
                            for (jj in 2:number.of.names[i]) authors.initials <- paste(authors.initials, 
                                                                                       toupper(strsplit(TMP[[i]][jj], "")[[1]][1]), 
                                                                                       sep = "")
                        if (j < num.authors[i]) {
                            if (presenting.author[i] == j) 
                                tmp <- paste(tmp, ", \\Presenting{", 
                                             paste(my.data[i, author.firstname[j]], 
                                                   " ", my.data[i, author.lastname[j]], 
                                                   "}$^", j, "$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                             author.lastname[j]], authors.initials), 
                                                   sep = ""))
                            else tmp <- paste(tmp, paste(my.data[i, 
                                                                 author.firstname[j]], " ", my.data[i, 
                                                                                                    author.lastname[j]], "$^", j, "$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                                                                                  author.lastname[j]], authors.initials), 
                                                         sep = ""), sep = ", ")
                        }
                        else {
                            if (presenting.author[i] == j) 
                                tmp <- paste(tmp, "y \\Presenting{", 
                                             paste(my.data[i, author.firstname[j]], 
                                                   " ", my.data[i, author.lastname[j]], 
                                                   "}$^", j, "$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                             author.lastname[j]], authors.initials), 
                                                   sep = ""), sep = "")
                            else tmp <- paste(tmp, paste(my.data[i, 
                                                                 author.firstname[j]], " ", my.data[i, 
                                                                                                    author.lastname[j]], "$^", j, "$", generbook:::.fun.get.index.slo(my.data[i, 
                                                                                                                                                                  author.lastname[j]], authors.initials), 
                                                         sep = ""), sep = " y ")
                        }
                    }
            }
            autori[i] <- paste("{", tmp, "}", sep = "")
            if (num.authors[i] == 1) {
                tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                                                              author.institution[1]] != "", paste(my.data[i,
                                                                                                          author.institution[1]], ", ", sep = ""), ""),
                             ifelse(my.data[i, author.city[1]] != "", paste(my.data[i,
                                                                                    author.city[1]], ", ", sep = ""), ""), ifelse(my.data[i,
                                                                                                                                          author.country[1]] != "", paste(my.data[i,
                                                                                                                                                                                  author.country[1]]), ""), "}; \\Email{",
                             ifelse(!is.na(my.data[i, author.email[1]]),
                                    paste(my.data[i, author.email[1]]), ""),
                             "}\n", sep = "")
            }
            else {
                if (one.inst.true) {
                    tmp <- paste("\\Afilliation{", ifelse(my.data[i,
                                                                  author.institution[1]] != "", paste(my.data[i,
                                                                                                              author.institution[1]], ", ", sep = ""),
                                                          ""), ifelse(my.data[i, author.city[1]] !=
                                                                          "", paste(my.data[i, author.city[1]], ", ",
                                                                                    sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                                                                                                               "", paste(my.data[i, author.country[1]]),
                                                                                                           ""), "}\n\n\t\t\t\t\t$^1$\\Email{", ifelse(!is.na(my.data[i,
                                                                                                                                                                     author.email[1]]), paste(my.data[i, author.email[1]]),
                                                                                                                                                      ""), "}\n", sep = "")
                    for (j in 2:num.authors[i]) {
                        tmp <- paste(tmp, "$^", j, "$\\Email{", ifelse(!is.na(my.data[i,
                                                                                      author.email[j]]), paste(my.data[i, author.email[j]]),
                                                                       ""), "}\n", sep = "")
                    }
                }
                else {
                    tmp <- paste("\\Afilliation{$^", 1, "$", ifelse(my.data[i,
                                                                            author.institution[1]] != "", paste(my.data[i,
                                                                                                                        author.institution[1]], ", ", sep = ""),
                                                                    ""), ifelse(my.data[i, author.city[1]] !=
                                                                                    "", paste(my.data[i, author.city[1]], ", ",
                                                                                              sep = ""), ""), ifelse(my.data[i, author.country[1]] !=
                                                                                                                         "", paste(my.data[i, author.country[1]]),
                                                                                                                     ""), "}; \\Email{", ifelse(!is.na(my.data[i,
                                                                                                                                                               author.email[1]]), paste(my.data[i, author.email[1]]),
                                                                                                                                                ""), "}\n", sep = "")
                    for (j in 2:num.authors[i]) {
                        tmp <- paste(tmp, "\\Afilliation{$^", j,
                                     "$", ifelse(my.data[i, author.institution[j]] !=
                                                     "", paste(my.data[i, author.institution[j]],
                                                               ", ", sep = ""), ""), ifelse(my.data[i,
                                                                                                    author.city[j]] != "", paste(my.data[i,
                                                                                                                                         author.city[j]], ", ", sep = ""), ""),
                                     ifelse(my.data[i, author.country[j]] !=
                                                "", paste(my.data[i, author.country[j]]),
                                            ""), "}; \\Email{", ifelse(!is.na(my.data[i,
                                                                                      author.email[j]]), paste(my.data[i, author.email[j]]),
                                                                       ""), "}\n", sep = "")
                    }
                }
            }
            affiliazioni[i] <- paste("{", tmp, "}", sep = "")
            abstract[i] <- paste("{", my.data[i, pres.abstract], 
                                 "}", sep = "")
            
            # Marcos
            # palabras claves en palabrasClaves
            # campo de aplic en topic1
            # categoria metod en topic2
            # id en id
            # Elimino el resto de las notas
            # temp <- paste0("{", my.data[i, topic1], "}")
            temp <- paste0("{", my.data[i, palabrasClaves], 
                           "} \n {", my.data[i, topic1],
                           "} \n {", my.data[i, topic2],
                           "} \n {", my.data[i, id],
                           "} \n {", my.data[i, idBase], "}")
            # comento lo que sigue
            # if (noNotes == F) 
            #     temp <- paste("{Topic1: ", my.data[i, topic1], 
            #                   ", Topic2: ", my.data[i, topic2], ". Abstract ID: ", 
            #                   my.data[i, id], ". Accepted: ", my.data[i, 
            #                                                           accept], ". Notes: ", my.data[i, notes], 
            #                   ". Ref1: ", my.data[i, ref2], ". Ref2: ", my.data[i, 
            #                                                                     ref3], ". Ref3: ", my.data[i, ref4], "}", 
            #                   sep = "")
            # else temp <- paste("{Abstract ID: ", my.data[i, id], 
            #                    ". Topic1: ", my.data[i, topic1], ", Topic2: ", 
            #                    my.data[i, topic2], ".}", sep = "")
            zz <- file(paste(my.data[i, id], ".tex", sep = ""), 
                       "w")
            cat("\\A", titolo[i], autori[i], affiliazioni[i], 
                temp, abstract[i], sep = "\n", file = zz)
            close(zz)
        }
    }
    zz <- file(paste("abstractList.tex", sep = ""), "w")
    for (i in my.data[, id]) cat("\\input{", i, ".tex}\n", sep = "", 
                                 file = zz)
    close(zz)
    zz <- file(paste("abstractListAccepted.tex", sep = ""), "w")
    for (i in my.data[my.data[, accept] == "Yes", id]) cat("\\input{", 
                                                           i, ".tex}\n", sep = "", file = zz)
    close(zz)
    all.topics <- sort(unique(levels(my.data[, topic1]), levels(my.data[, 
                                                                        topic2])))
    all.topics <- all.topics[all.topics != "---" | all.topics == 
                                 ""]
    zz <- file("abstractListByTopic.tex", "w")
    for (ii in 1:length(all.topics)) {
        which.abstracts = unique(which(my.data[, topic1] == all.topics[ii] | 
                                           my.data[, topic1] == all.topics[ii]))
        cat("{\\bf \\Large ", as.character(all.topics[ii]), "}\\\\\\\\", 
            sep = " ", file = zz)
        for (i in which.abstracts) cat(titolo[i], "\\\\", autori[i], 
                                       "\\\\", affiliazioni[i], paste(" ID=", my.data[i, 
                                                                                      id], "; Day=", my.data[i, notesDay], "Payment=", 
                                                                      my.data[i, notesPayment], "\\\\ Topics={\\small ", 
                                                                      as.character(my.data[i, topic1]), as.character(my.data[i, 
                                                                                                                             topic2]), "} \\\\\\\\", sep = " "), sep = " ", 
                                       file = zz)
        cat("\\\\ \\clearpage", file = zz)
    }
    close(zz)
    my.data.accepted = my.data[my.data[, accept] == "Yes", ]
    all.topics = sort(unique(levels(my.data.accepted[, topic1]), 
                             levels(my.data.accepted[, topic2])))
    all.topics = all.topics[all.topics != "---" | all.topics == 
                                ""]
    zz <- file("abstractListByTopicAccepted.tex", "w")
    for (ii in 1:length(all.topics)) {
        which.abstracts = unique(which(my.data.accepted[, topic1] == 
                                           all.topics[ii] | my.data.accepted[, topic1] == all.topics[ii]))
        cat("{\\bf \\Large ", as.character(all.topics[ii]), "}\\\\\\\\", 
            sep = " ", file = zz)
        for (i in which.abstracts) cat(titolo[my.data[, accept] == 
                                                  "Yes"][i], "\\\\", autori[my.data[, accept] == "Yes"][i], 
                                       "\\\\", affiliazioni[my.data[, accept] == "Yes"][i], 
                                       paste(" ID=", my.data.accepted[i, id], "; Day=", 
                                             my.data.accepted[i, notesDay], "Payment=", my.data.accepted[i, 
                                                                                                         notesPayment], "\\\\ Topics={\\small ", as.character(my.data.accepted[i, 
                                                                                                                                                                               topic1]), as.character(my.data.accepted[i, 
                                                                                                                                                                                                                       topic2]), "} \\\\\\\\", sep = " "), sep = " ", 
                                       file = zz)
        cat("\\\\ \\clearpage", file = zz)
    }
    close(zz)
    setwd(initial.wd)
    return(list(My.data = my.data, PA = presenting.author))
}


generate.programOverview_Marcos <-function(my.program.file, dirAbstracts, abstract.id=c(10:14)){
    #my.program.file: name of the tab-delimited file where the program is saved
    #dirAbstracts: directory where the abstracts will be stored, it must exist
    #abstract.id: colums where the abstracts to be included in each session are reported
    
    init.wd=getwd()
    
    #read the program
    my.program<-read.delim(my.program.file, sep="\t")
    my.program<-my.program[!is.na(my.program[,1]) & my.program[,1]!="",]
    
    #number of rows included in the program
    num.rows<-dim(my.program)[1]
    
    setwd(dirAbstracts)
    
    #obtain ordered program
    my.order<-order(my.program$Day*100+my.program$TimeBegin)
    my.program<-my.program[my.order,]
    
    #number of different days
    number.days<-unique(my.program$Day)
    
    #number of rooms 
    room.names<-unique(my.program$Room[my.program$Room!=""])
    number.rooms<-length(room.names)
    
    #times
    my.time<-my.program$Day*100+my.program$TimeBegin
    
    
    #number of abstracts per session
    number.abstracts<-as.numeric(apply(my.program[,abstract.id], 1, function(x) sum(!is.na(x))))
    
    
    
    #which session names have to be split because they are too long, max 25 characters per line, therefore max lenght is 50 characters
    which.split<-ifelse(nchar(as.character(my.program$Name))>=25, 1, 0)
    # Marcos elimino esto porque genera un error. Si queda largo el nombre de la sesion lo acomodo manualmente
    # which.split <- rep(0, length(which.split))
    
    #added an extra element at the end, that will be used for session at times where not all the rooms have a session
    where.split<-rep(NA, num.rows+1)
    names.split<-vector("list", num.rows+1)
    
    names.split[[num.rows+1]]<-""
    where.split[num.rows+1]<-2
    
    #same thing for the names of the session
    my.program.name<-c(as.character(my.program$Name), "")
    
    
    #point where names must be split
    for(i in c(1:num.rows)[which.split==1]){
        names.split[[i]]<-tmp<-strsplit(as.character(my.program$Name[i]), split=" ")
        where.split[i]<-which(cumsum(unlist(lapply(tmp, nchar)))>=25)[1]
        #if the last word is "too" long
        if(cumsum(unlist(lapply(tmp, nchar)))[length(tmp[[1]])]<=25) where.split[i]<-length(tmp[[1]])
    }
    
    #modified sept1/2010: to fix a bug that precluded the printing in the program overview of session where one had to be split and the other hadn't 
    for(i in c(1:num.rows)[which.split==0]){
        names.split[[i]]<-tmp<-as.character(my.program$Name[i])
        #where.split[i]<-which(cumsum(unlist(lapply(tmp, nchar)))>=25)[1]
        #if the last word is "too" long
        #if(cumsum(unlist(lapply(tmp, nchar)))[length(tmp[[1]])]<=25) where.split[i]<-length(tmp[[1]])
    }
    
    
    
    
    
    
    #deriving where.split for the names that do not need to be split
    for(i in c(1:num.rows)[which.split==0]){
        tmp<-strsplit(as.character(my.program$Name[i]), split=" ")
        where.split[i]<-length(tmp)[[1]]+1
    }
    
    #added the last element as for where.splot, names.split, etc
    which.split<-c(which.split, 0)
    
    #paste(unlist(tmp)[1:(where.split[i]-1)], concatenate=" ")
    
    
    #indicator for avoiding lines that were already included in the table - same time, different room
    my.done<-rep(FALSE, num.rows)
    
    zz<-file("programOverview.tex", "w")
    
    cat("\\noindent\\\\
        \\thispagestyle{empty}
        \\begin{center}
        \\Large
        % \\textbf{Program} \\\\ [0.5cm]
        \\begin{flushright}
        \\vspace{17cm} {\\Huge \\em{ \\textbf{PROGRAMA}}} \\\\ [0.5cm]
        \\end{flushright}
        \\normalsize
        \\end{center}
        %\\noindent  \\hrulefill \\\\[0.5cm]
        \\small
        \\clearpage
        \\pagestyle{fancy}
        \\renewcommand{\\Date}{}
        %%\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\} #marcos le saque esto
        %% ------------------------------------------- Session start
        \\renewcommand{\\Session}{}
        \\SetHeader{}{\\textbf{Program Overview}}
        %%--------------------------------------------
        \\vspace*{-1.0cm}
        \\begin{center} %end of the first cat, header of the file and beginning of the table
        \\begin{tabular}{|l|| l |", file=zz) 
    cat(rep("c", number.rooms, sep=" "), "|}\\hline", file=zz)
    cat("&&", paste(room.names, collapse="&"), "\\\\\\hline\\hline\n", file=zz) #end cat
    
    #counter within the day
    day.within<-0
    #indicator for end of the day reached with the previous wirtten record
    day.changed<-FALSE
    
    for(i in 1:num.rows){
        
        cat(i, "\n")
        
        #checking if this record was already included
        if(my.done[i]==FALSE){
            
            #check if there is a change of day
            if(i>1) { if(my.program$DayTable[i]!=my.program$DayTable[i-1]) day.within<-0
            #check if there was a a change in the day in the next record, and write the line uder the previous record, type of line depends on the change of the day or not
            if(day.changed==T) my.line<-"\\\\\\hline\\hline" else my.line<-paste("\\\\\\cline{2-", number.rooms+2, "}")
            cat(my.line, "\n",	file=zz)
            } 
            
            #calculate time of the session
            
            if(my.program$TimeBegin[i]>10) time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:5] else time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:4]
            if(!is.na(my.program$TimeEnd[i])) {if(my.program$TimeEnd[i]>10) time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:5] else 	time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:4]} else time.end<-NA
            
            
            
            #writing the date at the first entry for the day
            if(day.within==0) tmp.day<-as.character(my.program$DayTable[i]) else tmp.day<-""
            day.within<-day.within+1
            
            
            
            #are there any other sessions at the same time and which
            which.same.time<-which(my.time[i]==my.time)
            
            #common session, without any room specified, like breaks, llunch, etc, it is centered in the table
            if(my.program$Room[i]=="") {tmp.session<-paste("\\multicolumn{", number.rooms, "}{c|}{\\cellcolor[gray]{0.9}", my.program$Name[i], "}")
            #tmp.time<-paste(paste(time.begin, collapse=""), " -- ", paste(time.end, collapse=""))
            tmp.time<-paste(paste(time.begin, collapse=""), ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, collapse="")), ""), sep="") 
            
            
            cat(paste(tmp.day, tmp.time, tmp.session, sep="&"), file=zz)
            if(i<num.rows) day.changed<-ifelse(my.program$Day[i]!=my.program$Day[i+1], TRUE, FALSE) 
            #cat("\\\\\\cline{2-", number.rooms+2, "} \n", file=zz)
            
            #
            ##### try Thr } else {if(length(which.same.time)>1)    {
            } else  {# not a session that has no room definition
                ###} else {
                which.room<-vector("list", number.rooms) 
                which.index<-numeric(number.rooms)
                for(ii in 1:number.rooms){
                    tmp<-which(my.program$Room==room.names[ii])	
                    #INDEX FOR ROOM I AT THIS TIME
                    iii<-tmp[is.element(tmp, which.same.time)]
                    my.done[iii]<-TRUE
                    #indexes to use for this time, ordered, if not all the rooms have a session a reference to an empty line is added
                    which.index[ii]<-ifelse(length(iii)==1, iii, num.rows+1)
                    #indicator of the latest used record - records should be ordered?
                    my.max<-max(which.index[!is.element(which.index, num.rows+1)])
                    
                    if(i<num.rows) day.changed<-ifelse(my.program$Day[i]!=my.program$Day[my.max+1], TRUE, FALSE)
                }#end for ii
                
                #if there is the need to split the names of the sessions		
               
                if(any(which.split[which.index]==1)){
                    #first part of name of the session - split beacuse they are too long
                    tmp.session<-paste(paste(unlist(lapply(which.index, function(j) paste(unlist(names.split[[j]])[1:(where.split[j]-1)], collapse=" "))), collapse="&"), "\\\\")
                    #tmp.session.2<-paste(paste(unlist(lapply(which.index, function(j) paste(unlist(names.split[[j]])[-c(1:(where.split[j]-1))], collapse=" "))), collapse="&"), "\\\\\\cline{2-", number.rooms+2, "}", sep="")
                    tmp.session.2<-paste(paste(unlist(lapply(which.index, function(j) paste(unlist(names.split[[j]])[-c(1:(where.split[j]-1))], collapse=" "))), collapse="&"),  sep="")
                    tmp.time<-paste("\\multirow{2}{*}{", paste(time.begin, collapse=""),
                                    ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, collapse=""), "}"), "}"), sep="")



                    cat(paste(tmp.day, tmp.time, tmp.session, sep="&"), file=zz)
                    cat("\n", file=zz)

                    cat(paste("", "", tmp.session.2, sep="&"), file=zz)
                    cat("\n", file=zz)

                    #no need to split the name of any of the sessions
                    #			} else	{tmp.session<-paste(paste(my.program$Name[which.index], collapse="&"), "\\\\", sep="")
                    #} else	{tmp.session<-paste(paste(my.program.name[which.index], collapse="&"), "\\\\", sep="")
                } else	{
                    tmp.session<-paste(paste(my.program.name[which.index], collapse="&"), sep="")
                    #tmp.time<-paste(paste(time.begin, collapse=""), " -- ", paste(time.end, collapse=""))
                    tmp.time<-paste(paste(time.begin, collapse=""), ifelse(!is.na(time.end[1]), paste(" --", paste(time.end, collapse="")), ""), sep="") 
                    
                    cat(paste(tmp.day, tmp.time, tmp.session, sep="&"), file=zz)
                    my.max<-max(which.index[!is.element(which.index, num.rows+1)])
                    
                    if(i<num.rows) day.changed<-ifelse(my.program$Day[i]!=my.program$Day[my.max+1], TRUE, FALSE)
                    
                 }#end for ii
                
                
                #cat(my.line, file=zz)
                
                cat("\n", file=zz)
                
            } # end else {tmp.session...}
            
            #}#end for ii	
            #end else which.same.time>1
            #} else {
            
            
            
            #}#end else which.same.time 1
            
            
            #end else which.same.time
            
        }# end if my.done
        
        
    }#end for i
    
    
    cat("\\\\\\hline", "\n" , "\\end{tabular} \\end{center}
        \\clearpage", file=zz)
    
    close(zz)
    
    #restoring initial working directory
    setwd(init.wd)
    
        }#########################end generate program overview#############################


generate.program_Marcos <-function(my.program.file, my.filename, dirAbstracts, abstract.id=c(10:14), author.lastname=seq(23, by=6, length.out=7),  author.firstname=seq(24, by=6, length.out=7),  accept=17, pres.title=18, id=75, outfile.name="program.tex")
{
    #my.program.file: name of the tab-delimited file where the program is saved
    #my.filename: name of the file with the abstract database
    #dirAbstracts: directory where the abstracts will be stored, it must exist
    #abstract.id: colums where the abstracts to be included in each session are reported
    #author.lastname: columns that contain the last name of the authors
    #author.first: columns that contain the first name of the authors
    #accept: column that contains the acceptance decision, must be " Yes" for the acceptance of the abstract
    #pres.title: column that contains the title of the presentation
    #id: abstract id column in the database of abstracts
    
    #save initial working directory
    init.wd=getwd()
    
    #read the program
    my.program<-read.delim(my.program.file, sep="\t", encoding = "UTF-8") # le agregue encoding para que no me haga lio con los acentos
    my.program<-my.program[!is.na(my.program[,1]) & my.program[,1]!="",]
    
    #number of rows included in the program
    num.rows<-dim(my.program)[1]
    
    
    #obtain ordered program
    my.order<-order(my.program$Day*100+my.program$TimeBegin)
    my.program<-my.program[my.order,]
    
    #number of different days
    number.days<-unique(my.program$Day)
    
    
    #times
    my.time<-my.program$Day*100+my.program$TimeBegin
    
    
    
    num.rows<-dim(my.program)[1]
    
    #read data from abstracts
    my.data<-read.delim(my.filename, sep="\t")
    #remove the empty records
    #calculate the number of authors for each abstract
    num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
    my.data<-my.data[num.authors!=0,]
    #remove duplicated records and rejected papers, accepted paper must be indicated with "Yes" in the accept column
    my.data<-my.data[my.data[,accept]=="Yes",]
    #recalculate the number of authors
    num.authors<-apply(my.data[,author.lastname], 1, function(x) sum(!is.na(x) & x!=""))
    
    #number of abstracts per session
    number.abstracts<-as.numeric(apply(my.program[,abstract.id], 1, function(x) sum(!is.na(x))))
    
    setwd(dirAbstracts)
    
    
    #zz<-file("program.tex", "w")
    zz<-file(outfile.name, "w")
    
    cat("\\clearpage \n \\pagestyle{fancy}
        % ------------------------------------------------- Next day -----
        \\renewcommand{\\Date}{}
        %%\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\} # marcos le saque esto
        %% ------------------------------------------- Session start
        \\renewcommand{\\Session}{}
        \\SetHeader{\\textbf{", as.character(my.program$DayLong[1]), 
        "}}{}", "\n", file=zz, sep="")
    
    
    
    
    
    for(i in 1:num.rows){
        #problems with too few decimal numbers.... trick to avoid 10.3 instead of 10.30 (input: 10.301)
        cat(i, "\n")
        
        #additional problem with hours starting begore 10, less figures!
        if(my.program$TimeBegin[i]>10) time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:5] else time.begin<-strsplit(as.character(my.program$TimeBegin[i]), "")[[1]][1:4]
        if(!is.na(my.program$TimeEnd[i])) {if(my.program$TimeEnd[i]>10) time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:5] else 	time.end<-strsplit(as.character(my.program$TimeEnd[i]), "")[[1]][1:4]} else time.end<-NA
        #write time
        cat("\\PrSectionHeader{", time.begin, sep="", file=zz)
        if(!is.na(time.end[1])) cat("--", time.end, "}", sep="", file=zz)  else cat("}", file=zz, sep="")
        
        #wirte session name
        cat("{", as.character(my.program$Name[i]), "}", file=zz, sep="")
        
        #wirte Hall
        if(my.program$Room[i]!="") cat("{(", as.character(my.program$Room[i]), ")}", file=zz, sep="") else cat("{}", file=zz, sep="")
        
        #write chair
        if(my.program$Chair[i]!="") cat("{Chair: ", as.character(my.program$Chair[i]), "}", file=zz, sep="") else cat("{}", file=zz, sep="")
        
        cat("\n", file=zz)
        
        
        ###############abstrac titles and authors###############
        
        if(number.abstracts[i]>0){
            cat("\\begin{enumerate}\n", file=zz)
            
            for(ii in 1:number.abstracts[i]){
                
                cat(ii, " ")
                
                my.index<- which(my.data[,id]==my.program[i,abstract.id[ii]])
                
                tmp<-paste(my.data[my.index, author.firstname[1] ], " ", my.data[my.index, author.lastname[1] ], sep="")
                if(num.authors[my.index]>1)	for(j in 2:num.authors[my.index]){
                    if(j<num.authors[my.index]) 	tmp<-paste(tmp, paste(my.data[my.index, author.firstname[j] ], " ", my.data[my.index, 		author.lastname[j] ], sep=""), sep=", ") else tmp<-paste(tmp, paste(my.data[my.index, author.firstname[j] ], " ", my.data[my.index, author.lastname[j] ], sep=""), sep=" y ") }
                
                autori<-tmp
                
                #	cat("\\PrTalk{",  as.character(my.data[my.index, pres.title]), "} {", autori,  "}", sep="", file=zz)
                #modified sept2010, the names of the authors in the program start at a new line
                cat("\\PrTalk{",  as.character(my.data[my.index, pres.title]), "} \\newline {", autori,  "}", sep="", file=zz)
            }#end for ii 
            cat("\\end{enumerate}\n", file=zz)
        } #end if number.abstracts>0
        
        
        
        
        #check if the Day changes
        if(i!=num.rows){
            if(my.program$Day[i+1]!=my.program$Day[i]){
                cat("\\clearpage \n \\pagestyle{fancy}
                    % ------------------------------------------------- Next day -----
                    \\renewcommand{\\Date}{}
                    %\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\} #marcos le saque esto
                    %% ------------------------------------------- Session start
                    \\renewcommand{\\Session}{}
                    \\SetHeader{\\textbf{", as.character(my.program$DayLong[i+1]), 
                    "}}{}", "\n", file=zz, sep="")
                    }
        }
        
        }#end for i
    
    close(zz)
    
    
    
    #######################generate abstract list######################
    
    ###########Generate abstract lists######################
    
    
    #sessionw with at least 1 abstract
    session.id<- which(number.abstracts>0)
    
    zz<-file("abstracts.tex", "w")
    
    
    cat("\\noindent\\\\
        \\thispagestyle{empty}
        \\begin{center}
        \\Large
        % \\textbf{Program} \\\\ [0.5cm]
        \\begin{flushright}
        \\vspace{17cm} {\\Huge \\em{ \\textbf{RESÚMENES}}} \\\\ [0.5cm]
        \\end{flushright}
        \\normalsize
        \\end{center}
        %\\noindent  \\hrulefill \\\\[0.5cm]
        \\small
        \\clearpage
        \\pagestyle{fancy}
        \\renewcommand{\\Date}{}
        %\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}
        %% ------------------------------------------- Session start
        \\renewcommand{\\Session}{}
        \\SetHeader{}{\\textbf{Program Overview}}
        %%--------------------------------------------
        %\\begin{center}
        %  \\Large
        %   \\textbf{Abstracts} \\\\ [0.5cm]
        %  \\normalsize
        %  \\begin{flushright}
        %   \\vspace{17cm} {\\Huge \\em{ \\textbf{ABSTRACTS}}} \\\\ [0.5cm]
        %   \\end{flushright}
        %   \\normalsize
        
        
        % \\end{center}
        %\\noindent\\  %\\hrulefill \\\\
        \\small
        \\clearpage 
        \\pagestyle{fancy}
        \\renewcommand{\\Date}{", 
        as.character(my.program$DayShort[session.id[1]]), 
        "}
        %\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}
        \\renewcommand{\\Session}{", as.character(my.program$Name[session.id[1]]), "}
        \\Section{\\Session}
        \\SetHeader{Resúmenes}{\\textit{Congreso Interamericano de Estadística}}", file=zz, sep="")
    
    
    
    
    for(i in 1:length(session.id)){
        #problems with too few decimal numbers.... trick to avoid 10.3 instead of 10.30 (input: 10.301)
        cat(i, "\n")
        
        for(j in 1:number.abstracts[session.id[i]]){
            cat("\\input{", my.program[session.id[i],abstract.id[j]], ".tex}\n\\makebox[\\linewidth]{\\dashdotted}\\par\n", file=zz, sep="")
        }#end for j
        # cat("\\clearpage\n", file=zz) #le saco esto para que no salte de pagina al cambiar de sesion
        
        #not at the end
        if(i!=length(session.id)){
            # cat("\\renewcommand{\\Session}{", as.character(my.program$Name[session.id[i+1]]), "}
            #     \\Section{\\Session}
            #     \\SetHeader{\\Date}{\\Session}", file=zz, sep="")
            # esto lo cambio para que todas las sesiones tengan siempre los mismos encabezados de pagina
            cat("\\renewcommand{\\Session}{", as.character(my.program$Name[session.id[i+1]]), "} \\Section{\\Session}", file=zz, sep="")
            
            if(my.program$Day[session.id[i]]!=my.program$Day[session.id[i+1]]) 
                cat("\\renewcommand{\\Date}{", 
                    as.character(my.program$DayShort[session.id[i+1]]), 
                    "}
                    %\\addtocontents{toc}{\\hfill\\textbf{\\Date}\\\\}", file=zz, sep="") 
            
        }#end if i!=session.id
        
    }#end for i
    
    close(zz)
    
    #restore initial working directory
    setwd(init.wd)
    
    ###########End Generate abstract lists######################
    }###################end generate program###########################
