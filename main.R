# library(devtools)
# install_github("generbook", "llaarraa")
library(generbook)
library(readr)
library(dplyr)
library(readxl)
library(stringi)


pathwd <- "~/Downloads/LibroResumenes/"
pathwd <- "C://Users/marco/Google Drive/CIE2017/LibroResumenes/Prueba4/"
tag <- "ArchivosGeneradosAutomaticamente/"
setwd(pathwd)
source(paste0(pathwd, "funciones.R"))

# Importante sobre encoding en windows
# R genera los archivos con "native.enc" encoding
# Esto genera que el archivo styleAndData.tex quede con encoding ANSI y los acentos (por ej ResÃºmenes) no son reconocidos en Latex
# Opcion 1: cambiar el encoding default de R para que use UTF-8 al hacer los archivos
# Esto hace que la funcion generate.abstracts ya no pueda leer los archivos de input
# Opcion 2: modificar la funcion para que guarde archivos en UTF8, pero lo mismo mucho trabajo
# Opcion 3: una vez que se crearon los archivos, agarrar el styleAndData.tex, abrirlo con NotePad y guardarlo como UTF8
# getOption("encoding")
# options(encoding = "UTF-8")
# options(encoding = "native.enc")

#################
# PASO 1: PREPARAR LA BASE CON LOS DATOS DE LOS TRABAJOS
#################

# Leer datos.
# contr <- read_delim("C:/Users/marco/Google Drive/CIE2017/LibroResumenes/Prueba3/contr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)
contr <- read_excel(paste0(pathwd, "trabajos_final-27-9_marcos.xlsx"))
#str(contr)

# Identificar idx de cols con nombre y afiliaciones de autores
autorNombreIdx <- grep("AutorNombre", colnames(contr))
autorApellidoIdx <- grep("AutorApellido", colnames(contr))
autorAfiliacionIdx <- grep("AutorFiliacion", colnames(contr))

# Unir palabras claves en un unico campo
contr$PalabrasClaves <- apply(contr[, paste0("Palabra", 1:6)], 1, function(x) {
    x <- x[!is.na(x)]
    paste0(x, collapse = "; ")
})

# Pasar a minusculas los emails y las palabras claves
contr$Email <- tolower(contr$Email)
contr$PalabrasClaves <- tolower(contr$PalabrasClaves)

# Pasar a mayusculas titulo, autores y afiliaciones
contr$Descripcion <- toupper(contr$Descripcion)
for (i in 1:nrow(contr)) {
    contr[i, autorNombreIdx] <- toupper(contr[i, autorNombreIdx])
    contr[i, autorApellidoIdx] <- toupper(contr[i, autorApellidoIdx])
    contr[i, autorAfiliacionIdx] <- toupper(contr[i, autorAfiliacionIdx])
}

# Crear ID
contr$ID <- paste(contr$`Cod Persona`, contr$`Numero de Trabajo`, sep = "-")

# maximo numero de autores
maxCantAut <- max(contr$Cantautores)

# Necesito una columna para el primer nombre de cada autor (que en este borrador no lo tengo)
# ya no
# contr[paste0("AutorPrimerNombre", 1:maxCantAut)] <- ""

# Se necesita una columna de email, para cada autor
contr[paste0("Email", 2:maxCantAut)] <- ""
# contr[paste0("Ciudad", 1:maxCantAut)] <- ""
# contr[paste0("Pais", 1:maxCantAut)] <- ""

# Necesito una columna T/F para indicar los autores que presentan
contr[paste0("AutorPresenta", 1:maxCantAut)] <- ""
contr[, "AutorPresenta1"] <- TRUE

# agregar una columna NA para cosas que pide pero no usamos
contr$nada <- ""

# agregar otras columnas que pide pero no usamos
contr$accept <- "Yes"

# agregar una columna de ids numerica para que pueda usar yo
contr$IDmarcos <- 1:nrow(contr)

# Identificar idx de cols con nombre y afiliaciones de autores
autorApellidoIdx <- grep("AutorApellido", colnames(contr))
autorNombreIdx <- grep("AutorNombre", colnames(contr))
autorAfiliacionIdx <- grep("AutorFiliacion", colnames(contr))
emailIdx <- grep("Email", colnames(contr))
tituloIdx <- grep("Descripcion", colnames(contr))
resumenIdx <- grep("Resumen", colnames(contr))
clavesIdx <- grep("PalabrasClaves", colnames(contr))
idIdx <- grep("IDmarcos", colnames(contr))
idBaseIdx <- grep("ID", colnames(contr))[1]
naIdx <- grep("nada", colnames(contr))
acceptIdx <- grep("accept", colnames(contr))
pagoIdx <- grep("Pagado", colnames(contr))
presentaIdx <- grep("AutorPresenta", colnames(contr))
campoAplIdx <- grep("Campo aplicacion", colnames(contr))
categMetIdx <- grep("Categoria Metodologica", colnames(contr))

# Ojo con los % y &. Hay que ponerle un simbolo de escape
any(grepl("%", contr$Descripcion))
contr$Descripcion[grep("%", contr$Descripcion)]
contr$Descripcion <- gsub("%", "\\\\%", contr$Descripcion)

any(grepl("%", contr$Resumen))
# contr$Resumen[grep("%", contr$Resumen)]
contr$Resumen <- gsub("%", "\\\\%", contr$Resumen)
contr$Resumen <- gsub("&", "\\\\&", contr$Resumen)

# Otros simbolos que generan lio
contr$Resumen <- gsub("»", '"', contr$Resumen)
contr$Resumen <- gsub("«", '"', contr$Resumen)

# Hay demasiados espacios en blanco, saltos de linea en los resumenes, con \r y \n
contr$Resumen <- gsub("\r|\n|\t", " ", contr$Resumen)
contr$Resumen <- gsub("  |   |    |     |      ", " ", contr$Resumen) # porque si hay \r \n seguidos quedan muchos espacios
contr$Descripcion <- gsub("\r|\n|\t", " ", contr$Descripcion)
contr$Descripcion <- gsub("  |   |    |     |      ", " ", contr$Descripcion)

# pruebas
# ver = c("ad\n\rsdf", "as\nfs\rd", "ad\r\n\r", "sfas\r\rd")
# grepl("\r", ver)
# ver2 = gsub("\r|\n", " ", ver)
# ver2
# gsub("  |   |    |     ", " ", ver2)
# ver = as.character(contr[contr$ID=="31-1", "Resumen"])
# ver = c("ad%sdf", "asfsd", "%", "sf%asd%")
# grepl("%", x = ver)
# gsub("%", "\\\\%", ver)

# Algunos campos de aplicacion tmb tienen espacios
unique(contr$`Campo aplicacion`)
contr$`Campo aplicacion` <- gsub("\r|\n", "", contr$`Campo aplicacion`)
unique(contr$`Campo aplicacion`)
unique(contr$`Categoria Metodologica`)
contr$`Categoria Metodologica` <- gsub("\r|\n", "", contr$`Categoria Metodologica`)
unique(contr$`Categoria Metodologica`)

# Hacer que los campos de aplicacion y las categorias metodologicas tengan mayuscula solo al inicio
firstup <- function(x) {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
    x
}
contr$`Campo aplicacion` <- firstup(contr$`Campo aplicacion`)
contr$`Categoria Metodologica` <- firstup(contr$`Categoria Metodologica`)

# Sacar puntos al final de los titulos
contr$Descripcion <- stri_replace_last_regex(contr$Descripcion, "\\.","")

# Sacar los trabajos que no se presentan
contr <- contr[contr$Estado == "Aprobado", ]

# Escribir y guardar tabla como tab txt
write_delim(contr, paste0(pathwd, "contr.txt"), delim = "\t", na = "", col_names = T)

# Ojo con los simbolos ^. Aca no lo puedo cambiar porque me da como que todos tienen ese simbolo.
# Creo que tiene que ver con los caracteres especiales y la codificacion
# ASi que ahora abro el archivo contr.txt y hago un buscar reemplzar ^ por \^


# cargarlo para probar
contr <- read_delim("C:/Users/marco/Google Drive/CIE2017/LibroResumenes/Prueba4/contr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

#################
# PASO 2 PARA EL PROGRAMA: COMUNICACIONES ORALES
#################

# ids segun campo de aplicacion
# idsCampo <- split(contr$IDmarcos, contr$`Campo aplicacion`)
# maxAbsPorSesion <- max(sapply(idsCampo, length))
contr <- filter(contr, substr(Sesion, 1, 1) == "O")
idsCampo <- split(contr$IDmarcos, contr$Sesion)
maxAbsPorSesion <- max(sapply(idsCampo, length))


# Leer datos
sesiones <- read_excel(paste0(pathwd, "trabajos_final-27-9_marcos.xlsx"), sheet = 2)
sesiones$Name <- gsub("\r|\n", "", sesiones$Name)
sesiones[paste0("Abstract", 1:maxAbsPorSesion)] <- NA

for (i in 1:nrow(sesiones)) {
    abs <- idsCampo[[sesiones$Name[i]]]
    length(abs) <- maxAbsPorSesion
    sesiones[i, paste0("Abstract", 1:maxAbsPorSesion)] <- abs
}

# Agregar nombre largo
sesiones$NombreLargo <- paste0(sesiones$Room, ": ", sesiones$Temas)
# aux <- split(contr$`Campo aplicacion`, contr$Sesion)
# camposUnicos <- sapply(aux, unique)
# sesiones$NombreLargo <- character(nrow(sesiones))
# for (i in 1:nrow(sesiones)) {
#     sesiones$NombreLargo[i] <- paste0(camposUnicos[[sesiones$Name[i]]], collapse = "; ")
# }

write_delim(sesiones, paste0(pathwd, "comOrales.txt"), delim = "\t", col_names = T)

absIdx <- grep("Abstract", colnames(sesiones))

sesiones$Hora <- paste0(substr(sesiones$TimeBegin, 1, 2), ":", substr(sesiones$TimeBegin, 3, 4), "--", substr(sesiones$TimeEnd, 1, 2), ":", substr(sesiones$TimeEnd, 3, 4))
sesSplit <- split(sesiones, sesiones$DayLong)
dias <- sapply(sesSplit, function(x) unique(x$DayLong))
absIdx <- grep("Abstract", colnames(sesiones))

archivo <- file("C:/Users/marco/Google Drive/CIE2017/Programa/comunicaciones.tex", "w", encoding = "UTF-8")
cat("\\clearpage",
    "\\pagestyle{fancy}",
    "\\vspace*{1cm}",
    "\\centerline{\\textbf{\\LARGE{Comunicaciones Orales}}}",
    "\\SetHeader{Comunicaciones Orales}{\\textit{Congreso Interamericano de Estadística}}"
    , file = archivo, sep = "\n")

for (dia in dias) {
    cat("\n\\renewcommand{\\Session}{", dia, "} \\Section{\\Session}\n\n", file = archivo, sep = "")
    datos <- sesSplit[[dia]]
    cat("{\\linespread{1.5} \n\\begin{longtable}{ >{\\centering \\small} m{.15\\textwidth \\vskip 0.05in} >{\\raggedright\\arraybackslash} m{.78\\textwidth \\vskip 0.05in} }  \n\n", file = archivo, sep = "")
    for (ses in datos$Name) {
        estaHora = as.character(datos[datos$Name == ses, "Hora"])
        cat(estaHora, " & \\textbf{", as.character(datos[datos$Name == ses, "NombreLargo"]), "} \\\\ \n", sep = "", file = archivo)
        cat("\\hline \n \n", file = archivo)
        
        ids <- as.numeric(datos[datos$Name == ses, absIdx])[!is.na( as.numeric(datos[datos$Name == ses, absIdx]))]
        
        # contrSub <- contr[contr$IDmarcos %in% ids, ]
        # ids <- contrSub$IDmarcos[order(contrSub$`Categoria Metodologica`)]
        # contrSub$`Categoria Metodologica`[order(contrSub$`Categoria Metodologica`)]
        
        if (estaHora == "10:30--13:00") {
            horas <- c("10:30--10:50", "10:50--11:10", "11:10--11:30", "11:30--11:50", "11:50--12:10", "12:10--12:30", "12:30--12:50")
        } else {
            horas <- c("14:00--14:20", "14:20--14:40", "14:40--15:00", "15:00--15:20", "15:20--15:40", "15:40--16:00")
        }
        #horas <- c(horas, horas, horas)
        
        for (i in 1:length(ids)) {
            id <- ids[i]
            trabajo <- contr[contr$IDmarcos == id, ]
            nAut <- trabajo$Cantautores
            #autores <- paste0(trabajo[, autorApellidoIdx][1:nAut], collapse = ", ")
            
            autores <- ""
            soloNombre <- as.character(trabajo[, autorNombreIdx][1:nAut])
            soloNombre <- sapply(soloNombre, function(x) strsplit(x, " ")[[1]][[1]])
            apellidos <- as.character(trabajo[, autorApellidoIdx][1:nAut])
            for (aut in 1:nAut) {
                autores <- paste0(autores, apellidos[aut], ", ", soloNombre[aut])
                if (nAut > 1 & aut < nAut) autores <- paste0(autores, "; ")
            }
            
            cat("\\footnotesize ", horas[i], " & \\footnotesize \\textbf {", trabajo$Descripcion, "} \n\\newline \\tiny {", autores, "} \\\\ \n", file = archivo, sep = "")
        }
        cat("\n & \\\\ \n\n", file = archivo, sep = "")
    }
    cat("\\end{longtable}} \n", file = archivo, sep = "")
}
close(archivo)

#################
# PASO 3 PARA EL PROGRAMA: POSTERS
#################

contr <- read_delim("C:/Users/marco/Google Drive/CIE2017/LibroResumenes/Prueba4/contr.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

# ids segun campo de aplicacion
# idsCampo <- split(contr$IDmarcos, contr$`Campo aplicacion`)
# maxAbsPorSesion <- max(sapply(idsCampo, length))
contr <- filter(contr, substr(Sesion, 1, 1) == "P")
idsCampo <- split(contr$IDmarcos, contr$Sesion)
maxAbsPorSesion <- max(sapply(idsCampo, length))


# Leer datos
sesiones <- read_excel(paste0(pathwd, "trabajos_final-27-9_marcos.xlsx"), sheet = 3)
sesiones$Name <- gsub("\r|\n", "", sesiones$Name)
sesiones[paste0("Abstract", 1:maxAbsPorSesion)] <- NA

for (i in 1:nrow(sesiones)) {
    abs <- idsCampo[[sesiones$Name[i]]]
    length(abs) <- maxAbsPorSesion
    sesiones[i, paste0("Abstract", 1:maxAbsPorSesion)] <- abs
}

# Agregar nombre largo
sesiones$NombreLargo <- paste0(sesiones$Room, ": ", sesiones$Temas)
write_delim(sesiones, paste0(pathwd, "basePosters.txt"), delim = "\t", col_names = T)

absIdx <- grep("Abstract", colnames(sesiones))

sesiones$Hora <- paste0(substr(sesiones$TimeBegin, 1, 2), ":", substr(sesiones$TimeBegin, 3, 4), "--", substr(sesiones$TimeEnd, 1, 2), ":", substr(sesiones$TimeEnd, 3, 4))
sesSplit <- split(sesiones, sesiones$Name)
absIdx <- grep("Abstract", colnames(sesiones))

archivo <- file("C:/Users/marco/Google Drive/CIE2017/Programa/posters.tex", "w", encoding = "UTF-8")
cat("\\clearpage",
    "\\pagestyle{fancy}",
    "\\vspace*{1cm}",
    "\\centerline{\\textbf{\\LARGE{Sesión Pósters}}}",
    "\\SetHeader{Sesión Pósters}{\\textit{Congreso Interamericano de Estadística}}"
    , file = archivo, sep = "\n")

#cat("\\vspace*{1cm} \n {\\setlength{\\parindent}{0cm} \n Una vez finalizada la primera ronda de 10 minutos de exhibición de cada póster, se procederá a una segunda ronda repartiendo el tiempo restante equitativamente entre los trabajos presentados en cada monitor.}"
#     , file = archivo, sep = "")
cat("\n\\renewcommand{\\Session}{Viernes 20 de octubre} \\Section{\\Session}\n\n", file = archivo, sep = "")
cat("{\\linespread{1.5} \n\\begin{longtable}{ >{\\centering \\small} m{.15\\textwidth \\vskip 0.05in} >{\\raggedright\\arraybackslash} m{.78\\textwidth \\vskip 0.05in} }  \n\n", file = archivo, sep = "")

for (datos in sesSplit) {
    
    
    
    
    estaHora = as.character(datos$Hora)
    cat(estaHora, " & \\textbf{", as.character(datos$NombreLargo), "} \\\\ \n", sep = "", file = archivo)
    cat("\\hline \n \n", file = archivo)
    
    ids <- as.numeric(datos[, absIdx])[!is.na( as.numeric(datos[, absIdx]))]
    
    if (estaHora == "10:30--13:00") {
        horas <- c("10:30--10:40", "10:40--10:50", "10:50--11:00", "11:00--11:10", "11:10--11:20", "11:20--11:30", "11:30--11:40", "11:40--11:50", "11:50--12:00", "12:00--12:10")
    } else {
        horas <- c("14:00--14:10", "14:10--14:20", "14:20--14:30", "14:30--14:40", "14:40--14:50", "14:50--15:00", "15:00--15:10", "15:10--15:20", "15:20--15:30", "15:30--15:40")
    }
    #horas <- c(horas, horas, horas)
    
    for (i in 1:length(ids)) {
        id <- ids[i]
        trabajo <- contr[contr$IDmarcos == id, ]
        nAut <- trabajo$Cantautores
        #autores <- paste0(trabajo[, autorApellidoIdx][1:nAut], collapse = ", ")
        
        autores <- ""
        soloNombre <- as.character(trabajo[, autorNombreIdx][1:nAut])
        soloNombre <- sapply(soloNombre, function(x) strsplit(x, " ")[[1]][[1]])
        apellidos <- as.character(trabajo[, autorApellidoIdx][1:nAut])
        for (aut in 1:nAut) {
            autores <- paste0(autores, apellidos[aut], ", ", soloNombre[aut])
            if (nAut > 1 & aut < nAut) autores <- paste0(autores, "; ")
        }
        
        cat("\\footnotesize ", horas[i], " & \\footnotesize \\textbf {", trabajo$Descripcion, "} \n\\newline \\tiny {", autores, "} \\\\ \n", file = archivo, sep = "")
    }
    horaFinal <- paste0(substr(horas[i], 8, 12), "--", substr(estaHora, 8, 12))
    cat("\\footnotesize ", horaFinal, " & \\footnotesize \\textbf {Retransmisión en el mismo orden de los trabajos precedentes} \\\\ \n \n", file = archivo, sep = "")
    cat("\n & \\\\ \n\n", file = archivo, sep = "")
    
    
}
cat("\\end{longtable}} \n", file = archivo, sep = "")

close(archivo)



#################
# PASO 4: PREPARAR LA BASE CON LOS DATOS DE LAS SESIONES PARA EL LIBRO DE RESUMENES
#################

# OPCION DEFINITIVA

# Tienen que ir ordenados primero com orales y luego posters, 
# Las com orales segun campo metodologico, y los posters segun area aplicacion
# Luego alfabeticamente
# Hago este ordenamiento, considerando que las sesiones es la combinacion de tipo
# de presentacion (oral, poster) con catg metodologica o aplic segun corresponda

unique(contr$`Categoria Metodologica`)
unique(contr$`Campo aplicacion`)
idx <- contr$Trabajo == "Comunicacion oral"
contr$Sesiones[idx] <- paste(contr$Trabajo[idx], contr$`Categoria Metodologica`[idx], sep = " - ")
idx <- contr$Trabajo == "Poster"
contr$Sesiones[idx] <- paste(contr$Trabajo[idx], contr$`Campo aplicacion`[idx], sep = " - ")

# Ordenarlas por tipo, y luego alfabeticamente
contr <- contr[order(contr$Sesiones, contr$Descripcion), ]

# ids segun sesion
idsCampo <- split(contr$IDmarcos, contr$Sesiones)
maxAbsPorSesion <- max(sapply(idsCampo, length))

# Crear data con cada uno de los temas
sesiones <- data.frame(Name = unique(contr$Sesiones), stringsAsFactors = F)
# creo cualquier cosa pero es necesario que estas vbles esten:
sesiones$Chair <- paste0("Chair", 1:nrow(sesiones))
sesiones$Day <- 1:nrow(sesiones)
sesiones$DayLong <- 1:nrow(sesiones)
sesiones$DayShort <- 1:nrow(sesiones)
sesiones$DayTable <- 1:nrow(sesiones)
sesiones$Room <- 1:nrow(sesiones)
sesiones$TimeBegin <- 1:nrow(sesiones) + 10000
sesiones$TimeEnd <- 1:nrow(sesiones) + 10010

# sesiones <- read_excel(paste0(pathwd, "trabajos_final-27-9_marcos.xlsx"), sheet = 2)
# sesiones$Name <- gsub("\r|\n", "", sesiones$Name)
sesiones[paste0("Abstract", 1:maxAbsPorSesion)] <- NA

for (i in 1:nrow(sesiones)) {
    abs <- idsCampo[[sesiones$Name[i]]]
    length(abs) <- maxAbsPorSesion
    sesiones[i, paste0("Abstract", 1:maxAbsPorSesion)] <- abs
}

# Agregar nombre largo (el nombre bien, que es la categ sin Com Oral o Poster adelante)
sesiones$NombreLargo <- sapply(strsplit(sesiones$Name, " - "), function(x) x[[2]])
# aux <- split(contr$`Campo aplicacion`, contr$Sesion)
# camposUnicos <- sapply(aux, unique)
# sesiones$NombreLargo <- character(nrow(sesiones))
# for (i in 1:nrow(sesiones)) {
#     sesiones$NombreLargo[i] <- paste0(camposUnicos[[sesiones$Name[i]]], collapse = "; ")
# }

write_delim(sesiones, paste0(pathwd, "sesionesLibroDefinitivo.txt"), delim = "\t", col_names = T)
absIdx <- grep("Abstract", colnames(sesiones))


#################
# PASO 4: OPCIÓN 2
#################


# Tienen que ir ordenados primero com orales y luego posters, 
# luego por tema de categ metodologica y luego alfabbeticamente
# Hago este ordenamiento, considerando que las sesiones es la combinacion de tipo
# de presentacion (oral, poster) con catg metodologica

unique(contr$`Categoria Metodologica`)
contr$Sesiones <- paste(contr$Trabajo, contr$`Categoria Metodologica`, sep = " - ")


# Ordenarlas por tipo, categoria metodologica y luego alfabeticamente
contr <- contr[order(contr$Trabajo, contr$`Categoria Metodologica`, contr$Descripcion), ]

# ids segun sesion
idsCampo <- split(contr$IDmarcos, contr$Sesiones)
maxAbsPorSesion <- max(sapply(idsCampo, length))

# Crear data con cada uno de los temas
sesiones <- data.frame(Name = unique(contr$Sesiones), stringsAsFactors = F)
# creo cualquier cosa pero es necesario que estas vbles esten:
sesiones$Chair <- paste0("Chair", 1:nrow(sesiones))
sesiones$Day <- 1:nrow(sesiones)
sesiones$DayLong <- 1:nrow(sesiones)
sesiones$DayShort <- 1:nrow(sesiones)
sesiones$DayTable <- 1:nrow(sesiones)
sesiones$Room <- 1:nrow(sesiones)
sesiones$TimeBegin <- 1:nrow(sesiones) + 10000
sesiones$TimeEnd <- 1:nrow(sesiones) + 10010

# sesiones <- read_excel(paste0(pathwd, "trabajos_final-27-9_marcos.xlsx"), sheet = 2)
# sesiones$Name <- gsub("\r|\n", "", sesiones$Name)
sesiones[paste0("Abstract", 1:maxAbsPorSesion)] <- NA

for (i in 1:nrow(sesiones)) {
    abs <- idsCampo[[sesiones$Name[i]]]
    length(abs) <- maxAbsPorSesion
    sesiones[i, paste0("Abstract", 1:maxAbsPorSesion)] <- abs
}

# Agregar nombre largo (el nombre bien, que es la categ sin Com Oral o Poster adelante)
sesiones$NombreLargo <- sapply(strsplit(sesiones$Name, " - "), function(x) x[[2]])
# aux <- split(contr$`Campo aplicacion`, contr$Sesion)
# camposUnicos <- sapply(aux, unique)
# sesiones$NombreLargo <- character(nrow(sesiones))
# for (i in 1:nrow(sesiones)) {
#     sesiones$NombreLargo[i] <- paste0(camposUnicos[[sesiones$Name[i]]], collapse = "; ")
# }

write_delim(sesiones, paste0(pathwd, "sesionesLibro.txt"), delim = "\t", col_names = T)
absIdx <- grep("Abstract", colnames(sesiones))

#######################
# PASO 4 opcion 3
####################

# Quieren una opcion donde solo esten separados por comunicaciones y posters
# y luego alfabeticametne

unique(contr$Trabajo)
contr$Trabajo[contr$Trabajo == "Comunicacion oral"] <- "Comunicaciones orales"
contr$Trabajo[contr$Trabajo == "Poster"] <- "Pósters"

# Ordenarlas por tipo luego alfabeticamente
contr <- contr[order(contr$Trabajo, contr$Descripcion), ]

# ids segun sesion
idsCampo <- split(contr$IDmarcos, contr$Trabajo)
maxAbsPorSesion <- max(sapply(idsCampo, length))

# Crear data con cada uno de los temas
sesiones <- data.frame(Name = unique(contr$Trabajo), stringsAsFactors = F)
# creo cualquier cosa pero es necesario que estas vbles esten:
sesiones$Chair <- paste0("Chair", 1:nrow(sesiones))
sesiones$Day <- 1:nrow(sesiones)
sesiones$DayLong <- 1:nrow(sesiones)
sesiones$DayShort <- 1:nrow(sesiones)
sesiones$DayTable <- 1:nrow(sesiones)
sesiones$Room <- 1:nrow(sesiones)
sesiones$TimeBegin <- 1:nrow(sesiones) + 10000
sesiones$TimeEnd <- 1:nrow(sesiones) + 10010

# sesiones <- read_excel(paste0(pathwd, "trabajos_final-27-9_marcos.xlsx"), sheet = 2)
# sesiones$Name <- gsub("\r|\n", "", sesiones$Name)
sesiones[paste0("Abstract", 1:maxAbsPorSesion)] <- NA

for (i in 1:nrow(sesiones)) {
    abs <- idsCampo[[sesiones$Name[i]]]
    length(abs) <- maxAbsPorSesion
    sesiones[i, paste0("Abstract", 1:maxAbsPorSesion)] <- abs
}

# Agregar nombre largo (el nombre bien, que es la categ sin Com Oral o Poster adelante)
# sesiones$NombreLargo <- sapply(strsplit(sesiones$Name, " - "), function(x) x[[2]])
# aux <- split(contr$`Campo aplicacion`, contr$Sesion)
# camposUnicos <- sapply(aux, unique)
# sesiones$NombreLargo <- character(nrow(sesiones))
# for (i in 1:nrow(sesiones)) {
#     sesiones$NombreLargo[i] <- paste0(camposUnicos[[sesiones$Name[i]]], collapse = "; ")
# }

write_delim(sesiones, paste0(pathwd, "sesionesLibro_SinCateg.txt"), delim = "\t", col_names = T)
absIdx <- grep("Abstract", colnames(sesiones))

#################
# PASO 3: GENERAR LOS ARCHIVOS TEX CON LOS ABSTRACTS
#################

# A esta funcion le modifique el separador entre las afiliaciones y los emails
# Reemplace todos los "and" por "y"
# Modifique la parte de las notas y los topics para que devuelva las palabras claves que antes no tenia,
# topic1 sea campo de aplicacion, topic2 sea area metod
# Solo en el estilo AS2012, modifique las afiliaciones, porque si ciudad, pais, email no tenian nada, quedaban los espacios vacios en el tex producido
# Le agregue otro ID, para tener uno numerico para R, y otro que es el de la base
generate.abstracts_Marcos(my.filename = paste0(pathwd, "contr.txt")
                   , dirAbstracts = paste0(pathwd, tag)
                   , author.lastname = autorApellidoIdx
                   , author.firstname = autorNombreIdx
                   , author.institution = autorAfiliacionIdx
                   , author.city = rep(naIdx, maxCantAut) # NA para todos
                   , author.country = rep(naIdx, maxCantAut) # NA para todos
                   , author.email = emailIdx
                   , author.presenting = presentaIdx
                   , pres.title = tituloIdx
                   , pres.abstract = resumenIdx
                   , accept = acceptIdx
                   , topic1 = campoAplIdx
                   , topic2 = categMetIdx
                   , id = idIdx
                   , notes = NA
                   , ref2 = NA
                   , ref3 = NA
                   , ref4 = NA
                   , accept.all = T
                   , noNotes = T
                   , notesDay = naIdx
                   , notesPayment = pagoIdx
                   , palabrasClaves = clavesIdx
                   , idBase = idBaseIdx
)

# A esta le habia pueesto PROGRAMA en espaniol pero al final no la uso
generate.programOverview_Marcos(my.program.file = paste0(pathwd, "sesionesLibro.txt")
                         , dirAbstracts = paste0(pathwd, tag)
                         , abstract.id = absIdx) 

# Me genera un erro que no se que es, creo que puede estar relacionado a que divide el nombre de las sesiones
# Pero es la que genera la tabla del cronograma general, no la voy a usar


# A esta le modifique para que entre cada "input(x.tex)" le agregue una linea de separacion
# le saque un clearpage para que no salte de pagina cuando termina una sesion en el abstract.tex
# le agregue encoding a la lectura del contribuciones.txt porque si no me hacia lio con los acentos
# Le cambie cuando empieza una sesion nueva de abstracts que no pongo salto de pagina y que no cambie los
# encabezados de pagina (siempre los mismos encabezados)
generate.program_Marcos(my.program.file = paste0(pathwd, "sesionesLibroDefinitivo.txt")
                 , my.filename = paste0(pathwd, "contr.txt")
                 , dirAbstracts = paste0(pathwd, tag)
                 , abstract.id = absIdx
                 , author.lastname = autorApellidoIdx
                 , author.firstname = autorNombreIdx
                 , accept = acceptIdx
                 , pres.title = tituloIdx
                 , id = idIdx
)

# Para la segunda opcion donde solo se separa en Comunicaciones y Posters
generate.program_Marcos(my.program.file = paste0(pathwd, "sesionesLibro_SinCateg.txt")
                        , my.filename = paste0(pathwd, "contr.txt")
                        , dirAbstracts = paste0(pathwd, tag)
                        , abstract.id = absIdx
                        , author.lastname = autorApellidoIdx
                        , author.firstname = autorNombreIdx
                        , accept = acceptIdx
                        , pres.title = tituloIdx
                        , id = idIdx
)

write.styleAndData.tex(dirAbstracts = paste0(pathwd, tag)
                       , Book = "Book_submitted" #default
                       , BookTitle = "\\ " # para que quede vacio
                       , Style = "AS2012" #default AS2012
                       , BookSubmittedTitle = "Submitted ABSTRACTS" #default
                       , BookSubmittedAndAcceptedTitle = "Accepted ABSTRACTS" #default
                       , DocYear = "2017"
                       , ConferenceTitle.line1 = "\\ "
                       , ConferenceTitle.line2 = "Libro de ResÃºmenes"
                       , Date = "17-20 Octubre 2017"
                       , Place = "Rosario, Argentina"
                       , URL = "www.cie2017.s-a-e.org.ar/congreso.php"
                       , Organizer = "UNR"
                       , PublishedBy = "UNR Editora"
                       , Editors = "Marcos, Gonzalo"
                       , PrintedBy = "Impresora"
                       , Circulation = 200
                       , ScientificComm = list("Cuesta, Cristina", "Mari, Gonzalo")
                       , OrganizingComm = list("Mendez, Nanda", "Boggio, Gabriela", "Otro, mas", "Uno, mas")
                       , ISBN = "sdfasfasdf"
                       , ISBN2 = ""
                       , CIP = ""
                       , Sponsors = list("sponsor1", "sponsor2")
                       , CenterPage2 = "algo para poner aca?"
                       , BottomPage2 = "y aca?"
                       , Footer = "17-20 Octubre 2017, Rosario, Argentina"
                       , FigureFile = "../Graficos/logo_CIE"
                       , copy.TeXTemplates = T #default T
)

#####################
# palabras claves cortadas

palabraIdx <- grep("Palabra", colnames(contr))
palabraIdx <- palabraIdx[-length(palabraIdx)]
palabras <- contr[, palabraIdx]

largos <- matrix(NA, nrow(palabras), ncol(palabras))
for (i in 1:nrow(palabras)) {
    for (j in 1:ncol(palabras)) {
        largos[i, j] <- nchar(palabras[i, j])
    }
}

max(largos, na.rm = T)
which(largos == 40, arr.ind = T)
# hay uno que tiene largo 40 pero fue agregado a mano por mi
table(largos)
# el tope eran 20 caracters
which(largos > 20, arr.ind = T) # estan bien
which(largos == 19, arr.ind = T)
which(largos == 20, arr.ind = T)

# Cuales son los trabajos que tiene palabras con 20 caracteres
trabajosCortados <- apply(largos, 1, function(x) any(x == 20, na.rm = T))
sum(trabajosCortados)

tmp <- file("palabrasCortadas18.txt", open = "w", encoding = "UTF-8")
for (i in 1:nrow(largos)) {
    if (any(largos[i, ] == 18, na.rm = T)) {
        cat("IDbase: ", contr$ID[i], " | IDmarcos: ", contr$IDmarcos[i], "\n", sep = "", file = tmp)
        for (j in 1:ncol(largos)) {
            if (!is.na(largos[i, j])) {
                if (largos[i, j] == 18) {
                    cat(as.character(palabras[i, j]), "\n", file = tmp)
                }
            }
        }
        cat("----------------------------\n\n", file = tmp)
    }
}
close(tmp)
