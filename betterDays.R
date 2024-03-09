####################
### Better days  ###
####################
### Días mejores ###
####################

############################################################################
## Descripcion                                                            ##
## Este codigo sirve para tomar todas las columnas de un csv o tsv        ##
## que contengan datos de fechas en formatos diferentes y transformarlas  ##
## en formato universal yyyy-mm-dd. A tener en cosideracion:              ##
## el separador utilizado en las fechas debe ser "/" o "-".               ##
############################################################################

### Requirements
library(ggplot2)

### Arguments
args = commandArgs(trailingOnly=TRUE)
# Si no se han indicado dos argumentos, no se ejecuta
if(length(args)!=2) {
	stop("Usage: Rscript betterDates.R <input file> <output file>")
}

### Variables
infile=args[1] #tabla de entrada
outfile=args[2] #tabla de salida
dayFirst=TRUE #flag que guarda si en la anterior iteracion el dia iba primero
# Para la primera iteracion se da por hecho que esta en primer lugar el dia

### Functions
# Funcion que parsea fecha en funcion del separador
parseDate <- function(sep, date){
	# divido la fecha por su separador
	date_split=strsplit(date, sep)[[1]]
	# creo una lista donde se guardarán los valores
	lista <- list()
	# busco donde esta el año
	# si el ultimo numero tiene cuatro cifras, es un año
	if(nchar(date_split[length(date_split)])==4) {
		lista$year <- date_split[length(date_split)]
		# se quita el valor de año del vector
		date_split <- date_split[-c(length(date_split))]	
	# si el año no esta el ultimo sera el primero
	}else if(nchar(date_split[1])==4) {
		lista$year=date_split[1]
		# se quita el valor de año del vector
		date_split <- date_split[-c(1)]
		# si el año aparece en primer lugar, el dia estara en ultima posicion
		dayFirst=FALSE
	# si ninguno parece ser el año porque tiene dos cifras
	# suponemos que esta en ultimo lugar
	# y le añadimos las dos cifras que le faltan
	}else{
		year=paste0("20", date_split[length(date_split)])
		lista$year <- year
		# se quita el valor de año del vector
		date_split <- date_split[-c(length(date_split))]
	}

	# busco donde esta el mes
	# si solo queda un elemento en el vector (para fechas %m-%y)
	# se da por hecho que es un mes
	if(length(date_split)==1) {
		lista$month = date_split[1]
	# si queda mas de un valor en el vector
	# busco el dia
	# si es mayor a 12, es dia
	}else if(date_split[1] > 12) {
		lista$day = date_split[1]
		lista$month = date_split[2]
		dayFirst=TRUE #se guarda esta flag como True
	}else if(date_split[2] > 12) {
		lista$day = date_split[2]
		lista$month = date_split[1]
		dayFirst=FALSE 
	# y si no se puede saber el orden del dia y el mes
	# nos ayudamos de la flag dayFirst
	# y echamos un warning porque no es posible saberlo con seguridad
	}else if(dayFirst) {
		lista$day = date_split[1]
		lista$month = date_split[2]
		print(paste0("WARNING: orden dia-mes incierto en fecha ", date))
		print("Se presupone primero: DIA")
	}else{
		lista$day = date_split[2]
		lista$month = date_split[1]
		print(paste0("WARNING: orden dia-mes incierto en fecha ", date))
		print("Se presupone primero: MES")
	}
	# devuelve la lista
 	return(lista)
 }

formatDate <- function(lista){
	if("day" %in% names(lista)) {
		# si el dia solo tiene un caracter le añado un 0
		if(nchar(lista$day)==1) {
			lista$day=paste0("0", lista$day)
		}
	}
	if("month" %in% names(lista)) {
		# si el mes solo tiene un caracter le añado un 0
		if(nchar(lista$month)==1) {
			lista$month=paste0("0", lista$month)
		}
	}
	# si hay dia
	if("day" %in% names(lista)) {
		format_date=paste(lista$year, lista$month, lista$day, sep="-")
	}else{
		format_date=paste(lista$year, lista$month, sep="-")
	}
	return(list(format_date, lista))
}
		

### Code
## Adivina el separador de tabla
if(endsWith(infile, "tsv")){
	separador="\t"
	df <- read.table(file=infile, sep=separador, header=TRUE)
}else if(endsWith(infile, "csv")) {
	separador=","
	df <- read.table(file=infile, sep=separador, header=TRUE)
}else{
	print("Formato desconocido. Solo CSV o TSV")
	stop()
}

## Extrae vector de columnas que pueden ser fechas
date_cols <- c()
# bucle que recorre los nombres de las columnas
for(n in colnames(df)){
if(grepl("fecha", tolower(n), fixed=TRUE) || grepl("date", tolower(n), fixed=TRUE)) date_cols <- c(date_cols, n)
}


print(paste0("Columna con fechas: ",date_cols))

## Bucle que recorre el vector y las filas de la tabla
print("Dataframe de entrada:")
print(df)
for(j in date_cols) {
	vector_meses <- c()
	vector_años <- c()
	for(i in 1:nrow(df)) {
		fecha <- df[i, j]
		# si el separador de la fecha es -
		if(grepl("-", fecha)) {
			pfecha=parseDate("-", fecha) #fecha parseada
			# guardo mes y año en el vector correspondiente
			fechaylista  <- formatDate(pfecha) #fecha formateada
			ffecha <- fechaylista[[1]]
			# y me guardo la lista para los graficos
			flista <- fechaylista[[2]]
			vector_meses <- append(vector_meses, flista$month)
			vector_años <- append(vector_años, flista$year)
		# si el separador de la fecha es /
		}else if(grepl("/", fecha)) {
			pfecha=parseDate("/", fecha) #fecha parseada
			# guardo mes y año en el vector correspondiente
			fechaylista  <- formatDate(pfecha) #fecha formateada
			ffecha <- fechaylista[[1]]
			# y me guardo la lista para los graficos
			flista <- fechaylista[[2]]
			vector_meses <- append(vector_meses, flista$month)
			vector_años <- append(vector_años, flista$year)
		}else{
			# cuando la fecha es solo año
			if(nchar(fecha)==4) {
				# no necesita formateo
				ffecha=as.character(fecha)
			}else{
				# print("esta se le añade 20:")
				# print(fecha)
				ffecha=paste0("20", fecha)
			}
			# guardo año en el vector correspondiente
			vector_años <- append(vector_años, ffecha)
		}
		df[i, j]=ffecha #cambia el valor de fecha en la tabla
	}

	# si alguna de las fechas de la columna j tiene mes
	# se dibuja grafico de barras
	if(length(vector_meses)>0) {
		df_meses <- data.frame(vector_meses)
		colnames(df_meses) <- paste0(j, ". Months")
		print(colnames(df_meses))
		pl <- ggplot(df_meses, aes(x = .data[[colnames(df_meses)]]))
		pl <- pl + geom_bar(color="darkslategray", fill="darkslategray4", width=0.5) + theme_light()
		ggsave(filename=paste0(j,"_months.png"), device="png", plot=pl)
	}
	df_años <- data.frame(vector_años)
	colnames(df_años) <- paste0(j, ". Years")
	print(colnames(df_años))
	ply <- ggplot(df_años, aes(x = .data[[colnames(df_años)]]))
	ply <- ply + geom_bar(color="darkmagenta", fill="darkorchid1", width=0.5) + theme_light()
	ggsave(filename=paste0(j,"_years.png"), device="png", plot=ply)

}

print("Dataframe de salida:")
print(df)
# guarda el dataframe con las fechas en formato yyyy-mm-dd
# en valores separados por el mismo separador que la tabla de entrada
write.table(df, file=outfile, sep=separador, row.names=FALSE)



############################################################################
