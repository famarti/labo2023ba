# Ensemble de arboles de decision
# utilizando el naif metodo de Arboles Azarosos
# entreno cada arbol en un subconjunto distinto de atributos del dataset

# limpio la memoria
rm(list = ls()) # Borro todos los objetos
gc() # Garbage Collection

require("data.table")
require("rpart")

# parmatros experimento
PARAM <- list()

# Establezco la semilla aleatoria, cambiar por SU primer semilla
PARAM$semilla <- 123457

# parametros  arbol
# entreno cada arbol con solo 50% de las variables variables
PARAM$feature_fraction <- 0.5
# voy a generar 500 arboles, a mas arboles mas tiempo de proceso y MEJOR MODELO
#  pero ganancias marginales
PARAM$num_trees_max <- 500

#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui comienza el programa

setwd("~/buckets/b1/") # Establezco el Working Directory

config_table <- fread("./datasets/mis_arboles.csv")
config_table$experimento <- seq(3210, length.out = nrow(config_table))

# Definir carpeta base del experimento
base_experimento <- "./exp/"

# cargo los datos
dataset <- fread("./datasets/dataset_pequeno.csv")

# Bucle para variar hiperparámetros
for (i in 1:nrow(config_table)) { # config_table es la tabla que proporcionaste
    PARAM$rpart_param <- list(
        "cp" = config_table$cp[i],
        "minsplit" = config_table$minsplit[i],
        "minbucket" = config_table$minbucket[i],
        "maxdepth" = config_table$maxdepth[i]
    )

    PARAM$experimento <- config_table$experimento[i] # Actualizar el experimento

    # Crear carpeta para la configuración actual
    carpeta_experimento <- paste0(base_experimento, "KA", PARAM$experimento, "/")
    dir.create(carpeta_experimento, showWarnings = FALSE)
    setwd(carpeta_experimento)


    # que tamanos de ensemble grabo a disco, pero siempre debo generar los 500
    grabar <- c(1, 5, 10, 50, 100, 200, 500)


    # defino los dataset de entrenamiento y aplicacion
    dtrain <- dataset[foto_mes == 202107]
    dapply <- dataset[foto_mes == 202109]

    # aqui se va acumulando la probabilidad del ensemble
    dapply[, prob_acumulada := 0]

    # Establezco cuales son los campos que puedo usar para la prediccion
    # el copy() es por la Lazy Evaluation
    campos_buenos <- copy(setdiff(colnames(dtrain), c("clase_ternaria")))



    # Genero las salidas
    set.seed(PARAM$semilla) # Establezco la semilla aleatoria

    for (arbolito in 1:PARAM$num_trees_max) {
        qty_campos_a_utilizar <- as.integer(length(campos_buenos)
        * PARAM$feature_fraction)

        campos_random <- sample(campos_buenos, qty_campos_a_utilizar)

        # paso de un vector a un string con los elementos
        # separados por un signo de "+"
        # este hace falta para la formula
        campos_random <- paste(campos_random, collapse = " + ")

        # armo la formula para rpart
        formulita <- paste0("clase_ternaria ~ ", campos_random)

        # genero el arbol de decision
        modelo <- rpart(formulita,
            data = dtrain,
            xval = 0,
            control = PARAM$rpart_param
        )

        # aplico el modelo a los datos que no tienen clase
        prediccion <- predict(modelo, dapply, type = "prob")

        dapply[, prob_acumulada := prob_acumulada + prediccion[, "BAJA+2"]]

        if (arbolito %in% grabar) {
            # Genero la entrega para Kaggle
            umbral_corte <- (1 / 40) * arbolito
            entrega <- as.data.table(list(
                "numero_de_cliente" = dapply[, numero_de_cliente],
                "Predicted" = as.numeric(dapply[, prob_acumulada] > umbral_corte)
            )) # genero la salida

            # Guardar resultados en un archivo único
            nom_arch <- paste0(
                "KA", PARAM$experimento, "_",
                "cp", PARAM$rpart_param$cp, "_",
                "minsplit", PARAM$rpart_param$minsplit, "_",
                "minbucket", PARAM$rpart_param$minbucket, "_",
                "maxdepth", PARAM$rpart_param$maxdepth, "_",
                sprintf("%.3d", arbolito), ".csv"
            )
            fwrite(entrega, file = nom_arch, sep = ",")

            cat(arbolito, " ")

            # Volver al directorio base del experimento
            setwd(base_experimento)
        }
    }
}
