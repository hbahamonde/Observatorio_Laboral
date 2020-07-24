# Este script es para el Observatorio Laboral de la UOH.
## Hector Bahamonde, PhD
## Prodesor Asistente, Instituto de Ciencias Sociales, UOH.

cat("\014")
rm(list=ls())
graphics.off()


# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load data
p_load(readxl)
data <- read_excel("/Users/hectorbahamonde/RU/research/Observatorio_Laboral/enadel.xlsx", col_names = TRUE) 


########################################################################################################################
# Empresa y Genero
########################################################################################################################

# Data tiene outliers: empresas con 625 gerentas!?
# View(data.frame(data$a4_1_b,data$actividad))

# Exclude outliers: empresas que tienen mas de 10 gerentas out.
#data <- data[!(data$a4_1_b>10),]

# summary
hist(data$a4_1_b)
summary(data$a4_1_b)

# Recode DV: from count to binary
data$a4_1_b <- ifelse(data$a4_1_b > 0, 1, 0)

# Finding #1: No estamos tan mal.
table(data$a4_1_b) 
lattice::histogram(as.factor(data$a4_1_b), main = "Porcent de Empresas con Gerentas")

# Recode Data
empresa.genero.d = data.frame(
        mujeres = as.numeric(data$a4_1_b), # Cantidad de mujeres gerentas
        rubro = as.numeric(as.factor(data$actividad)), # actividad
        competencias = as.numeric(as.factor(data$b4a_o1_1)), # Competencias
        certificacion = as.numeric(as.factor(data$b4b_o1)), # Certificacion
        reclutamiento = as.numeric(as.factor(data$b6_1)) # Via de reclutamiento
)

## Rubro
## table(empresa.genero.d$rubro, data$actividad)
### 1. Contru
### 2. Industria
### 3. Transporte
### 4. Turismo

## Competencias
## table(empresa.genero.d$competencias, data$b4a_o1_1)
### 1. Competencias actitudinales
### 2. Competencias básicas
### 3. Competencias conductuales
### 4. Competencias directivas o de gestión
### 5. Competencias en idiomas extranjeros
### 6. Competencias en tecnologías de la información
### 7. Competencias técnicas
### 8. Otras.

## Certificacion
## table(empresa.genero.d$certificacion, data$b4b_o1)
### 1. No
### 2. Si

## Reclutamiento
## table(empresa.genero.d$reclutamiento, data$b6_1)
### 1. Avisos en las inmediaciones de la empresa o banco de C.V
### 2. Bolsa Nacional de Empleo
### 3. Contratación de empresas de reclutamiento
### 4. Diario o radio
### 5. Oficina Municipal de Información Laboral (OMIL)
### 6.  Otro
### 7. Plataforma web de empleo pagada (trabajando.com, laborum, linkedin)
### 8.  Plataforma web privada de empleo gratuita (yapo) excluyendo redes sociales como facebook, twitter o instagram
### 9.  Recomendaciones de trabajadores de la empresa u otros actores. Ej: boca a boca.
### 10. Redes de profesionales o egresados (mailing list)
### 11. Redes personales del empleador
### 12. Redes sociales (facebook, twitter, instagram)



###############
# Modelo
###############

# Zelig
p_load(Zelig)


z.out <- zelig(mujeres ~ 
                       rubro +  
                       competencias +
                       certificacion +
                       reclutamiento, # incluir: tamano de la empresa (personas o $). 
               model = "logit", 
               #weights = w, 
               data = empresa.genero.d,
               cite = F)

## Funcion para SE
std <- function(x) sd(x)/sqrt(length(x))


###############
# Predicciones por rubro
###############

set.seed(2020)

sim.rubro = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out, rubro=1)))),
                mean(get_qi(sim(setx(z.out, rubro=2)))),
                mean(get_qi(sim(setx(z.out, rubro=3)))),
                mean(get_qi(sim(setx(z.out, rubro=4))))
                ),
        Rubro = c(
                "Construccion",
                "Industria",
                "Transporte",
                "Turismo"
                ),
        lower = c(
                mean(get_qi(sim(setx(z.out, rubro=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=1))))/sqrt(length(get_qi(sim(setx(z.out, rubro=1))))),
                mean(get_qi(sim(setx(z.out, rubro=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=2))))/sqrt(length(get_qi(sim(setx(z.out, rubro=2))))),
                mean(get_qi(sim(setx(z.out, rubro=3)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=3))))/sqrt(length(get_qi(sim(setx(z.out, rubro=3))))),
                mean(get_qi(sim(setx(z.out, rubro=4)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=4))))/sqrt(length(get_qi(sim(setx(z.out, rubro=4)))))
                ),
        upper = c(mean(get_qi(sim(setx(z.out, rubro=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=1))))/sqrt(length(get_qi(sim(setx(z.out, rubro=1))))),
                  mean(get_qi(sim(setx(z.out, rubro=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=2))))/sqrt(length(get_qi(sim(setx(z.out, rubro=2))))),
                  mean(get_qi(sim(setx(z.out, rubro=3)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=3))))/sqrt(length(get_qi(sim(setx(z.out, rubro=3))))), 
                  mean(get_qi(sim(setx(z.out, rubro=4)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, rubro=4))))/sqrt(length(get_qi(sim(setx(z.out, rubro=4)))))
                  )
)


# Plot
p_load(ggplot2)
ggplot(sim.rubro, aes(x=reorder(Rubro, -Probabilidad), y=Probabilidad, fill=Rubro)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 


###############
# Predicciones por Competencias
###############

set.seed(2020)
sim.competencias = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out, competencias=1)))),
                mean(get_qi(sim(setx(z.out, competencias=2)))),
                mean(get_qi(sim(setx(z.out, competencias=3)))),
                mean(get_qi(sim(setx(z.out, competencias=4)))),
                mean(get_qi(sim(setx(z.out, competencias=5)))),
                mean(get_qi(sim(setx(z.out, competencias=6)))),
                mean(get_qi(sim(setx(z.out, competencias=7)))),
                mean(get_qi(sim(setx(z.out, competencias=8))))),
        Competencias = c(
                "Actitudinales",
                "Básicas", 
                "Conductuales", 
                "Directivas/Gestión", 
                "Idiomas", 
                "Computacion",
                "Técnicas", 
                "Otras"),
        lower = c(
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=1))))/sqrt(length(get_qi(sim(setx(z.out, competencias=1))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=2))))/sqrt(length(get_qi(sim(setx(z.out, competencias=2))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=3))))/sqrt(length(get_qi(sim(setx(z.out, competencias=3))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=4))))/sqrt(length(get_qi(sim(setx(z.out, competencias=4))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=5))))/sqrt(length(get_qi(sim(setx(z.out, competencias=5))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=6))))/sqrt(length(get_qi(sim(setx(z.out, competencias=6))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=7))))/sqrt(length(get_qi(sim(setx(z.out, competencias=7))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=8))))/sqrt(length(get_qi(sim(setx(z.out, competencias=8)))))
                ),
        upper = c(
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=1))))/sqrt(length(get_qi(sim(setx(z.out, competencias=1))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=2))))/sqrt(length(get_qi(sim(setx(z.out, competencias=2))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=3))))/sqrt(length(get_qi(sim(setx(z.out, competencias=3))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=4))))/sqrt(length(get_qi(sim(setx(z.out, competencias=4))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=5))))/sqrt(length(get_qi(sim(setx(z.out, competencias=5))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=6))))/sqrt(length(get_qi(sim(setx(z.out, competencias=6))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=7))))/sqrt(length(get_qi(sim(setx(z.out, competencias=7))))),
                mean(get_qi(sim(setx(z.out, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, competencias=8))))/sqrt(length(get_qi(sim(setx(z.out, competencias=8)))))
                )
)


# Plot
p_load(ggplot2)
ggplot(sim.competencias, aes(x=reorder(Competencias,- Probabilidad), y=Probabilidad, fill=Competencias)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 


###############
# Predicciones por Certificacion
###############

set.seed(2020)
sim.certificacion = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out, certificacion=1)))),
                mean(get_qi(sim(setx(z.out, certificacion=2))))
                ),
        Certificacion = c(
                "No",
                "Si"
                ),
        lower = c(
                mean(get_qi(sim(setx(z.out, certificacion=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, certificacion=1))))/sqrt(length(get_qi(sim(setx(z.out, certificacion=1))))),
                mean(get_qi(sim(setx(z.out, certificacion=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, certificacion=2))))/sqrt(length(get_qi(sim(setx(z.out, certificacion=2)))))
                ),
        upper = c(
                mean(get_qi(sim(setx(z.out, certificacion=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, certificacion=1))))/sqrt(length(get_qi(sim(setx(z.out, certificacion=1))))), 
                mean(get_qi(sim(setx(z.out, certificacion=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, certificacion=2))))/sqrt(length(get_qi(sim(setx(z.out, certificacion=2)))))
                )
        )

# Plot
p_load(ggplot2)
ggplot(sim.certificacion, aes(x=reorder(Certificacion, -Probabilidad), y=Probabilidad, fill=Certificacion)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 


###############
# Predicciones por Reclutamiento
###############
set.seed(2020)
sim.reclutamiento = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out, reclutamiento=1)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=2)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=3)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=4)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=5)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=6)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=7)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=8)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=9)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=10)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=11)))),
                mean(get_qi(sim(setx(z.out, reclutamiento=12))))
        ),
        Reclutamiento = c(
                "Avisos (inmediaciones o banco de C.Vs)", 
                "Bolsa Nacional de Empleo", 
                "Empresas de reclutamiento", 
                "Diario/Radio", 
                "Oficina Municipal de Información Laboral (OMIL)", 
                "Otro", 
                "Plataforma pagada (Trabajando.com, Laborum, LinkedIn)", 
                "Plataforma gratuita (Yapo)", 
                "Boca a boca", 
                "Redes (egresados)", 
                "Redes personales del empleador", 
                "Redes sociales (Facebook, Twitter, Instagram)"
                ),
        lower = c(
                mean(get_qi(sim(setx(z.out, reclutamiento=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=1))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=1))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=2))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=2))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=3)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=3))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=3))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=4)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=4))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=4))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=5)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=5))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=5))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=6)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=6))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=6))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=7)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=7))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=7))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=8)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=8))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=8))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=9)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=9))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=9))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=10)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=10))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=10))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=11)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=11))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=11))))),
                mean(get_qi(sim(setx(z.out, reclutamiento=12)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=12))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=12)))))
        ),
        upper = 
                c(
                        mean(get_qi(sim(setx(z.out, reclutamiento=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=1))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=1))))), 
                        mean(get_qi(sim(setx(z.out, reclutamiento=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=2))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=2))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=3)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=3))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=3))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=4)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=4))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=4))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=5)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=5))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=5))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=6)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=6))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=6))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=7)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=7))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=7))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=8)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=8))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=8))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=9)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=9))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=9))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=10)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=10))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=10))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=11)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=11))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=11))))),
                        mean(get_qi(sim(setx(z.out, reclutamiento=12)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out, reclutamiento=12))))/sqrt(length(get_qi(sim(setx(z.out, reclutamiento=12)))))
                )
)

# Plot
p_load(ggplot2)
ggplot(sim.reclutamiento, aes(x=reorder(Reclutamiento, -Probabilidad), y=Probabilidad, fill=Reclutamiento)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 



########################################################################################################################
# Empresa y Desigualdad
########################################################################################################################
cat("\014")
rm(list=ls())
graphics.off()


# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load data
p_load(readxl)
data <- read_excel("/Users/hectorbahamonde/RU/research/Observatorio_Laboral/enadel.xlsx", col_names = TRUE) 


# descrip
table(data$b3e_1) 
lattice::histogram(as.factor(data$b3e_1), main = "Porcent de Relacion Contractual")
## Assumption: Precariedad: Boleta de honorarios > Definido > Indefinido  (Modelo es OPROBIT)

# Recode Tuvo bacantes, cuantas?
data$b1_cuantas[is.na(data$b1_cuantas)] <- 0
p_load(tigerstats)
densityplot(data$b1_cuantas)


## Vacasntes
## table(precariedad.d$vacantes, data$b1_cuantas)

###############
# Modelo
###############


# Recode Data
precariedad.d = data.frame(
        precariedad = as.factor(data$b3e_1), # honorarios, Definido, Indefinido
        rubro = as.numeric(as.factor(data$actividad)), # actividad
        competencias = as.numeric(as.factor(data$b4a_o1_1)), # Competencias
        certificacion = as.numeric(as.factor(data$b4b_o1)), # Certificacion
        vacantes = as.numeric(data$b1_cuantas), # cuantas vacantes hubieron.
        educacion.requerida = as.numeric(as.factor(data$b3c_1)), # educacion de la gente que se recluta
        capacitacion = as.numeric(as.factor(data$c1_1)) # trabajadores de esta empresa han participado en algún curso formal de capacitación
        )

# modelo oprobit (no funciona con tres categorias)
# precariedad.d$precariedad <- factor(precariedad.d$precariedad, 
#                                    ordered = TRUE, 
#                                    levels = c("Indefinido", "Definido", "Boleta de honorarios")
#                                    )

# convertir a logit
precariedad.d$precariedad[precariedad.d$precariedad == "Boleta de honorarios"] <- NA
precariedad.d$precariedad = ifelse(as.numeric(as.factor(precariedad.d$precariedad))==2,1,0 )
lattice::histogram(precariedad.d$precariedad)


# Zelig
p_load(zeligverse)


options(scipen=1000000) 
z.out.precariedad <- zelig(precariedad ~ 
                                   rubro +  
                                   competencias +
                                   certificacion + 
                                   educacion.requerida + 
                                   vacantes +
                                   capacitacion, 
               model = "logit", 
               #weights = w, 
               data = precariedad.d,
               cite = F)

summary(z.out.precariedad)


## Funcion para SE
std <- function(x) sd(x)/sqrt(length(x))


###############
# Predicciones por rubro
###############

set.seed(2020)
sim.rubro.precariedad = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out.precariedad, rubro=1)))),
                mean(get_qi(sim(setx(z.out.precariedad, rubro=2)))),
                mean(get_qi(sim(setx(z.out.precariedad, rubro=3)))),
                mean(get_qi(sim(setx(z.out.precariedad, rubro=4))))
        ),
        Rubro = c(
                "Construccion",
                "Industria",
                "Transporte",
                "Turismo"
        ),
        lower = c(
                mean(get_qi(sim(setx(z.out.precariedad, rubro=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=1))))),
                mean(get_qi(sim(setx(z.out.precariedad, rubro=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=2))))),
                mean(get_qi(sim(setx(z.out.precariedad, rubro=3)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=3))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=3))))),
                mean(get_qi(sim(setx(z.out.precariedad, rubro=4)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=4))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=4)))))
        ),
        upper = c(mean(get_qi(sim(setx(z.out.precariedad, rubro=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=1))))),
                  mean(get_qi(sim(setx(z.out.precariedad, rubro=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=2))))),
                  mean(get_qi(sim(setx(z.out.precariedad, rubro=3)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=3))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=3))))), 
                  mean(get_qi(sim(setx(z.out.precariedad, rubro=4)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, rubro=4))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, rubro=4)))))
        )
)

# Plot
p_load(ggplot2)
ggplot(sim.rubro.precariedad, aes(x=reorder(Rubro, -Probabilidad), y=Probabilidad, fill=Rubro)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 


###############
# Predicciones por Competencias
###############

set.seed(2020)
sim.precariedad.competencias = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out.precariedad, competencias=1)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=2)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=3)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=4)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=5)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=6)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=7)))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=8))))),
        Competencias = c(
                "Actitudinales",
                "Básicas", 
                "Conductuales", 
                "Directivas/Gestión", 
                "Idiomas", 
                "Computacion",
                "Técnicas", 
                "Otras"),
        lower = c(
                mean(get_qi(sim(setx(z.out.precariedad, competencias=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=1))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=2))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=3)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=3))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=3))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=4)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=4))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=4))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=5)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=5))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=5))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=6)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=6))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=6))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=7)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=7))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=7))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=8)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=8))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=8)))))
        ),
        upper = c(
                mean(get_qi(sim(setx(z.out.precariedad, competencias=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=1))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=2))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=3)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=3))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=3))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=4)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=4))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=4))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=5)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=5))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=5))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=6)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=6))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=6))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=7)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=7))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=7))))),
                mean(get_qi(sim(setx(z.out.precariedad, competencias=8)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, competencias=8))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, competencias=8)))))
        )
)


# Plot
p_load(ggplot2)
ggplot(sim.precariedad.competencias, aes(x=Competencias, y=Probabilidad, fill=Competencias)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 



###############
# Predicciones por Vacantes
###############
set.seed(2020)
sim.precariedad.vacantes = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes))))),
                mean(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes)))))),
        Vacantes = c(
                "No",
                "Si (Max)"),
        lower = c(
                mean(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes))))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes)))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes)))))),
                mean(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes))))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes)))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes))))))
                
        ),
        upper = c(
                mean(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes))))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes)))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, vacantes=min(precariedad.d$vacantes)))))),
                mean(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes))))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes)))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, vacantes=max(precariedad.d$vacantes))))))
        )
)

# Plot
p_load(ggplot2)
ggplot(sim.precariedad.vacantes, aes(x=Vacantes, y=Probabilidad, fill=Vacantes)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
                      position=position_dodge(.9)) 


###############
# Predicciones por educacion.requerida
###############

set.seed(2020)
sim.precariedad.educacion.requerida = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1)))),
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2)))),
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3)))),
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4)))),
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5)))),
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6)))),
                mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7))))
                ),
                Educacion = c(
                        "Básica",
                        "CH",
                        "TP", 
                        "Postgrado",
                        "Profesional",
                        "Sin Requisito",
                        "Técnico Nivel Superior"
                        ),
                lower = c(
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7)))))
                        
                ),
                upper = c(
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=1))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=2))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=3))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=4))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=5))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=6))))),
                        mean(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, educacion.requerida=7)))))
                )
        )
        
# Plot
p_load(ggplot2)


ggplot(sim.precariedad.educacion.requerida, aes(x=Educacion, y=Probabilidad, fill=Educacion)) + 
geom_bar(stat="identity", color="black", 
         position=position_dodge()) +
geom_errorbar(aes(ymin=lower, ymax=upper), width=.2,
              position=position_dodge(.9)) 


###############
# Predicciones por capacitacion
###############

set.seed(2020)
sim.precariedad.capacitacion = data.frame(
        Probabilidad = c(
                mean(get_qi(sim(setx(z.out.precariedad, capacitacion=1)))),
                mean(get_qi(sim(setx(z.out.precariedad, capacitacion=2))))
        ),
        Capacitacion = c(
                "No",
                "Si"
        ),
        lower = c(
                mean(get_qi(sim(setx(z.out.precariedad, capacitacion=1)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, capacitacion=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, capacitacion=1))))),
                mean(get_qi(sim(setx(z.out.precariedad, capacitacion=2)))) - qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, capacitacion=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, capacitacion=2)))))
        ),
        upper = c(
                mean(get_qi(sim(setx(z.out.precariedad, capacitacion=1)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, capacitacion=1))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, capacitacion=1))))),
                mean(get_qi(sim(setx(z.out.precariedad, capacitacion=2)))) + qnorm(0.975)*sd(get_qi(sim(setx(z.out.precariedad, capacitacion=2))))/sqrt(length(get_qi(sim(setx(z.out.precariedad, capacitacion=2)))))
        )
)
        
# Plot
p_load(ggplot2)


ggplot(sim.precariedad.capacitacion, aes(x=Capacitacion, y=Probabilidad, fill=Capacitacion)) + 
        geom_bar(stat="identity", color="black", 
                 position=position_dodge()) +
        geom_errorbar(aes(ymin=lower, ymax=upper), width=.1,
                      position=position_dodge(.9)) 



