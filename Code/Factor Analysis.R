vparty <- rio::import(here::here("Data", "V-Party-V2.csv"))

#### SELECCIÓN DE CASOS Y VARIABLES 
paises <- c("Argentina", "Bolivia", "Brazil", "Chile", "Colombia", "Costa Rica",
            "Dominican Republic","Ecuador", "El Salvador","Guatemala", "Honduras", 
            "Mexico", "Nicaragua", "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela")

base <- vparty[vparty$country_name %in% paises,]
base <- base[base$year >2000,]
# v2paanteli: anti-elitism
# v2paplur: political pluralism
# v2paminor: minority rights
# v2paimmig: immigration
# v2palgbt: LGBT social equality
# v2paculsup: cultural superiority
# v2parelig: religious principles
# v2pagender: gender equality
# v2pariglef: economic left-right
# v2pawelf: welfare

base <- dplyr::select(base, v2paanteli, v2paplur, v2paminor, v2paimmig, v2palgbt,
               v2paculsup, v2parelig, v2pariglef, v2pawelf )

names(base) <- c("Anti.elite", "Pluralism", "Minorities", "Immigration", "LGBTQ",
                 "Cultural.Sup", "Religion", "Economic", "Welfare")


### CONSIDERACIONES PREVIAS

# Hay que eliminar los NAs
colSums(is.na(base))
base <- base[!is.na(base$Anti.elite),]
colSums(is.na(base))

# test de Kaiser–Meyer–Olkin (KMO) (deben ser MSA>0,49)
psych::KMO(base)

# Test de bartlett (H0: no correlated, no adecuado para PCA)
psych::cortest.bartlett(base)



## ANALISIS FACTORIAL
ev <- eigen(cor(base)) # eigenvalues
ev$values # Dos factores parecen ser suficientes
psych::scree(base, pc=FALSE) # Aquí también dos factores 
psych::fa.parallel(base, fa="fa") # El parallel analysis muestra tres


# Analisis de factores
Nfacs <- 2  # Vamos a ir por dos factores
# la rotación promax no supone independencia lineal
fit <- factanal(base, Nfacs, rotation = "promax")
print(fit, digits=2, cutoff=0.1, sort=TRUE)
# la matriz de correlación de factores muestra que tal vez fue buena idea no asumir independencia

# Gráfico de factores
load <- fit$loadings[,1:2]
plot(load,type="n") # set up plot
text(load,labels=names(base),cex=.7)

# Diagrama de factores
loads <- fit$loadings
psych::fa.diagram(loads, cut = 0.3, family="Cambria") 


jpeg(filename = here::here("Figures","factores_eng.jpg"), 
     width = 1200, height = 1500, res = 300)
psych::fa.diagram(loads, main = "",cut = 0.1, e.size = 0.05, ic = F, rsize = 0.3, )
dev.off()


jpeg(filename = here::here("Figures","scree.jpg"), 
     width = 1500, height = 1200, res = 300)
psych::scree(base, pc=FALSE, main="")
dev.off()

