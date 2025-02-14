
packageList <- c('Matrix','wesanderson','gstlearn')
source("loadingLibraries.R")
source("loadingAddsOnFunctions.R")


### modèle de base R1=R2=0.25
### R1 et R2 modifiés en fonction des variables d'une DB pour avoir R1=5 et R2 = 10
testModel = Model_createFromParam(type=ECov_MATERN(),ranges=c(0.25,0.25))#,param=1, ranges=c(1,1),sill=1)
testGrid <- DbGrid_create(x0=c(0,0),dx=c(1,1),nx=c(100,100))
testGrid['r1'] <- rep(5,10000)
testGrid['r2'] <- rep(20,10000)
testModel$getCova(0)$makeRangeNoStatDb("r1",0,db=testGrid)
testModel$getCova(0)$makeRangeNoStatDb("r2",1,db=testGrid)
spdeSimu <- simulateSPDE(NULL,testGrid,testModel,NULL,1)
plotGrid(testGrid,'SimuSPDE')
# ==> simu semble  cohérente avec le modèle modifié par les portées issues de la grille
# ==> l'anisotropie est bien orientée 

### modèle de base R1=R2=100
### R1 et R2 modifiés comme précédemment
testModel = Model_createFromParam(type=ECov_MATERN(),ranges=c(100,100))#,param=1, ranges=c(1,1),sill=1)
testGrid <- DbGrid_create(x0=c(0,0),dx=c(1,1),nx=c(100,100))
testGrid['r1'] <- rep(5,10000)
testGrid['r2'] <- rep(20,10000)
testModel$getCova(0)$makeRangeNoStatDb("r1",0,db=testGrid)
testModel$getCova(0)$makeRangeNoStatDb("r2",1,db=testGrid)
spdeSimu <- simulateSPDE(NULL,testGrid,testModel,NULL,1)
plotGrid(testGrid,'SimuSPDE')
# ==> simu n'est plus du toutr cohérente avec le modèle modifié par les portées issues de la grille
# ==> il y a interférence entre les portées indiquées on moment de générer le modèle et celle alimentant les non stationnarités


