code.dir <- "./code"
data.dir <- "./data"

dat <- read.csv(file=sprintf("%s/2013_11_01_MIA_BKN.csv", data.dir))

source(sprintf("%s/constants.R", code.dir)) # loads libraries and constants used throughout code
source(sprintf("%s/graphics.R", code.dir))  # graphics/plotting functions
par(mar=c(0, 0, 0, 0))
data.plotter(dat, 1800)

source(sprintf("%s/data_formatting.R", code.dir))
source(sprintf("%s/covariates.R", code.dir))

poss <- possession.indicator(dat) # infer ballcarrier... takes about a minute
tdat <- rearrange.data(dat, poss) # re-shuffle columns by to ballcarrier... (2 min)
tdat <- offensive.halfcourt(tdat) # transforming to offensive halfcourt
tdat <- offensive.ballcarrier(tdat)
touchID <- get.touchID(tdat)
covariates <- getAllCovars(tdat) # get covariates... (3 min)
tdat <- data.frame(tdat, touchID=touchID, covariates)
save(tdat, file=sprintf("%s/tdat.Rdata", data.dir))

load(sprintf("%s/playerbases.Rdata", data.dir))
players <- read.csv(sprintf("%s/players2013.csv", data.dir))
head(players)

par(mfrow=c(1,5))
for(i in 1:5)
  spatialPlot0(df[i, ], legend=F)


par(mfrow=c(1,5))
for(i in 1:5)
  spatialPlot0(nmf.basis[i, ], legend=F)

df.lowrank <- nmf.coef %*% nmf.basis
par(mfrow=c(1,5))
for(i in 1:5)
  spatialPlot0(df.lowrank[i, ], legend=F)

K <- matrix(NA, nrow=nrow(df), ncol=nrow(df))
for(i in 1:nrow(K)){
  this.coef <- nmf.coef[i, ] / sum(nmf.coef[i, ])
  K[i, ] <- apply(nmf.coef, 1, function(r) sum((r / sum(r) - this.coef)^2))
}
H <- 0 * K
for(i in 1:nrow(H)){
  inds <- order(K[i, ])[1:8 + 1]
  H[i,inds] <- H[inds, i] <- 1
}

this.player <- grep("Horford", players$lastname)
paste(players$firstname, players$lastname)[which(H[this.player, ] == 1)]

par(mfrow = c(2,5))
for(i in 1:10)
  spatialPlot1(take.basis[i, ], legend=F)

player.id <- players$player_id[which(players$firstname == "LeBron")]
load(sprintf("%s/micros/%s.Rdata", data.dir, player.id))
# x component of LeBron James' micro model during ball possession
xtable(with.ball$io.x$summary.fixed[, 1:5]) 

par(mfrow=c(1,2), mar=c(0,0,0,0))
vectorPlot(with.ball)
vectorPlot(without.ball)
