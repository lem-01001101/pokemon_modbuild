## Setup Variables / Load Data ##

library(SignifReg)
library(MASS)
library(car)

pokemon = read.csv("pokemon_updated.csv",header=TRUE)
attach(pokemon)

scope1 = ~ attack + defense + sp_attack + sp_defense +  base_egg_steps + base_happiness + base_total + cap_rate +
 experience_growth + hp + speed + is_legendary + against_bug + against_dark + against_dragon + against_electric +
 against_fairy + against_fight +against_fire +against_flying +against_ghost + against_grass + against_ground +
 against_ice + against_normal + against_poison + against_psychic +against_rock +against_steel +against_water

## Building the model ##

m1 = SignifReg(null_model,direction="both",criterion="r-adj",scope=scope1)


## Boxcox Transformation ##

bc = boxcox(m1)
lambda <- bc$x[which.max(bc$y)]

bc_m1 = lm((weight_kg^lambda - 1)/lambda ~ base_total + base_happiness + speed + hp + against_poison +
 is_legendary + sp_attack + against_water)


## Check Diagnostics ##

summary(bc_m1)
vif(bc_m1)
plot(bc_m1)



heightmodel <- lm(height_m ~ against_bug + against_dark + against_dragon + against_electric + against_fairy + against_fight + against_fire + against_flying + against_ghost + against_grass + against_ground + against_ice + against_normal + against_poison + against_psychic + against_rock + against_steel + against_water + attack + base_egg_steps + base_happiness + base_total + cap_rate + hp + sp_defense + speed + type1+ type2 + generation + is_legendary + weight_kg + experience_growth + defense + sp_attack)
nullmodel <- lm(height_m ~ 1)
scope = list(lower=formula(nullmodel), upper=formula(heightmodel) )

select = SignifReg(nullmodel, scope = scope, direction = "both", trace = FALSE, criterion = "r-adj", alpha = 1)
vif(select)
AIC(select)
par(mfrow=c(2,2))
plot(select)
summary(select)

selectF = SignifReg(nullmodel, scope = scope, direction = "both", trace = FALSE, criterion = "r-adj", alpha = 1)
vif(selectF)
AIC(selectF)
par(mfrow=c(2,2))
plot(selectF)
summary(selectF)


bc = boxcox(select) ##since select and selectF are the same model
lambda <- bc$x[which.max(bc$y)]
bc_height = lm((height_m^lambda - 1)/lambda ~ weight_kg + base_total + hp + generation + base_happiness + type2 +against_electric + against_psychic + sp_defense + is_legendary + experience_growth + against_ice + against_grass)

summary(bc_height)
vif(bc_height)
par(mfrow=c(2,2))
plot(bc_height)
vif(bc_height)
