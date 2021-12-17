
##################################
# conditional frequencies ########
##################################

## area #################
area <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)), by=Area]
area$freq <- area$total_claim/area$total_exp
area = area[order(area$Area), ]
area_freq = as.numeric(area$freq)
names(area_freq) = as.character(area$Area)
area_den = as.numeric(area$total_claim)
names(area_den) = as.character(area$Area)

## vehicle power ##########
vehpower <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
              by=VehPower]
vehpower$freq <- vehpower$total_claim/vehpower$total_exp
vehpower = vehpower[order(vehpower$VehPower), ]
vehpower_freq = as.numeric(vehpower$freq)
names(vehpower_freq) = as.character(vehpower$VehPower)
vehpower_den = as.numeric(vehpower$total_claim)
names(vehpower_den) = as.character(vehpower$VehPower)

## vehicle age ########## 
vehage <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
            by=VehAge]
vehage$freq <- vehage$total_claim/vehage$total_exp
vehage = vehage[order(vehage$VehAge), ]
vehage_freq = as.numeric(vehage$freq)
names(vehage_freq) = as.character(vehage$VehAge)
vehage_den = as.numeric(vehage$total_claim)
names(vehage_den) = as.character(vehage$VehAge)

# driver age #############
drivage <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
             by=DrivAge]
drivage$freq <- drivage$total_claim/drivage$total_exp
drivage = drivage[order(drivage$DrivAge), ]
drivage_freq = as.numeric(drivage$freq)
names(drivage_freq) = as.character(drivage$DrivAge)
drivage_den = as.numeric(drivage$total_claim)
names(drivage_den) = as.character(drivage$DrivAge)

# vehicle brand ############
veh_brand <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
               by=VehBrand]
veh_brand$freq <- veh_brand$total_claim/veh_brand$total_exp
veh_brand = veh_brand[order(veh_brand$VehBrand), ]
vehbrand_freq = as.numeric(veh_brand$freq)
names(vehbrand_freq) = as.character(veh_brand$VehBrand)
vehbrand_den = as.numeric(veh_brand$total_claim)
names(vehbrand_den) = as.character(veh_brand$VehBrand)

# density ############# 
den_gruppi = cut( A$Density, 10, ordered_result = T)
A$den_ord = den_gruppi

density <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
             by=den_ord]
density$freq <- density$total_claim/density$total_exp
density = density[order(density$den_ord), ]
density_freq = as.numeric(density$freq)
names(density_freq) = 1:length(density$den_ord)
density_den = as.numeric(density$total_claim)
names(density_den) = 1:length(density$den_ord)

## gas ###########
vehgas <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
             by=VehGas]
vehgas$freq <- vehgas$total_claim/vehgas$total_exp
vehgas = vehgas[order(vehgas$VehGas), ]
vehgas_freq = as.numeric(vehgas$freq)
names(vehgas_freq) = as.character(vehgas$VehGas)

vehgas_den = as.numeric(vehgas$total_claim)
names(vehgas_den) = as.character(vehgas$VehGas)

## bonusmalus ##########
bm_gruppi = cut( A$BonusMalus, 10, ordered_result = T)
A$bm_ord = bm_gruppi
bonusmalus <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
            by=bm_ord]
bonusmalus = bonusmalus[order(bonusmalus$bm_ord), ]
bonusmalus$freq <- bonusmalus$total_claim/bonusmalus$total_exp
bonusmalus_freq = as.numeric(bonusmalus$freq)
names(bonusmalus_freq) = 1:length(bonusmalus$bm_ord)

bonusmalus_den = as.numeric(bonusmalus$total_claim)
names(bonusmalus_den) = 1:length(bonusmalus$bm_ord)

## region ###########
region <- A[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
                by=Region]
region$freq <- region$total_claim/region$total_exp
region = region[order(region$Region), ]
region_freq = as.numeric(region$freq)
names(region_freq) = as.character(region$Region)

region_den = as.numeric(region$total_claim)
names(region_den) = as.character(region$Region)

## save results ######
frequencies = list(Area = area_freq, 
                 VehPower =  vehpower_freq, 
                 VehAge = vehage_freq,
                 DrivAge = drivage_freq, 
                 VehBrand = vehbrand_freq, 
                 Density = density_freq, 
                 VehGas = vehgas_freq, 
                 BonusMalus = bonusmalus_freq, 
                 Region = region_freq)


densities = list(Area = area_den, 
                   VehPower =  vehpower_den, 
                   VehAge = vehage_den,
                   DrivAge = drivage_den, 
                   VehBrand = vehbrand_den, 
                   Density = density_den, 
                   VehGas = vehgas_den, 
                   BonusMalus = bonusmalus_den, 
                   Region = region_den)


## capped variables #########
vehage2 <- AA[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
              by=VehAge]
vehage2 = vehage2[order(vehage2$VehAge), ]
vehage2$freq <- vehage2$total_claim/vehage2$total_exp
v2 = vehage2$freq
names(v2) = vehage2$VehAge

drivage2 <- AA[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
               by=DrivAge]
drivage2 = drivage2[order(drivage2$DrivAge), ]
drivage2$freq <- drivage2$total_claim/drivage2$total_exp
d2 = drivage2$freq
names(d2) = drivage2$DrivAge

bonusmalus2 <- AA[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
                  by=BonusMalus]
bonusmalus2 = bonusmalus2[order(bonusmalus2$BonusMalus), ]
bonusmalus2$freq <- bonusmalus2$total_claim/bonusmalus2$total_exp
b2 = bonusmalus2$freq
names(b2) = bonusmalus2$BonusMalus

vehpower2 <- AA[,.(total_claim=sum(ClaimNb), total_exp=sum(Exposure), n=length(ClaimNb)),
                  by=VehPower]
vehpower2 = vehpower2[order(vehpower2$VehPower), ]
vehpower2$freq <- vehpower2$total_claim/vehpower2$total_exp
vp2 = vehpower2$freq
names(vp2) = vehpower2$VehPower

mod = list(VehAge = v2, 
           DrivAge = d2, 
           BonusMalus = b2, 
           VehPower = vp2)


