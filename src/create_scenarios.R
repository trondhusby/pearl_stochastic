## title: creating stochastic scenarios for pearl
## author: trond
## date: 07.07.2020

## for reproducibility
set.seed(123)

## house keeping
library(data.table)
library(cbsodataR)
library(ggplot2)
library(gnm)
library(knitr)
library(forecast)
library(gridExtra)
library(readxl)

## some functions
source('handy_functions.R')


## read in data from CBS and convert to 2016 list
if (!file.exists("../output/inp_data.RData")) {
    source("read_data.R")
    save(list = c("pop_dt_2018gem", "mig_dt_2018gem",
                  "woon_dt_2018gem", "inp_dt", "inp_wn_dt"),
         file = "../output/inp_data.RData")
} else {
    load("../output/inp_data.RData")
}

## read in data on net growth housing supply and regional factor from PEARL
zip_file <- "../data/Input_PEARL_2019.zip"

## function for parsing data
read_pearl_input <- function(col_nms, inp_nm){
    data.table(read.table(unz(zip_file, inp_nm),
                          header = F, sep = '\t') ## read everything into one column
               )[-c(1:2),
                 ][,
                   idx := 1:.N
                   ][,
                     c(col_nms) := tstrsplit(as.character(V1), split = "\\s+", fixed = F),
                     by = idx
                     ][,
                       c(col_nms) := lapply(.SD, as.numeric),
                       .SDcols = c(col_nms)
                       ][,
                         -c('V1', 'idx', 'blank')
                         ]
}


## regionale factor
regfactor_nm <- grep("InternalMigrationRegionalFactor",
                     unzip(zip_file, list = T)$Name,
                     value = T)

regfactor_dt <- read_pearl_input(c('blank', 'year', 'code', 'geslacht', 'rf'),
                                 regfactor_nm[2]
                                 )

## extend to 2050
## regfactor_ex <- copy(regfactor_dt[rep(seq(1, length(unique(code))), 2050 - 2025),
##                                   ])[,
##                                      year := rep(seq(2026, 2050), each = 2*length(unique(code)))
##                                      ]

## regfactor_dt <- rbindlist(list(regfactor_dt, regfactor_ex))


## housing
netgrowth_supply_nm <- grep("HousingSupply",
                            unzip(zip_file, list = T)$Name,
                            value = T)[2]

netgrowth_supply_dt <- read_pearl_input(c('blank', 'year', 'code', 'w'),
                                 netgrowth_supply_nm
                                 )

## estimate models: GLMS


## set starting values for the regional factor trend
start_krf <- inp_dt[leeftijd != 'Totaal' & year > 1997,
                      mean(regional_factor),
                      by = year
                    ][,
                      log(V1 / mean(V1))
                      ]

## find number of NAs for initialisation
n_nas <- ncol(model.matrix(~as.factor(code) + as.factor(geslacht) + as.factor(code)*year,
                           data = inp_dt[leeftijd != 'Totaal' & year > 1997,
                                         .(regional_factor = unique(regional_factor)),
                                         by = c('year', 'code', 'geslacht')
                                        ]))

## models for regional factor
LC.gaussian.reg <- gnm(regional_factor ~
                           as.factor(code) +
                           as.factor(geslacht) +
                           Mult(as.factor(code), as.factor(year)),
                        family = gaussian(link = 'log'),
                        start = c(rep(NA, n_nas), start_krf),
                        data = inp_dt[leeftijd != 'Totaal' & year > 1997,
                                     .(regional_factor = unique(regional_factor)),
                                     by = c('year', 'code', 'geslacht')
                                    ]
                        )

## find index of time-trend coefficients
tt_names <- paste0("Mult(as.factor(code), .).",
                   paste0("as.factor(year)", 1998:2017)
                   )
tt_ind_r <- match(tt_names, names(coefficients(LC.gaussian.reg)))

## estimate model used for prediction
LC.gaussian.reg.pred <- glm(regional_factor ~
                                  as.factor(code) +
                                  as.factor(geslacht) + 
                            as.factor(code)*k,
                            family=gaussian(link = 'log'),
                            data =merge(
                                inp_dt[leeftijd != 'Totaal' & year > 1997,
                                       .(regional_factor = unique(regional_factor)),
                                       by = c('year', 'code', 'geslacht')
                                       ],
                                data.table(year = seq(1998, 2017),
                                           k = as.numeric(coefficients(LC.gaussian.reg)[tt_ind_r])),
                                by = 'year'
                            ))

## set starting values for the housing construction trend
start_k_w <- inp_wn_dt[!is.na(woningen_rmean),
                     mean(woningen_rmean),
                     by = year
                     ][,
                       ##log(V1 / mean(V1))
                       normalise_vec(V1)
                       ]

             
## estimate model
LC.poisson.h <- gnm(woningen_rmean ~
                            log(offset(bevolking)) +
                            as.factor(code)+
                            Mult(as.factor(code), as.factor(year)),
                        family=poisson(link = 'log'),
                        start = c(rep(NA, n_nas), start_k_w),
                        data= inp_wn_dt[!is.na(woningen_rmean)]
                    )

## find index of time-trend coefficients
tt_ind_h <- match(tt_names, names(coefficients(LC.poisson.h)))

LC.poisson.pred.h <- glm(woningen_rmean ~
                            log(offset(bevolking)) +
                            as.factor(code)+
                            as.factor(code)*k,
                        family=poisson(link = 'log'),
                        data=merge(inp_wn_dt,
                                   data.table(year = seq(1998, 2017),
                                              k = as.numeric(coefficients(LC.poisson.h)[tt_ind_h])),
                                   by = 'year')
                        )

## estimate models: ETS

## simulations settings
sim_len_rf <- length(seq(2018, 2050))
sim_len_w <- length(seq(2018, 2059))
n_sim <- 1000

## create time series object of trend variables
y_r <- ts(as.numeric(coefficients(LC.gaussian.reg)[tt_ind_r]),
          start = 1998)

y_w <- ts(as.numeric(coefficients(LC.poisson.h)[tt_ind_h]),
          start = 1998)

## estimate time series models
rf_mod <- ets(y_r, model = 'ANN')
w_mod <- ets(y_w, model = 'ANN')

## simulations
sims <- lapply(seq(1, n_sim),
               function(x) {
                   data.table(w = simulate(w_mod, nsim = sim_len_w),
                              rf = c(simulate(rf_mod, nsim = sim_len_rf),
                                           rep(NA, sim_len_w - sim_len_rf)
                                           )
                              )
               })

## temporary data frame for simulation: regionale factor
tmp_dt <- rbindlist(lapply(seq(1, sim_len_rf),
                           function(x){
                               inp_dt[year == 2017 & leeftijd == 'Totaal',
                                          .(code, year, leeftijd, geslacht)
                                          ][,
                                            year := 2017 + x
                                            ]
                           }))

sim_regio <- rbindlist(lapply(seq(1,n_sim),
                              function(x){
                                  k <- sims[[x]]$rf[!is.na(sims[[x]]$rf)]
                                  rbind(
                                      data.table(tmp_dt[geslacht == 'mannen', ],
                                                 run = x,
                                                 k = rep(as.numeric(k),
                                                         each = length(unique(tmp_dt$code)))
                                                 ),
                                      data.table(tmp_dt[geslacht == 'vrouwen', ],
                                                 run = x,
                                                 k = rep(as.numeric(k),
                                                         each = length(unique(tmp_dt$code)))
                                                 )
                                  )
                              })
                       )

## use estimated models to forecast regional moves
sim_regio[,
          regional_factor_k := exp(predict(LC.gaussian.reg.pred, newdata=.SD)),
          by = run            
          ][,
            ':=' (k_old = k, k = mean(k)),
            by = c('code', 'year', 'geslacht')
            ][,
              regional_factor_mean := exp(predict(LC.gaussian.reg.pred, newdata=.SD)),
              by = run
              ]

## temporary data frame for simulation
tmp_dt <- rbindlist(lapply(seq(1, sim_len_w),
                           function(x){
                             data.table(inp_wn_dt[!is.na(woningen_rmean)],
                                        fitted(LC.poisson.h)
                                        )[year == 2017,
                                          ][,
                                            year := 2017 + x
                                            ]
                           }))

sim_woning <- rbindlist(lapply(seq(1,n_sim),
                              function(x){
                                  k_w <- sims[[x]]$w
                                  data.table(tmp_dt,
                                             run = x,
                                             k = rep(as.numeric(k_w),
                                                     each = length(unique(tmp_dt$code)))
                                             )
                              })
                        )

## use estimated models to forecast regional housing construction
sim_woning[,
      nieuwbouw_k := exp(predict(LC.poisson.pred.h, newdata=.SD)),
      by = run            
      ][,
        ':=' (k_old = k, k = mean(k)),
        by = c('code', 'year')
        ][,
          nieuwbouw_mean := exp(predict(LC.poisson.pred.h, newdata=.SD)),
          by = run
          ][,
            ':=' (net_growth_k = ifelse(year == 2018,
                                        nieuwbouw_k - V2,
                                        nieuwbouw_k - shift(nieuwbouw_k)
                                        ),
                  net_growth_mean = ifelse(year == 2018,
                                           nieuwbouw_mean - V2,
                                           nieuwbouw_mean - shift(nieuwbouw_mean)
                                           )
                  ),
            by = .(code, run)
            ][,
              ':=' (woningen_k = V2 + cumsum(net_growth_k),
                    woningen_mean = V2 + cumsum(net_growth_mean)
                    ),
              by = .(code, run)
              ]

## adjust mean
sim_regio[, g_numeric := ifelse(geslacht == 'mannen', 1, 2)]
setkey(regfactor_dt, year, code, geslacht)
setkey(sim_regio, year, code, g_numeric)
sim_regio[regfactor_dt,
          pearl_input := i.rf
          ][,
            pearl_input := ifelse(is.na(pearl_input), pearl_input[year == 2025], pearl_input),
            by = .(code, geslacht)
            ]
setkey(netgrowth_supply_dt, year, code)
setkey(sim_woning, year, code)
sim_woning[netgrowth_supply_dt, pearl_input := i.w]

## scaling function
mse_fn <- function(w, x, y) {
    mean((y - sum(w*x)/n_sim)^2)
}

## scale the regional factor
sim_regio[,
          adj_factor := optim(par = rep(1, .N),
                              mse_fn,
                              x = regional_factor_k,
                              y = pearl_input,
                              method = "L-BFGS-B",
                              lower = 0,
                              upper = 5
                              )$par,
          by = .(code, geslacht, year)
          ]

## scale woning
sim_woning[,
          adj_factor := optim(par = rep(1, .N),
                              mse_fn,
                              x = net_growth_k,
                              y = pearl_input,
                              method = "L-BFGS-B",
                              upper = 10,
                              lower = -10
                              )$par,
          by = .(code, year)
          ]

## save stuff
saveRDS(sim_regio, '../output/sim_regio.rds')
saveRDS(sim_woning, '../output/sim_woning.rds')

## sim_regio <- readRDS('../output/sim_regio_bak.rds')

## plots
melt(sim_regio[code %in% c(363, 599, 715, 917) & run < 200,
                 .(code, year, run, geslacht, regional_factor_k, regional_factor_k*adj_factor)
                 ],
            id.vars = c('code', 'year', 'geslacht', 'run')
     ) %>%
    ggplot(., aes(year, value)) +
    geom_line(aes(group = run)) + 
    facet_wrap(variable~code + geslacht, scales = 'free') +
    geom_smooth(method = 'lm') + 
    theme_bw()


## Adam (363), roffa (599), terneuzen (715) en heerlen (917)
melt(sim_woning[code %in% c(363, 599, 715, 917) & run < 200,
                 .(code, year, run, net_growth_k, net_growth_k*adj_factor)
                 ],
     id.vars = c('code', 'year', 'run')
     ) %>%
    ggplot(., aes(year, value)) +
    geom_line(aes(group = run)) + 
    facet_wrap(variable~code, scales = 'free') +
    geom_smooth(method = 'lm') + 
    theme_bw()



