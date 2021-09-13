## title: read in data from cbs and adjust to 2016 code
## date: 27.03.2020
## author: trond


## read cbs data
cbs_dt <- data.table(cbs_get_toc(Language='nl'))
##cbs_get_meta('60048ned')

## migration data
## binnen mannen: 33:43
## binnen vrouwen: 45:55
## buiten vestiging manne: 102:112
## buiten vestigin vrouwen: 114:124
## buiten vertrek manne: 170:181
## buiten vertrek vrouwen: 182:193

mig_dt <- data.table(cbs_get_data('60048ned',
                                  ##RegioS = has_substring('CR') | has_substring('NL')
                                  RegioS = has_substring('GM') | has_substring('NL')
                                  )
                     )[,
                       c(1L:2L, 34L:57L, 103L:126L, 172:195)
                       ]

## population data
pop_dt <- data.table(cbs_get_data('37713',
                                  Migratieachtergrond = "T001040",
                                  ##RegioS = has_substring('CR') | has_substring('NL')
                                  RegioS = has_substring('GM') | has_substring('NL')
                                  ))

## woning data
woon_dt <- data.table(cbs_get_data('70072ned',
                                   ##RegioS = has_substring('CR') | has_substring('NL'),
                                   RegioS = has_substring('GM') | has_substring('NL'),
                                   select = c('Perioden', 'RegioS', 'VoorraadOp1Januari_90')
                                   )
                      )[!is.na(VoorraadOp1Januari_90),
                        ][,
                          ':=' (
                              year = as.numeric(gsub('JJ00', '', Perioden)),
                              code = as.numeric(substr(RegioS, 3, nchar(RegioS)))
                          )
                          ]
## data wrangling

## create slightly more intelligent names (1)
new_names <- data.table(a = rep(unlist(lapply(c('binnen', 'buiten.vestiging', 'buiten.vertrek'),
                                          function(x) {
                                              paste0(x, c('_mannen', '_vrouwen'))
                                          })
                                   ), each = 12),
                        b = unlist(lapply(strsplit(names(mig_dt)[-c(1,2)], split = "_"),
                                          function(x) {
                                              if(grepl('Totaal', x[1])) {
                                                  'Totaal'
                                              } else {
                                                  x[2]
                                              }
                                          }))
                        )[,
                          new_names := paste(a, b, sep = '_')
                          ]

setnames(mig_dt, 3:ncol(mig_dt), new_names$new_names)

## create slightly more intelligent names (2)
mig_ag_groups <- unique(new_names$b)
pop_dt_ag <- data.table(names(attributes(pop_dt$Leeftijd)$labels),
                        attributes(pop_dt$Leeftijd)$labels
                        )[,
                          dict :=
                              c(mig_ag_groups[1:7], rep(mig_ag_groups[8], 2),
                                rep(mig_ag_groups[9], 2), rep(mig_ag_groups[10], 3),
                                rep(mig_ag_groups[11], 4), rep(mig_ag_groups[12], 3)
                                ) 
                          ]

setkey(pop_dt, Leeftijd)
setkey(pop_dt_ag, V2)
pop_dt[pop_dt_ag, Leeftijd_groep := i.dict]

## long (1)
mig_dt_long <- melt(mig_dt,
                    id.vars = c('RegioS', 'Perioden')
                    )[!is.na(value),
                      ][,
                        c('type', 'geslacht', 'leeftijd') := tstrsplit(as.character(variable),
                                                                       split = '_')
                        ][,
                          ':=' (code = as.numeric(substr(RegioS, 3, nchar(RegioS))),
                                year = as.numeric(substr(Perioden, 1, 4))
                                )
                          ]

## long (2)
pop_dt_long <- melt(pop_dt[,
                      .(RegioS, Perioden, leeftijd = Leeftijd_groep,
                        mannen = Mannen_2, vrouwen = Vrouwen_3)
                      ],
                    id.vars = c('RegioS', 'Perioden', 'leeftijd')
                    )[,
                      ':=' (##year = as.numeric(gsub('JJ00', '', Perioden)) - 1,
                          year = as.numeric(gsub('JJ00', '', Perioden)),
                          code = as.numeric(substr(RegioS, 3, nchar(RegioS)))
                      )
                      ][,
                        sum(value),
                        by = c('code', 'year', 'leeftijd', 'variable')
                        ]

## data on municipality recodes and existing municipality codes every year
code_changes <- data.table(read_excel('../data/Recode Municipalities - Vector.xlsm', sheet = 3))[,c(1L:7L)]
codes_year <- mig_dt_long[code != 1, unique(code), by = year]

## calculate fractions of population for using the municipality recoder backwards
pop_tot <- pop_dt_long[!is.na(V1) & code != 1 & leeftijd == 'Totaal', sum(V1), by = c('year', 'code')]
pop_tot[, year := year + 1]
setkey(code_changes, jaar, oud_code)
setkey(pop_tot, year, code)
code_changes[pop_tot,
             pop := i.V1
             ][!is.na(pop),
               back_frac := pop / sum(pop),
               by = c('jaar', 'nieuw_code')
               ][,
                 change_date := as.IDate(paste(jaar, maand, dag, sep = '-'))
                 ]

## recode migration municipality
mig_dt_2018gem <- lapply(seq(1995, 2017),
                         function(x) {
                             mig_dt_long[RegioS != 'NL01  ' & year == x & type != 'buiten.vertrek',
                                         recode_vector_recursive(dat = .SD,
                                                                 var_name = 'value',
                                                                 in_year = x,
                                                                 out_year = 2018),
                                         by = c('type', 'geslacht', 'leeftijd')
                                         ]
                         })

## sum departures and internal moves
mig_dt_2018gem  <- rbindlist(mig_dt_2018gem)[,
                                             sum(value), ## sum over binnen and buiten
                                             by=c('code','year','geslacht','leeftijd')
                                             ]

## recode population municipality
pop_dt_2018gem <- lapply(seq(1995, 2018),
                         function(x) {
                             pop_dt_long[code != 1 & !is.na(V1) & year == x,
                                         recode_vector_recursive(dat = .SD,
                                                                 var_name = 'V1',
                                                                 in_year = x,
                                                                 out_year = 2018),
                                         by = c('variable', 'leeftijd')
                                         ]
                         })

## calculate population at risk
pop_dt_2018gem  <- rbindlist(pop_dt_2018gem)[,
                                             year := year - 1 ## measured on 01.01
                                             ][,
                                               risk_pop := (V1 + shift(V1))/2,
                                               by = c('code','leeftijd', 'variable')
                                               ]

## recode woon dt to 2018 municipality
woon_dt_2018gem <- lapply(seq(1995, 2018),
                          function(x) {
                              woon_dt[code != 1 & year == x,
                                    recode_vector_recursive(dat = .SD,
                                                            var_name = 'VoorraadOp1Januari_90',
                                                            in_year = x,
                                                            out_year = 2018)
                                    ]
                         })

woon_dt_2018gem  <- rbindlist(woon_dt_2018gem)[,
                                               year := year - 1 ## measured on 01.01
                                               ]

## merge
inp_dt <- merge(pop_dt_2018gem[!is.na(risk_pop)],
                mig_dt_2018gem,
                by.x = c('code', 'year', 'leeftijd', 'variable'),
                by.y = c('code', 'year', 'leeftijd', 'geslacht'),
                all.x = T
                )[,
                  leeftijd := factor(leeftijd, levels = unique(mig_dt_long$leeftijd))
                  ][is.na(V1.y) & !is.na(risk_pop),
                    V1.y := 0
                    ]

setnames(inp_dt,
         c('variable', 'V1.x', 'V1.y'),
         c('geslacht', 'bevolking', 'verhuizingen')
         )

## add population data to housing model and calculate 3-year rolling average
inp_wn_dt <- merge(woon_dt_2018gem,
                 inp_dt[leeftijd == 'Totaal',
                        .(bevolking = sum(bevolking)),
                        by = c('code', 'year')
                        ],
                 by = c('code', 'year')
                 )[,
                   woningen_rmean :=  frollmean(VoorraadOp1Januari_90, n = 3),
                   by = c('code')
                   ]


## calculate regional factor
inp_dt[!is.na(risk_pop),
       verhuiskans := sum(verhuizingen[leeftijd != 'Totaal']) / sum(risk_pop[leeftijd != 'Totaal']),
       by = c('year', 'geslacht', 'leeftijd')
       ][,
         estimated_moves := verhuiskans * risk_pop
         ][,
           moves_sum := verhuizingen[leeftijd == 'Totaal'],
           by = c('year', 'geslacht', 'code')
           ][leeftijd != 'Totaal',
             moves_rmean := frollmean(moves_sum, n = 3, na.rm = T),
             by = c('geslacht', 'leeftijd', 'code')
             ][leeftijd != 'Totaal',
               regional_factor := moves_rmean / sum(estimated_moves, na.rm = T),
               by = c('year', 'geslacht', 'code')
               ]

## end of code
