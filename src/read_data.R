## title: reading data
## date: 27.03.2020
## author: trond

## house keeping
library(data.table)
library(cbsodataR)
library(ggplot2)
library(gnm)

## read data
cbs_dt <- data.table(cbs_get_toc(Language='nl'))

## migration data
mig_dt <- data.table(cbs_get_data('60048ned',
                                  RegioS = has_substring('GM') | has_substring('NL')
                                  )
                     )[,
                       c(1L:2L, 23L:33L, 47L:57L, 161L:171L, 173L:183L)
                       ]

mig_dt[grepl('NL01', RegioS) & grepl('2018', Perioden), ]

## population data
pop_dt <- data.table(cbs_get_data('70648ned',
                     RegioS = has_substring('GM') | has_substring('NL')
                     ))

## create slightly more intelligent names
new_names <- data.table(a = rep(unlist(lapply(c('binnen', 'buiten'),
                                          function(x) {
                                              paste0(x, c('_mannen', '_vrouwen'))
                                          })
                                   ), each = 11),
                        b = unlist(lapply(strsplit(names(mig_dt)[-c(1,2)], split = "_"),
                                          function(x) {
                                              x[2]
                                          }))
                        )[,
                          new_names := paste(a, b, sep = '_')
                          ]

setnames(mig_dt, 3:ncol(mig_dt), new_names$new_names)

## long 
mig_dt_long <- melt(mig_dt,
                    id.vars = c('RegioS', 'Perioden')
                    )[!is.na(value),
                      ][,
                        idx:=1:.N
                        ][,
                          ':=' (
                              type = strsplit(as.character(variable), split = '_')[[1]][1],
                              geslacht = strsplit(as.character(variable), split = '_')[[1]][2],
                              leeftijd = strsplit(as.character(variable), split = '_')[[1]][3]
                          ),
                          by=idx
                          ][,
                            idx := NULL
                            ]

## sum over binnen en buiten
mig_dt_long <- mig_dt_long[Perioden %in% paste0(seq(1995, 2018), 'JJ00'),
                           sum(value),
                           by=c('RegioS','Perioden','geslacht','leeftijd')
                           ]

## lee carter model
EXPO <- read.table(
  "http://freakonometrics.free.fr/Exposures-France.txt",
  header=TRUE,skip=2)
DEATH <- read.table(
  "http://freakonometrics.free.fr/Deces-France.txt",
  header=TRUE,skip=0) ### !!!! 0
base=data.frame(
  D=DEATH$Total,
  E=EXPO$Total,
  X=as.factor(EXPO$Age),
  T=as.factor(EXPO$Year))
library(gnm)
listeage=c(101:109,"110+")
sousbase=base[! base$X %in% listeage,]
 # on met des nombres car il faut calculer T-X
sousbase$X=as.numeric(as.character(sousbase$X))
sousbase$T=as.numeric(as.character(sousbase$T))
sousbase$C=sousbase$T-sousbase$X
sousbase$E=pmax(sousbase$E,sousbase$D)


              



## woon data
woon_dt <- data.table(cbs_get_data('70072ned',
                                   RegioS = has_substring('GM') | has_substring('NL'),
                                   select = c('Perioden', 'RegioS', 'Nieuwbouwwoningen_91')
                                   )
                      )[!is.na(Nieuwbouwwoningen_91),
                        ]




## 



