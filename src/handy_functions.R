## min max normalisation
normalise_vec <- function(x) {
    (x - min(x)) / (max(x) - min(x))
}

reverse_normalise_vec <- function(x, y) {
    y*(max(x) - min(x))+max(x)-1
}

## recoding municipalities vector year on year
recode_vector_year_on_year <- function(dat, var_name, in_year, out_year) {
    if (abs(out_year - in_year) != 1) stop("Not year on year input")
    ## cat(paste('out year', out_year), '\n')
    direction <- ifelse(in_year < out_year, "forward", "backward")
    ## data
    tmp1 <- dat[!is.na(get(var_name)),
               c('code', 'year', var_name),
               with=F]
    ## municipality codes in the out year
    mun_codes <- codes_year[year==out_year]$V1
    ## recode data
    if (direction == "forward") {
        start_d <- as.IDate(paste0(in_year, '-01-01'))
        end_d <- as.IDate(paste0(out_year, '-01-01'))
        ## subset code changes
        rec_dt <- code_changes[change_date %between% c(start_d, end_d),
                               .(jaar, change_date, oud_code, frac, back_frac, nieuw_code, maand, dag)
                               ]
        ## merge 
        tmp2 <- merge(rec_dt,
                 tmp1,
                 by.x = 'oud_code', by.y = 'code', all.x = T, all.y = F
                 )[,
                   .(code = nieuw_code, year, get(var_name) * frac)
                   ][V3>0,
                     sum(V3),
                     by = c('code', 'year')
                     ]
        ## gather
        out <- unique(rbindlist(list(tmp2[code %in% mun_codes],
                                     tmp1[code %in% mun_codes]
                                     ),
                                use.names = F
                                )[,
                                  sum(V1),
                                  by = c('code', 'year')
                                  ])
    } else if (direction == "backward") {
        start_d <- as.IDate(paste0(out_year, '-12-31'))
        end_d <- as.IDate(paste0(in_year, '-12-31'))
        ## subset code changes
        rec_dt <- code_changes[change_date %between% c(start_d, end_d),
                           .(jaar, change_date, oud_code, frac, back_frac, nieuw_code, maand, dag)
                           ]
        ## merge
        tmp2 <- merge(rec_dt,
                 tmp1,
                 by.x = 'nieuw_code', by.y = 'code', all.x = T, all.y = F
                 )[,
                   .(code = oud_code, year, get(var_name) * back_frac)
                   ][V3>0,
                     sum(V3),
                     by = c('code', 'year')
                     ]
        ## combine with non-modified data
        ##redundant_codes <- rec_dt[!frac == 0 | oud_code %in% nieuw_code, unique(nieuw_code)]
        ## gather
        out <- unique(rbindlist(list(tmp2[code %in% mun_codes],
                                     tmp1[code %in% mun_codes]
                                     ),
                                use.names = F
                                )[,
                                  sum(V1),
                                  by = c('code', 'year')
                                  ])
    }
    setnames(out, c('code', 'year', var_name))
    return(out)
}
    
## recoding municipalities vector year on year
recode_vector_recursive <- function(dat, var_name, in_year, out_year) {
    ## initialisation
    in_year_tmp <- in_year
    tmp <- dat
    if (out_year == in_year_tmp) {
        out <- tmp[!is.na(get(var_name)),
               c('code', 'year', var_name),
               with=F]
    } else {
        ## forward pass: update codes recursively each year
        while(out_year - in_year_tmp > 0) {
            if (out_year - in_year_tmp > 1) {
                tmp <- recode_vector_recursive(tmp,
                                               var_name,
                                               in_year_tmp,
                                               in_year_tmp + 1)
            } else {
                out <- recode_vector_year_on_year(tmp,
                                                  var_name,
                                                  in_year_tmp,
                                                  out_year)
            }
            in_year_tmp <- in_year_tmp + 1
        }
        ## backward pass: update codes recursively each year
        while(out_year - in_year_tmp < 0) {
            if (abs(out_year - in_year_tmp) > 1) {
                tmp <- recode_vector_recursive(tmp,
                                               var_name,
                                               in_year_tmp,
                                               in_year_tmp - 1)
            } else {
                out <- recode_vector_year_on_year(tmp,
                                                  var_name,
                                                  in_year_tmp,
                                                  out_year)
            }
            in_year_tmp <- in_year_tmp - 1
        }
    }
    ## update year
    out[, year := in_year]
    return(out)
}
    

## function for recoding municipalities (vector version)
recode_vec <- function(dat, var_name, in_year, out_year) {
    out <- dat[year==in_year & !is.na(get(var_name)),
               c('code', 'year', var_name),
               with=F]
    for (yr in seq(in_year, out_year-1)) {
        rec_dt <- code_changes[jaar == yr + 1 | (jaar == yr & maand > 1),
                                .(jaar, oud_code, frac, nieuw_code, maand, dag)
                                ]
        tmp <- merge(rec_dt,
                     out,
                     by.x = 'oud_code', by.y = 'code', all.x = T, all.y = F
                     )[,
                       .(oud_code, nieuw_code, get(var_name) * frac)
                       ][V3>0,
                         .(in_year, sum(V3)),
                         by = nieuw_code
                         ]
        out <- unique(rbindlist(list(tmp,
                                     out[!code %in% rec_dt[frac<1, oud_code]]
                                     )
                                )[,
                                  .(in_year, sum(V2)),
                                  by = nieuw_code
                                  ])
        setnames(out, c('code', 'year', var_name))
    }
    return(out)
}

## function for recoding municipalities - several variables
recode_vector_vars <- function(dat,vars, in_year, out_year) {
    tmp <- lapply(seq_along(vars),
                  function(x) {
                      recode_vector_recursive(dat, vars[x], in_year, out_year)
                  })
    for (i in 1:length(tmp)) {
        if (i == 1) {
            out <- tmp[[i]]
        } else {
            out <- merge(out, tmp[[i]], by = c('code', 'year'))
        }
    }
    return(out)
}

## function for recoding municipality codes year on year
recode_matrix_year_on_year <- function(dat, var_name, in_year, out_year) {
    if (out_year - in_year > 1) stop("Not year on year input")
    rec_dt <- code_changes[(jaar == in_year & maand > 1) | (jaar == out_year & maand == 1),
                           .(jaar, oud_code, frac, nieuw_code, maand, dag)
                           ]
    ## replace origins
    tmp1 <- merge(dat[code_orig %in% rec_dt$oud_code | code_dest %in% rec_dt$oud_code],
                 rec_dt,
                 by.x = 'code_orig',
                 by.y='oud_code',
                 all.x = T,
                 allow.cartesian = T)
    ## replace destinations
    tmp2 <- merge(tmp1, rec_dt,
                 by.x = 'code_dest',
                 by.y='oud_code',
                 all.x = T,
                 allow.cartesian = T)
    ## fix na's in origins or destinations
    tmp2[is.na(frac.y),
        ':=' (frac.y = 1, nieuw_code.y = code_dest)
        ][is.na(frac.x),
          ':=' (frac.x = 1, nieuw_code.x = code_orig)
          ][,
            moved := frac.x * frac.y * get(var_name)
            ]
    ## calculate moves over new municipalities
    tmp <- tmp2[frac.x * frac.y > 0,
               sum(moved),
               by = c('nieuw_code.x', 'nieuw_code.y', 'year')
               ]
    ## combine with non-modified data  
    old_codes <- rec_dt[!frac == 0 | nieuw_code %in% oud_code, unique(oud_code)]
    out <- unique(
        rbindlist(list(tmp,
                       dat[!(code_orig %in% old_codes | code_dest %in% old_codes)]
                       )
                  )[,
                    sum(V1),
                    by = c('nieuw_code.x', 'nieuw_code.y', 'year')
                    ])
    setnames(out, c('code_orig', 'code_dest', 'year', var_name))
    ## fix friese meeren...
    if (in_year == 2016) {
        out[code_orig == 1921,
            code_orig := 1940
            ][code_dest == 1921,
              code_dest := 1940
              ]
    }
    return(out)
}

## function for recoding municipality codes recursively
recode_matrix_recursive <- function(dat, var_name, in_year, out_year, dir = 'forward') {
    ## initialisation
    in_year_tmp <- in_year
    tmp <- dat
    ## update codes recursively each year
    while(out_year - in_year_tmp > 0) {
        if (out_year - in_year_tmp > 1) {
            tmp <- recode_matrix_recursive(tmp,
                                           var_name,
                                           in_year_tmp,
                                           in_year_tmp + 1)
        } else {
            out <- recode_matrix_year_on_year(tmp,
                                              var_name,
                                              in_year_tmp,
                                              out_year)
        }
        in_year_tmp <- in_year_tmp + 1
    }
    ## update year
    out[, year := in_year]
    return(out)
}
