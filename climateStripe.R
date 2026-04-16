

get_chunk = function(X, breaks) {
    if (!is.null(breaks)) {
        chunk = split(X, cut(X, breaks, labels=FALSE))
    } else {
        chunk = list(X)
        names(chunk) = 1
    }
    return (chunk)
}



get_breaks_function = function (breaks, isDate=TRUE,
                                d_breaks=0,
                                break_round=-1,
                                add_breaks=NULL,
                                rm_breaks=NULL) {

    get_breaks = function (X) {
        if (isDate) {
            Xmin = round(lubridate::year(min(X)), break_round)
            Xmax = round(lubridate::year(max(X)), break_round)
            if (Xmax-Xmin <= 1) {
                Xmin = lubridate::year(X)[1]
                Xmax = lubridate::year(X)[1] + 1
            }
            res = seq.Date(from=as.Date(paste0(Xmin, "-01-01")) +
                               d_breaks,
                           to=as.Date(paste0(Xmax, "-01-01")) +
                               d_breaks,
                           by=breaks)
        } else {
            Xmin = round(min(X), break_round)
            Xmax = round(max(X), break_round)
            res = seq(from=Xmin + d_breaks,
                      to=Xmax + d_breaks,
                      by=breaks)
        }

        if (!is.null(add_breaks)) {
            res = sort(c(res, add_breaks))
        }

        if (!is.null(rm_breaks)) {
            res = res[!(res %in% rm_breaks)]
        }

        return (res)
    }

    return (get_breaks)
}


gg_climateStripe = function (Date, Value,
                             min_value=NULL,
                             max_value=NULL,
                             palette_name='RdBu',
                             palette_n_shade=8,
                             palette_reverse=FALSE,
                             palette_is_center=TRUE,
                             palette_q_extrem=0,
                             stripe_space_factor=0,
                             stripe_chunk_by=NULL,
                             stripe_chunk_reverse=FALSE,
                             is_x_axis=FALSE,
                             is_y_axis=FALSE,
                             x_size=NULL,
                             y_size=14,
                             x_color=NULL,
                             y_color=NULL,
                             x_hjust=NULL,
                             x_vjust=NULL,
                             y_hjust=NULL,
                             y_vjust=-0.1,
                             x_breaks=seq.Date(as.Date("1972-01-01"),
                                               as.Date("1972-12-01"),
                                               "months"),
                             y_breaks=NULL,
                             x_label_format="%m",
                             y_label_format="%Y",
                             x_date_breaks=NULL,
                             y_date_breaks="years",
                             x_expand=ggplot2::expansion(add=c(0, 1)),
                             y_expand=ggplot2::expansion(add=c(1, 0)),
                             y_position="right",
                             axis_margin=ggplot2::margin(0, 0, 0, 0,
                                                         unit="mm")) {

    if (is.null(x_date_breaks)) {
        x_date_breaks = ggplot2::waiver()
    }
    if (is.null(y_date_breaks)) {
        y_date_breaks = ggplot2::waiver()
    }
    
    chunkDate = get_chunk(Date, stripe_chunk_by)

    dDate = Date[2] - Date[1]
    i = 1
    while (is.na(dDate)) {
        i = i + 1
        dDate = Date[i+1] - Date[i]
    }

    nChunk = length(chunkDate)

    if (is.null(min_value)) {
        minValue_tmp = quantile(Value, palette_q_extrem)
    } else {
        minValue_tmp = min_value
    }
    if (is.null(max_value)) {
        maxValue_tmp = quantile(Value, 1-palette_q_extrem)
    } else {
        maxValue_tmp = max_value
    }
    
    minValue = min(c(minValue_tmp, maxValue_tmp))
    maxValue = max(c(minValue_tmp, maxValue_tmp))

    if (is.null(min_value))
    if (palette_q_extrem %in% c(0, 1)) {
        include = TRUE
    } else {
        include = FALSE
    }

    colorStep = palette_n_shade*2+1
    Palette = get_palette(palette_name, colorStep, palette_reverse)
    res = compute_colorBin(minValue, maxValue,
                           colorStep,
                           center=palette_is_center,
                           include=include)

    stripe_color = unlist(sapply(Value, get_color,
                                 upBin=res$upBin,
                                 lowBin=res$lowBin,
                                 Palette=Palette))

    nStripeALL = sapply(chunkDate, length)
    nStripe_max = max(nStripeALL, na.rm=TRUE)
    
    stripe_space = dDate * stripe_space_factor

    stripe_max_xmin = chunkDate[nStripeALL == nStripe_max][[1]]
    stripe_max_xmax = stripe_max_xmin+dDate

    climateStripe = ggplot2::ggplot() + ggplot2::theme_void() +
        ggplot2::theme(plot.margin=axis_margin)

    if (is.null(x_color)) {
        x_color = IPCCgrey40
    }
    if (is.null(y_color)) {
        y_color = IPCCgrey40
    }
    
    if (is_x_axis) {
        climateStripe =
            climateStripe +
            ggplot2::theme(axis.text.x=
                               ggplot2::element_text(color=x_color,
                                                     size=x_size,
                                                     hjust=x_hjust,
                                                     vjust=x_vjust))
    } else {
        x_breaks = NULL
        x_date_breaks = ggplot2::waiver()
        x_label_format = ggplot2::waiver()
        x_expand = c(0, 0)
    }
    
    if (is_y_axis) {
        climateStripe =
            climateStripe +
            ggplot2::theme(axis.text.y=
                               ggplot2::element_text(color=y_color,
                                                     size=y_size,
                                                     hjust=y_hjust,
                                                     vjust=y_vjust))
    } else {
        y_breaks = NULL
        y_date_breaks = ggplot2::waiver()
        y_label_format = ggplot2::waiver()
        y_expand = c(0, 0)
    }


    for (i in 1:nChunk) {
        chunk = chunkDate[[i]]
        nStripe = length(chunk)

        stripe_xmin = stripe_max_xmin[1:nStripe]
        stripe_xmax = stripe_max_xmax[1:nStripe]
        
        stripe_ymin = rep(chunk[1], nStripe)
        stripe_ymax = rep(chunk[length(chunk)], nStripe)
        if (i < nChunk) {
            stripe_ymax = stripe_ymax + dDate - stripe_space
        }

        fill = stripe_color[match(chunk, Date)]

        climateStripe = climateStripe +
            ggplot2::annotate("rect",
                              xmin=stripe_xmin,
                              xmax=stripe_xmax,
                              ymin=stripe_ymin,
                              ymax=stripe_ymax,
                              fill=fill,
                              color=NA)
    }

    climateStripe = climateStripe +
        
        ggplot2::scale_x_date(breaks=x_breaks,
                              date_breaks=x_date_breaks,
                              date_labels=x_label_format,
                              expand=x_expand) +
        
        ggplot2::scale_y_date(breaks=y_breaks,
                              date_breaks=y_date_breaks,
                              date_labels=y_label_format,
                              expand=y_expand,
                              position=y_position)

    # if (stripe_chunk_reverse) {

    # library(scales)

    # reverse2_trans <- function() {
    #     trans_new(
    #         "reverse2",
    #         function(x) -1 * as.numeric(x), # Force values to be numeric for Date objects
    #         function(x) -1 * as.numeric(x)
    #     )
    # }

    # ggplot(economics, aes(date, unemploy)) +
    #     geom_line() +
    #     scale_x_continuous(
    #         trans = c("date", "reverse2")
    #     )
    
    # climateStripe = climateStripe + ggplot2::scale_y_reverse()
    # }
    
    return (climateStripe)
}


