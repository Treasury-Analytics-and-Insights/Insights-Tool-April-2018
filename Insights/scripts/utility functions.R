alignCenter <- function(el) {
  htmltools::tagAppendAttributes(el,
                                 style="margin-left:auto;margin-right:auto;"
  )
}


debounce <- function(expr, millis, env = parent.frame(), quoted = FALSE,
                     domain = getDefaultReactiveDomain()) {
  
  force(millis)
  
  f <- exprToFunction(expr, env, quoted)
  label <- sprintf("debounce(%s)", paste(deparse(body(f)), collapse = "\n"))
  
  v <- reactiveValues(
    trigger = NULL,
    when = NULL # the deadline for the timer to fire; NULL if not scheduled
  )  
  
  # Responsible for tracking when f() changes.
  observeEvent(f(), {
    # The value changed. Start or reset the timer.
    v$when <- Sys.time() + millis/1000
  }, ignoreNULL = FALSE)
  
  # This observer is the timer. It rests until v$when elapses, then touches
  # v$trigger.
  observe({
    if (is.null(v$when))
      return()
    
    now <- Sys.time()
    if (now >= v$when) {
      v$trigger <- runif(1)
      v$when <- NULL
    } else {
      invalidateLater((v$when - now) * 1000, domain)
    }
  })
  
  # This is the actual reactive that is returned to the user. It returns the
  # value of f(), but only invalidates/updates when v$trigger is touched.
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}


get_palette <- function(..., n, alpha) {
  colors <- colorRampPalette(...)(n)
  paste(colors, sprintf("%x", ceiling(255*alpha)), sep="")
}


change_names <- function(df, from, to, reminder = TRUE) {
  
  ## error checking
  # check if from exist in df
  
  if (!all(from %in% colnames(df))) {
    stop ("undefined column names selected")
  }
  
  # check if the length of the mapping match
  len.from <- length(from)
  len.to <- length(to)
  
  if (len.from != len.to) {
    stop("argument imply differing length of vectors:", len.from, ",", len.to)
  }
  
  org.from <- from
  org.to <- to
  
  ## process to modify the specified names
  field.names <- names(df)
  field.names <- field.names[!(field.names %in% from)]
  
  to <- c(to, field.names)
  from <- c(from, field.names)
  
  names(to) <- from
  names(df) <- to[names(df)]
  
  if (reminder) {
    cat("the column name(s) have been modified:\n")
    indicator <- paste(org.from, " to ", org.to, "\n", sep = "")
    indicator[1] <- paste(" ", indicator[1], sep = "")
    cat(indicator)
  }  
  
  return(df)
  
}

hc_hist <- function(x, y, title = NULL, subtitle = NULL, xaxis, yaxis,
                    col, col_highlight, df, tooltip, id = "", 
                    pointFormat = NULL, valueFormat = NULL, pre_selected = 0) {
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        load = JS(
          paste0(
            "function() {
              this.series[0].data[",pre_selected,"].update({ color: '", col_highlight, "' }, true, false)}"
          )
        )
      )
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xaxis),
             categories = df[[x]]) %>%
    hc_yAxis(title = list(text = yaxis)#,
             # labels = list(
             #   format = ifelse(is.null(valueFormat), '{value}%', valueFormat)
             # )
    ) %>%
    hc_add_series(data = df[[y]], color = col) %>%
    hc_tooltip(
      headerFormat = "<b>{point.x}</b> <br>",
      pointFormat = ifelse(is.null(pointFormat), paste0(tooltip, ": <b>{point.y:.1f}</b>"), paste0(tooltip, pointFormat))
    ) %>%
    hc_plotOptions(
      column = list(
        borderColor = col_highlight,
        borderWidth = 2,
        cursor = "pointer",
        point = list(
          events = list(
            click = JS(
              paste0(
                "function() {
                Shiny.onInputChange('", id, "click', {", x, ": this.category});

                for (var i = 0; i < this.series.data.length; i++) {
                this.series.data[i].update({ color: '", col, "' }, true, false);
                }
                this.update({ color: '", col_highlight, "' }, true, false)}"
              )
            )
          )
        )
      ),
      series = list(
        showInLegend = FALSE,
        pointPadding = 0,
        groupPadding = 0.05
      ))
}

hc_hist_comp <- function(x, y1, y2, y1_label, y2_label,
                         title, subtitle, xaxis, yaxis, 
                         col_n, col_y, df, id = "", 
                         pointFormat = NULL, valueFormat = NULL,pre_selected = 0) {
  highchart() %>%
    hc_chart(
      type = "column",
      events = list(
        load = JS(
          paste0(
            "function() {
            var chart = this.xAxis[0]
            chart.removePlotLine('plot-line-1');
            chart.addPlotLine({
            value: this.series[0].data[",pre_selected,"].x,
            color: '#FF0000',
            width: 2,
            id: 'plot-line-1'
            })
            console.log(this);
            }"
          )
          )
        )
    ) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_xAxis(title = list(text = xaxis),
             categories = df[[x]]) %>%
    hc_yAxis(title = list(text = yaxis),
             label = list(
               format = ifelse(is.null(valueFormat), "{value:,.0f}", valueFormat)
             )) %>%
    hc_add_series(name = y1_label, data = df[[y1]], color = col_n) %>%
    hc_add_series(name = y2_label, data = df[[y2]], color = col_y) %>%
    hc_tooltip(
      headerFormat = "<b>{point.x}</b> <br>",
      pointFormat = ifelse(is.null(pointFormat), "{series.name}: <b>{point.y:,.0f}</b><br>", pointFormat),
      shared = TRUE
    ) %>%
    hc_plotOptions(
      column = list(
        cursor = "pointer",
        point = list(
          events = list(
            click = JS(
              paste0(
                "function() {
                         Shiny.onInputChange('", id, "click', {", x, ": this.category});
                         var chart = this.series.chart.xAxis[0]
                         chart.removePlotLine('plot-line-1');
                         chart.addPlotLine({
                         value: this.x,
                         color: '#FF0000',
                         width: 2,
                         id: 'plot-line-1'
                         })}"
              )
            )
          )
        )
      ),
      series = list(
        pointPadding = 0,
        groupPadding = 0.1
      ))
}

mig_all_popn_pyramid_perc <- function(dat, label, 
                                      # title_str, 
                                      en_x, net = T,
                                      col_in = vt.mig_all_col_in,
                                      col_out = vt.mig_all_col_out,
                                      col_net = vt.mig_all_col_neutral){
  
  hc <- highchart() %>%
    hc_add_series(data = dat %>% 
                    filter(FLOW_DIR == "in") %>%
                    mutate(COL = col_in),
                  type = "column",
                  # name = "Curr",
                  hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                        y = POPN_PERC,
                        color = COL,
                        label = LABEL)) %>% 
    hc_add_series(data = dat %>% 
                    filter(FLOW_DIR == "out") %>%
                    mutate(COL = col_out),
                  type = "column",
                  # name = "Prev",
                  hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                        y = POPN_PERC,
                        color = COL,
                        label = LABEL)) %>%
    hc_tooltip(
      headerFormat = "<b>{series.name}: {point.x}</b> <br>",
      formatter = JS("function(){
         return(this.point.label + ': ' + Math.abs(this.y).toFixed(2) + '%')
         }")
    ) %>% 
    hc_legend() %>% 
    hc_plotOptions(
      column = list(stacking = "normal")
    ) %>% 
    hc_yAxis(
      labels = list(
        formatter = JS("function(){
                       return(Math.abs(this.value) + '%')
                       }")
      ),
      reversed = T,
      labels = list(rotation = 0,
                    autoRotation = F)
    ) %>% 
    # hc_title(text = title_str) %>% 
    hc_chart(inverted = T)
  
  if(en_x){
    hc = hc %>% 
      hc_xAxis(
        # type = "category"
        categories = label,
        reversed = F,
        labels = list(enabled = en_x,
                      rotation = 0,
                      autoRotation = F)
      )
  }
  
  if(!en_x){
    hc = hc %>% 
      hc_xAxis(
        # type = "category"
        categories = label,
        reversed = F,
        labels = list(enabled = en_x,
                      rotation = 0,
                      autoRotation = F),
        
        minorTickLength = 0,
        tickLength = 0,
        lineWidth = 0,
        minorGridLineWidth = 0,
        lineColor = 'transparent'
      )
  }
  
  if(net){
    hc = hc %>%
      hc_add_series(data = dat %>% 
                      group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
                      summarise(VALUE = sum(POPN_PERC)) %>%
                      mutate(COL = col_net,
                             LABEL = paste(FLOW_DEMOGRAPHIC_VALUE, "- Netflow")),
                    type = "scatter",
                    # name = "Prev",
                    hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                          y = VALUE,
                          color = COL,
                          label = LABEL))
  }
  
  return(hc)
  
}

mig_all_popn_pyramid <- function(dat, label, 
                                 #title_str,
                                 en_x, net = T,
                                 col_in = vt.mig_all_col_in,
                                 col_out = vt.mig_all_col_out,
                                 col_net = vt.mig_all_col_neutral){
  
  hc <- highchart() %>%
    hc_add_series(data = dat %>% 
                    filter(FLOW_DIR == "in")  %>% 
                    mutate(COL = col_in) %>% 
                    mutate(VALUE = -VALUE),
                  type = "column",
                  # name = "Curr",
                  hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                        y = VALUE,
                        color = COL,
                        label = LABEL)) %>% 
    hc_add_series(data = dat %>% 
                    filter(FLOW_DIR == "out") %>% 
                    mutate(COL = col_out),
                  type = "column",
                  # name = "Prev",
                  hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                        y = VALUE,
                        color = COL,
                        label = LABEL)) %>% 
    hc_tooltip(
      headerFormat = "<b>{series.name}: {point.x}</b> <br>",
      formatter = JS("function(){
                     return(this.point.label + ': ' + Math.abs(this.y))
}")
    ) %>% 
    hc_legend() %>% 
    hc_plotOptions(
      column = list(stacking = "normal")
    ) %>% 
    hc_yAxis(
      labels = list(
        formatter = JS("function(){
                       return(Math.abs(this.value))
                       }")
      ),
      reversed = T,
      labels = list(rotation = 0,
                    autoRotation = F)
    ) %>% 
    # hc_title(text = title_str) %>% 
    hc_chart(inverted = T)
  
  if(en_x){
    hc = hc %>% 
      hc_xAxis(
        # type = "category"
        categories = label,
        reversed = F,
        labels = list(enabled = en_x,
                      rotation = 0,
                      autoRotation = F)
      )
  }
  
  if(!en_x){
    hc = hc %>% 
      hc_xAxis(
        # type = "category"
        categories = label,
        reversed = F,
        labels = list(enabled = en_x,
                      rotation = 0,
                      autoRotation = F),
        
        minorTickLength = 0,
        tickLength = 0,
        lineWidth = 0,
        minorGridLineWidth = 0,
        lineColor = 'transparent'
      )
  }
  
  if(net){
    hc = hc %>%
      hc_add_series(data = dat %>% 
                      mutate(VALUE = ifelse(FLOW_DIR == "in", -VALUE, VALUE)) %>% 
                      group_by(FLOW_DEMOGRAPHIC_VALUE) %>% 
                      summarise(VALUE = sum(VALUE)) %>%
                      mutate(COL = col_net,
                             LABEL = paste(FLOW_DEMOGRAPHIC_VALUE, "- Netflow")),
                    type = "scatter",
                    # name = "Prev",
                    hcaes(x = FLOW_DEMOGRAPHIC_VALUE,
                          y = VALUE,
                          color = COL,
                          label = LABEL))
  }
  
  return(hc)
  
}

#for using local images for icons and valueBoxes
#Adapted from
#https://blog.snap.uaf.edu/2017/01/11/custom-images-for-shiny-dashboard-valuebox-icons/
#https://gist.github.com/leonawicz/0fab3796b02a62b7f3bd0c02a171f0b7

local_icon <- function(name, class = NULL){
  if(is.null(name$src))
    stop("'name' must be a named list with a 'src' element
         and optionally 'width' (defaults to 100%).")
  if(is.null(name$width)) name$width <- "100%"
  return(tags$img(class="img img-local", src=name$src, width=name$width))
}

local_value_box <- function (value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL){
  shinydashboard:::validateColor(color)
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = icon$name)
  if(!is.null(icon)){
    if(!icon$name %in% c("i", "img")) stop("'icon$name' must be 'i' or 'img'.")
    iconClass <- if(icon$name=="i") "icon-large" else "img"
  }
  # boxContent <- div(class = paste0("small-box bg-", color), 
  #                   div(class = "inner", h3(value), p(subtitle)), if (!is.null(icon)) 
  #                     div(class = iconClass, icon)
  #                   )
  boxContent <- div(class = paste0("small-box bg-", color), 
                    fluidRow(
                      style = 'padding:5px;',
                      column(
                        width = 6,
                        h3(value)
                        ),
                      column(
                        width = 6,
                        align = 'center',
                        div(class = iconClass, icon)
                      )
                    ),
                    fluidRow(
                      column(width = 12,
                             style = 'padding-left:20px; padding-right:20px;',
                             p(HTML(subtitle))
                             )
                    )
                    )
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  div(class = if (!is.null(width)) 
    paste0("col-sm-", width), boxContent)
}

#For the map csv formatting
national_map_csv_reformat = function(df, col_mapping_df){
  
  df = df %>%
    .[,col_mapping_df$NAME] %>% 
    inner_join(df.mig_flow_type_mapping,
               by = "FLOW_TYPE") %>% 
    inner_join(df.mig_natural_change_flow_dir_mapping %>% 
                 select(FLOW_DIR_VALUE, FLOW_DIR) %>% 
                 rename(FLOW_DIR_LABEL = FLOW_DIR) %>% 
                 rename(FLOW_DIR = FLOW_DIR_VALUE),
               by = c("FLOW_DIR")) %>% 
    select(-FLOW_DIR, -FLOW_TYPE) %>% 
    rename(FLOW_DIR = FLOW_DIR_LABEL,
           FLOW_TYPE = FLOW_TYPE_LABEL) %>% 
    change_names(col_mapping_df$NAME,
                 col_mapping_df$RENAME)%>% 
    .[,col_mapping_df$RENAME]
  
  return(df)
}

ta_map_csv_reformat = function(df, col_mapping_df){
  
  df = df %>% 
    inner_join(df.mig_flow_type_mapping,
               by = "FLOW_TYPE")  %>% 
    inner_join(df.mig_natural_change_flow_dir_mapping %>% 
                 select(FLOW_DIR_VALUE, FLOW_DIR) %>% 
                 rename(FLOW_DIR_LABEL = FLOW_DIR) %>% 
                 rename(FLOW_DIR = FLOW_DIR_VALUE),
               by = c("FLOW_DIR")) %>% 
    select(-FLOW_DIR, -FLOW_TYPE) %>% 
    rename(FLOW_DIR = FLOW_DIR_LABEL,
           FLOW_TYPE = FLOW_TYPE_LABEL) %>%
    change_names(col_mapping_df$NAME,
                 col_mapping_df$RENAME) %>% 
    .[,col_mapping_df$RENAME]
  
  return(df)
}

#For the movements between locations csv formatting
location_csv_reformat = function(df, col_mapping_df){
  
  df = df %>% 
    inner_join(df.mig_trend_flow_type_mapping,
               by = "FLOW_TYPE") %>% 
    inner_join(df.mig_trend_param_flow_map %>% 
                 rename(FLOW_DIR_LABEL = NAME),
               by = c("FLOW_DIR" = "VALUE")) %>% 
    select(-FLOW_TYPE, -FLOW_DIR) %>% 
    rename(FLOW_TYPE = FLOW_TYPE_LABEL,
           FLOW_DIR = FLOW_DIR_LABEL) %>% 
    change_names(col_mapping_df$NAME,
                 col_mapping_df$RENAME) %>% 
    .[,col_mapping_df$RENAME]
  
  return(df)
  
}

#For the population and demographic changes
popn_characteristic_csv_reformat = function(df, col_mapping_df){
  
  df = df %>% 
    mutate_if(is.character,
              function(x){ifelse(is.na(x),
                                 "",
                                 x)})  %>% 
    change_names(col_mapping_df$NAME,
                 col_mapping_df$RENAME) %>% 
    .[,col_mapping_df$RENAME]
  
  return(df)
  
}