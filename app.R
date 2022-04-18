library(shiny)
library(reactable)
library(magrittr)
library(ggplot2)
library(safetyData)
library(Tplyr)
library(dplyr)
library(purrr)
library(rlang)

tab <- tplyr_table(adam_adlbc, TRTA, where=PARAMCD == "CK" & startsWith(VISIT, "WEEK") & BNRIND != "") %>%
  add_layer(
    group_shift(vars(row = BNRIND, column = ANRIND), by = vars(PARAM, VISIT))
  )

b_tab <- build(tab, metadata = TRUE) %>%
  apply_row_masks() %>%
  select(row_id, starts_with("row"), starts_with("var")) %>%
  relocate(row_id, row_label1, row_label2, row_label3, var1_Placebo_N, var1_Placebo_H,
           `var1_Xanomeline Low Dose_N`, `var1_Xanomeline Low Dose_H`,
           `var1_Xanomeline High Dose_N`, `var1_Xanomeline High Dose_H`)


get_metadata_filters <- function(tab, row, col) {
  req(row(), col())
  tmp <- tab$metadata %>%
    filter(row_id == row()) %>%
    select(col()) %>%
    extract2(1) %>%
    extract2(1)
  
  tmp
}

ui <- fillPage(
  column(8,
         reactableOutput("demoTab")
  ),
  
  column(4,
         plotOutput("LabsBySubGroup", click = "plot_click"),
         tableOutput("renalTable")
  )
)



server <- function(input, output) {
  
  row <- reactive(b_tab[input$row$index,1]$row_id)
  col <- reactive(input$col$column)
  
  output$demoTab <- renderReactable(
    reactable(
      select(b_tab, -row_id, -starts_with("ord")),
      sortable = FALSE,
      onClick = JS("function(rowInfo, colInfo) {
                      if (window.Shiny) {
                        Shiny.setInputValue('row', { index: rowInfo.index + 1 })
                        Shiny.setInputValue('col', { column: colInfo.id })
                        }
                    }"),
      height = 450,
      defaultPageSize = 11,
      columns = list(
        row_label1 = colDef(name = "Parameter"),
        row_label2 = colDef(name = "VISIT"),
        row_label3 = colDef(name = "ANRIND"),
        var1_Placebo_N = colDef(name = "Placebo Normal"),
        var1_Placebo_H = colDef(name = "Placebo High"),
        `var1_Xanomeline Low Dose_N` = colDef(name = "Xano Low Normal"),
        `var1_Xanomeline Low Dose_H` = colDef(name = "Xano Low High"),
        `var1_Xanomeline High Dose_N` = colDef(name = "Xano High Normal"),
        `var1_Xanomeline High Dose_H` = colDef(name = "Xano High High")
      )
    )
  )
  
  meta_filters <- reactive({
    req(row, col)
    get_metadata_filters(tab, row, col)
  })
  
  f_usubjid <- reactive({
    req(meta_filters)
    tmp <- tab$target %>%
      filter(!!!meta_filters()$filters) %>%
      extract2("USUBJID")
    
    tmp
  })
  
  f_tab_ae <- reactive({
    req(f_usubjid())
    adam_adae %>%
      filter(USUBJID %in% f_usubjid(), AEBODSYS %in% c("GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
                                                       "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
                                                       "CARDIAC DISORDERS",
                                                       "NERVOUS SYSTEM DISORDERS"))
  })
  
  f_tab_labs <- reactive({
    req(f_usubjid())
    adam_adlbc %>%
      filter(USUBJID %in% f_usubjid(), PARAM == "Creatinine (umol/L)")
  })
  
  
  output$LabsBySubGroup <- renderPlot({
    req(f_tab_labs())
    f_tab_labs() %>%
      ggplot(aes(y = AVAL, x = ADY, group = PARAM, color = PARAM)) +
      geom_smooth() +
      geom_count()
  })
  
  output$renalTable <- renderTable({
    f_tab_labs() %>%
      select(USUBJID, PARAM, ADY, AVAL)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
