## Load data
 load_data_fun <- function(data){
   data %>% 
      select(
        c(dateOccurred, description, Animal, Identifier, latitude, longitude)
      ) %>% 
      mutate(
        Animal = str_to_lower(Animal),
        Identifier = str_to_lower(Identifier)
      ) %>% 
      filter(
        str_detect(Animal, pattern = "rhino"),
        dateOccurred >= input$dateRange[1] & dateOccurred <= input$dateRange[2]
      )
}