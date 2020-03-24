library("tidyverse")

function(input, output, session){
  
  #----- for 1st tabNav, Star Narrow -----  
  output$star_narrow <- renderTable({
    # extract data from the original dataset
    starwars %>%
      select(name, species, homeworld, height) %>%
      filter(height <= input$height_limit_star_narrow) %>%
      arrange(desc(height))
    
  }, striped = TRUE, 
  hover = TRUE, 
  na = "[Missing]")
  #----- for 2nd tabNav, Star Wide -----
  output$star_wide <- renderTable({
    # extract data from the original dataset   
    starwars %>%
      select(name:species) %>%
      filter(height <= input$height_limit_star_wide) %>%
      arrange(desc(height))
    
  })
  #----- for 3rd tabNav, Star List -----
  output$star_lists <- renderTable({
    # extract data from the original dataset  
    starwars %>%
      select(name, films, vehicles, starships) %>%
     mutate_if(is.list, list(~map_chr(., ~paste(.x, collapse = "<br>"))  ))
    
  }, width = "100%", 
  sanitize.text.function = identity)
  
}