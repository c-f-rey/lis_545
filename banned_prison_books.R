library(dplyr)
library(ggplot2)
library(plotly)

banned_books <- read.csv("~/Desktop/data/banned_prison_books/banned_book_data_combined_lists.csv" , stringsAsFactors = FALSE)

# Group total bans by year, plot

yearly_bans <- banned_books %>% 
  filter(year %in% c(1991 : 2022)) %>% #filter out outlier year
  group_by(year) %>% 
  count()

yearly_bans_graph <- ggplot(data = yearly_bans) +
                      geom_col(mapping = aes(
                        x = year ,
                        y = n ,
                        text = paste("</br> Year:" , year ,
                                     "</br> Total Bans:" , n)
                      )) +
                      labs(title = "Total Yearly Prison Book Bans 1991 - 2022" , 
                           x = "Year" , 
                           y = "Total Bans")
print(yearly_bans_graph)

yearly_bans_plotly <- ggplotly(yearly_bans_graph)
print(yearly_bans_plotly, tooltip = "text")

# Group total bans by state, plot

state_bans <- banned_books %>% 
  group_by(state_arc) %>% 
  count()

state_bans_graph <- ggplot(data = state_bans) +
                    geom_col(mapping = aes(
                      x = n , 
                      y = reorder(state_arc, n) ,
                      text = paste("</br> State:" , state_arc ,
                                   "</br> Total Bans" , n)
                    )) +
                    labs(title = "Total Book Bans by State 1815 - 2022" , 
                         x = "Books Banned" , 
                         y = "State")
print(state_bans_graph)

state_bans_plotly <- ggplotly(state_bans_graph , tooltip = "text")

print(state_bans_plotly)

# Group yearly bans by state, plot

yearly_state_bans <- banned_books %>% 
  filter(year %in% c(1991 : 2022)) %>% #remove outlier years from WI
  group_by(year , state_arc) %>% 
  count()

yearly_state_bans_graph <- ggplot(data = yearly_state_bans) +
                            geom_line(mapping = aes(
                              x = year ,
                              y = n , 
                              color = state_arc ,
                              group = state_arc ,
                              text = paste("</br> Year:" , year ,
                                           "</br> State:" , state_arc ,
                                           "</br> Annual Bans:" , n)
                            )) +
                            labs(title = "Annual Book Bas by State 1991 - 2022" ,
                                 x = "Year" ,
                                 y = "Books Banned")

print(yearly_state_bans_graph)

ggplotly(yearly_state_bans_graph , tooltip = "text")


# books banned

top_books_banned <- banned_books %>% 
  group_by(publication , reason) %>% 
  count() %>% 
  filter(n %in% c(155 : 70))

names(top_books_banned) [names(top_books_banned) == "n"] <- "n_banned"

top_banned_books_graph <- ggplot(data = top_books_banned) + 
  geom_col(mapping = aes(
    x = n_banned ,
    y = reorder(publication , n_banned))) +
      labs(title = "Most Banned Publications in Prisons" , 
           x = "Number Banned" ,
           y = "Publication")

print(top_banned_books_graph)

#reasons given 

top_reasons_given <- banned_books %>% 
  group_by(reason) %>% 
  count() %>% 
  filter(n %in% c(25026 : 137))

top_reasons_given_graph <- ggplot(data = top_reasons_given) +
  geom_col(mapping = aes(
           x = n ,
           y = reorder(reason , n))) +
  labs(title = "Most Common Reasons Given for Book Bans in Prisons" , 
       x = "Occurences" ,
       y = "Reason")

print(top_reasons_given_graph)

