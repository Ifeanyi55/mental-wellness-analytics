library(tidyverse)
library(highcharter)

# read csv file
mental_wellness_df <- read.csv("mental_wellness.csv")
view(mental_wellness_df)

# publication with the most researchers
projects_with_most_researchers <- mental_wellness_df |> 
  group_by(Publication) |> 
  summarize(num_of_researchers = n()) |> 
  arrange(desc(num_of_researchers))

projects_with_most_researchers |> view()

# top 20 pie
pie_hc <- projects_with_most_researchers[1:20,] |> 
  hchart(
    type = "pie",
    hcaes(x = Publication,y = num_of_researchers),
    name = "Number of Researchers",
    shadow = T) |> 
  hc_title(text = "<b>Top 20 Projects With The Most Researchers</b>")

pie_hc

# gauge plot of the highest number of researchers
gauge <- highchart() |> 
  hc_pane(startAngle = -90,
          endAngle = 90,
          background = list(
            shape = "arc",
            innerRadius = "60%",
            outerRadius = "100%",
            borderWidth = 5
          )) |> 
  hc_yAxis(
    labels = list(style = list(fontSize = "15px",color = "black"))
  ) |> 
  hc_colors(colors = "green") |> 
  hc_add_series(data = 100, 
                type = "solidgauge",
                name = "Number of Researchers",
                dataLabels = list(
                  useHTML = T,
                  borderWidth = 0,
                  borderColor = "black",
                  style = list(fontSize = "20px",color = "green")
                )) |> 
  hc_tooltip(enabled = T) |> 
  hc_title(text = "<b>Highest Number of Researchers Collaborating on One Research Project</b>")

gauge

# affiliations with most researchers
aff_with_most_researchers <- mental_wellness_df |> 
  group_by(Affiliation) |> 
  summarize(num_of_researchers = n()) |> 
  arrange(desc(num_of_researchers))

aff_with_most_researchers

# top 20 bar
hc <- aff_with_most_researchers[1:20,] |> 
  hchart(type = "bar",
         hcaes(x = Affiliation, 
               y = num_of_researchers),
         name = "Number of Researchers",
         color = "#0174c3",
         shadow = T) |> 
  hc_title(text = "<b>Top 20 Affiliations With The Most Researchers</b>")

hc

# institutions with most researchers
inst_with_most_researchers <- mental_wellness_df |> 
  group_by(Institution) |> 
  summarize(num_of_researchers = n()) |> 
  arrange(desc(num_of_researchers))

inst_with_most_researchers |> view()


pie_hc2 <-  inst_with_most_researchers |>
  hchart(
    hcaes(x = Institution, y = num_of_researchers),
    type = "pie",
    name = "Number of Researchers",
    shadow = T) |>     
  hc_title(text = "<b>Institution Categories With The Most Researchers</b>")

pie_hc2 

# top 20 researchers involved in most collaboration
researcher_most_appearing <- mental_wellness_df |> 
  group_by(Authors) |> 
  summarize(Frequency = n()) |> 
  arrange(desc(Frequency))

researcher_most_appearing |> view()  

hc_bar <- researcher_most_appearing[1:20,] |> 
  hchart(
    type = "bar",
    hcaes(x = Authors,
          y = Frequency),
    shadow = T,
    name =  "Number of Collaborations",
    color = "#ff3b01"
  ) |> 
  hc_title(text = "<b>Top 20 Researchers Involved in Most Collaborations</b>")

hc_bar

