library(openalexR)
library(tidyverse)

# set options
options(openalexR.mailto = "idiayeifeanyi@yahoo.com")


mental_wellness <- oa_fetch(
  entity = "works",
  title.search = c("depression","anxiety","ptsd"),
  cited_by_count = ">50",
  from_publication_date = "2019-01-01",
  to_publication_date = "2023-09-30",
  options = list(sort = "cited_by_count:desc"),
  verbose = FALSE
)

mental_wellness |> view()

# grab authors and group them according to collaboration
authors_collaboration_groups <- list()
for (i in 1:nrow(mental_wellness)){
  authors_collaboration_groups[[i]] <- mental_wellness$author[[i]][2]
}

all_authors <- c()
for (i in 1:length(authors_collaboration_groups)) {
  
  all_authors <- c(all_authors,authors_collaboration_groups[[i]][[1]])
  
}

# grab author position
authors_position <- list()
for (i in 1:nrow(mental_wellness)){
  authors_position[[i]] <- mental_wellness$author[[i]][4]
}
authors_position

all_authors_positions <- c() # grab all authors positions
for (i in 1:length(authors_position)) {
  
  all_authors_positions <- c(all_authors_positions,authors_position[[i]][[1]])
  
}
all_authors_positions

# grab author affiliation
authors_affiliation <- list()
for (i in 1:nrow(mental_wellness)){
  authors_affiliation[[i]] <- mental_wellness$author[[i]][7]
}
authors_affiliation

all_authors_affiliation <- c() # grab all authors affiliations
for (i in 1:length(authors_affiliation)) {
  
  all_authors_affiliation <- c(all_authors_affiliation,authors_affiliation[[i]][[1]])
  
}
all_authors_affiliation


# grab authors institution country code
authors_institution_country_code <- list()
for (i in 1:nrow(mental_wellness)){
  authors_institution_country_code[[i]] <- mental_wellness$author[[i]][9]
}
authors_institution_country_code

all_authors_institution_country_code <- c() # grab all authors institution country code
for (i in 1:length(authors_institution_country_code)) {
  
  all_authors_institution_country_code <- c(all_authors_institution_country_code,authors_institution_country_code[[i]][[1]])
  
}
all_authors_institution_country_code

# grab author institution type
authors_institution_type <- list()
for (i in 1:nrow(mental_wellness)){
  authors_institution_type[[i]] <- mental_wellness$author[[i]][10]
}
authors_institution_type

all_authors_institution_type <- c() # grab all authors institution type
for (i in 1:length(authors_institution_type)) {
  
  all_authors_institution_type <- c(all_authors_institution_type,authors_institution_type[[i]][[1]])
  
}
all_authors_institution_type

# get length of each authors collaboration
authors_length <- c()
for(authors in 1:length(authors_collaboration_groups)){
  authors_length <- c(authors_length,authors_collaboration_groups[[authors]] |> nrow())
}



# create authors data frame
authorAtt_df <- data.frame(
  Authors = all_authors,
  Position = all_authors_positions,
  Affiliation = all_authors_affiliation,
  Institution = all_authors_institution_type
)

authorAtt_df$`Institution Country` <- all_authors_institution_country_code
authorAtt_df |> view()

# write to csv
write.csv(authorAtt_df,file = "AuthorsDF.csv",row.names = F)


# publication attributes
# grab all publications

publications <- list()
for (i in 1:nrow(mental_wellness)){
  
  publications[[i]] <- rep(mental_wellness$display_name[i], each = authors_length[i])
  
}

all_publications <- c()
for(i in 1:length(publications)){
  all_publications <- c(all_publications,publications[[i]])
}

all_publications

# grab all so
pub_so <- list()
for(i in 1:nrow(mental_wellness)){
  pub_so[[i]] <- rep(mental_wellness$so[i], each = authors_length[i])
}

all_so <- c()
for(i in 1:length(pub_so)){
  all_so <- c(all_so,pub_so[[i]])
}

all_so

# grab all host organization
hostOrg <- list()
for(i in 1:nrow(mental_wellness)){
  hostOrg[[i]] <- rep(mental_wellness$host_organization[i], each = authors_length[i])
}

all_hostOrg <- c()
for(i in 1:length(hostOrg)){
  all_hostOrg <- c(all_hostOrg,hostOrg[[i]])
}

# grab all cited by count
citedby_count <- list()
for(i in 1:nrow(mental_wellness)){
  citedby_count[[i]] <- rep(mental_wellness$cited_by_count[i], each = authors_length[i])
}

all_citedby_count <- c()
for(i in 1:length(citedby_count)){
  all_citedby_count <- c(all_citedby_count,citedby_count[[i]])
}

# grab all publication year
pub_year <- list()
for(i in 1:nrow(mental_wellness)){
  pub_year[[i]] <- rep(mental_wellness$publication_year[i], each = authors_length[i])
}

all_pub_year <- c()
for(i in 1:length(citedby_count)){
  all_pub_year <- c(all_pub_year,pub_year[[i]])
}

# grab all type
type <- list()
for(i in 1:nrow(mental_wellness)){
  type[[i]] <- rep(mental_wellness$type[i], each = authors_length[i])
}

all_type <- c()
for(i in 1:length(type)){
  all_type <- c(all_type,type[[i]])
}

# grab all abstract
abstract <- list()
for(i in 1:nrow(mental_wellness)){
  abstract[[i]] <- rep(mental_wellness$ab[i], each = authors_length[i])
}

all_abstracts <- c()
for(i in 1:length(abstract)){
  all_abstracts <- c(all_abstracts,abstract[[i]])
}

# update the authors data frame
{
  authorAtt_df$Publication <- all_publications
  authorAtt_df$`Abstract` <- all_abstracts
  authorAtt_df$`Publication Type` <- all_type
  authorAtt_df$`Publication Year` <- all_pub_year
  authorAtt_df$`Cited By Count` <- all_citedby_count
  authorAtt_df$`Host Organization` <- all_hostOrg
  authorAtt_df$SO <- all_so
  
}

authorAtt_df |> view()

# filter out NAs
authorAtt_df <- authorAtt_df |> 
  na.omit()

# move abstract column to behind Publication
authorAtt_df <- authorAtt_df |> 
  relocate(Abstract,.after = Publication)

# write to csv
write.csv(authorAtt_df,file = "mental_wellness.csv",row.names = F)
