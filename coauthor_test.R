#geonames script used co_authors_full_info

co_authors_no_orcid <- co_authors_full_info |> 
  filter(orcid2 == "")

# for first test, 300 rows
# query orcid by doi

co_authors_no_orcid_dois <- co_authors_no_orcid |> 
  distinct(doi)

str(co_authors_no_orcid_dois)
doi_search <- as.list(co_authors_no_orcid_dois$doi)


#89 dois with missing co_authors

doi_search_results <- orcid_doi(dois=doi_search)

#this gives us ORCIDS that claim this DOI (at least one ORCID in each list should by MtA)
#can I use this to create a compound key? each list is a coauthorship with one known ORCID

doi_orcids <- doi_search_results %>%
  map_dfr(., as_tibble) %>%
  janitor::clean_names()
  
doi_orcids_unique <- doi_orcids |> 
  distinct(orcid_identifier_path)

# 141 remove mta authors
#   doi_orcids_unique <- as.data.frame(doi_orcids_unique)
#   unique_orcids_df <- as.data.frame(unique_orcids)
#   
#   str(doi_orcids_unique)
#   str(unique_orcids_df)
#   
# missing_coauths <- doi_orcids_unique |> 
#   anti_join(doi_orcids_unique, unique_orcids_df, join_by(orcid_identifier_path == unique_orcids))
  
orcid_search2 <- as.list(doi_orcids_unique$orcid_identifier_path)

my_orcid_person2 <- rorcid::orcid_person(orcid_search2)


my_orcid_person_data2 <- my_orcid_person2 %>% {
  dplyr::tibble(
    given_name = purrr::map_chr(., purrr::pluck, "name", "given-names", "value", .default=NA_character_),
    created_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_integer_),
    last_modified_date = purrr::map_chr(., purrr::pluck, "name", "created-date", "value", .default=NA_character_),
    family_name = purrr::map_chr(., purrr::pluck, "name", "family-name", "value", .default=NA_character_),
    credit_name = purrr::map_chr(., purrr::pluck, "name", "credit-name", "value", .default=NA_character_),
    other_names = purrr::map(., purrr::pluck, "other-names", "other-name", "content", .default=NA_character_),
    orcid_identifier_path = purrr::map_chr(., purrr::pluck, "name", "path", .default = NA_character_),
    biography = purrr::map_chr(., purrr::pluck, "biography", "content", .default=NA_character_),
    researcher_urls = purrr::map(., purrr::pluck, "researcher-urls", "researcher-url", .default=NA_character_),
    emails = purrr::map(., purrr::pluck, "emails", "email", "email", .default=NA_character_),
    keywords = purrr::map(., purrr::pluck, "keywords", "keyword", "content", .default=NA_character_),
    external_ids = purrr::map(., purrr::pluck, "external-identifiers", "external-identifier", .default=NA_character_))
} %>%
  dplyr::mutate(created_date = anytime::anydate(as.double(created_date)/1000),
                last_modified_date = anytime::anydate(as.double(last_modified_date)/1000))


#now left join

my_orcid_person_data3 <- my_orcid_person_data2 |> 
  select(c(given_name, family_name, orcid_identifier_path)) |> 
  rename(lname2 = family_name)

possible_coauthors <- left_join(co_authors_no_orcid, my_orcid_person_data3)

write_csv(possible_coauthors, "./data/possible_coauthors.csv")
