library(magrittr)

#### initialize package ####

system("trident init -p ../zenodo_v1/c1global1nfd_public.bed -o ../c1global1nfd_public")

#### compile janno context data table ####

# load data
fam <- readr::read_delim(
  "../zenodo_v1/c1global1nfd_public.fam",
  delim = " ", col_names = FALSE
)
indiv_meta <- readr::read_delim(
  "../zenodo_v1/c1global1nfd_public.indiv_meta",
  delim = " "
)
coord <- readr::read_delim(
  "../zenodo_v1/c1global1nfd_public.coord",
  delim = " ", col_names = c("long", "lat")
)

# check overlap and fix missmatches
setdiff(indiv_meta$IID, fam$X2)
setdiff(indiv_meta$FID, fam$X1)
# [1] "Czech Republic"
indiv_meta <- indiv_meta %>%
  dplyr::mutate(
    FID = dplyr::case_match(
      FID,
      "Czech Republic" ~ "Czech",
      .default = FID
    )
  )
setdiff(indiv_meta$FID, fam$X1)

# compile .janno file
janno <- tibble::tibble(
  Poseidon_ID = indiv_meta$IID,
  Genetic_Sex = dplyr::case_match(
    fam$X5,
    1 ~ "M",
    2 ~ "F",
    0 ~ "U"
  ),
  Group_Name  = purrr::map2_chr(
    indiv_meta$FID, indiv_meta$abbrev,
    \(x,y) paste(x, y, sep = ";")
  ),
  Group_Long = indiv_meta$FID,
  Group_Short = indiv_meta$abbrev,
  Group_Color = indiv_meta$color,
  Alternative_IDs = indiv_meta$originalID,
  Longitude   = coord$long,
  Latitude    = coord$lat,
  Publication = stringr::str_replace_all(indiv_meta$wasDerivedFrom, " ", "") %>%
    dplyr::case_match(
      .,
      "Hunter-Zincketal2010" ~ "HunterZincketal2010",
      .default = .
    ) %>%
    purrr::map_chr(
      .,
      \(x,y) paste(x, "Peteretal2019", "Shastryetal2025", sep = ";")
    )
)

# overwrite .janno file
readr::write_tsv(janno, "../c1global1nfd_public/c1global1nfd_public.janno")
system("trident validate -d ../c1global1nfd_public")

#### prepare .bib file ####

# manual work based on the papers.txt file in the archive
system("trident validate -d ../c1global1nfd_public")

#### add the usual meta information to the package ####

# add myself as contributor in POSEIDON.yml
# add a meaningful description in POSEIDON.yml
# add checksums
system("trident rectify -d ../c1global1nfd_public --checksumAll")

#### final check ####

system("trident validate -d ../c1global1nfd_public --fullGeno --debug")
