library(Biostrings)
library(janitor)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(writexl)
# Function ----------------------------------------------------------------####
extract_table <-function(DNA_file, AA_file){
  filenames<-unlist(strsplit(as.character(DNA_file), "/"))
  filenames<-filenames[grep("fna", filenames)]
  mag_name<-unlist(strsplit(filenames, "\\.fna"))
  mag_name<-mag_name[1]
  termination<-unlist(strsplit(filenames, "\\.f"))
  #termination<-".fna"
  mag_name<-termination[1]
  #mag_name<-termination
  
  dna<-readDNAStringSet(DNA_file)
  size<-as_tibble(sum(nchar(dna))) %>%
    dplyr::rename(Genome_size = value)
  gc <-
    dna %>%
    letterFrequency("GC", as.prob = TRUE) %>%
    tibble::as_tibble() %>%
    clean_names() %>%
    summarize(GC = mean(g_c))
  
  AA<-readAAStringSet(AA_file)
  Coding_sequences<-as_tibble(length(AA)) %>%
    dplyr::rename(Coding_sequences = value)
  
  first<-bind_cols(size, gc, Coding_sequences)
  rownames(first)<-mag_name 
  second_1<-first %>%
    rownames_to_column() %>%
    dplyr::rename(Bin_name = rowname)
  
  return(second_1)
}

# Extract the paths for the genome and proteome files ---------------------####
genomes<-list.files("data/Bin_example/Genome/")
genomes_files<-paste0("/Users/kiley/Documents/Bin_visualization/data/Bin_example/Genome/", genomes)

proteimes<-list.files("data/Bin_example/Proteome/")
proteimes_files<-paste0("/Users/kiley/Documents/Bin_visualization/data/Bin_example/Proteome/", proteimes)

# Use map to create the table for multiple files --------------------------####
genome_statistics<-map2_df(genomes_files, proteimes_files, extract_table) #%>%
  #rename(Short_name = Bin_name )
# Write a excel file ------------------------------------------------------####
write_xlsx(genome_statistics, path ="data/genome_statistics.xlsx")
