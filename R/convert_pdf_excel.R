library(openxlsx)
library(pdftools)
library(stringi)
library(dplyr)
library(stringr)
library(tidyverse)

fun_x <- function(dataSub){
  browser()
  dataSub <- dataSub[-c(1:3)]
  dataSub <- dataSub[-length(dataSub)]
  
  dataCon <- c()
  for(rr in 1:length(dataSub)){
    val <- dataSub[rr] %>% 
      trimws() %>% 
      gsub('\\s{2,}', '_', .) %>% 
      strsplit(., '_') %>% 
      .[[1]]
    last_v = length(val)
    rowd <- c(
      strsplit(val[1],' ')[[1]][1],
      strsplit(val[1],' ')[[1]][2:length(strsplit(val[1],' ')[[1]])] %>% paste(collapse = ''),
      
      val[2:(last_v-1)],
      
      strsplit(val[last_v],' ')[[1]][1],
      strsplit(val[last_v],' ')[[1]][2:length(strsplit(val[last_v],' ')[[1]])] %>% paste(collapse = '')
    ) 
    if(length(rowd) ==6){
      dataCon <- rbind(dataCon, rowd)
    }
  }
  
  dataCon = as.data.frame(dataCon)
  return(dataCon)
}

#get pdf path
pdf_file <- file.path("./data/pdf/Listas_Inhabilitados_Beni_EG_2020.pdf")

#Extract all data from PDF
data <- pdf_text(pdf_file)

data2 <- pdftools::pdf_data(pdf_file)

data <- str_split(data, '\n')

dataCon <- c()


st = Sys.time()

res <- lapply(data, fun_x) %>% 
  data.table::rbindlist() %>% 
  distinct()
Sys.time() - st


# Remove all rows 
df_a <- res 
df_a$V1 <- as.numeric(df_a$V1)

#remove all rows where V1 is na
df_a <- df_a %>% 
  drop_na()



write.xlsx(df_a, file="./data/excel/Listas_Inhabilitados_Beni_EG_2020.xlsx", sheetName = "Sheet1", 
           col.names = TRUE, row.names = FALSE, append = FALSE, overwrite = T)



