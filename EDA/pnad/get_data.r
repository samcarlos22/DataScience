library(PNADcIBGE)

variables <- c("Ano","UF", "V2007", "V2009", "V2010", "V3001", "VD3005", "VD4020", "VD4035", "V1028")
dadosPNADc <- get_pnadc(year = 2021, quarter = 1, vars = variables, design = FALSE)
dadosPNADc1 <- get_pnadc(year = 2020, quarter = 1, vars = variables, design = FALSE)
dadosPNADc2 <- get_pnadc(year = 2020, quarter = 2, vars = variables, design = FALSE)
dadosPNADc3 <- get_pnadc(year = 2020, quarter = 3, vars = variables, design = FALSE)
dadosPNADc4 <- get_pnadc(year = 2020, quarter = 4, vars = variables, design = FALSE)
dadosPNADc5 <- get_pnadc(year = 2019, interview = 5, design = FALSE)

write.csv(dadosPNADc, 'pnad2021_01.csv')
write.csv(dadosPNADc1, 'pnad2020_01.csv')
write.csv(dadosPNADc2, 'pnad2020_02.csv')
write.csv(dadosPNADc3, 'pnad2020_03.csv')
write.csv(dadosPNADc4, 'pnad2020_04.csv')
write.csv(dadosPNADc5, 'pnad2019.csv')