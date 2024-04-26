#setwd("D:/DATA/MEGAsync/2024/Monev/Monev SAKTI/R_report_output")

### Library ####
library(googlesheets4)
library(tidyverse)
library(lubridate)
library(plotly)
library(reactable)
library(htmltools)

### Input Data ####
gs4_deauth()
do_output <- read_sheet("https://docs.google.com/spreadsheets/d/1xOzRQA1ZLS0lf5zhMJkh_pSsiIq9kNybp8sqoA4uPqo/edit?usp=sharing", sheet = "KRO dan RO") 
target_output <- read_sheet("https://docs.google.com/spreadsheets/d/1xOzRQA1ZLS0lf5zhMJkh_pSsiIq9kNybp8sqoA4uPqo/edit?usp=sharing", sheet = "Target", range = "A1:H61")
gabungan_output <- read_sheet("https://docs.google.com/spreadsheets/d/1xOzRQA1ZLS0lf5zhMJkh_pSsiIq9kNybp8sqoA4uPqo/edit?usp=sharing", sheet = "Gabungan", range = "A1:N61")

#### Definisi Operasional RO Dit. SPO ####
#glimpse(do_output)
df_do <- do_output %>% select(`Klasifikasi Rincian Output`, `Rincian Output`, `Definisi Operasional`, Satuan, `Cara Perhitungan`)

#### Tabel Informasi Target PCRO dan RVRO ####
#glimpse(target_output)
target_output$Bulan <- format(target_output$Bulan, "%m-%B")
target_output$`Target PCRO Akumulasi` <- round(target_output$`Target PCRO Akumulasi`,2)
df1 <- target_output %>% select(`Klasifikasi Rincian Output`, `Rincian Output`, Bulan, `Target RVRO Akumulasi`, `Target PCRO Akumulasi`)
df1 <- df1 %>% pivot_longer(4:5, names_to = "Jenis Target", values_to = "Target")

df_target <- df1 %>% pivot_wider(names_from = Bulan, values_from = Target)
#writexl::write_xlsx(df_target,"Tabel Informasi Target PCRO dan RVRO.xlsx")

#### Tabel Realisasi PCRO dan RVRO ####
#glimpse(gabungan_output)
gabungan_output$Bulan <- format(gabungan_output$Bulan, "%m-%B")
#gabungan_output[,-9] %>% drop_na(Bulan) %>% filter(Bulan %in% "02-February")
df_gabungan <- gabungan_output[,-9] %>% drop_na(Bulan)
df_gabungan$`Realisasi PCRO Akumulasi` <- round(df_gabungan$`Realisasi PCRO Akumulasi`,2)
df_gabungan$Capaian <- round(df_gabungan$Capaian,2)
df_gabungan$Gap <- round(df_gabungan$Gap,2)

#### Grafik TPCRO dengan PCRO untuk setiap Rincian Output ####
target_df1 <- target_output %>% select(`Rincian Output`, Bulan, `Target PCRO Akumulasi`)
realisasi_df1 <- gabungan_output %>% select(`Rincian Output`, Bulan, `Realisasi PCRO Akumulasi`)

graph <- left_join(target_df1, realisasi_df1, by = c("Rincian Output", "Bulan"))
long_graph <- graph %>% pivot_longer(3:4, names_to = "Nilai", values_to = "Persen")
#membuat grafik
glimpse(graph)
long_graph$Persen <- round(long_graph$Persen,2)
long_graph$`Rincian Output` <- factor(long_graph$`Rincian Output`, 
                                 levels = c("001 - Rekomendasi Kebijakan Keamanan, Mutu, Gizi dan Manfaat Pangan Olahan yang Diselesaikan",
                                            "001 - Standar Pangan Olahan yang Disusun",
                                            "001 - Sarana Pengawasan Pangan Olahan",
                                            "001 - Perangkat pengolah data dan komunikasi",
                                            "001 - Masyarakat yang ditingkatkan pengetahuannya melalui KIE"),
                                 labels = c("001 - Rekomendasi Kebijakan Keamanan, Mutu, Gizi dan Manfaat\nPangan Olahan yang Diselesaikan",
                                            "001 - Standar Pangan Olahan yang Disusun",
                                            "001 - Sarana Pengawasan Pangan Olahan",
                                            "001 - Perangkat pengolah data dan komunikasi",
                                            "001 - Masyarakat yang ditingkatkan pengetahuannya\nmelalui KIE"))
long_graph$Nilai <- factor(long_graph$Nilai, 
                          levels = c("Target PCRO Akumulasi","Realisasi PCRO Akumulasi"))
p <- ggplot(long_graph, aes(x = Bulan, y = Persen, fill = Nilai, label = Persen)) +
  geom_bar(position = "dodge", stat = "identity") +
  ylim(0,120) + 
  geom_text(size = 3, position = position_dodge(width = .9), vjust = -1.5, color = "black") +
  scale_fill_manual(values = c("#feb24c","#f03b20")) +
  facet_wrap(~`Rincian Output`,ncol = 2) +
  theme_bw()
p <- p + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#pict1 <- ggplotly(p)  

#### Grafik TRVRO dengan RVRO untuk setiap Rincian Output ####
target_df2 <- target_output %>% select(`Rincian Output`, Bulan, `Target RVRO Akumulasi`)
realisasi_df2 <- gabungan_output %>% select(`Rincian Output`, Bulan, `Realisasi RVRO Akumulasi`)

graph2 <- left_join(target_df2, realisasi_df2, by = c("Rincian Output", "Bulan"))
long_graph2 <- graph2 %>% pivot_longer(3:4, names_to = "Nilai", values_to = "Total")
#View(long_graph2)

#membuat grafik
#glimpse(graph2)
long_graph2$Total <- round(long_graph2$Total,2)
long_graph2$`Rincian Output` <- factor(long_graph2$`Rincian Output`, 
                                      levels = c("001 - Rekomendasi Kebijakan Keamanan, Mutu, Gizi dan Manfaat Pangan Olahan yang Diselesaikan",
                                                 "001 - Standar Pangan Olahan yang Disusun",
                                                 "001 - Sarana Pengawasan Pangan Olahan",
                                                 "001 - Perangkat pengolah data dan komunikasi",
                                                 "001 - Masyarakat yang ditingkatkan pengetahuannya melalui KIE"),
                                      labels = c("001 - Rekomendasi Kebijakan Keamanan, Mutu, Gizi dan Manfaat\nPangan Olahan yang Diselesaikan",
                                                 "001 - Standar Pangan Olahan yang Disusun",
                                                 "001 - Sarana Pengawasan Pangan Olahan",
                                                 "001 - Perangkat pengolah data dan komunikasi",
                                                 "001 - Masyarakat yang ditingkatkan pengetahuannya\nmelalui KIE"))
long_graph2$Nilai <- factor(long_graph2$Nilai, 
                           levels = c("Target RVRO Akumulasi","Realisasi RVRO Akumulasi"))

p1 <- ggplot(long_graph2, aes(x = Bulan, y = Total, fill = Nilai, label = Total)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(size = 3, position = position_dodge(width = .9), vjust = -1.5, color = "black") +
  scale_fill_manual(values = c("#feb24c","#f03b20")) +
  facet_wrap(~`Rincian Output`,ncol = 2) +
  theme_bw()
p1 <- p1 + theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
p1
