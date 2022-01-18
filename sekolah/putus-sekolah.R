
library(tidyverse)
library(magick)
library(scales)
library(extrafont)

df <- read.csv("~/visualisasi-mingguan/sekolah/number-of-out-of-school-children.csv")

df_idn <- df %>% 
  filter(Entity == "Indonesia") %>% #filter data dari indonesia 
  drop_na() %>% #filter data no value
  select(-c("Code")) %>% 
  gather(key = type, value = count, -c("Entity", "Year")) %>% 
  mutate(lab = case_when(
    type == "Out.of.school.children.of.primary.school.age..female..number."~"SD, perempuan",
    type == "Out.of.school.children.of.primary.school.age..male..number."~"SD, laki-laki",
    type == "Out.of.school.adolescents.of.lower.secondary.school.age..female..number."~"SMP, perempuan",
    type == "Out.of.school.adolescents.of.lower.secondary.school.age..male..number."~"SMP, laki-laki",
    type == "Out.of.school.youth.of.upper.secondary.school.age..female..number." ~ "SMA, perempuan",
    type == "Out.of.school.youth.of.upper.secondary.school.age..male..number." ~ "SMA, laki-laki", 
  ))

df_idn$lab <- factor(
  df_idn$lab, 
  c("SD, perempuan",
    "SD, laki-laki",
    "SMP, perempuan",
    "SMP, laki-laki",
    "SMA, perempuan",
    "SMA, laki-laki"
  ))

final1 <-  df_idn %>% 
  filter(Year == "2014") %>% 
  arrange(desc(lab)) %>% 
  mutate(
    ypos = cumsum(count))


pal <-  c("#FF4C29", "#f18336", "#e7c370", "#00847e", "#3c4e66", "#082032")

df_idn <- df_idn %>%
  mutate(
    col_lab = case_when(
      lab == "SD, perempuan"~"#FF4C29",
      lab == "SD, laki-laki"~"#f18336",
      lab == "SMP, perempuan"~"#e7c370",
      lab == "SMP, laki-laki"~"#00847e",
      lab == "SMA, perempuan"~"#3c4e66",
      lab == "SMA, laki-laki"~"#082032"
    ))



# Plot --------------------------------------------------------------------




finalplot <-  ggplot(df_idn, aes(x = Year, y = count, fill = lab)) +
  geom_area() +
  scale_x_continuous(limits = c(2001, 2015),
                     breaks = c(2001, 2003, 2005, 2007, 2009, 2012, 2014)) +
  scale_y_continuous(breaks = seq(0, 14000000, by = 2000000), #Tentukan skala y
                     labels = c("0", "2 Juta", "4 Juta", "6 Juta", "8 Juta", 
                                "10 Juta", "12 Juta", "14 Juta")) +
  scale_fill_manual(breaks = df_idn$lab,
                    values = df_idn$col_lab) +
  scale_color_manual(breaks = df_idn$lab,
                     values = df_idn$col_lab) +
  labs(title = "Jumlah Anak Putus Sekolah di Indonesia, 2001-2014",
       subtitle = "Pada awal abad 21, terdapat 12.75 juta anak yang putus sekolah. Angka ini, \nseperti yang tampak di chart, mulai menurun seiring waktu. Data tahun 2014 \nmenunjukkan terdapat 7.89 juta anak yang putus sekolah.",
       caption = "Source : UNESCO (Via Our World in Data) \nChart : Mely Santoso (@melysantoso)") +
  geom_text(data = final1, aes(y = ypos-650000, label = lab, color = lab), x =2014, hjust = 0, family = "Trebuchet MS", size = 4, fontface = "bold") +
  guides(fill = F, color = F) +
  theme(plot.title = element_text(family = "Geometr212 BkCn BT", face = "bold", size = 24, vjust = 1),
        plot.subtitle = element_text(color = 'grey30', size = 16, vjust = 1),
        plot.caption = element_text(color = 'grey40', size = 11, hjust = 0),
        legend.title=element_blank(), 
        #legend.position = c(0.8, 0.8),
        panel.background = element_rect(fill = "#f0f0f0"), # ganti backgound jadi abu-abu
        plot.background = element_rect(fill = "#f0f0f0"), # ganti background plot jadi abu-abu
        #legend.background = element_blank(), #element_rect(fill = "#f0f0f0"), # ganti background legend jadi abu-abu
        panel.border = element_blank(), # ilangin panel border
        panel.grid.major = element_blank(), # ngilangin grid border
        panel.grid.minor = element_blank(), # ngilangin grid border 
        #panel.background = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        #axis.text.x = element_blank(),
        #legend.key.size=unit(0.6, 'cm'),
        #legend.text=element_text(family = "Calibri", size=14, hjust = 0),
        axis.text=element_text(face = "bold", size=14))

finalplot

logo <- image_read("~/tidytuesday/ourworldindata/water access/weightaverage.png") #pake library(magick)
finalplot
grid::grid.raster(logo, x = 0.97, y = 0.99, just = c('right', 'top'), width = unit(1, 'inches')) #setting lokasi logo

