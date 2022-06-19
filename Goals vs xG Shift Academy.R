## Load package yang dibutuhkan
pacman::p_load(ggplot2,
               ggtext,
               worldfootballR,
               dplyr,
               powerjoin,
               ggrepel,
               extrafont)

## Ambil dataframe untuk aspek shooting dari fbref.com ; untuk function2nya
## bisa dilihat di https://github.com/JaseZiv/worldfootballR
big5_player_shooting <-
  worldfootballR::fb_big5_advanced_season_stats(
    season_end_year = c(2022),
    stat_type = "shooting",
    team_or_player = "player"
  ) %>%
  ## Kita filter pemain yang menit bermainnya setidaknya 900 menit
  dplyr::filter(Mins_Per_90 >= 10)

## Load data posisi pemain yang lebih detail ; data harus dalam bentuk .csv
## File nanti dishare di github
player_pos <-
  utils::read.csv("C:/Users/Leonardus/Desktop/R stuff/Detailed Positions Players.csv",
                  sep = ";")

## Gabung kedua dataframe menggunakan inner join dari package powerjoin dan pilih statistik
## yang kita butuhkan ; disini kita akan menggunakan goals dan expected goals (xG)
shooting_df <- powerjoin::power_inner_join(big5_player_shooting[c(
  "Player",
  "Season_End_Year",
  "Squad",
  "Comp",
  "Nation",
  "Age",
  "Mins_Per_90",
  "Gls_Standard",
  "xG_Expected"
)],
player_pos[c("Player", "Main.Position")]) %>%
  ## Filter hanya untuk penyerang tengah saja (Centre-Forward)
  dplyr::filter(Main.Position == "Centre-Forward") %>%
  ## Sesuaikan statistik menjadi per 90
  dplyr::mutate(
    Gls_Per_90 = as.numeric(Gls_Standard / Mins_Per_90),
    xG_Per_90 = as.numeric(xG_Expected / Mins_Per_90),
    Gls_minus_xG_Per_90 = as.numeric(Gls_Per_90 - xG_Per_90)
  )

## Sekarang kita coba bikin plot (x,y) dari dataframe yg telah kita buat
p <- ggplot2::ggplot(data = shooting_df,
                     x = xG_Per_90,
                     y = Gls_Per_90) +
  ## Buat garis trendline dengan kemiringan (gradien) 1
  geom_abline(
    slope = 1,
    linetype = "dashed",
    colour = "lightgray",
    alpha = 0.25
  ) +
  ## Sekarang, kita plot poin-poinnya ; x = xG per 90, y = Goals per 90
  geom_point(
    data = shooting_df %>%
      ## Disini kita ingin filter hanya untuk pemain yang
      ## 5% tertinggi dan terendah untuk :
      ## a. Goals per 90
      ## b. xG per 90
      ## c. Goals - xG per 90
      filter(
        Gls_Per_90 >= quantile(Gls_Per_90, c(.95))
        | xG_Per_90 >= quantile(xG_Per_90, c(.95))
        |
          Gls_minus_xG_Per_90 >= quantile(Gls_minus_xG_Per_90, c(.95))
        | Gls_Per_90 <= quantile(Gls_Per_90, c(.05))
        | xG_Per_90 <= quantile(xG_Per_90, c(.05))
        |
          Gls_minus_xG_Per_90 <= quantile(Gls_minus_xG_Per_90, c(.05))
      ),
    aes(x = xG_Per_90,
        y = Gls_Per_90,
        ## Warna poin dibedakan dari liga
        color = Comp)
  ) +
  geom_point(
    data = shooting_df %>%
      ## Disini kita ingin filter (lagi) hanya untuk pemain yang
      ## SELAIN 5% tertinggi dan terendah untuk :
      ## a. Goals per 90
      ## b. xG per 90
      ## c. Goals - xG per 90
      filter(
        Gls_Per_90 <= quantile(Gls_Per_90, c(.95))
        | xG_Per_90 <= quantile(xG_Per_90, c(.95))
        |
          Gls_minus_xG_Per_90 <= quantile(Gls_minus_xG_Per_90, c(.95))
        | Gls_Per_90 >= quantile(Gls_Per_90, c(.05))
        | xG_Per_90 >= quantile(xG_Per_90, c(.05))
        |
          Gls_minus_xG_Per_90 >= quantile(Gls_minus_xG_Per_90, c(.05))
      ),
    aes(x = xG_Per_90,
        y = Gls_Per_90,
        color = Comp),
    ## Alpha ini mengatur seberapa transparan poin tsb
    ## Kita hanya ingin menegaskan pemain yang menonjold dari sisi statistik
    ## Maka itu, kita buat poin ini lebih transparan
    alpha = 0.15
  ) +
  ggrepel::geom_text_repel(
    data = shooting_df %>%
      ## Disini kita ingin filter (lagi) hanya untuk pemain yang
      ## 5% tertinggi dan terendah untuk :
      ## a. Goals per 90
      ## b. xG per 90
      ## c. Goals - xG per 90
      filter(
        Gls_Per_90 >= quantile(Gls_Per_90, c(.95))
        | xG_Per_90 >= quantile(xG_Per_90, c(.95))
        |
          Gls_minus_xG_Per_90 >= quantile(Gls_minus_xG_Per_90, c(.95))
        | Gls_Per_90 <= quantile(Gls_Per_90, c(.05))
        | xG_Per_90 <= quantile(xG_Per_90, c(.05))
        |
          Gls_minus_xG_Per_90 <= quantile(Gls_minus_xG_Per_90, c(.05))
      ),
    aes(x = xG_Per_90,
        y = Gls_Per_90,
        ## Label dengan nama masing-masing pemain
        label = Player),
    ## Setiap label diberikan garis, maka min.segment.length = 0
    min.segment.length = 0,
    family = "Montserrat",
    colour = "lightgray"
  ) +
  ggplot2::theme_minimal() +
  ## Ini bisa diutak-atik sendiri, kalian lihat dari setiap function apa yang berubah
  ggplot2::theme(
    text = element_text(family = "Montserrat"),
    title = element_text(size = 14),
    legend.position = "top",
    plot.background = element_rect(fill = "gray15", colour = "gray15"),
    panel.background = element_rect(fill = "gray15", colour = "gray15"),
    legend.title = element_blank(),
    legend.text = element_text(colour = "lightgray", size = 8),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(colour = "lightgray"),
    axis.text = element_text(colour = "lightgray"),
    axis.text.x = element_text(margin = margin(t = 5, b = 5)),
    axis.text.y = element_text(margin = margin(l = 5, r = 5)),
    plot.title = element_text(
      colour = "lightgray",
      hjust = 0,
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      colour = "lightgray",
      hjust = 0,
      face = "bold",
      size = 8
    ),
    plot.caption = element_markdown(
      colour = "lightgray",
      hjust = 1,
      size = 8
    ),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  ) +
  ## Mengatur batas-batas angka di axis x dan y
  ggplot2::scale_x_continuous(limits = c(0, max(shooting_df$xG_Per_90)),
                              breaks = seq(0, 1, by = 0.25)) +
  ggplot2::scale_y_continuous(limits = c(0, max(shooting_df$Gls_Per_90)),
                              breaks = seq(0, 1, by = 0.25)) +
  ## Menulis judul, subjudul dan keterangan dari plot yang telah dibuat
  ggplot2::labs(
    title = "Bundesliga, Liganya Striker Top Eropa",
    subtitle = "Top 5 European Leagues | 2021-22",
    caption = "by **@myusufraihan** & **@ruangtaktik** | Data: Statsbomb via fbref.com",
    y = "Goals per 90",
    x = "xG per 90"
  )

p

##
ggplot2::ggsave(p,
                file = "C:/Users/Leonardus/Desktop/R stuff/Shift Academy/Goals vs xG.png",
                dpi = 300,
                device = 'png')
