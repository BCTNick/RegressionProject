## @knitr librerie


## @knitr mappa esl ---------------------------------------------------------------

geodata <- get_eurostat_geospatial(nuts_level = 2)  |> 
  filter(CNTR_CODE != "AL") |> 
  filter(CNTR_CODE != "CH") |> 
  filter(CNTR_CODE != "IS") |> 
  filter(CNTR_CODE != "LI") |> 
  filter(CNTR_CODE != "ME") |> 
  filter(CNTR_CODE != "MK") |> 
  filter(CNTR_CODE != "NO") |> 
  filter(CNTR_CODE != "RS") |> 
  filter(CNTR_CODE != "TR") |> 
  filter(CNTR_CODE != "UK") 


map_data <- inner_join(geodata, dataset, by = c("geo" = "nuts_id" ))


map_esl <- tm_shape(geodata,
                    projection = "EPSG:3035",
                    xlim = c(2400000, 7000000),
                    ylim = c(1320000, 5650000)) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons("esl",
              palette = "Greens",
              title="(%)")+
  tm_layout(legend.position = c("right", "top"))

print(map_esl)


## @knitr boxplot esl -------------------------------------------------------------

dataset$region <- sapply(dataset$nuts_id, assign_region)
dataset$region <- factor(dataset$region,
                         levels = c("Southern Europe", "Western Europe", "Northern Europe", "Eastern Europe"))

mean_esl_by_c <-
  get_eurostat_json("edat_lfse_16",
                    filters = list(
                      geo = NUTS2$CNTR_CODE,
                      age = "Y18-24",
                      sex = "T",
                      time = 2023
                    )) |> 
  dplyr::select(country = geo, mean_esl_by_c = values)

dataplot <- dataset |> 
  left_join(NUTS2 [, c("geo", "CNTR_CODE")], by = c("nuts_id" = "geo"))  |>
  rename(country = CNTR_CODE) |>
  left_join(mean_esl_by_c, by = "country")

boxplot_esl <- ggplot(dataplot, aes(x = reorder(country, mean_esl_by_c), y = esl, fill = region)) +
  geom_boxplot() +
  geom_point(aes(y = mean_esl_by_c), color = "red", size = 3, shape = 18) + 
  labs(x = "Country",
       y = "Early School Leavers (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  theme(legend.position = c(0, 1),
          legend.justification = c(0, 1))

print(boxplot_esl)



## @knitr mappa italia esl --------------------------------------------------------

map_it_esl <- tm_shape(filter(geodata, CNTR_CODE == "IT"),
                       projection = "EPSG:3035",
                       xlim = c(4000000, 5100000),
                       ylim = c(1520000, 2650000)) +
  tm_fill("lightgrey") +
  tm_shape(filter(map_data, CNTR_CODE == "IT")) +
  tm_polygons("esl",
              title = "Early School\nLeavers (%)",
              palette = "Greens", border.col = "white")

map_it_esl


## @knitr mappa italia income --------------------------------------------------------

map_it_esl <- tm_shape(filter(geodata, CNTR_CODE == "IT"),
                       projection = "EPSG:3035",
                       xlim = c(4000000, 5100000),
                       ylim = c(1520000, 2650000)) +
  tm_fill("lightgrey") +
  tm_shape(filter(map_data, CNTR_CODE == "IT")) +
  tm_polygons("disp_inc",
              title = "Thousands of Euro",
              palette = "Oranges", border.col = "white")

map_it_esl


## @knitr mappa income ------------------------------------------------------------

map_inc <- tm_shape(geodata,
                 projection = "EPSG:3035",
                 xlim = c(2400000, 7000000),
                 ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons("disp_inc",
              palette = "Oranges",
              title="Thousands of Euro")+
  tm_layout(legend.position = c("right", "top"))


print(map_inc)


## @knitr scatter income ------------------------------------------------------------

map_data$region <- sapply(map_data$id, assign_region)
map_data$region <- factor(map_data$region,
                         levels = c("Southern Europe", "Western Europe", "Northern Europe", "Eastern Europe"))

scatterplot_disp_inc <- ggplot(map_data, aes(x = disp_inc, y = esl, color = region)) +
  geom_point(size = 2) +
  labs(
    title = paste('Correlation: ', round(cor(map_data$disp_inc, map_data$esl), 2)),
    x = 'Mean Disposable Income (Thousands of Euro)',
    y = 'Early School Leaving (%)',
    color = 'Region') +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

print(scatterplot_disp_inc)


## @knitr box_cox_disp_inc  ----------------------------------------------------------

lmfit_disp_inc <- lm(esl ~ disp_inc, dataset)
boxcox(lmfit_disp_inc, lambda = seq(0,1,by=0.01))
set.seed(123)
sampled_dataset <- dataset[sample(nrow(dataset), 100, replace = FALSE), ] |> 
  mutate(z_esl = (esl^0.5-1)-0.5,
         z_disp_inc = log(disp_inc))

lmfit_disp_inc <- lm(z_esl ~ z_disp_inc, sampled_dataset)


## @knitr model_disp_inc

lmfit_disp_inc |> 
  as_flextable()

## @knitr tests_disp_inc
shapiro <- shapiro.test(rstandard(lmfit_disp_inc))
bp <- bptest(lmfit_disp_inc) 
bw <- durbinWatsonTest(lmfit_disp_inc)

c("Shapiro-Wilk" = shapiro$p.value, "Breusch-Pagan" = bp$p.value, "Durbin-Watson" = bw$p) |> 
  t() |> 
  kable()


## @knitr mappa unemployment ------------------------------------------------------------

map_unemployment <- tm_shape(geodata,
                    projection = "EPSG:3035",
                    xlim = c(2400000, 7000000),
                    ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons("unemployment",
              palette = "Blues",
              title="Unemployment Rate")+
  tm_layout(legend.position = c("right", "top"))


print(map_unemployment)


## @knitr scatter unemployment ------------------------------------------------------------

scatterplot_unemployment <- ggplot(map_data, aes(x = unemployment, y = esl, color = region)) +
  geom_point(size = 2) +
  labs(
    title = paste('Correlation: ', round(cor(map_data$unemployment, map_data$esl), 2)),
    x = 'Unemployment Rate',
    y = 'Early School Leaving (%)',
    color = 'Region') +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

print(scatterplot_unemployment)


## @knitr tests_unemployment---------------------------------------------------------
lmfit_unemployment <- lm(z_esl ~ unemployment, sampled_dataset)

shapiro <- shapiro.test(rstandard(lmfit_unemployment))
bp <- bptest(lmfit_unemployment) 
bw <- durbinWatsonTest(lmfit_unemployment)
c("Shapiro-Wilk" = shapiro$p.value, "Breusch-Pagan" = bp$p.value, "Durbin-Watson" = bw$p) |> 
  t() |> 
  kable()


## @knitr model_unemployment
lmfit_unemployment |> 
  as_flextable()


## @knitr mappa HRST ------------------------------------------------------------

map_HRST <- tm_shape(geodata,
                             projection = "EPSG:3035",
                             xlim = c(2400000, 7000000),
                             ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons("HRST",
              palette = "Purples",
              title="HRST(%)")+
  tm_layout(legend.position = c("right", "top"))


print(map_HRST)


## @knitr scatter HRST ------------------------------------------------------------

scatterplot_HRST <- ggplot(map_data, aes(x = HRST, y = esl, color = region)) +
  geom_point(size = 2) +
  labs(
    title = paste('Correlation: ', round(cor(map_data$HRST, map_data$esl), 2)),
    x = 'HRST(%)',
    y = 'Early School Leaving (%)',
    color = 'Region') +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

print(scatterplot_HRST)


## @knitr tests_HRST---------------------------------------------------------
lmfit_HRST <- lm(z_esl ~ HRST, sampled_dataset)

shapiro <- shapiro.test(rstandard(lmfit_HRST))
bp <- bptest(lmfit_HRST) 
bw <- durbinWatsonTest(lmfit_HRST)
c("Shapiro-Wilk" = shapiro$p.value, "Breusch-Pagan" = bp$p.value, "Durbin-Watson" = bw$p) |> 
  t() |> 
  kable()


## @knitr model_HRST
lmfit_HRST |> 
  as_flextable()


## @knitr mappa amLSE ------------------------------------------------------------

map_amLSE <- tm_shape(geodata,
                     projection = "EPSG:3035",
                     xlim = c(2400000, 7000000),
                     ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons("amLSE",
              palette = "YlOrRd",
              title="(%)")+
  tm_layout(legend.position = c("right", "top"))


print(map_amLSE)


## @knitr scatter amLSE ------------------------------------------------------------

scatterplot_amLSE <- ggplot(map_data, aes(x = amLSE, y = esl, color = region)) +
  geom_point(size = 2) +
  labs(
    title = paste('Correlation: ', round(cor(map_data$amLSE, map_data$esl), 2)),
    x = 'amLSE(%)',
    y = 'Early School Leaving (%)',
    color = 'Region') +
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  theme(legend.position = c(1, 1),
        legend.justification = c(1, 1))

print(scatterplot_amLSE)


## @knitr model_amLSE -------------------------------------------------
lmfit_amLSE <- lm(z_esl ~ amLSE, sampled_dataset)

lmfit_amLSE |> 
  as_flextable()


## @knitr model_tourism-----------------------------------------
lmfit_tourism <- lm(z_esl ~ tourism, sampled_dataset)
lmfit_tourism |> 
  as_flextable()


## @knitr mappa holydays_gr_10w ------------------------------------------------------------

map_holidays_gr_10w <- tm_shape(geodata, projection = "EPSG:3035", xlim = c(2400000, 7000000), ylim = c(1320000, 5650000)) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons(
    col = "holydays_gr_10w", # Colonna con i dati da mappare
    palette = c("0" = "lightblue", "1" = "orange"), # Colori per i valori 0 e 1
    labels = c("<10", ">10"), # Etichette personalizzate per la legenda
    legend.format = list(text.separator = " - ") # Formattazione della legenda
  ) +
  tm_layout(legend.position = c("right", "top"))

print(map_holidays_gr_10w)


## @knitr model_holydays_gr_10w---------------------------------------------------------
lmfit_holydays_gr_10w <- lm(z_esl ~ holydays_gr_10w, sampled_dataset)

lmfit_holydays_gr_10w |> 
  as_flextable()


## @knitr graph_holydays_gr_10w---------------------------------------------------------
ggplot(dataset, aes(x = esl, fill = as.factor(holydays_gr_10w))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(
    values = c("0" = "lightblue", "1" = "orange"),  # Customize colors if desired
    labels = c("<10 weeks", ">10 weeks")  # Custom labels for legend
  ) +
  labs(
    x = "ESL",
    y = "Density",
    fill = "Holidays"
  ) +
  theme_minimal()


## @knitr mappa region ------------------------------------------------------------

map_region <- tm_shape(geodata,
                      projection = "EPSG:3035",
                      xlim = c(2400000, 7000000),
                      ylim = c(1320000, 5650000)
) +
  tm_fill("lightgrey") +
  tm_shape(map_data) +
  tm_polygons("region",
              palette = "Pastel2")+
  tm_layout(legend.position = c("right", "top"))


print(map_region)


## @knitr summary ---------------------------------------------------------

dataset |> 
  dplyr::select(-c(nuts_id, nuts_name, holydays_gr_10w, region)) |> 
  summary() |> 
  t() |> 
  kable()


## @knitr correlation matrix ------------------------------------------------------

dataset |> 
  dplyr::select(-c(nuts_id, nuts_name, holydays_gr_10w, region)) |> 
  ggpairs(upper = list(continuous = wrap("cor", size = 3)),
          lower = list(continuous = wrap("points", size = 0.5, alpha = 0.2)),
          diag = list(continuous = wrap("barDiag", binwidth = 1)))

