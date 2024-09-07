## @knitr librerie ----------------------------------------------------------------


## @knitr CHOW TEST --------------------------------------------------

subset_sud <- dataset  |>  
  filter(region ==  1)

subset_non_sud <- dataset |> 
  filter(region != 1)

chow.test(y1 = as.matrix(subset_sud$esl), 
          x1 = as.matrix(subset_sud[, 4:9]), 
          y2 = as.matrix(subset_non_sud$esl), 
          x2 = as.matrix(subset_non_sud[, 4:9])) |> 
  t() |> 
  kable(digits = 3)


## @knitr modello multiplo completo con esl-----------------------------------------------------------------

#centratura esplicative

for (i in 4:9) {
  dataset[,i] <- dataset[,i] - mean(dataset[,i])
}

lmfittot <- lm(esl ~ ., dataset[, 3: ncol(dataset)])
lmfittot|> 
  as_flextable() 


## @knitr residualsvsleverage
dataset[8,2] <- "Region of Bruxelles"
plot(lmfittot, which = 5, labels.id = paste(dataset$nuts_name,"(", dataset$nuts_id, ")"), caption = FALSE, sub.caption = "")

## @knitr qqplot
plot(lmfittot, which = 2, labels.id = paste(dataset$nuts_name,"(", dataset$nuts_id, ")"), caption = "", sub.caption = "")

## @knitr normale
plot(density(rstandard(lmfittot)), main = "")
curve(dnorm(x,0,1),add=TRUE,col="blue")

## @knitr tests
shapiro <- shapiro.test(rstandard(lmfittot))
jarque <- jarque.bera.test(rstandard(lmfittot))
ks <- ks.test(rstudent(lmfittot), "pnorm")

c("Shapiro-Wilk" = shapiro$p.value, "Jarque Bera" = jarque$p.value, "Kolmogorov-Smirnov" = ks$p.value) |> 
  t() |> 
  kable(digits = 3)
  

## @knitr residualsvsfitted
plot(lmfittot, which = 1, labels.id = paste(dataset$nuts_name,"(", dataset$nuts_id, ")"), caption = "", sub.caption = "")

## @knitr breusch
bp <- bptest(lmfittot) 
c("BP" = bp$statistic, "df" = bp$parameter, "p-value" = bp$p.value) |> 
  t() |> 
  kable(digits = 3)

## @knitr fgls
predicted_variance <- lm(I(resid(lmfittot)^2) ~ ., dataset[, 4: ncol(dataset)]) |> 
  fitted()
lmfittot_fgls <- lm(esl ~ ., dataset[, 3: ncol(dataset)], weights = (1 / predicted_variance))
lmfittot_fgls|> 
  as_flextable() 


## @knitr fgls_shapiro_bp_dw
bp <- bptest(lmfittot_fgls)
shapiro <- shapiro.test(rstandard(lmfittot_fgls))
bw <- durbinWatsonTest(lmfittot_fgls)
c("Shapiro-Wilk Test p-value" = shapiro$p.value, "Breusch-Pagan Test p-value" = bp$p.value, "Durbin-Watson p-value" = bw$p) |> 
  t() |> 
  kable(digits = 3)


## @knitr redis_map
map_data <- map_data  |> 
  mutate(olsresid = resid(lmfittot))  

tm_shape(geodata,
         projection = "EPSG:3035",
         xlim = c(2400000, 7000000),
         ylim = c(1320000, 5650000)) +
  tm_fill("lightgrey") +
  tm_shape(map_data, unit = "mi") +
  tm_polygons(col = "olsresid", style = "quantile", palette = "-RdBu", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"))


## @knitr sampled_dataset_test
set.seed(123)
sampled_dataset <- dataset[sample(nrow(dataset), 100, replace = FALSE), ]
lmfittot_sd <- lm(esl ~ ., data = sampled_dataset[, 3: ncol(dataset)])

shapiro <- shapiro.test(rstandard(lmfittot_sd))
bp <- bptest(lmfittot_sd) 
bw <- durbinWatsonTest(lmfittot_sd)
c("Shapiro-Wilk" = shapiro$p.value, "Breusch-Pagan" = bp$p.value, "Durbin-Watson" = bw$p) |> 
  t() |> 
  kable(digits = 3)


## @knitr confronto_tot
stargazer(lmfittot, lmfittot_fgls, lmfittot_sd,
          column.labels = c("OLS S.E.", "FGLS", "OLS S.D."),
          type = "text",
          keep.stat = c("n","rsq")) 


## @knitr vif
cbind("OLS" = vif(lmfittot), "FGLS" = vif(lmfittot_fgls), "OLS S.D." = vif(lmfittot_sd)) |> 
  kable(digits = 3)


## @knitr best subset -------------------------------------------------------------

regfit.full<-regsubsets(esl ~ disp_inc + density + unemployment + HRST + amLSE + tourism + holydays_gr_10w + region, dataset[, 3: ncol(dataset)])
reg.summary<-summary(regfit.full)

plot(regfit.full, scale="adjr2")
points(3, 4, col = "red", pch = 19, cex = 1.5)
points(5, 4, col = "red", pch = 19, cex = 1.5)
points(6, 4, col = "red", pch = 19, cex = 1.5)
points(9, 4, col = "red", pch = 19, cex = 1.5)


## @knitr bic
plot(regfit.full,scale="bic")
points(3, 7, col = "red", pch = 19, cex = 1.5)
points(5, 7, col = "red", pch = 19, cex = 1.5)
points(6, 7, col = "red", pch = 19, cex = 1.5)
points(9, 7, col = "red", pch = 19, cex = 1.5)


## @knitr modello completo con variabili selezionate ----------------------------------------

lmfitsel <- lm(esl ~ density + amLSE + HRST + region, data = dataset)

lmfitsel |> 
  as_flextable()

## @knitr residualsvsleverage_sel
plot(lmfitsel, which = 5, labels.id = paste(dataset$nuts_name,"(", dataset$nuts_id, ")"), caption = FALSE, sub.caption = "")

## @knitr qqplot_sel
plot(lmfitsel, which = 2, labels.id = paste(dataset$nuts_name,"(", dataset$nuts_id, ")"), caption = "", sub.caption = "")

## @knitr normale_sel
plot(density(rstandard(lmfitsel)), main = "")
curve(dnorm(x,0,1),add=TRUE,col="blue")

## @knitr tests_sel
shapiro <- shapiro.test(rstandard(lmfitsel))
jarque <- jarque.bera.test(rstandard(lmfitsel))
ks <- ks.test(rstudent(lmfitsel), "pnorm")

c("Shapiro-Wilk" = shapiro$p.value, "Jarque Bera" = jarque$p.value, "Kolmogorov-Smirnov" = ks$p.value) |> 
  t() |> 
  kable(digits = 3)

## @knitr test_su_un_gruppo_di_parametri
test_su_un_gruppo_di_parametri <- anova(lmfitsel, lmfittot)
  c("RSS of the Reduct Model" = test_su_un_gruppo_di_parametri$Res.Df[1], "RSS of the Complete Model" = test_su_un_gruppo_di_parametri$Res.Df[2], "p-value" = test_su_un_gruppo_di_parametri$`Pr(>F)`[2]) |> 
  kable(digits = 3)

## @knitr residualsvsfitted_sel
plot(lmfitsel, which = 1, labels.id = paste(dataset$nuts_name,"(", dataset$nuts_id, ")"), caption = "", sub.caption = "")
  

## @knitr breusch_sel
bp <- bptest(lmfitsel) 
c("BP" = bp$statistic, "df" = bp$parameter, "p-value" = bp$p.value) |> 
  t() |> 
  kable(digits = 3)


## @knitr autocorrelazione  (Durbin-Watson test)_sel
bw <- durbinWatsonTest(lmfitsel)
c("Autocorrelation" = bw$r, "D-W Statistic" = bw$dw, "p-value" = bw$p) |> 
  t() |> 
  kable(digits = 3)


## @knitr redis_map_sel
map_data <- map_data |> 
  mutate(olsresid = resid(lmfitsel))

tm_shape(geodata,
         projection = "EPSG:3035",
         xlim = c(2400000, 7000000),
         ylim = c(1320000, 5650000)) +
  tm_fill("lightgrey") +
  tm_shape(map_data, unit = "mi") +
  tm_polygons(col = "olsresid", style = "quantile", palette = "-RdBu", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 2, 4), text.size = 1, position = c("right", "bottom")) +
  tm_layout(legend.position = c("right", "top"))

## @knitr sampled_dataset_summary--------------------------------
set.seed(1)
sampled_dataset <- dataset[sample(nrow(dataset), 100, replace = FALSE), ]
lmfitsel_sd <- lm(esl ~ density + amLSE + HRST + region, data = sampled_dataset)
lmfitsel_sd |> 
  as_flextable()


## @knitr sampled_dataset_test_sel 
shapiro <- shapiro.test(rstandard(lmfitsel_sd))
bp <- bptest(lmfitsel_sd) 
bw <- durbinWatsonTest(lmfitsel_sd)
c("Shapiro-Wilk" = shapiro$p.value, "Breusch-Pagan" = bp$p.value, "Durbin-Watson" = bw$p) |> 
  t() |> 
  kable(digits = 3)


## @knitr vif_sel
cbind("OLS" = vif(lmfitsel), "OLS S.D." = vif(lmfitsel_sd)) |> 
  t() |> 
  kable(digits = 3)


## @knitr sampled_dataset_test_sel_for --------------------------------

sw_cnt <- 0
bp_cnt <- 0
dw_cnt <- 0

set.seed(1)
for (i in 1:100) {
  sampled_dataset <- dataset[sample(nrow(dataset), 100, replace = FALSE), ]
  lmfitsel_sd <- lm(esl ~ density + amLSE + HRST + region, data = sampled_dataset)

  shapiro <- shapiro.test(rstandard(lmfitsel_sd))
  if (shapiro$p.value > 0.05) {
    sw_cnt <- sw_cnt + 1
    
    bp <- bptest(lmfitsel_sd)
    if (bp$p.value > 0.05) {
      bp_cnt <- bp_cnt + 1
    }
    
    dw <- durbinWatsonTest(lmfitsel_sd)
    if (dw$p > 0.05) {
      dw_cnt <- dw_cnt + 1
    }
  }
}

data.frame(
  Test = c("Shapiro-Wilk", "Breusch-Pagan", "Durbin-Watson"),
  `Number of Times p-value > 0.05` = c(sw_cnt, bp_cnt, dw_cnt)
) |> 
  kable(digits = 3)


## @knitr regressione quantile ----------------------------------------------------

qrfit <- rq(esl ~ amLSE + region ,tau=c(0.1,0.25,0.5,0.75,0.9),data=dataset)
summary(qrfit)
plot(qrfit)

