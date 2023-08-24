#Importação do Dados
dados = read.table("~/Prova de R/wine.data", sep = ",", header = TRUE,
                    col.names = c("class","Alcohol","Malicacid","Ash","Alcalinity_of_ash","Magnesium",
                                  "Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins",
                                  "Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline"))
dados

nome_col = c("Alcohol","Malicacid","Ash","Alcalinity_of_ash","Magnesium",
            "Total_phenols","Flavanoids","Nonflavanoid_phenols","Proanthocyanins",
            "Color_intensity","Hue","OD280_OD315_of_diluted_wines","Proline")

#Fazendo a media e o desvio padrão de todos vinhos

desvio_padrao = function(dataset){
  resultado = sapply(dataset, sd)
  return(resultado)
}
media = function(dataset){
  resultado = sapply(dataset, mean)
  return(resultado)
}



dadosstd = desvio_padrao(dados)
dadosstd
dadosmean  = media(dados)
dadosmean


subset_dados_1 = dados[dados$class == 1, ]
dados_subset_1_std = desvio_padrao(subset_dados_1)
dados_subset_1_std
dados_subset_1_mean  = media(subset_dados_1)
dados_subset_1_std

subset_dados_2 = dados[dados$class == 2, ]
dados_subset_2_std = desvio_padrao(subset_dados_2)
dados_subset_2_std
dados_subset_2_mean  = media(subset_dados_2)
dados_subset_2_mean

subset_dados_3 = dados[dados$class == 3, ]
dados_subset_3_std = desvio_padrao(subset_dados_3)
dados_subset_3_std
dados_subset_3_mean  = media(subset_dados_3)
dados_subset_3_mean

#plotagem da Linha de densidade.
dados$class = factor(dados$class)
options(repr.plot.width=8, repr.plot.height=5)
for (nome in nome_col) {
  grf = plot_density_class(dados |> dplyr::select(class,nome),class = "class",label_x ="Tipo de Vinho",label_y =nome ,colors=c("red", "green", "blue"))
  plot(grf)
}
#plotagem de boxplot

for (nome in nome_col) {
  grf = plot_boxplot_class(dados |> dplyr::select(class,nome),class = "class",label_x ="Tipo de Vinho",label_y =nome ,colors=c("red", "green", "blue"))
  plot(grf)
}

#plotagem dos Graficos de dispersão

for (nome in nome_col) {
  for (nome2 in nome_col) {
    if(nome!=nome2){
      grf = plot_scatter(dados |> dplyr::select(x = nome,
                                                value = nome2, variable = class),
                          label_x = nome, label_y = nome2,
                          colors=c("red", "green", "blue"))
      plot(grf)
    }
  }
}


#Discretização dos atributos numericos
for (nome in nome_col) {
  nome_coluna = paste(nome,"_discretizada", sep = "")
  limites = quantile(dados[[nome]], probs = c(0, 1/3, 2/3, 1))
  dados[nome_coluna] = as.factor(cut(dados[[nome]], breaks = limites,
                                          labels = c("Baixo", "Médio", "Alto"),
                                          include.lowest = TRUE))
}

#Mapeamento categorico do tipo de vinho
cm = categ_mapping("class")
dados_cm = transform(cm, dados)
ddados = dados[,"class", drop=FALSE]
dados_cm = transform(cm, ddados)

#Modelo de Agrupamento K-Means

# setup clustering
model = cluster_kmeans(k=13)

# build model
model = fit(model, dados[,2:14])
clu = cluster(model, dados[,2:14])
table(clu)

# evaluate model using external metric
eval = evaluate(model, clu, dados$class)
eval





