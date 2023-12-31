#Libs
library(nnet)
library(daltoolbox, lib.loc = "/usr/local/lib/R/site-library")
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
  nome_coluna = paste(nome,"_disc", sep = "")
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
dados = cbind(dados_cm, dados)



#Modelo de Predição

set.seed(123)  # Definir uma semente para reprodução dos resultados
sample_index = sample(nrow(dados), 0.7 * nrow(dados))  # 70% para treinamento
train_data = dados[sample_index, ]
test_data = dados[-sample_index, ]

# Treinamento do modelo de redes neurais
model = nnet(class ~ Alcohol + Total_phenols + Nonflavanoid_phenols, data = train_data, size = 5)

# Fazer previsões no conjunto de teste
predicted_probs = round(predict(model, newdata = test_data, type = "raw"),8)
predicted_classes = colnames(predicted_probs)[apply(predicted_probs, 1, which.max)]

# Calcular a acurácia
accuracy = round(mean(predicted_classes == test_data$class),4)
cat("Acurácia:", accuracy, "\n")

#Dados reais X Dados da Previsão
results <- data.frame(Real = test_data$class, Previsão = predicted_classes)
print(results)
