# trabalho métodos de pesquisa

## pacotes necessários
require(dplyr)
require(ggplot2)
require(janitor)
require(MASS)
require(DescTools)
## carregar base de dados

dados <- tibble(read.csv('dados_pesquisa.csv'))

#arruma os nomes das colunas
dados <- clean_names(dados)



#cria fatores
dados$genero <- factor(dados$genero)
dados$regime_de_trabalho <- factor(dados$regime_de_trabalho)
#fator com niveis ordenados.
#dados$faixa_salarial <-
#   factor(
#      dados$faixa_salarial,
#     levels = c("1", "1-2", "2-4", "4-6", "6-8", "8"),
#    ordered = TRUE
#)

#cria uma reposta dicotomica
dados$avaliacao_trabalho_atual <-
    ifelse(dados$avaliacao_trabalho_atual >= 3, 1, 0)
dados$anos_na_area


# Análise exploratória
dados2 <- dados
dados2$avaliacao_trabalho_atual <-
    factor(
        dados2$avaliacao_trabalho_atual,
        levels = c(0, 1),
        labels = c('Não satisfeito', 'Satisfeito')
    )

# Figura 1
## Proporção por gênero.

ggplot(dados2, aes(x = genero, fill = avaliacao_trabalho_atual)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c(
        "Não satisfeito" = "red3",
        "Satisfeito" = "green4"
    )) +
    labs(title = 'Figura 1: Proporção da satisfação no trabalho atual por gênero',
         x = 'Gênerro',
         y = 'Proprção',
         fill = 'Satisfação no trabalho atual') +
    theme_bw()



sum(dados$avaliacao_trabalho_atual) / length(dados$avaliacao_trabalho_atual)



# Figura 2
ggplot(
    dados2,
    aes(x = avaliacao_trabalho_atual, y = anos_na_area, fill = avaliacao_trabalho_atual)
) +
    geom_boxplot() +
    labs(title = 'Figura 2: Boxplot do tempo em anos na área atual e a satisfação no trabalho',
         x = 'Satisfação no trabalho atual',
         y = 'Tempo em anos na área') +
    scale_fill_manual(values = c(
        "Satisfeito" = "green4",
        "Não satisfeito" = "red3"
    )) +
    theme_bw() + theme(legend.position = "none")


# Figura 3

ggplot(dados2, aes(x = faixa_salarial, fill = avaliacao_trabalho_atual)) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c(
        "Não satisfeito" = "red3",
        "Satisfeito" = "green4"
    )) +
    labs(title = 'Figura 3: Proporção da satisfação no trabalho atual por faixa salarial',
         x = 'Faixa Salarial (em salários mínimos)',
         y = 'Proprção',
         fill = 'Satisfação no trabalho atual') +
    theme_bw()


# Primeiro ajuste

ajuste <-
    glm(avaliacao_trabalho_atual ~ .,
        data = dados,
        family = 'binomial')

# Tabela 2
summary(ajuste)


pchisq(ajuste$deviance,
       df = ajuste$df.residual,
       lower.tail = FALSE)








stepAIC(ajuste, direction = "backward", trace = T)

ajuste_final <-
    glm(
        formula = avaliacao_trabalho_atual ~ trabalho_presencialmente +
            faixa_salarial + meses_no_trabalho_atual,
        family = "binomial",
        data = dados
    )


# Tabela 3
summary(ajuste_final)

modelo_nulo <-
    glm(avaliacao_trabalho_atual ~ 1,
        data = dados,
        family = 'binomial')

# Tabela 4
knitr::kable(
anova(modelo_nulo, ajuste_final, test = 'Chisq'))

# Tabela 5
pseudo_r2 <-
    PseudoR2(ajuste_final, which = c("CoxSnell", "Nagelkerke", "McFadden"))
knitr::kable(pseudo_r2)


# Figura 4
par(mfrow = c(2, 2))
plot(ajuste_final)

ggplot(data = data.frame(residuos), aes(sample = residuos)) +
    stat_qq() + stat_qq_line()

qqnorm(resid(ajuste_final), pch = 20, cex = 1.5)
qqline(resid(ajuste_final))


# Tabela 6
#Teste de Hosmer e Lemeshow para a qualidade do ajuste (calibração do ajuste)
knitr::kable(ResourceSelection::hoslem.test(x = dados$avaliacao_trabalho_atual,
                               y = fitted(ajuste_final)))



# Tabela 1

colnames(dados)
table(dados$regime_de_trabalho)
nomes <-
    c(
        "Satisfação no trabalho atual",
        "- Satisfeito",
        "- Não Satisfeito",
        "" ,
        
        "Regime de Trabalho:",
        "- Autônomo ",
        "- CLT",
        "- Estagiário",
        "- PJ",
        "- Outros",
        '',
        "Idade (em anos)",
        '',
        'Anos na área',
        '',
        'Gênero:',
        '- Feminino',
        '- Masculino',
        '- Outros',
        '',
        'Faixa Salarial (em salários mínimos):',
        '- Até 1',
        '- 1-2',
        '- 2-4',
        '- 4-6',
        '- 6-8',
        '- Mais que 8','',
        
        'Trabalha presencialmente:',
        '- Sim, todos dias',
        '- Sim, alguns dias',
        ' Não','',
        
        'Meses no trabalho atual:'
        
    )
valores <- c('', 65, 21, '', '', 5,61,8,2,10,
             
             '', " Valores númericos (inteiros)", '',
             " Valores númericos (inteiros)", '', '',
             
             50,34,2,'','',
             30,20,23,1,1,1,'','',
             64,18,4,'',
             'Valores númericos (inteiros)'
             
             
             
             
             )




data.frame(Variáveis = nomes, Valores=valores)


?knitr::kable()




